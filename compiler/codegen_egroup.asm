; codegen_egroup.asm - `except*`
;
; `except*` is a different statement from `except`, not a variant of one
; clause.  Where `except` picks the first clause that matches and stops,
; `except*` runs EVERY clause that matches part of an exception group, each on
; its own subgroup, and re-raises whatever no clause claimed.
;
; So the stack carries three things across the whole chain rather than one
; exception: the original group, a list of everything the clause bodies
; themselves raised, and the part not yet matched.  CHECK_EG_MATCH splits the
; remainder in two, and INTRINSIC_PREP_RERAISE_STAR at the end decides between
; falling through and re-raising what is left.
;
;     [handler -> clean, lasti]
;     PUSH_EXC_INFO            ; prev, exc
;     BUILD_LIST 0             ; prev, exc, list
;     COPY 2                   ; prev, exc, list, exc
;     for each clause:
;         <type>; CHECK_EG_MATCH        ; ..., rest, match
;         COPY 1; POP_JUMP_IF_NONE next
;         STORE name (or POP_TOP)       ; ..., rest
;         [handler -> raised, lasti]
;         <body>
;         [pop]; <clear name>; JUMP_FORWARD after
;       raised:  <clear name>; LIST_APPEND 3; POP_TOP; JUMP_FORWARD after
;       next:    POP_TOP
;       after:
;     LIST_APPEND 1            ; the unmatched remainder joins the list
;     CALL_INTRINSIC_2 PREP_RERAISE_STAR
;     COPY 1; POP_JUMP_IF_NOT_NONE reraise
;     POP_TOP; POP_EXCEPT; JUMP_FORWARD end
;   reraise: SWAP 2; POP_EXCEPT; RERAISE 0
;   clean:   COPY 3; POP_EXCEPT; RERAISE 1
;
; A clause body that raises does not stop the chain: its exception goes into
; the list and the next clause still gets a look at the remainder.  That is
; what the inner handler round each body is for, and why its region opens at
; the depth where `rest` is on top.

%include "macros.inc"
%include "object.inc"
%include "opcodes.inc"
%include "value.inc"
%include "compiler.inc"

extern ast_at
extern ast_child
extern ast_obj_at
extern cg_block
extern cg_clear_exc_name
extern cg_emit
extern cg_emit_jump
extern cg_exc_cleanup
extern cg_label_bind
extern cg_label_new
extern cg_nameop
extern cg_pop_handler
extern cg_push_handler
extern cg_finally_push
extern cg_finally_pop
extern cg_body
extern cg_expr
extern comp_error
extern exc_SyntaxError_type

CG_ESTAR_MARK equ 0x7ffffffd

global cg_except_star_clauses

; --- cg_except_star_clauses ---
ES_COMP  equ 8
ES_UNIT  equ 16
ES_NODE  equ 24
ES_END   equ 32
ES_LINE  equ 40
ES_H     equ 48
ES_I     equ 56
ES_N     equ 64
ES_CLEAN equ 72
ES_RERA  equ 80
ES_FRAME equ 88           ; + 3 pushes = 112

section .text

;; ============================================================================
;; cg_except_star_clauses(Comp *c, CompUnit *u, uint32_t try, uint64_t end)
;;   -> 1 ok, 0 error
;; ============================================================================
DEF_FUNC cg_except_star_clauses, ES_FRAME
    push rbx
    push r12
    push r13
    mov rbx, rdi
    mov r12, rsi
    mov r13, rdx
    mov [rbp - ES_END], rcx

    mov rdi, rbx
    mov rsi, r13
    call ast_at
    mov ecx, [rax + AstNode.lineno]
    mov [rbp - ES_LINE], rcx
    mov ecx, [rax + AstNode.a]          ; the block of clauses
    mov [rbp - ES_H], rcx
    test ecx, ecx
    jz .ok

    mov rdi, r12
    call cg_label_new
    mov [rbp - ES_CLEAN], rax
    mov rdi, r12
    mov rsi, rax
    mov edx, 1                          ; lasti
    call cg_push_handler

    mov rdi, r12
    mov esi, OP_PUSH_EXC_INFO
    xor edx, edx
    mov rcx, [rbp - ES_LINE]
    call cg_emit
    mov rdi, r12
    mov esi, OP_BUILD_LIST
    xor edx, edx
    mov rcx, [rbp - ES_LINE]
    call cg_emit
    ; The group itself stays beneath everything, because PREP_RERAISE_STAR
    ; needs it at the end to rebuild what is left.
    mov rdi, r12
    mov esi, OP_COPY
    mov edx, 2
    mov rcx, [rbp - ES_LINE]
    call cg_emit

    mov rdi, rbx
    mov rsi, [rbp - ES_H]
    call ast_at
    mov ecx, [rax + AstNode.nchild]
    mov [rbp - ES_N], rcx
    mov qword [rbp - ES_I], 0
.clause_loop:
    mov rax, [rbp - ES_I]
    cmp rax, [rbp - ES_N]
    jae .finish
    mov rdi, rbx
    mov rsi, [rbp - ES_H]
    call ast_at
    mov rsi, rax
    mov rdx, [rbp - ES_I]
    mov rdi, rbx
    call ast_child
    mov rdi, rbx
    mov rsi, r12
    mov rdx, rax
    call cg_one_star_except
    test eax, eax
    jz .fail
    inc qword [rbp - ES_I]
    jmp .clause_loop

.finish:
    ; Whatever no clause claimed joins the list of what the bodies raised, and
    ; the intrinsic decides between the two ways out.
    mov rdi, r12
    mov esi, OP_LIST_APPEND
    mov edx, 1
    mov rcx, [rbp - ES_LINE]
    call cg_emit
    mov rdi, r12
    mov esi, OP_CALL_INTRINSIC_2
    mov edx, INTRINSIC_PREP_RERAISE_STAR
    mov rcx, [rbp - ES_LINE]
    call cg_emit
    mov rdi, r12
    mov esi, OP_COPY
    mov edx, 1
    mov rcx, [rbp - ES_LINE]
    call cg_emit
    mov rdi, r12
    call cg_label_new
    mov [rbp - ES_RERA], rax
    mov rdi, r12
    mov esi, OP_POP_JUMP_IF_NOT_NONE
    mov rdx, rax
    mov rcx, [rbp - ES_LINE]
    call cg_emit_jump

    ; Nothing left: drop the None and restore the previous exception state.
    mov rdi, r12
    mov esi, OP_POP_TOP
    xor edx, edx
    mov rcx, [rbp - ES_LINE]
    call cg_emit

    ; The protected region ends here, before either exit's POP_EXCEPT.  Leaving
    ; it open over them means the RERAISE below is caught by its own cleanup
    ; block, which pops an exception state that is already gone.
    mov rdi, r12
    call cg_pop_handler

    mov rdi, r12
    mov esi, OP_POP_EXCEPT
    xor edx, edx
    mov rcx, [rbp - ES_LINE]
    call cg_emit
    mov rdi, r12
    mov esi, OP_JUMP_FORWARD
    mov rdx, [rbp - ES_END]
    mov rcx, [rbp - ES_LINE]
    call cg_emit_jump

    ; Something is left: it goes out in place of the original group.
    mov rdi, r12
    mov rsi, [rbp - ES_RERA]
    call cg_label_bind
    mov rdi, r12
    mov esi, OP_SWAP
    mov edx, 2
    mov rcx, [rbp - ES_LINE]
    call cg_emit
    mov rdi, r12
    mov esi, OP_POP_EXCEPT
    xor edx, edx
    mov rcx, [rbp - ES_LINE]
    call cg_emit
    mov rdi, r12
    mov esi, OP_RERAISE
    xor edx, edx
    mov rcx, [rbp - ES_LINE]
    call cg_emit

    mov rdi, rbx
    mov rsi, r12
    mov rdx, [rbp - ES_CLEAN]
    mov rcx, [rbp - ES_LINE]
    call cg_exc_cleanup
.ok:
    mov eax, 1
    jmp .ret
.fail:
    xor eax, eax
.ret:
    pop r13
    pop r12
    pop rbx
    leave
    ret
END_FUNC cg_except_star_clauses

;; ============================================================================
;; cg_one_star_except(Comp *c, CompUnit *u, uint32_t handler) -> 1 ok, 0 error
;; One `except*` clause.  Entered with the unmatched remainder on top; leaves
;; the new remainder there, matched or not.
;; ============================================================================
OS_COMP  equ 8
OS_UNIT  equ 16
OS_NODE  equ 24
OS_LINE  equ 40
OS_NAME  equ 48
OS_NEXT  equ 56
OS_AFTER equ 64
OS_RAISE equ 72
OS_FRAME equ 88           ; + 3 pushes = 112
DEF_FUNC_LOCAL cg_one_star_except, OS_FRAME
    push rbx
    push r12
    push r13
    mov rbx, rdi
    mov r12, rsi
    mov r13, rdx

    mov rdi, rbx
    mov rsi, r13
    call ast_at
    mov ecx, [rax + AstNode.lineno]
    mov [rbp - OS_LINE], rcx
    mov ecx, [rax + AstNode.b]
    mov [rbp - OS_NAME], rcx
    mov ecx, [rax + AstNode.a]          ; the type -- required for except*
    test ecx, ecx
    jz .bare

    mov edx, ecx
    mov rdi, rbx
    mov rsi, r12
    call cg_expr
    test eax, eax
    jz .fail
    mov rdi, r12
    mov esi, OP_CHECK_EG_MATCH
    xor edx, edx
    mov rcx, [rbp - OS_LINE]
    call cg_emit

    mov rdi, r12
    call cg_label_new
    mov [rbp - OS_NEXT], rax
    mov rdi, r12
    call cg_label_new
    mov [rbp - OS_AFTER], rax

    mov rdi, r12
    mov esi, OP_COPY
    mov edx, 1
    mov rcx, [rbp - OS_LINE]
    call cg_emit
    mov rdi, r12
    mov esi, OP_POP_JUMP_IF_NONE
    mov rdx, [rbp - OS_NEXT]
    mov rcx, [rbp - OS_LINE]
    call cg_emit_jump

    ; Bind the subgroup, or drop it.
    cmp qword [rbp - OS_NAME], 0
    je .discard
    mov rsi, [rbp - OS_NAME]
    mov rdi, rbx
    call ast_obj_at
    mov rdx, rax
    mov rdi, rbx
    mov rsi, r12
    mov ecx, CTX_STORE
    xor r8d, r8d
    call cg_nameop
    test eax, eax
    jz .fail
    jmp .body
.discard:
    mov rdi, r12
    mov esi, OP_POP_TOP
    xor edx, edx
    mov rcx, [rbp - OS_LINE]
    call cg_emit

.body:
    ; A body that raises does not stop the chain: what it raised joins the
    ; list, and the next clause still sees the remainder.  The region opens
    ; here, with the remainder on top, which is the depth it unwinds to.
    mov rdi, r12
    call cg_label_new
    mov [rbp - OS_RAISE], rax
    mov rdi, r12
    mov rsi, rax
    mov edx, 1                          ; lasti
    call cg_push_handler

    mov rdi, r12
    mov esi, CG_ESTAR_MARK
    call cg_finally_push
    mov rdi, rbx
    mov rsi, r12
    mov rdx, r13
    call cg_body
    push rax
    mov rdi, r12
    call cg_finally_pop
    pop rax
    test eax, eax
    jz .fail
    mov rdi, r12
    call cg_pop_handler

    mov rdi, rbx
    mov rsi, r12
    mov rdx, [rbp - OS_NAME]
    mov rcx, [rbp - OS_LINE]
    call cg_clear_star_name
    test eax, eax
    jz .fail
    mov rdi, r12
    mov esi, OP_JUMP_FORWARD
    mov rdx, [rbp - OS_AFTER]
    mov rcx, [rbp - OS_LINE]
    call cg_emit_jump

    mov rdi, r12
    mov rsi, [rbp - OS_RAISE]
    call cg_label_bind
    mov rdi, rbx
    mov rsi, r12
    mov rdx, [rbp - OS_NAME]
    mov rcx, [rbp - OS_LINE]
    call cg_clear_star_name
    test eax, eax
    jz .fail
    mov rdi, r12
    mov esi, OP_LIST_APPEND
    mov edx, 3
    mov rcx, [rbp - OS_LINE]
    call cg_emit
    mov rdi, r12
    mov esi, OP_POP_TOP                 ; the offset the unwinder pushed
    xor edx, edx
    mov rcx, [rbp - OS_LINE]
    call cg_emit
    mov rdi, r12
    mov esi, OP_JUMP_FORWARD
    mov rdx, [rbp - OS_AFTER]
    mov rcx, [rbp - OS_LINE]
    call cg_emit_jump

    ; No match: drop the None CHECK_EG_MATCH left.
    mov rdi, r12
    mov rsi, [rbp - OS_NEXT]
    call cg_label_bind
    mov rdi, r12
    mov esi, OP_POP_TOP
    xor edx, edx
    mov rcx, [rbp - OS_LINE]
    call cg_emit

    mov rdi, r12
    mov rsi, [rbp - OS_AFTER]
    call cg_label_bind
    mov eax, 1
    jmp .ret

.bare:
    mov rdi, rbx
    lea rsi, [rel exc_SyntaxError_type]
    CSTRING rdx, "except* must name an exception type"
    mov rax, r13
    xor ecx, ecx
    xor r8d, r8d
    call comp_error
.fail:
    xor eax, eax
.ret:
    pop r13
    pop r12
    pop rbx
    leave
    ret
END_FUNC cg_one_star_except

;; ============================================================================
;; cg_clear_star_name(Comp *c, CompUnit *u, uint32_t nameobj, int line)
;; The name unbinding, skipped when the clause did not bind one.
;; ============================================================================
DEF_FUNC_BARE cg_clear_star_name
    test edx, edx
    jnz cg_clear_exc_name
    mov eax, 1
    ret
END_FUNC cg_clear_star_name

ASM_INIT
