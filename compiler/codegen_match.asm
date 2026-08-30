; codegen_match.asm - the `match` statement
;
; One contract runs through everything here:
;
;   cg_pattern(pat, fail) is entered with the value to match on top of the
;   stack, and CONSUMES it either way.  On a match it falls through with the
;   pattern's names bound; on a mismatch it jumps to `fail`.
;
; Making failure consume the subject too is what keeps the nesting simple.
; CPython instead tracks how many values a pattern has left above the subject
; and pops exactly that many on each failure path (its `pc->on_top`), which
; produces tighter code and a great deal more bookkeeping.  Here the caller
; keeps its own spare copy, so no pattern ever has to know what is beneath it.
;
; A case is then:
;
;       COPY 1                  ; the spare, for the next case
;       <pattern, fail -> next>
;       [<guard>; POP_JUMP_IF_FALSE next]
;       POP_TOP                 ; drop the spare
;       <body>
;       JUMP_FORWARD end
;     next:
;
; Sub-patterns are the one place the contract needs help: UNPACK_SEQUENCE
; leaves n values on the stack, and a failure at element k has to drop the
; k+1..n that are still there.  Each sequence, mapping and class pattern builds
; a small ladder of POP_TOPs for that, one entry per depth -- the same shape as
; CPython's fail_pop.

%include "macros.inc"
%include "object.inc"
%include "opcodes.inc"
%include "value.inc"
%include "compiler.inc"

extern ast_at
extern ast_child
extern ast_obj_at
extern cg_block
extern cg_body
extern cg_const
extern cg_emit
extern cg_emit_jump
extern cg_expr
extern cg_cmpop
extern cg_label_bind
extern cg_label_new
extern cg_nameop
extern comp_error
extern comp_intern_cstr
extern exc_SyntaxError_type
extern none_singleton
extern bool_true
extern bool_false
extern tuple_new

global cg_s_match

section .text

;; ============================================================================
;; cg_s_match(Comp *c, CompUnit *u, uint32_t node) -> 1 ok, 0 error
;; ============================================================================
SM_COMP  equ 8
SM_UNIT  equ 16
SM_NODE  equ 24
SM_LINE  equ 32
SM_I     equ 40
SM_N     equ 48
SM_END   equ 56
SM_NEXT  equ 64
SM_CASE  equ 72
SM_FRAME equ 88           ; + 3 pushes = 112
DEF_FUNC cg_s_match, SM_FRAME
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
    mov [rbp - SM_LINE], rcx
    mov ecx, [rax + AstNode.nchild]
    mov [rbp - SM_N], rcx

    ; The subject, evaluated once.
    mov edx, [rax + AstNode.a]
    mov rdi, rbx
    mov rsi, r12
    call cg_expr
    test eax, eax
    jz .fail

    mov rdi, r12
    call cg_label_new
    mov [rbp - SM_END], rax
    mov qword [rbp - SM_I], 0
.case_loop:
    mov rax, [rbp - SM_I]
    cmp rax, [rbp - SM_N]
    jae .no_more
    mov rdi, rbx
    mov rsi, r13
    call ast_at
    mov rsi, rax
    mov rdx, [rbp - SM_I]
    mov rdi, rbx
    call ast_child
    mov [rbp - SM_CASE], rax

    mov rdi, r12
    call cg_label_new
    mov [rbp - SM_NEXT], rax

    ; A spare copy, because a pattern consumes what it is given and the next
    ; case still needs the subject.
    mov rdi, r12
    mov esi, OP_COPY
    mov edx, 1
    mov rcx, [rbp - SM_LINE]
    call cg_emit

    mov rdi, rbx
    mov rsi, [rbp - SM_CASE]
    call ast_at
    mov edx, [rax + AstNode.a]
    mov rdi, rbx
    mov rsi, r12
    mov rcx, [rbp - SM_NEXT]
    call cg_pattern
    test eax, eax
    jz .fail

    ; The guard runs with the names already bound, which is what lets it read
    ; them; a guard that fails leaves them bound, exactly as CPython does.
    mov rdi, rbx
    mov rsi, [rbp - SM_CASE]
    call ast_at
    mov ecx, [rax + AstNode.b]
    test ecx, ecx
    jz .no_guard
    mov edx, ecx
    mov rdi, rbx
    mov rsi, r12
    call cg_expr
    test eax, eax
    jz .fail
    mov rdi, r12
    mov esi, OP_POP_JUMP_IF_FALSE
    mov rdx, [rbp - SM_NEXT]
    mov rcx, [rbp - SM_LINE]
    call cg_emit_jump
.no_guard:

    mov rdi, r12
    mov esi, OP_POP_TOP                 ; the spare
    xor edx, edx
    mov rcx, [rbp - SM_LINE]
    call cg_emit

    mov rdi, rbx
    mov rsi, [rbp - SM_CASE]
    call ast_at
    mov rdx, rax
    mov rdi, rbx
    mov rsi, r12
    mov rdx, [rbp - SM_CASE]
    call cg_body
    test eax, eax
    jz .fail
    mov rdi, r12
    mov esi, OP_JUMP_FORWARD
    mov rdx, [rbp - SM_END]
    mov rcx, [rbp - SM_LINE]
    call cg_emit_jump

    mov rdi, r12
    mov rsi, [rbp - SM_NEXT]
    call cg_label_bind
    inc qword [rbp - SM_I]
    jmp .case_loop

.no_more:
    ; No case matched: the subject is dropped and control carries on.  `match`
    ; is not required to be exhaustive.
    mov rdi, r12
    mov esi, OP_POP_TOP
    xor edx, edx
    mov rcx, [rbp - SM_LINE]
    call cg_emit
    mov rdi, r12
    mov rsi, [rbp - SM_END]
    call cg_label_bind
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
END_FUNC cg_s_match

;; ============================================================================
;; cg_pattern(Comp *c, CompUnit *u, uint32_t pat, uint64_t fail) -> 1 ok, 0 err
;; Consumes the value on top of the stack; falls through on a match, jumps to
;; `fail` on a mismatch.  See the contract at the top of the file.
;; ============================================================================
CP3_COMP  equ 8
CP3_UNIT  equ 16
CP3_NODE  equ 24
CP3_FAIL  equ 32
CP3_LINE  equ 40
CP3_KIND  equ 48
CP3_A     equ 56
CP3_L1    equ 64
CP3_L2    equ 72
CP3_FRAME equ 88          ; + 3 pushes = 112
DEF_FUNC cg_pattern, CP3_FRAME
    push rbx
    push r12
    push r13
    mov rbx, rdi
    mov r12, rsi
    mov r13, rdx
    mov [rbp - CP3_FAIL], rcx

    mov rdi, rbx
    mov rsi, r13
    call ast_at
    mov ecx, [rax + AstNode.lineno]
    mov [rbp - CP3_LINE], rcx
    movzx ecx, byte [rax + AstNode.kind]
    mov [rbp - CP3_KIND], rcx
    mov ecx, [rax + AstNode.a]
    mov [rbp - CP3_A], rcx

    mov rax, [rbp - CP3_KIND]
    cmp eax, AST_PAT_CAPTURE
    je .capture
    cmp eax, AST_PAT_VALUE
    je .value
    cmp eax, AST_PAT_AS
    je .as_pattern
    cmp eax, AST_PAT_OR
    je .or_pattern
    cmp eax, AST_PAT_SEQUENCE
    je .sequence
    cmp eax, AST_PAT_MAPPING
    je .mapping
    cmp eax, AST_PAT_CLASS
    je .class_pattern
    mov rdi, rbx
    lea rsi, [rel exc_SyntaxError_type]
    CSTRING rdx, "unsupported pattern"
    mov rax, r13
    xor ecx, ecx
    xor r8d, r8d
    call comp_error
    jmp .fail

;; `x` binds and always matches; `_` matches and binds nothing.
.capture:
    cmp qword [rbp - CP3_A], 0
    jne .bind
    mov rdi, r12
    mov esi, OP_POP_TOP
    xor edx, edx
    mov rcx, [rbp - CP3_LINE]
    call cg_emit
    jmp .ok
.bind:
    mov rsi, [rbp - CP3_A]
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
    jmp .ok

;; A literal or a dotted name.  None, True and False compare by identity and
;; everything else by equality -- `case 1:` has to match True, because 1 ==
;; True, while `case True:` must not match 1.
.value:
    mov edx, [rbp - CP3_A]
    mov rdi, rbx
    mov rsi, r12
    call cg_expr
    test eax, eax
    jz .fail
    mov rdi, rbx
    mov rsi, r13
    call ast_at
    cmp byte [rax + AstNode.subkind], 0
    je .by_equality
    mov rdi, r12
    mov esi, OP_IS_OP
    xor edx, edx
    mov rcx, [rbp - CP3_LINE]
    call cg_emit
    jmp .test_bool
.by_equality:
    mov rdi, r12
    mov esi, CMPOP_EQ
    mov rdx, [rbp - CP3_LINE]
    call cg_cmpop
.test_bool:
    mov rdi, r12
    mov esi, OP_POP_JUMP_IF_FALSE
    mov rdx, [rbp - CP3_FAIL]
    mov rcx, [rbp - CP3_LINE]
    call cg_emit_jump
    jmp .ok

;; `p as name`: match p against a copy, then bind the original.  A failed
;; inner pattern has consumed only the copy, so the original still has to go.
.as_pattern:
    cmp qword [rbp - CP3_A], 0
    je .as_bind                         ; `_ as name` needs no test at all

    mov rdi, r12
    call cg_label_new
    mov [rbp - CP3_L1], rax             ; the inner pattern's failure
    mov rdi, r12
    call cg_label_new
    mov [rbp - CP3_L2], rax             ; past the failure block

    mov rdi, r12
    mov esi, OP_COPY
    mov edx, 1
    mov rcx, [rbp - CP3_LINE]
    call cg_emit
    mov rdi, rbx
    mov rsi, r12
    mov rdx, [rbp - CP3_A]
    mov rcx, [rbp - CP3_L1]
    call cg_pattern
    test eax, eax
    jz .fail
.as_bind:
    mov rdi, rbx
    mov rsi, r13
    call ast_at
    mov esi, [rax + AstNode.b]
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
    cmp qword [rbp - CP3_A], 0
    je .ok

    mov rdi, r12
    mov esi, OP_JUMP_FORWARD
    mov rdx, [rbp - CP3_L2]
    mov rcx, [rbp - CP3_LINE]
    call cg_emit_jump
    mov rdi, r12
    mov rsi, [rbp - CP3_L1]
    call cg_label_bind
    mov rdi, r12
    mov esi, OP_POP_TOP                 ; the original, which no one took
    xor edx, edx
    mov rcx, [rbp - CP3_LINE]
    call cg_emit
    mov rdi, r12
    mov esi, OP_JUMP_FORWARD
    mov rdx, [rbp - CP3_FAIL]
    mov rcx, [rbp - CP3_LINE]
    call cg_emit_jump
    mov rdi, r12
    mov rsi, [rbp - CP3_L2]
    call cg_label_bind
    jmp .ok

.or_pattern:
    mov rdi, rbx
    mov rsi, r12
    mov rdx, r13
    mov rcx, [rbp - CP3_FAIL]
    call cg_pat_or
    jmp .ret
.sequence:
    mov rdi, rbx
    mov rsi, r12
    mov rdx, r13
    mov rcx, [rbp - CP3_FAIL]
    call cg_pat_sequence
    jmp .ret
.mapping:
    mov rdi, rbx
    mov rsi, r12
    mov rdx, r13
    mov rcx, [rbp - CP3_FAIL]
    call cg_pat_mapping
    jmp .ret
.class_pattern:
    mov rdi, rbx
    mov rsi, r12
    mov rdx, r13
    mov rcx, [rbp - CP3_FAIL]
    call cg_pat_class
    jmp .ret
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
END_FUNC cg_pattern

;; ============================================================================
;; cg_pat_or(Comp *c, CompUnit *u, uint32_t pat, uint64_t fail)
;; `p1 | p2 | ...`.  Every alternative but the last matches against a copy, so
;; a failure still leaves the subject for the next one; the last consumes it.
;; ============================================================================
PR_COMP  equ 8
PR_UNIT  equ 16
PR_NODE  equ 24
PR_FAIL  equ 32
PR_LINE  equ 40
PR_I     equ 48
PR_N     equ 56
PR_OK    equ 64
PR_NEXT  equ 72
PR_FRAME equ 88           ; + 3 pushes = 112
DEF_FUNC_LOCAL cg_pat_or, PR_FRAME
    push rbx
    push r12
    push r13
    mov rbx, rdi
    mov r12, rsi
    mov r13, rdx
    mov [rbp - PR_FAIL], rcx

    mov rdi, rbx
    mov rsi, r13
    call ast_at
    mov ecx, [rax + AstNode.lineno]
    mov [rbp - PR_LINE], rcx
    mov ecx, [rax + AstNode.nchild]
    mov [rbp - PR_N], rcx

    mov rdi, r12
    call cg_label_new
    mov [rbp - PR_OK], rax
    mov qword [rbp - PR_I], 0
.loop:
    mov rax, [rbp - PR_I]
    mov rcx, [rbp - PR_N]
    dec rcx
    cmp rax, rcx
    jae .last

    mov rdi, r12
    call cg_label_new
    mov [rbp - PR_NEXT], rax
    mov rdi, r12
    mov esi, OP_COPY
    mov edx, 1
    mov rcx, [rbp - PR_LINE]
    call cg_emit
    mov rdi, rbx
    mov rsi, r13
    call ast_at
    mov rsi, rax
    mov rdx, [rbp - PR_I]
    mov rdi, rbx
    call ast_child
    mov rdx, rax
    mov rdi, rbx
    mov rsi, r12
    mov rcx, [rbp - PR_NEXT]
    call cg_pattern
    test eax, eax
    jz .fail
    ; Matched: the spare is still below, and belongs to nobody now.
    mov rdi, r12
    mov esi, OP_POP_TOP
    xor edx, edx
    mov rcx, [rbp - PR_LINE]
    call cg_emit
    mov rdi, r12
    mov esi, OP_JUMP_FORWARD
    mov rdx, [rbp - PR_OK]
    mov rcx, [rbp - PR_LINE]
    call cg_emit_jump
    mov rdi, r12
    mov rsi, [rbp - PR_NEXT]
    call cg_label_bind
    inc qword [rbp - PR_I]
    jmp .loop

.last:
    mov rdi, rbx
    mov rsi, r13
    call ast_at
    mov rsi, rax
    mov rdx, [rbp - PR_I]
    mov rdi, rbx
    call ast_child
    mov rdx, rax
    mov rdi, rbx
    mov rsi, r12
    mov rcx, [rbp - PR_FAIL]
    call cg_pattern
    test eax, eax
    jz .fail
    mov rdi, r12
    mov rsi, [rbp - PR_OK]
    call cg_label_bind
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
END_FUNC cg_pat_or

;; ============================================================================
;; cg_pat_ladder(CompUnit *u, uint64_t *labels, uint64_t n, uint64_t fail,
;;               int line)
;; The failure ladder a destructuring pattern needs: labels[k] pops k values
;; and falls into labels[k-1], and labels[0] jumps to `fail`.  Sub-pattern k of
;; n jumps to labels[n-1-k], because that is how many of its siblings are still
;; on the stack when it gives up.
;; ============================================================================
LD_UNIT  equ 8
LD_LABS  equ 16
LD_N     equ 24
LD_FAIL  equ 32
LD_LINE  equ 40
LD_I     equ 48
LD_FRAME equ 56           ; + 1 push = 64
DEF_FUNC_LOCAL cg_pat_ladder, LD_FRAME
    push rbx
    mov rbx, rdi
    mov [rbp - LD_LABS], rsi
    mov [rbp - LD_N], rdx
    mov [rbp - LD_FAIL], rcx
    mov [rbp - LD_LINE], r8
    mov rax, [rbp - LD_N]
    mov [rbp - LD_I], rax
.loop:
    cmp qword [rbp - LD_I], 0
    je .bottom
    mov rax, [rbp - LD_LABS]
    mov rcx, [rbp - LD_I]
    mov rsi, [rax + rcx*8]
    mov rdi, rbx
    call cg_label_bind
    mov rdi, rbx
    mov esi, OP_POP_TOP
    xor edx, edx
    mov rcx, [rbp - LD_LINE]
    call cg_emit
    dec qword [rbp - LD_I]
    jmp .loop
.bottom:
    mov rax, [rbp - LD_LABS]
    mov rsi, [rax]
    mov rdi, rbx
    call cg_label_bind
    mov rdi, rbx
    mov esi, OP_JUMP_FORWARD
    mov rdx, [rbp - LD_FAIL]
    mov rcx, [rbp - LD_LINE]
    call cg_emit_jump
    mov eax, 1
    pop rbx
    leave
    ret
END_FUNC cg_pat_ladder

;; The most sub-patterns one destructuring pattern may have.  The failure
;; ladder is one label per depth and lives in the emitter's own frame, so it is
;; bounded rather than grown; CPython's limit here is the same order.
PAT_MAX_ITEMS equ 64

;; ============================================================================
;; cg_pat_sequence(Comp *c, CompUnit *u, uint32_t pat, uint64_t fail)
;;
;;     MATCH_SEQUENCE; POP_JUMP_IF_FALSE fail1
;;     GET_LEN; LOAD_CONST n; COMPARE_OP == (or >=); POP_JUMP_IF_FALSE fail1
;;     UNPACK_SEQUENCE n   (or UNPACK_EX for a starred element)
;;     <sub-pattern for each, innermost failure into the ladder>
;;
;; MATCH_SEQUENCE leaves the subject in place and pushes a flag, so the first
;; two failures still have the subject to drop -- that is ladder depth 1.
;; ============================================================================
PS2_COMP  equ 8
PS2_UNIT  equ 16
PS2_NODE  equ 24
PS2_FAIL  equ 32
PS2_LINE  equ 40
PS2_I     equ 48
PS2_N     equ 56
PS2_STAR  equ 64
PS2_LABS  equ 72 + PAT_MAX_ITEMS * 8
PS2_FRAME equ ((PS2_LABS + 15) / 16) * 16 + 8    ; + 3 pushes = 16-aligned
DEF_FUNC_LOCAL cg_pat_sequence, PS2_FRAME
    push rbx
    push r12
    push r13
    mov rbx, rdi
    mov r12, rsi
    mov r13, rdx
    mov [rbp - PS2_FAIL], rcx

    mov rdi, rbx
    mov rsi, r13
    call ast_at
    mov ecx, [rax + AstNode.lineno]
    mov [rbp - PS2_LINE], rcx
    mov ecx, [rax + AstNode.nchild]
    mov [rbp - PS2_N], rcx
    mov ecx, [rax + AstNode.b]          ; 1 + the starred index, or 0
    mov [rbp - PS2_STAR], rcx
    cmp qword [rbp - PS2_N], PAT_MAX_ITEMS
    jae .too_many

    ; One ladder label per depth: 0 for "nothing left of mine", then one per
    ; sub-pattern still on the stack.
    mov rax, [rbp - PS2_N]
    inc rax
    mov [rbp - PS2_I], rax
.mklabs:
    cmp qword [rbp - PS2_I], 0
    js .labs_done
    mov rdi, r12
    call cg_label_new
    mov rcx, [rbp - PS2_I]
    lea rdx, [rbp - PS2_LABS]
    mov [rdx + rcx*8], rax
    dec qword [rbp - PS2_I]
    cmp qword [rbp - PS2_I], 0
    jge .mklabs
.labs_done:

    mov rdi, r12
    mov esi, OP_MATCH_SEQUENCE
    xor edx, edx
    mov rcx, [rbp - PS2_LINE]
    call cg_emit
    mov rdi, r12
    mov esi, OP_POP_JUMP_IF_FALSE
    lea rax, [rbp - PS2_LABS]
    mov rdx, [rax + 8]                  ; one value of ours is still there
    mov rcx, [rbp - PS2_LINE]
    call cg_emit_jump

    mov rdi, r12
    mov esi, OP_GET_LEN
    xor edx, edx
    mov rcx, [rbp - PS2_LINE]
    call cg_emit
    mov rax, [rbp - PS2_N]
    cmp qword [rbp - PS2_STAR], 0
    je .exact_len
    dec rax                             ; the star soaks up the rest
.exact_len:
    mov rsi, rax
    V_PACK_I64 rsi, rcx                 ; a small count is always an immediate
    mov rdi, r12
    call cg_const
    mov rdx, rax
    mov rdi, r12
    mov esi, OP_LOAD_CONST
    mov rcx, [rbp - PS2_LINE]
    call cg_emit
    mov esi, CMPOP_EQ
    cmp qword [rbp - PS2_STAR], 0
    je .have_cmp
    mov esi, CMPOP_GE
.have_cmp:
    mov rdi, r12
    mov rdx, [rbp - PS2_LINE]
    call cg_cmpop
    mov rdi, r12
    mov esi, OP_POP_JUMP_IF_FALSE
    lea rax, [rbp - PS2_LABS]
    mov rdx, [rax + 8]
    mov rcx, [rbp - PS2_LINE]
    call cg_emit_jump

    ; Take the sequence apart.  UNPACK_SEQUENCE consumes the subject and pushes
    ; the elements so that the first is on top, which is the order the
    ; sub-patterns want.
    cmp qword [rbp - PS2_STAR], 0
    jne .unpack_ex
    mov rdi, r12
    mov esi, OP_UNPACK_SEQUENCE
    mov rdx, [rbp - PS2_N]
    mov rcx, [rbp - PS2_LINE]
    call cg_emit
    jmp .items
.unpack_ex:
    ; UNPACK_EX's oparg is (before | after << 8) round the starred element.
    mov rax, [rbp - PS2_STAR]
    dec rax                             ; the starred index
    mov rdx, [rbp - PS2_N]
    sub rdx, rax
    dec rdx                             ; how many follow it
    shl rdx, 8
    or rdx, rax
    mov rdi, r12
    mov esi, OP_UNPACK_EX
    mov rcx, [rbp - PS2_LINE]
    call cg_emit

.items:
    mov qword [rbp - PS2_I], 0
.item_loop:
    mov rax, [rbp - PS2_I]
    cmp rax, [rbp - PS2_N]
    jae .matched
    mov rdi, rbx
    mov rsi, r13
    call ast_at
    mov rsi, rax
    mov rdx, [rbp - PS2_I]
    mov rdi, rbx
    call ast_child
    mov rdx, rax
    ; A starred element binds the rest as a list, so it is a plain capture of
    ; whatever UNPACK_EX left in its slot.
    mov rdi, rbx
    mov rsi, r12
    mov rcx, [rbp - PS2_N]
    sub rcx, [rbp - PS2_I]
    dec rcx                             ; siblings still on the stack
    lea rax, [rbp - PS2_LABS]
    mov rcx, [rax + rcx*8]
    call cg_pattern
    test eax, eax
    jz .fail
    inc qword [rbp - PS2_I]
    jmp .item_loop

.matched:
    mov rdi, r12
    call cg_label_new
    mov [rbp - PS2_STAR], rax           ; reused: past the ladder
    mov rdi, r12
    mov esi, OP_JUMP_FORWARD
    mov rdx, rax
    mov rcx, [rbp - PS2_LINE]
    call cg_emit_jump
    mov rdi, r12
    lea rsi, [rbp - PS2_LABS]
    mov rdx, [rbp - PS2_N]
    ; `case []` has no sub-patterns, but MATCH_SEQUENCE's own failure still has
    ; the subject to drop, so depth 1 must exist.
    cmp rdx, 1
    jae .have_depth
    mov edx, 1
.have_depth:
    mov rcx, [rbp - PS2_FAIL]
    mov r8, [rbp - PS2_LINE]
    call cg_pat_ladder
    mov rdi, r12
    mov rsi, [rbp - PS2_STAR]
    call cg_label_bind
    mov eax, 1
    jmp .ret

.too_many:
    mov rdi, rbx
    lea rsi, [rel exc_SyntaxError_type]
    CSTRING rdx, "too many sub-patterns in one pattern"
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
END_FUNC cg_pat_sequence

;; ============================================================================
;; cg_pat_class(Comp *c, CompUnit *u, uint32_t pat, uint64_t fail)
;;
;;     <class>; LOAD_CONST (kwnames); MATCH_CLASS npos
;;     COPY 1; POP_JUMP_IF_NONE fail1
;;     UNPACK_SEQUENCE total
;;     <sub-pattern for each attribute, in the order MATCH_CLASS returned them>
;;
;; MATCH_CLASS does all the work of finding __match_args__ and reading the
;; attributes; it hands back a tuple of them, or None.  The positional
;; sub-patterns come first in that tuple and the keyword ones follow, which is
;; the order the parser already put them in.
;; ============================================================================
PK2_COMP  equ 8
PK2_UNIT  equ 16
PK2_NODE  equ 24
PK2_FAIL  equ 32
PK2_LINE  equ 40
PK2_I     equ 48
PK2_N     equ 56
PK2_NPOS  equ 64
PK2_DONE  equ 72
PK2_LABS  equ 80 + PAT_MAX_ITEMS * 8
PK2_FRAME equ ((PK2_LABS + 15) / 16) * 16 + 8    ; + 3 pushes = 16-aligned
DEF_FUNC_LOCAL cg_pat_class, PK2_FRAME
    push rbx
    push r12
    push r13
    mov rbx, rdi
    mov r12, rsi
    mov r13, rdx
    mov [rbp - PK2_FAIL], rcx

    mov rdi, rbx
    mov rsi, r13
    call ast_at
    mov ecx, [rax + AstNode.lineno]
    mov [rbp - PK2_LINE], rcx
    mov ecx, [rax + AstNode.nchild]
    mov [rbp - PK2_N], rcx
    mov ecx, [rax + AstNode.b]
    mov [rbp - PK2_NPOS], rcx
    cmp qword [rbp - PK2_N], PAT_MAX_ITEMS
    jae .too_many

    mov rax, [rbp - PK2_N]
    inc rax
    mov [rbp - PK2_I], rax
.mklabs:
    mov rdi, r12
    call cg_label_new
    mov rcx, [rbp - PK2_I]
    lea rdx, [rbp - PK2_LABS]
    mov [rdx + rcx*8], rax
    dec qword [rbp - PK2_I]
    cmp qword [rbp - PK2_I], 0
    jge .mklabs

    mov rdi, rbx
    mov rsi, r13
    call ast_at
    mov edx, [rax + AstNode.a]          ; the class
    mov rdi, rbx
    mov rsi, r12
    call cg_expr
    test eax, eax
    jz .fail

    ; The keyword names, as one constant tuple.
    mov rdi, rbx
    mov rsi, r12
    mov rdx, r13
    mov rcx, [rbp - PK2_NPOS]
    mov r8, [rbp - PK2_N]
    call cg_pat_kwnames
    test eax, eax
    jz .fail
    mov rdx, rax
    dec rdx                             ; cg_pat_kwnames biases by one
    mov rdi, r12
    mov esi, OP_LOAD_CONST
    mov rcx, [rbp - PK2_LINE]
    call cg_emit

    mov rdi, r12
    mov esi, OP_MATCH_CLASS
    mov rdx, [rbp - PK2_NPOS]
    mov rcx, [rbp - PK2_LINE]
    call cg_emit
    mov rdi, r12
    mov esi, OP_COPY
    mov edx, 1
    mov rcx, [rbp - PK2_LINE]
    call cg_emit
    mov rdi, r12
    mov esi, OP_POP_JUMP_IF_NONE
    lea rax, [rbp - PK2_LABS]
    mov rdx, [rax + 8]                  ; the None is still there to drop
    mov rcx, [rbp - PK2_LINE]
    call cg_emit_jump

    mov rdi, r12
    mov esi, OP_UNPACK_SEQUENCE
    mov rdx, [rbp - PK2_N]
    mov rcx, [rbp - PK2_LINE]
    call cg_emit

    mov qword [rbp - PK2_I], 0
.item_loop:
    mov rax, [rbp - PK2_I]
    cmp rax, [rbp - PK2_N]
    jae .matched
    mov rdi, rbx
    mov rsi, r13
    call ast_at
    mov rsi, rax
    mov rdx, [rbp - PK2_I]
    mov rdi, rbx
    call ast_child
    ; A keyword sub-pattern wraps the pattern proper; unwrap it.
    push rax
    mov rdi, rbx
    mov rsi, rax
    call ast_at
    movzx ecx, byte [rax + AstNode.kind]
    cmp ecx, AST_PAT_KEYWORD
    jne .have_sub
    mov ecx, [rax + AstNode.b]
    mov [rsp], rcx
.have_sub:
    pop rdx
    mov rdi, rbx
    mov rsi, r12
    mov rcx, [rbp - PK2_N]
    sub rcx, [rbp - PK2_I]
    dec rcx
    lea rax, [rbp - PK2_LABS]
    mov rcx, [rax + rcx*8]
    call cg_pattern
    test eax, eax
    jz .fail
    inc qword [rbp - PK2_I]
    jmp .item_loop

.matched:
    mov rdi, r12
    call cg_label_new
    mov [rbp - PK2_DONE], rax
    mov rdi, r12
    mov esi, OP_JUMP_FORWARD
    mov rdx, rax
    mov rcx, [rbp - PK2_LINE]
    call cg_emit_jump
    mov rdi, r12
    lea rsi, [rbp - PK2_LABS]
    mov rdx, [rbp - PK2_N]
    cmp rdx, 1
    jae .have_depth
    mov edx, 1
.have_depth:
    mov rcx, [rbp - PK2_FAIL]
    mov r8, [rbp - PK2_LINE]
    call cg_pat_ladder
    mov rdi, r12
    mov rsi, [rbp - PK2_DONE]
    call cg_label_bind
    mov eax, 1
    jmp .ret

.too_many:
    mov rdi, rbx
    lea rsi, [rel exc_SyntaxError_type]
    CSTRING rdx, "too many sub-patterns in one pattern"
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
END_FUNC cg_pat_class

;; ============================================================================
;; cg_pat_kwnames(Comp *c, CompUnit *u, uint32_t pat, uint64_t npos,
;;                uint64_t n) -> rax = 1 + the const index, or 0 on error
;; The tuple of keyword names MATCH_CLASS reads, built at compile time.
;; ============================================================================
KN_COMP  equ 8
KN_UNIT  equ 16
KN_NODE  equ 24
KN_NPOS  equ 32
KN_N     equ 40
KN_TUPLE equ 48
KN_I     equ 56
KN_FRAME equ 72           ; + 3 pushes = 96
DEF_FUNC_LOCAL cg_pat_kwnames, KN_FRAME
    push rbx
    push r12
    push r13
    mov rbx, rdi
    mov r12, rsi
    mov r13, rdx
    mov [rbp - KN_NPOS], rcx
    mov [rbp - KN_N], r8

    mov rdi, r8
    sub rdi, rcx
    call tuple_new
    test rax, rax
    jz .fail
    mov [rbp - KN_TUPLE], rax

    mov rax, [rbp - KN_NPOS]
    mov [rbp - KN_I], rax
.loop:
    mov rax, [rbp - KN_I]
    cmp rax, [rbp - KN_N]
    jae .done
    mov rdi, rbx
    mov rsi, r13
    call ast_at
    mov rsi, rax
    mov rdx, [rbp - KN_I]
    mov rdi, rbx
    call ast_child
    mov rdi, rbx
    mov rsi, rax
    call ast_at
    mov esi, [rax + AstNode.a]          ; the keyword's name
    mov rdi, rbx
    call ast_obj_at
    test rax, rax
    jz .fail
    INCREF rax
    mov rdx, [rbp - KN_TUPLE]
    mov rdx, [rdx + PyTupleObject.ob_item]
    mov rcx, [rbp - KN_I]
    sub rcx, [rbp - KN_NPOS]
    mov [rdx + rcx*8], rax
    inc qword [rbp - KN_I]
    jmp .loop
.done:
    mov rdi, r12
    mov rsi, [rbp - KN_TUPLE]
    call cg_const
    inc rax                             ; bias, so index 0 is not "failed"
    jmp .ret
.fail:
    xor eax, eax
.ret:
    pop r13
    pop r12
    pop rbx
    leave
    ret
END_FUNC cg_pat_kwnames

;; ============================================================================
;; cg_pat_mapping(Comp *c, CompUnit *u, uint32_t pat, uint64_t fail)
;;
;;     MATCH_MAPPING; POP_JUMP_IF_FALSE fail1
;;     GET_LEN; LOAD_CONST nkeys; COMPARE_OP >=; POP_JUMP_IF_FALSE fail1
;;     LOAD_CONST (keys); MATCH_KEYS
;;     COPY 1; POP_JUMP_IF_NONE fail3
;;     UNPACK_SEQUENCE nkeys
;;     <sub-pattern per value>
;;     [**rest]
;;
;; MATCH_KEYS consumes neither the subject nor the keys tuple: it pushes its
;; result on top of both, so from there on three things are in play and the
;; failure depths run to nkeys + 2 rather than nkeys.  Getting that wrong is
;; silent -- the stack ends one deep and the damage surfaces somewhere else.
;;
;; A `**rest` is built by copying the subject and deleting the keys that were
;; named; no opcode produces the remainder directly.  CPython arrives at the
;; same place through a run of SWAPs that keeps everything on the stack; going
;; through a plain copy is shorter and does the same thing.
;; ============================================================================
PM2_COMP  equ 8
PM2_UNIT  equ 16
PM2_NODE  equ 24
PM2_FAIL  equ 32
PM2_LINE  equ 40
PM2_I     equ 48
PM2_N     equ 56
PM2_NKEY  equ 64
PM2_REST  equ 72
PM2_DONE  equ 80
PM2_LABS  equ 88 + PAT_MAX_ITEMS * 8
PM2_FRAME equ ((PM2_LABS + 15) / 16) * 16 + 8    ; + 3 pushes = 16-aligned
DEF_FUNC_LOCAL cg_pat_mapping, PM2_FRAME
    push rbx
    push r12
    push r13
    mov rbx, rdi
    mov r12, rsi
    mov r13, rdx
    mov [rbp - PM2_FAIL], rcx

    mov rdi, rbx
    mov rsi, r13
    call ast_at
    mov ecx, [rax + AstNode.lineno]
    mov [rbp - PM2_LINE], rcx
    mov ecx, [rax + AstNode.nchild]     ; key/pattern pairs, flattened
    mov [rbp - PM2_N], rcx
    shr rcx, 1
    mov [rbp - PM2_NKEY], rcx
    mov ecx, [rax + AstNode.b]
    mov [rbp - PM2_REST], rcx
    cmp qword [rbp - PM2_NKEY], PAT_MAX_ITEMS
    jae .too_many

    ; Ladder depths run from 0 to nkeys + 2: the subject, the keys tuple, and
    ; whatever MATCH_KEYS or UNPACK_SEQUENCE put above them.
    mov rax, [rbp - PM2_NKEY]
    add rax, 3
    mov [rbp - PM2_I], rax
.mklabs:
    mov rdi, r12
    call cg_label_new
    mov rcx, [rbp - PM2_I]
    lea rdx, [rbp - PM2_LABS]
    mov [rdx + rcx*8], rax
    dec qword [rbp - PM2_I]
    cmp qword [rbp - PM2_I], 0
    jge .mklabs

    mov rdi, r12
    mov esi, OP_MATCH_MAPPING
    xor edx, edx
    mov rcx, [rbp - PM2_LINE]
    call cg_emit
    mov rdi, r12
    mov esi, OP_POP_JUMP_IF_FALSE
    lea rax, [rbp - PM2_LABS]
    mov rdx, [rax + 8]
    mov rcx, [rbp - PM2_LINE]
    call cg_emit_jump

    mov rdi, r12
    mov esi, OP_GET_LEN
    xor edx, edx
    mov rcx, [rbp - PM2_LINE]
    call cg_emit
    mov rsi, [rbp - PM2_NKEY]
    V_PACK_I64 rsi, rcx
    mov rdi, r12
    call cg_const
    mov rdx, rax
    mov rdi, r12
    mov esi, OP_LOAD_CONST
    mov rcx, [rbp - PM2_LINE]
    call cg_emit
    mov rdi, r12
    mov esi, CMPOP_GE
    mov rdx, [rbp - PM2_LINE]
    call cg_cmpop
    mov rdi, r12
    mov esi, OP_POP_JUMP_IF_FALSE
    lea rax, [rbp - PM2_LABS]
    mov rdx, [rax + 8]
    mov rcx, [rbp - PM2_LINE]
    call cg_emit_jump

    ; The keys, as one constant tuple.
    mov rdi, rbx
    mov rsi, r12
    mov rdx, r13
    call cg_pat_keys
    test eax, eax
    jz .fail
    mov rdx, rax
    dec rdx
    mov rdi, r12
    mov esi, OP_LOAD_CONST
    mov rcx, [rbp - PM2_LINE]
    call cg_emit
    mov rdi, r12
    mov esi, OP_MATCH_KEYS
    xor edx, edx
    mov rcx, [rbp - PM2_LINE]
    call cg_emit
    mov rdi, r12
    mov esi, OP_COPY
    mov edx, 1
    mov rcx, [rbp - PM2_LINE]
    call cg_emit
    mov rdi, r12
    mov esi, OP_POP_JUMP_IF_NONE
    lea rax, [rbp - PM2_LABS]
    mov rdx, [rax + 24]                 ; subject, keys and the None
    mov rcx, [rbp - PM2_LINE]
    call cg_emit_jump

    mov rdi, r12
    mov esi, OP_UNPACK_SEQUENCE
    mov rdx, [rbp - PM2_NKEY]
    mov rcx, [rbp - PM2_LINE]
    call cg_emit

    mov qword [rbp - PM2_I], 0
.value_loop:
    mov rax, [rbp - PM2_I]
    cmp rax, [rbp - PM2_NKEY]
    jae .values_done
    mov rdi, rbx
    mov rsi, r13
    call ast_at
    mov rsi, rax
    mov rdx, [rbp - PM2_I]
    shl rdx, 1
    inc rdx                             ; the value pattern of the pair
    mov rdi, rbx
    call ast_child
    mov rdx, rax
    mov rdi, rbx
    mov rsi, r12
    ; Still on the stack: the remaining values, plus the subject and the keys
    ; tuple underneath them.
    mov rcx, [rbp - PM2_NKEY]
    sub rcx, [rbp - PM2_I]
    inc rcx
    lea rax, [rbp - PM2_LABS]
    mov rcx, [rax + rcx*8]
    call cg_pattern
    test eax, eax
    jz .fail
    inc qword [rbp - PM2_I]
    jmp .value_loop

.values_done:
    ; The keys tuple has done its work.
    mov rdi, r12
    mov esi, OP_POP_TOP
    xor edx, edx
    mov rcx, [rbp - PM2_LINE]
    call cg_emit

    ; The subject is still here.  With a `**rest` it becomes the remainder;
    ; otherwise it is dropped.
    cmp qword [rbp - PM2_REST], 0
    je .drop_subject
    mov rdi, rbx
    mov rsi, r12
    mov rdx, r13
    call cg_pat_rest
    test eax, eax
    jz .fail
    jmp .matched
.drop_subject:
    mov rdi, r12
    mov esi, OP_POP_TOP
    xor edx, edx
    mov rcx, [rbp - PM2_LINE]
    call cg_emit

.matched:
    mov rdi, r12
    call cg_label_new
    mov [rbp - PM2_DONE], rax
    mov rdi, r12
    mov esi, OP_JUMP_FORWARD
    mov rdx, rax
    mov rcx, [rbp - PM2_LINE]
    call cg_emit_jump
    mov rdi, r12
    lea rsi, [rbp - PM2_LABS]
    mov rdx, [rbp - PM2_NKEY]
    add rdx, 2                          ; the subject and the keys tuple
    cmp rdx, 3
    jae .have_depth
    mov edx, 3
.have_depth:
    mov rcx, [rbp - PM2_FAIL]
    mov r8, [rbp - PM2_LINE]
    call cg_pat_ladder
    mov rdi, r12
    mov rsi, [rbp - PM2_DONE]
    call cg_label_bind
    mov eax, 1
    jmp .ret

.too_many:
    mov rdi, rbx
    lea rsi, [rel exc_SyntaxError_type]
    CSTRING rdx, "too many sub-patterns in one pattern"
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
END_FUNC cg_pat_mapping

;; ============================================================================
;; cg_pat_rest(Comp *c, CompUnit *u, uint32_t pat) -> 1 ok, 0 error
;; `**rest`: a copy of the subject with the named keys removed.  Entered with
;; the subject on top, which it consumes.
;;
;;     BUILD_MAP 0; SWAP 2; DICT_UPDATE 1     ; rest = dict(subject)
;;     for each key: COPY 1; LOAD_CONST k; DELETE_SUBSCR
;;     STORE rest
;; ============================================================================
PT3_COMP  equ 8
PT3_UNIT  equ 16
PT3_NODE  equ 24
PT3_LINE  equ 32
PT3_I     equ 40
PT3_N     equ 48
PT3_FRAME equ 56          ; + 3 pushes = 80
DEF_FUNC_LOCAL cg_pat_rest, PT3_FRAME
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
    mov [rbp - PT3_LINE], rcx
    mov ecx, [rax + AstNode.nchild]
    mov [rbp - PT3_N], rcx

    mov rdi, r12
    mov esi, OP_BUILD_MAP
    xor edx, edx
    mov rcx, [rbp - PT3_LINE]
    call cg_emit
    mov rdi, r12
    mov esi, OP_SWAP
    mov edx, 2
    mov rcx, [rbp - PT3_LINE]
    call cg_emit
    mov rdi, r12
    mov esi, OP_DICT_UPDATE
    mov edx, 1
    mov rcx, [rbp - PT3_LINE]
    call cg_emit

    ; DICT_UPDATE pops the source, leaving the copy.
    mov qword [rbp - PT3_I], 0
.key_loop:
    mov rax, [rbp - PT3_I]
    cmp rax, [rbp - PT3_N]
    jae .store
    mov rdi, r12
    mov esi, OP_COPY
    mov edx, 1
    mov rcx, [rbp - PT3_LINE]
    call cg_emit
    mov rdi, rbx
    mov rsi, r13
    call ast_at
    mov rsi, rax
    mov rdx, [rbp - PT3_I]
    mov rdi, rbx
    call ast_child
    mov rdx, rax
    mov rdi, rbx
    mov rsi, r12
    call cg_expr
    test eax, eax
    jz .fail
    mov rdi, r12
    mov esi, OP_DELETE_SUBSCR
    xor edx, edx
    mov rcx, [rbp - PT3_LINE]
    call cg_emit
    add qword [rbp - PT3_I], 2          ; keys sit at the even slots
    jmp .key_loop

.store:
    mov rdi, rbx
    mov rsi, r13
    call ast_at
    mov esi, [rax + AstNode.b]
    mov rdi, rbx
    call ast_obj_at
    mov rdx, rax
    mov rdi, rbx
    mov rsi, r12
    mov ecx, CTX_STORE
    xor r8d, r8d
    call cg_nameop
    jmp .ret
.fail:
    xor eax, eax
.ret:
    pop r13
    pop r12
    pop rbx
    leave
    ret
END_FUNC cg_pat_rest

;; ============================================================================
;; cg_pat_keys(Comp *c, CompUnit *u, uint32_t pat) -> 1 + const index, or 0
;; The tuple of keys MATCH_KEYS looks up.  They are constant expressions --
;; literals or dotted names -- but only literals can be folded into a tuple
;; here, which is why a non-literal key is rejected rather than emitted.
;; ============================================================================
PY2_COMP  equ 8
PY2_UNIT  equ 16
PY2_NODE  equ 24
PY2_TUPLE equ 32
PY2_I     equ 40
PY2_N     equ 48
PY2_FRAME equ 56          ; + 3 pushes = 80
DEF_FUNC_LOCAL cg_pat_keys, PY2_FRAME
    push rbx
    push r12
    push r13
    mov rbx, rdi
    mov r12, rsi
    mov r13, rdx

    mov rdi, rbx
    mov rsi, r13
    call ast_at
    mov ecx, [rax + AstNode.nchild]
    shr rcx, 1
    mov [rbp - PY2_N], rcx
    mov rdi, rcx
    call tuple_new
    test rax, rax
    jz .fail
    mov [rbp - PY2_TUPLE], rax

    mov qword [rbp - PY2_I], 0
.loop:
    mov rax, [rbp - PY2_I]
    cmp rax, [rbp - PY2_N]
    jae .done
    mov rdi, rbx
    mov rsi, r13
    call ast_at
    mov rsi, rax
    mov rdx, [rbp - PY2_I]
    shl rdx, 1
    mov rdi, rbx
    call ast_child
    mov rdi, rbx
    mov rsi, rax
    call ast_at
    movzx ecx, byte [rax + AstNode.kind]
    cmp ecx, AST_CONST
    jne .not_literal
    mov esi, [rax + AstNode.a]
    mov rdi, rbx
    call ast_obj_at
    test rax, rax
    jz .fail
    INCREF_V rax, rdx
    mov rdx, [rbp - PY2_TUPLE]
    mov rdx, [rdx + PyTupleObject.ob_item]
    mov rcx, [rbp - PY2_I]
    mov [rdx + rcx*8], rax
    inc qword [rbp - PY2_I]
    jmp .loop
.done:
    mov rdi, r12
    mov rsi, [rbp - PY2_TUPLE]
    call cg_const
    inc rax
    jmp .ret
.not_literal:
    mov rdi, rbx
    lea rsi, [rel exc_SyntaxError_type]
    CSTRING rdx, "a mapping pattern's keys must be literals"
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
END_FUNC cg_pat_keys

ASM_INIT
