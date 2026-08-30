; codegen.asm - AST to instruction stream
;
; One emitter per node kind, reached through a jump table indexed by AST kind.
; Emitters never write a CACHE word or an EXTENDED_ARG prefix: both are
; synthesized by the assembler from the opcode metadata table, so "I forgot the
; caches on LOAD_ATTR" is not a mistake this file can make.
;
; The invariant every expression emitter keeps is that it leaves exactly one
; value on the stack.  The depth checker in assemble.asm verifies it.

%include "macros.inc"
%include "object.inc"
%include "types.inc"
%include "value.inc"
%include "opcodes.inc"
%include "compiler.inc"

extern ast_at
extern ast_child
extern ast_obj_at
extern buf_free
extern buf_init
extern buf_push_u32
extern buf_reserve
extern comp_error

extern ap_memcmp
extern exc_SyntaxError_type

; --- Named frame-layout constants ---
CE_COMP  equ 8
CE_UNIT  equ 16
CE_NODE  equ 24
CE_NPTR  equ 32
CE_I     equ 40
CE_N     equ 48
CE_TMP   equ 56
CE_FRAME equ 56          ; + 3 pushes = 80

section .text

;; ============================================================================
;; cg_unit_init(CompUnit *u, PyStrObject *filename, PyStrObject *name)
;; ============================================================================
CU_UNIT  equ 8
CU_FILE  equ 16
CU_NAME  equ 24
CU_FRAME equ 24          ; + 1 push = 32
DEF_FUNC cg_unit_init, CU_FRAME
    push rbx
    mov rbx, rdi
    mov [rbp - CU_FILE], rsi
    mov [rbp - CU_NAME], rdx

    lea rdi, [rbx + CompUnit.instrs]
    mov esi, Instr_size
    call buf_init
    lea rdi, [rbx + CompUnit.labels]
    mov esi, 4
    call buf_init
    lea rdi, [rbx + CompUnit.consts]
    mov esi, 8
    call buf_init
    lea rdi, [rbx + CompUnit.names]
    mov esi, 8
    call buf_init
    lea rdi, [rbx + CompUnit.prefix]
    mov esi, 1
    call buf_init

    mov dword [rbx + CompUnit.flags], 0
    mov dword [rbx + CompUnit.argcount], 0
    mov dword [rbx + CompUnit.posonly], 0
    mov dword [rbx + CompUnit.kwonly], 0
    mov dword [rbx + CompUnit.nlocals], 0
    mov dword [rbx + CompUnit.stacksize], 0
    mov dword [rbx + CompUnit.firstline], 1
    mov dword [rbx + CompUnit.curline], 0
    mov rax, [rbp - CU_FILE]
    mov [rbx + CompUnit.filename], rax
    mov rax, [rbp - CU_NAME]
    mov [rbx + CompUnit.name], rax
    mov [rbx + CompUnit.qualname], rax

    pop rbx
    leave
    ret
END_FUNC cg_unit_init

;; ============================================================================
;; cg_unit_free(CompUnit *u)
;; The consts and names arrays hold borrowed references -- comp.objs owns every
;; literal -- so there is nothing to release beyond the storage itself.
;; ============================================================================
DEF_FUNC cg_unit_free, 8
    push rbx
    mov rbx, rdi
    lea rdi, [rbx + CompUnit.prefix]
    call buf_free
    lea rdi, [rbx + CompUnit.names]
    call buf_free
    lea rdi, [rbx + CompUnit.consts]
    call buf_free
    lea rdi, [rbx + CompUnit.labels]
    call buf_free
    lea rdi, [rbx + CompUnit.instrs]
    call buf_free
    pop rbx
    leave
    ret
END_FUNC cg_unit_free

;; ============================================================================
;; cg_emit(CompUnit *u, int opcode, uint32_t oparg, int lineno) -> Instr*
;; ============================================================================
CM_UNIT  equ 8
CM_OP    equ 16
CM_ARG   equ 24
CM_LINE  equ 32
CM_FRAME equ 40          ; + 1 push = 48
DEF_FUNC cg_emit, CM_FRAME
    push rbx
    mov rbx, rdi
    mov [rbp - CM_OP], rsi
    mov [rbp - CM_ARG], rdx
    mov [rbp - CM_LINE], rcx

    lea rdi, [rbx + CompUnit.instrs]
    mov esi, 1
    call buf_reserve

    mov rdx, [rbp - CM_OP]
    mov [rax + Instr.opcode], dl
    mov byte [rax + Instr.flags], 0
    mov word [rax + Instr.handler], 0
    mov rdx, [rbp - CM_ARG]
    mov [rax + Instr.oparg], edx
    mov rdx, [rbp - CM_LINE]
    mov [rax + Instr.line], edx
    mov dword [rax + Instr.offset], 0

    pop rbx
    leave
    ret
END_FUNC cg_emit

;; ============================================================================
;; cg_const(CompUnit *u, Value v) -> rax = index into co_consts
;;
;; Deduplicates on the raw Value word.  That is exactly conservative: identical
;; words are the same object or the same immediate, so nothing is merged that
;; must stay distinct -- notably 1 and 1.0 and True, whose encodings differ,
;; and 0.0 and -0.0, whose bit patterns do.  Two equal heap constants stay
;; separate, which costs a slot and is never wrong.
;; ============================================================================
DEF_FUNC cg_const, 8
    push rbx
    mov rbx, rdi
    mov rdx, [rbx + CompUnit.consts + Buf.data]
    mov rcx, [rbx + CompUnit.consts + Buf.len]
    xor eax, eax
.scan:
    cmp rax, rcx
    jae .append
    cmp [rdx + rax*8], rsi
    je .found
    inc rax
    jmp .scan
.append:
    lea rdi, [rbx + CompUnit.consts]
    call buf_push_ptr_local
    mov rax, [rbx + CompUnit.consts + Buf.len]
    dec rax
.found:
    pop rbx
    leave
    ret
END_FUNC cg_const

;; ============================================================================
;; cg_name(CompUnit *u, PyStrObject *s) -> rax = index into co_names
;;
;; Names must deduplicate by text, not by identity: the same identifier is
;; interned into a fresh object at each occurrence, and without this co_names
;; would grow one entry per mention.
;; ============================================================================
CN_UNIT  equ 8
CN_STR   equ 16
CN_I     equ 24
CN_FRAME equ 24          ; + 3 pushes = 48
DEF_FUNC cg_name, CN_FRAME
    push rbx
    push r12
    push r13
    mov rbx, rdi
    mov [rbp - CN_STR], rsi
    mov r12, [rbx + CompUnit.names + Buf.len]
    xor r13d, r13d
.scan:
    cmp r13, r12
    jae .append
    mov rax, [rbx + CompUnit.names + Buf.data]
    mov rdi, [rax + r13*8]
    mov rsi, [rbp - CN_STR]
    mov rdx, [rdi + PyStrObject.ob_size]
    cmp rdx, [rsi + PyStrObject.ob_size]
    jne .next
    lea rdi, [rdi + PyStrObject.data]
    lea rsi, [rsi + PyStrObject.data]
    mov [rbp - CN_I], r13
    call ap_memcmp
    mov r13, [rbp - CN_I]
    test eax, eax
    jz .found
.next:
    inc r13
    jmp .scan
.append:
    lea rdi, [rbx + CompUnit.names]
    mov rsi, [rbp - CN_STR]
    call buf_push_ptr_local
    mov r13, [rbx + CompUnit.names + Buf.len]
    dec r13
.found:
    mov rax, r13
    pop r13
    pop r12
    pop rbx
    leave
    ret
END_FUNC cg_name

;; ============================================================================
;; buf_push_ptr_local(Buf *b, uint64_t v)
;; A local alias so this file does not pull in the whole arena interface just
;; for one append.
;; ============================================================================
DEF_FUNC_BARE buf_push_ptr_local
    extern buf_push_ptr
    jmp buf_push_ptr
END_FUNC buf_push_ptr_local

;; ============================================================================
;; cg_label_new(CompUnit *u) -> rax = label id
;; A label names the position before the next instruction emitted, which makes
;; forward references the natural case: create it, jump to it, bind it later.
;; ============================================================================
DEF_FUNC cg_label_new, 8
    push rbx
    mov rbx, rdi
    lea rdi, [rbx + CompUnit.labels]
    mov esi, -1                          ; unbound
    call buf_push_u32
    mov rax, [rbx + CompUnit.labels + Buf.len]
    dec rax
    pop rbx
    leave
    ret
END_FUNC cg_label_new

;; ============================================================================
;; cg_label_bind(CompUnit *u, uint64_t label)
;; ============================================================================
DEF_FUNC_BARE cg_label_bind
    mov rax, [rdi + CompUnit.labels + Buf.data]
    mov rdx, [rdi + CompUnit.instrs + Buf.len]
    mov [rax + rsi*4], edx
    ret
END_FUNC cg_label_bind

;; ============================================================================
;; cg_emit_jump(CompUnit *u, int opcode, uint64_t label, int lineno)
;; Records the label id in the oparg; the assembler turns it into a delta once
;; every instruction's size is known.
;; ============================================================================
CJ_UNIT  equ 8
CJ_OP    equ 16
CJ_FRAME equ 24          ; + 1 push = 32
DEF_FUNC cg_emit_jump, CJ_FRAME
    push rbx
    mov rbx, rsi
    call cg_emit                        ; rdi/rsi/rdx/rcx already in place
    mov byte [rax + Instr.flags], IF_LABELARG | IF_JREL_FWD
    ; An unconditional jump ends the fallthrough edge; the depth pass needs to
    ; know that or it propagates a bogus depth into whatever follows.
    cmp rbx, OP_JUMP_FORWARD
    jne .done
    or byte [rax + Instr.flags], IF_NOFALL
.done:
    pop rbx
    leave
    ret
END_FUNC cg_emit_jump

;; ============================================================================
;; cg_cmpop(CompUnit *u, int cmpop, int lineno) -> emits one comparison
;;
;; Only the six ordering comparisons are COMPARE_OP.  `is` and `in` are
;; separate opcodes, which is why the compiler's comparison enum is wider than
;; CPython's CMP_* codes.
;;
;; COMPARE_OP's oparg is (index << 4) | mask.  apython ignores the low nibble
;; (op_compare_op does `shr ecx, 4`), but CPython's exact values are emitted so
;; that disassembling our output against CPython's stays a clean diff.
;; ============================================================================
CC_UNIT  equ 8
CC_OP    equ 16
CC_LINE  equ 24
CC_FRAME equ 24          ; + 1 push = 32
DEF_FUNC cg_cmpop, CC_FRAME
    push rbx
    mov rbx, rdi
    mov [rbp - CC_OP], rsi
    mov [rbp - CC_LINE], rdx

    cmp rsi, CMPOP_GE
    ja .not_compare
    lea rax, [rel cmp_oparg_table]
    movzx edx, byte [rax + rsi]
    mov rdi, rbx
    mov esi, OP_COMPARE_OP
    mov rcx, [rbp - CC_LINE]
    call cg_emit
    jmp .done

.not_compare:
    cmp rsi, CMPOP_NOTIN
    ja .identity
    ; CMPOP_IN -> CONTAINS_OP 0, CMPOP_NOTIN -> CONTAINS_OP 1
    mov rdx, rsi
    sub rdx, CMPOP_IN
    mov rdi, rbx
    mov esi, OP_CONTAINS_OP
    mov rcx, [rbp - CC_LINE]
    call cg_emit
    jmp .done

.identity:
    ; CMPOP_IS -> IS_OP 0, CMPOP_ISNOT -> IS_OP 1
    mov rdx, rsi
    sub rdx, CMPOP_IS
    mov rdi, rbx
    mov esi, OP_IS_OP
    mov rcx, [rbp - CC_LINE]
    call cg_emit
.done:
    mov eax, 1
    pop rbx
    leave
    ret
END_FUNC cg_cmpop

;; ============================================================================
;; cg_expr(Comp *c, CompUnit *u, uint32_t node) -> rax = 1 ok, 0 error
;;
;; Dispatches on the node kind.  Every emitter reached from here leaves exactly
;; one value on the stack; assemble.asm's depth pass is what holds them to it.
;; ============================================================================
DEF_FUNC cg_expr, CE_FRAME
    push rbx
    push r12
    push r13
    mov rbx, rdi                        ; Comp*
    mov r12, rsi                        ; CompUnit*
    mov r13, rdx                        ; node index

    test r13, r13
    jz .bad_node

    mov rdi, rbx
    mov rsi, r13
    call ast_at
    movzx eax, byte [rax + AstNode.kind]
    cmp eax, AST_COUNT
    jae .bad_node
    lea rcx, [rel cg_expr_table]
    mov rax, [rcx + rax*8]
    test rax, rax
    jz .unsupported

    mov rdi, rbx
    mov rsi, r12
    mov rdx, r13
    call rax
    pop r13
    pop r12
    pop rbx
    leave
    ret

.unsupported:
    mov rdi, rbx
    lea rsi, [rel exc_SyntaxError_type]
    CSTRING rdx, "this expression is not supported yet"
    mov rax, r13
    xor ecx, ecx
    xor r8d, r8d
    call comp_error
    xor eax, eax
    pop r13
    pop r12
    pop rbx
    leave
    ret

.bad_node:
    mov rdi, rbx
    lea rsi, [rel exc_SyntaxError_type]
    CSTRING rdx, "invalid syntax"
    xor ecx, ecx
    xor r8d, r8d
    call comp_error
    xor eax, eax
    pop r13
    pop r12
    pop rbx
    leave
    ret
END_FUNC cg_expr

;; ============================================================================
;; Expression emitters.  All share the signature
;;     fn(Comp *c, CompUnit *u, uint32_t node) -> rax = 1 ok, 0 error
;; ============================================================================

;; cg_e_const - LOAD_CONST
DEF_FUNC_LOCAL cg_e_const, CE_FRAME
    push rbx
    push r12
    push r13
    mov rbx, rdi
    mov r12, rsi
    mov r13, rdx

    mov rdi, rbx
    mov rsi, r13
    call ast_at
    mov [rbp - CE_NPTR], rax
    mov esi, [rax + AstNode.a]
    mov rdi, rbx
    call ast_obj_at                     ; rax = the borrowed Value
    mov rdi, r12
    mov rsi, rax
    call cg_const
    mov rdx, rax
    mov rax, [rbp - CE_NPTR]
    mov ecx, [rax + AstNode.lineno]
    mov rdi, r12
    mov esi, OP_LOAD_CONST
    call cg_emit
    mov eax, 1
    pop r13
    pop r12
    pop rbx
    leave
    ret
END_FUNC cg_e_const

;; cg_e_name - LOAD_NAME
;;
;; LOAD_NAME, not LOAD_FAST and not LOAD_GLOBAL.  At module and class scope
;; nothing is function-like, so every name goes through the frame's locals
;; mapping -- which is exactly the dict a caller hands to exec(src, d), and the
;; reason that call works at all.  Function scopes will select differently once
;; the symbol table exists.
DEF_FUNC_LOCAL cg_e_name, CE_FRAME
    push rbx
    push r12
    push r13
    mov rbx, rdi
    mov r12, rsi
    mov r13, rdx

    mov rdi, rbx
    mov rsi, r13
    call ast_at
    mov [rbp - CE_NPTR], rax
    mov esi, [rax + AstNode.a]
    mov rdi, rbx
    call ast_obj_at
    mov rdi, r12
    mov rsi, rax
    call cg_name
    mov rdx, rax
    mov rax, [rbp - CE_NPTR]
    mov ecx, [rax + AstNode.lineno]
    mov rdi, r12
    mov esi, OP_LOAD_NAME
    call cg_emit
    mov eax, 1
    pop r13
    pop r12
    pop rbx
    leave
    ret
END_FUNC cg_e_name

;; cg_e_binop - left, right, BINARY_OP
DEF_FUNC_LOCAL cg_e_binop, CE_FRAME
    push rbx
    push r12
    push r13
    mov rbx, rdi
    mov r12, rsi
    mov r13, rdx

    mov rdi, rbx
    mov rsi, r13
    call ast_at
    mov edx, [rax + AstNode.a]
    mov rdi, rbx
    mov rsi, r12
    call cg_expr
    test eax, eax
    jz .fail

    mov rdi, rbx
    mov rsi, r13
    call ast_at
    mov edx, [rax + AstNode.b]
    mov rdi, rbx
    mov rsi, r12
    call cg_expr
    test eax, eax
    jz .fail

    mov rdi, rbx
    mov rsi, r13
    call ast_at
    movzx edx, byte [rax + AstNode.subkind]
    mov ecx, [rax + AstNode.lineno]
    mov rdi, r12
    mov esi, OP_BINARY_OP
    call cg_emit
    mov eax, 1
.fail:
    pop r13
    pop r12
    pop rbx
    leave
    ret
END_FUNC cg_e_binop

;; cg_e_unaryop - operand, then the unary opcode
;;
;; Unary plus emits nothing at all: CPython drops it, and there is no
;; UNARY_POSITIVE opcode in 3.12 to emit even if we wanted one.
DEF_FUNC_LOCAL cg_e_unaryop, CE_FRAME
    push rbx
    push r12
    push r13
    mov rbx, rdi
    mov r12, rsi
    mov r13, rdx

    mov rdi, rbx
    mov rsi, r13
    call ast_at
    mov edx, [rax + AstNode.a]
    mov rdi, rbx
    mov rsi, r12
    call cg_expr
    test eax, eax
    jz .fail

    mov rdi, rbx
    mov rsi, r13
    call ast_at
    movzx edx, byte [rax + AstNode.subkind]
    mov ecx, [rax + AstNode.lineno]
    cmp edx, UOP_POS
    je .done                            ; +x is the identity
    mov esi, OP_UNARY_NEGATIVE
    cmp edx, UOP_NEG
    je .have_op
    mov esi, OP_UNARY_INVERT
    cmp edx, UOP_INVERT
    je .have_op
    mov esi, OP_UNARY_NOT
.have_op:
    mov rdi, r12
    xor edx, edx
    call cg_emit
.done:
    mov eax, 1
.fail:
    pop r13
    pop r12
    pop rbx
    leave
    ret
END_FUNC cg_e_unaryop

;; cg_e_boolop - `and` / `or`, short-circuiting through one shared exit
;;
;;     <operand>            for each but the last
;;     COPY 1
;;     POP_JUMP_IF_FALSE end        (IF_TRUE for `or`)
;;     POP_TOP
;;     <last operand>
;;   end:
;;
;; The value that decided the result is what the expression evaluates to, which
;; is why the operand is duplicated rather than tested and discarded.
CB_ALT   equ 8           ; the else / cleanup label
CB_NODE  equ 16
CB_COMP  equ 24
CB_END   equ 32
CB_I     equ 40
CB_N     equ 48
CB_OP    equ 56
CB_LINE  equ 64
CB_FRAME equ 72          ; + 3 pushes = 96
DEF_FUNC_LOCAL cg_e_boolop, CB_FRAME
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
    mov [rbp - CB_N], rcx
    mov ecx, [rax + AstNode.lineno]
    mov [rbp - CB_LINE], rcx
    movzx ecx, byte [rax + AstNode.subkind]
    mov esi, OP_POP_JUMP_IF_FALSE
    cmp ecx, BOOL_OR
    jne .have_op
    mov esi, OP_POP_JUMP_IF_TRUE
.have_op:
    mov [rbp - CB_OP], rsi

    mov rdi, r12
    call cg_label_new
    mov [rbp - CB_END], rax

    mov qword [rbp - CB_I], 0
.loop:
    mov rax, [rbp - CB_I]
    mov rcx, [rbp - CB_N]
    cmp rax, rcx
    jae .finish

    mov rdi, rbx
    mov rsi, r13
    call ast_at
    mov rsi, rax
    mov rdx, [rbp - CB_I]
    mov rdi, rbx
    call ast_child
    mov rdx, rax
    mov rdi, rbx
    mov rsi, r12
    call cg_expr
    test eax, eax
    jz .fail

    mov rax, [rbp - CB_I]
    inc rax
    mov [rbp - CB_I], rax
    cmp rax, [rbp - CB_N]
    jae .finish                         ; the last operand needs no test

    mov rdi, r12
    mov esi, OP_COPY
    mov edx, 1
    mov rcx, [rbp - CB_LINE]
    call cg_emit
    mov rdi, r12
    mov rsi, [rbp - CB_OP]
    mov rdx, [rbp - CB_END]
    mov rcx, [rbp - CB_LINE]
    call cg_emit_jump
    mov rdi, r12
    mov esi, OP_POP_TOP
    xor edx, edx
    mov rcx, [rbp - CB_LINE]
    call cg_emit
    jmp .loop

.finish:
    mov rdi, r12
    mov rsi, [rbp - CB_END]
    call cg_label_bind
    mov eax, 1
    pop r13
    pop r12
    pop rbx
    leave
    ret
.fail:
    xor eax, eax
    pop r13
    pop r12
    pop rbx
    leave
    ret
END_FUNC cg_e_boolop

;; cg_e_ifexp - body if test else orelse
DEF_FUNC_LOCAL cg_e_ifexp, CB_FRAME
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
    mov [rbp - CB_LINE], rcx

    mov rdi, r12
    call cg_label_new
    mov [rbp - CB_I], rax               ; the `else` label
    mov rdi, r12
    call cg_label_new
    mov [rbp - CB_END], rax

    mov rdi, rbx
    mov rsi, r13
    call ast_at
    mov edx, [rax + AstNode.a]          ; test
    mov rdi, rbx
    mov rsi, r12
    call cg_expr
    test eax, eax
    jz .fail

    mov rdi, r12
    mov esi, OP_POP_JUMP_IF_FALSE
    mov rdx, [rbp - CB_I]
    mov rcx, [rbp - CB_LINE]
    call cg_emit_jump

    mov rdi, rbx
    mov rsi, r13
    call ast_at
    mov edx, [rax + AstNode.b]          ; body
    mov rdi, rbx
    mov rsi, r12
    call cg_expr
    test eax, eax
    jz .fail

    mov rdi, r12
    mov esi, OP_JUMP_FORWARD
    mov rdx, [rbp - CB_END]
    mov rcx, [rbp - CB_LINE]
    call cg_emit_jump

    mov rdi, r12
    mov rsi, [rbp - CB_I]
    call cg_label_bind

    mov rdi, rbx
    mov rsi, r13
    call ast_at
    mov edx, [rax + AstNode.c]          ; orelse
    mov rdi, rbx
    mov rsi, r12
    call cg_expr
    test eax, eax
    jz .fail

    mov rdi, r12
    mov rsi, [rbp - CB_END]
    call cg_label_bind
    mov eax, 1
    pop r13
    pop r12
    pop rbx
    leave
    ret
.fail:
    xor eax, eax
    pop r13
    pop r12
    pop rbx
    leave
    ret
END_FUNC cg_e_ifexp

;; cg_e_compare - a chain, evaluated left to right with each operand once
;;
;;     <left> <operand 0>
;;     for each but the last comparison:
;;         SWAP 2 ; COPY 2 ; <compare> ; COPY 1
;;         POP_JUMP_IF_FALSE cleanup
;;         POP_TOP
;;         <next operand>
;;     <compare>
;;     JUMP_FORWARD end
;;   cleanup: SWAP 2 ; POP_TOP
;;   end:
;;
;; The dance keeps the operand that is about to be reused underneath the result
;; of the comparison just made, so `a < b < c` evaluates b exactly once -- which
;; is the whole reason a chain is one node rather than a fold.
DEF_FUNC_LOCAL cg_e_compare, CB_FRAME
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
    mov [rbp - CB_N], rcx
    mov ecx, [rax + AstNode.lineno]
    mov [rbp - CB_LINE], rcx
    mov edx, [rax + AstNode.a]
    mov rdi, rbx
    mov rsi, r12
    call cg_expr                        ; the left operand
    test eax, eax
    jz .fail

    cmp qword [rbp - CB_N], 1
    jne .chained

    ; The common case: one comparison, no bookkeeping at all.
    mov rdi, rbx
    mov rsi, r13
    call ast_at
    mov rsi, rax
    xor edx, edx
    mov rdi, rbx
    call ast_child                      ; child 0 is the operator
    mov [rbp - CB_OP], rax
    mov rdi, rbx
    mov rsi, r13
    call ast_at
    mov rsi, rax
    mov edx, 1
    mov rdi, rbx
    call ast_child                      ; child 1 is its operand
    mov rdx, rax
    mov rdi, rbx
    mov rsi, r12
    call cg_expr
    test eax, eax
    jz .fail
    mov rdi, r12
    mov rsi, [rbp - CB_OP]
    mov rdx, [rbp - CB_LINE]
    call cg_cmpop
    mov eax, 1
    pop r13
    pop r12
    pop rbx
    leave
    ret

.chained:
    mov rdi, r12
    call cg_label_new
    mov [rbp - CB_ALT], rax            ; cleanup label
    mov rdi, r12
    call cg_label_new
    mov [rbp - CB_END], rax

    ; Emit the first right-hand operand, then loop over the comparisons.
    mov rdi, rbx
    mov rsi, r13
    call ast_at
    mov rsi, rax
    mov edx, 1
    mov rdi, rbx
    call ast_child
    mov rdx, rax
    mov rdi, rbx
    mov rsi, r12
    call cg_expr
    test eax, eax
    jz .fail

    mov qword [rbp - CB_I], 0
.chain_loop:
    ; operator index i lives at child 2*i, its operand at child 2*i + 1
    mov rdi, rbx
    mov rsi, r13
    call ast_at
    mov rsi, rax
    mov rdx, [rbp - CB_I]
    shl rdx, 1
    mov rdi, rbx
    call ast_child
    mov [rbp - CB_OP], rax

    mov rax, [rbp - CB_I]
    inc rax
    cmp rax, [rbp - CB_N]
    jae .chain_last

    mov rdi, r12
    mov esi, OP_SWAP
    mov edx, 2
    mov rcx, [rbp - CB_LINE]
    call cg_emit
    mov rdi, r12
    mov esi, OP_COPY
    mov edx, 2
    mov rcx, [rbp - CB_LINE]
    call cg_emit
    mov rdi, r12
    mov rsi, [rbp - CB_OP]
    mov rdx, [rbp - CB_LINE]
    call cg_cmpop
    mov rdi, r12
    mov esi, OP_COPY
    mov edx, 1
    mov rcx, [rbp - CB_LINE]
    call cg_emit
    mov rdi, r12
    mov esi, OP_POP_JUMP_IF_FALSE
    mov rdx, [rbp - CB_ALT]
    mov rcx, [rbp - CB_LINE]
    call cg_emit_jump
    mov rdi, r12
    mov esi, OP_POP_TOP
    xor edx, edx
    mov rcx, [rbp - CB_LINE]
    call cg_emit

    mov rdi, rbx
    mov rsi, r13
    call ast_at
    mov rsi, rax
    mov rdx, [rbp - CB_I]
    shl rdx, 1
    add rdx, 3                          ; the next comparison's operand
    mov rdi, rbx
    call ast_child
    mov rdx, rax
    mov rdi, rbx
    mov rsi, r12
    call cg_expr
    test eax, eax
    jz .fail

    mov rax, [rbp - CB_I]
    inc rax
    mov [rbp - CB_I], rax
    jmp .chain_loop

.chain_last:
    mov rdi, r12
    mov rsi, [rbp - CB_OP]
    mov rdx, [rbp - CB_LINE]
    call cg_cmpop
    mov rdi, r12
    mov esi, OP_JUMP_FORWARD
    mov rdx, [rbp - CB_END]
    mov rcx, [rbp - CB_LINE]
    call cg_emit_jump

    mov rdi, r12
    mov rsi, [rbp - CB_ALT]
    call cg_label_bind
    mov rdi, r12
    mov esi, OP_SWAP
    mov edx, 2
    mov rcx, [rbp - CB_LINE]
    call cg_emit
    mov rdi, r12
    mov esi, OP_POP_TOP
    xor edx, edx
    mov rcx, [rbp - CB_LINE]
    call cg_emit
    mov rdi, r12
    mov rsi, [rbp - CB_END]
    call cg_label_bind

    mov eax, 1
    pop r13
    pop r12
    pop rbx
    leave
    ret
.fail:
    xor eax, eax
    pop r13
    pop r12
    pop rbx
    leave
    ret
END_FUNC cg_e_compare

section .rodata

;; COMPARE_OP oparg = (index << 4) | mask.  apython reads only the high nibble,
;; but CPython's exact bytes are emitted so a disassembly diff stays clean.
align 8
cmp_oparg_table:
    db 2        ; <
    db 26       ; <=
    db 40       ; ==
    db 55       ; !=
    db 68       ; >
    db 92       ; >=

;; AST kind -> expression emitter.  A zero entry means "not an expression, or
;; not implemented yet", and cg_expr reports it rather than emitting nothing.
align 8
cg_expr_table:
    dq 0              ;  0 AST_NULL
    dq cg_e_const     ;  1 AST_CONST
    dq cg_e_name      ;  2 AST_NAME
    dq cg_e_binop     ;  3 AST_BINOP
    dq cg_e_unaryop   ;  4 AST_UNARYOP
    dq cg_e_boolop    ;  5 AST_BOOLOP
    dq cg_e_compare   ;  6 AST_COMPARE
    dq cg_e_ifexp     ;  7 AST_IFEXP
    dq 0              ;  8 AST_LAMBDA
    dq 0              ;  9 AST_TUPLE
    dq 0              ; 10 AST_LIST
    dq 0              ; 11 AST_SET
    dq 0              ; 12 AST_DICT
    dq 0              ; 13 AST_CALL
    dq 0              ; 14 AST_ATTRIBUTE
    dq 0              ; 15 AST_SUBSCRIPT
    dq 0              ; 16 AST_SLICE
    dq 0              ; 17 AST_STARRED
    dq 0              ; 18 AST_DOUBLESTARRED
    dq 0              ; 19 AST_KEYWORD
    dq 0              ; 20 AST_NAMEDEXPR
    dq 0              ; 21 AST_YIELD
    dq 0              ; 22 AST_YIELDFROM
    dq 0              ; 23 AST_AWAIT
    dq 0              ; 24 AST_JOINEDSTR
    dq 0              ; 25 AST_FORMATTEDVALUE
    dq 0              ; 26 AST_LISTCOMP
    dq 0              ; 27 AST_SETCOMP
    dq 0              ; 28 AST_DICTCOMP
    dq 0              ; 29 AST_GENEXP
    dq 0              ; 30 AST_COMPREHENSION
    times (AST_COUNT - 31) dq 0


ASM_INIT
