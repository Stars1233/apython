; codegen_stmt.asm - Statement emitters
;
; Statement emitters leave nothing on the stack; expression emitters leave
; exactly one.  The depth pass in assemble.asm is what holds both to it, which
; is why a missing POP_TOP shows up as a co_stacksize disagreement rather than
; as a value that quietly accumulates.
;
; At module and class scope every name goes through STORE_NAME and LOAD_NAME.
; That is not a simplification: nothing there is function-like, so names live
; in the frame's locals mapping -- which is exactly the dict a caller hands to
; exec(src, d), and the reason that call works at all.

%include "macros.inc"
%include "object.inc"
%include "types.inc"
%include "value.inc"
%include "opcodes.inc"
%include "compiler.inc"

extern ast_at
extern ast_child
extern ast_obj_at
extern cg_children
extern cg_const
extern cg_emit
extern cg_emit_jump
extern cg_expr
extern cg_label_bind
extern cg_label_new
extern cg_name
extern comp_error

extern comp_empty_string
extern comp_intern
extern none_singleton
extern obj_decref
extern str_from_cstr_heap
extern tuple_new

extern exc_SyntaxError_type

; --- Named frame-layout constants ---
CST_COMP  equ 8
CST_UNIT  equ 16
CST_NODE  equ 24
CST_LINE  equ 32
CST_I     equ 40
CST_N     equ 48
CST_TMP   equ 56
CST_TMP2  equ 64
CST_FRAME equ 72          ; + 3 pushes = 96

section .text

;; ============================================================================
;; cg_stmt(Comp *c, CompUnit *u, uint32_t node) -> rax = 1 ok, 0 error
;; ============================================================================
DEF_FUNC cg_stmt, CST_FRAME
    push rbx
    push r12
    push r13
    mov rbx, rdi
    mov r12, rsi
    mov r13, rdx
    test r13, r13
    jz .bad

    mov rdi, rbx
    mov rsi, r13
    call ast_at
    movzx eax, byte [rax + AstNode.kind]
    cmp eax, AST_COUNT
    jae .bad
    lea rcx, [rel cg_stmt_table]
    mov rax, [rcx + rax*8]
    test rax, rax
    jz .unsupported
    mov rdi, rbx
    mov rsi, r12
    mov rdx, r13
    call rax
    jmp .ret

.unsupported:
    mov rdi, rbx
    lea rsi, [rel exc_SyntaxError_type]
    CSTRING rdx, "this statement is not supported yet"
    xor ecx, ecx
    xor r8d, r8d
    call comp_error
    xor eax, eax
    jmp .ret
.bad:
    mov rdi, rbx
    lea rsi, [rel exc_SyntaxError_type]
    CSTRING rdx, "invalid syntax"
    xor ecx, ecx
    xor r8d, r8d
    call comp_error
    xor eax, eax
.ret:
    pop r13
    pop r12
    pop rbx
    leave
    ret
END_FUNC cg_stmt

;; ============================================================================
;; cg_body(Comp *c, CompUnit *u, uint32_t node) -> rax = 1 ok, 0 error
;; Emit every statement in a node's child list.
;; ============================================================================
DEF_FUNC cg_body, CST_FRAME
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
    mov [rbp - CST_N], rcx
    mov qword [rbp - CST_I], 0
.loop:
    mov rax, [rbp - CST_I]
    cmp rax, [rbp - CST_N]
    jae .done
    mov rdi, rbx
    mov rsi, r13
    call ast_at
    mov rsi, rax
    mov rdx, [rbp - CST_I]
    mov rdi, rbx
    call ast_child
    mov rdx, rax
    mov rdi, rbx
    mov rsi, r12
    call cg_stmt
    test eax, eax
    jz .fail
    inc qword [rbp - CST_I]
    jmp .loop
.done:
    mov eax, 1
.fail:
    pop r13
    pop r12
    pop rbx
    leave
    ret
END_FUNC cg_body

;; ============================================================================
;; cg_store(Comp *c, CompUnit *u, uint32_t target) -> rax = 1 ok, 0 error
;;
;; Emit the store for one target, consuming the value already on the stack.
;; A tuple or list target unpacks first; a starred element inside one turns the
;; UNPACK_SEQUENCE into an UNPACK_EX whose oparg carries the counts on each
;; side of the star.
;; ============================================================================
CSV_COMP  equ 8
CSV_UNIT  equ 16
CSV_NODE  equ 24
CSV_LINE  equ 32
CSV_I     equ 40
CSV_N     equ 48
CSV_STAR  equ 56
CSV_NPTR  equ 64
CSV_FRAME equ 72          ; + 3 pushes = 96
DEF_FUNC cg_store, CSV_FRAME
    push rbx
    push r12
    push r13
    mov rbx, rdi
    mov r12, rsi
    mov r13, rdx

    mov rdi, rbx
    mov rsi, r13
    call ast_at
    mov [rbp - CSV_NPTR], rax
    mov ecx, [rax + AstNode.lineno]
    mov [rbp - CSV_LINE], rcx
    movzx eax, byte [rax + AstNode.kind]

    cmp eax, AST_NAME
    je .name
    cmp eax, AST_ATTRIBUTE
    je .attribute
    cmp eax, AST_SUBSCRIPT
    je .subscript
    cmp eax, AST_TUPLE
    je .sequence
    cmp eax, AST_LIST
    je .sequence
    jmp .bad

.name:
    mov rax, [rbp - CSV_NPTR]
    mov esi, [rax + AstNode.a]
    mov rdi, rbx
    call ast_obj_at
    mov rdi, r12
    mov rsi, rax
    call cg_name
    mov rdx, rax
    mov rdi, r12
    mov esi, OP_STORE_NAME
    mov rcx, [rbp - CSV_LINE]
    call cg_emit
    jmp .ok

.attribute:
    ; The value is already on the stack, so the object goes above it and
    ; STORE_ATTR takes them in that order.
    mov rax, [rbp - CSV_NPTR]
    mov edx, [rax + AstNode.a]
    mov rdi, rbx
    mov rsi, r12
    call cg_expr
    test eax, eax
    jz .fail
    mov rdi, rbx
    mov rsi, r13
    call ast_at
    mov esi, [rax + AstNode.b]
    mov rdi, rbx
    call ast_obj_at
    mov rdi, r12
    mov rsi, rax
    call cg_name
    mov rdx, rax
    mov rdi, r12
    mov esi, OP_STORE_ATTR
    mov rcx, [rbp - CSV_LINE]
    call cg_emit
    jmp .ok

.subscript:
    mov rax, [rbp - CSV_NPTR]
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
    mov rdi, r12
    mov esi, OP_STORE_SUBSCR
    xor edx, edx
    mov rcx, [rbp - CSV_LINE]
    call cg_emit
    jmp .ok

.sequence:
    mov rax, [rbp - CSV_NPTR]
    mov ecx, [rax + AstNode.nchild]
    mov [rbp - CSV_N], rcx

    ; Find a starred element, if any; its position splits the counts.
    mov qword [rbp - CSV_STAR], -1
    mov qword [rbp - CSV_I], 0
.find_star:
    mov rax, [rbp - CSV_I]
    cmp rax, [rbp - CSV_N]
    jae .have_star
    call .child
    mov rdi, rbx
    mov rsi, rax
    call ast_at
    movzx eax, byte [rax + AstNode.kind]
    cmp eax, AST_STARRED
    jne .find_next
    cmp qword [rbp - CSV_STAR], -1
    jne .two_stars
    mov rax, [rbp - CSV_I]
    mov [rbp - CSV_STAR], rax
.find_next:
    inc qword [rbp - CSV_I]
    jmp .find_star
.have_star:

    cmp qword [rbp - CSV_STAR], -1
    jne .unpack_ex
    mov rdi, r12
    mov esi, OP_UNPACK_SEQUENCE
    mov rdx, [rbp - CSV_N]
    mov rcx, [rbp - CSV_LINE]
    call cg_emit
    jmp .store_each
.unpack_ex:
    ; The oparg packs the count before the star in the low byte and the count
    ; after it in the high byte.
    mov rax, [rbp - CSV_N]
    sub rax, [rbp - CSV_STAR]
    dec rax                             ; elements after the star
    shl rax, 8
    or rax, [rbp - CSV_STAR]
    mov rdi, r12
    mov esi, OP_UNPACK_EX
    mov rdx, rax
    mov rcx, [rbp - CSV_LINE]
    call cg_emit

.store_each:
    mov qword [rbp - CSV_I], 0
.store_loop:
    mov rax, [rbp - CSV_I]
    cmp rax, [rbp - CSV_N]
    jae .ok
    call .child
    mov rdx, rax
    mov rdi, rbx
    mov rsi, rax
    call ast_at
    movzx eax, byte [rax + AstNode.kind]
    cmp eax, AST_STARRED
    jne .store_plain
    ; A starred target stores the list UNPACK_EX left in its place.
    mov rdi, rbx
    mov rsi, rdx
    call ast_at
    mov edx, [rax + AstNode.a]
.store_plain:
    mov rdi, rbx
    mov rsi, r12
    call cg_store
    test eax, eax
    jz .fail
    inc qword [rbp - CSV_I]
    jmp .store_loop

.two_stars:
    mov rdi, rbx
    lea rsi, [rel exc_SyntaxError_type]
    CSTRING rdx, "two starred expressions in assignment"
    xor ecx, ecx
    xor r8d, r8d
    call comp_error
    jmp .fail
.bad:
    mov rdi, rbx
    lea rsi, [rel exc_SyntaxError_type]
    CSTRING rdx, "cannot assign to that expression"
    xor ecx, ecx
    xor r8d, r8d
    call comp_error
.fail:
    xor eax, eax
    jmp .ret
.ok:
    mov eax, 1
    jmp .ret

; Local: the i'th child of the target node.  Only leaf calls follow.
.child:
    mov rdi, rbx
    mov rsi, r13
    call ast_at
    mov rsi, rax
    mov rdx, [rbp - CSV_I]
    mov rdi, rbx
    call ast_child
    ret

.ret:
    pop r13
    pop r12
    pop rbx
    leave
    ret
END_FUNC cg_store

;; ============================================================================
;; Statement emitters.  All share
;;     fn(Comp *c, CompUnit *u, uint32_t node) -> rax = 1 ok, 0 error
;; and leave the stack as they found it.
;; ============================================================================

;; cg_s_expr - a bare expression statement: evaluate it and throw it away.
DEF_FUNC_LOCAL cg_s_expr, CST_FRAME
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
    mov [rbp - CST_LINE], rcx
    mov edx, [rax + AstNode.a]
    mov rdi, rbx
    mov rsi, r12
    call cg_expr
    test eax, eax
    jz .fail
    mov rdi, r12
    mov esi, OP_POP_TOP
    xor edx, edx
    mov rcx, [rbp - CST_LINE]
    call cg_emit
    mov eax, 1
.fail:
    pop r13
    pop r12
    pop rbx
    leave
    ret
END_FUNC cg_s_expr

;; cg_s_pass - nothing at all.  A `pass` exists to satisfy the grammar, not the
;; code generator; an empty suite is impossible, so nothing needs emitting.
DEF_FUNC_LOCAL cg_s_pass, CST_FRAME
    mov eax, 1
    leave
    ret
END_FUNC cg_s_pass

;; cg_s_assign - `a = b = value`
;; The value is computed once; each extra target takes a COPY of it, because
;; assignment binds a value rather than re-evaluating an expression.
DEF_FUNC_LOCAL cg_s_assign, CST_FRAME
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
    mov [rbp - CST_LINE], rcx
    mov ecx, [rax + AstNode.nchild]
    mov [rbp - CST_N], rcx
    mov edx, [rax + AstNode.b]          ; the value
    mov rdi, rbx
    mov rsi, r12
    call cg_expr
    test eax, eax
    jz .fail

    mov qword [rbp - CST_I], 0
.loop:
    mov rax, [rbp - CST_I]
    cmp rax, [rbp - CST_N]
    jae .done
    ; Every target but the last needs its own copy of the value.
    mov rax, [rbp - CST_I]
    inc rax
    cmp rax, [rbp - CST_N]
    jae .no_copy
    mov rdi, r12
    mov esi, OP_COPY
    mov edx, 1
    mov rcx, [rbp - CST_LINE]
    call cg_emit
.no_copy:
    mov rdi, rbx
    mov rsi, r13
    call ast_at
    mov rsi, rax
    mov rdx, [rbp - CST_I]
    mov rdi, rbx
    call ast_child
    mov rdx, rax
    mov rdi, rbx
    mov rsi, r12
    call cg_store
    test eax, eax
    jz .fail
    inc qword [rbp - CST_I]
    jmp .loop
.done:
    mov eax, 1
.fail:
    pop r13
    pop r12
    pop rbx
    leave
    ret
END_FUNC cg_s_assign

;; cg_s_augassign - `a += b`
;; The target is loaded, combined and stored back.  A subscript or attribute
;; target evaluates its object twice here, which CPython also does.
DEF_FUNC_LOCAL cg_s_augassign, CST_FRAME
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
    mov [rbp - CST_LINE], rcx
    movzx ecx, byte [rax + AstNode.subkind]
    mov [rbp - CST_TMP], rcx
    mov ecx, [rax + AstNode.a]
    mov [rbp - CST_TMP2], rcx

    ; Load the target's current value.  It was marked CTX_STORE by the parser,
    ; so read it as a load by temporarily treating the node as an expression.
    mov rdi, rbx
    mov rsi, [rbp - CST_TMP2]
    call ast_at
    mov byte [rax + AstNode.subkind], CTX_LOAD
    mov rdi, rbx
    mov rsi, r12
    mov rdx, [rbp - CST_TMP2]
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

    mov rdi, r12
    mov esi, OP_BINARY_OP
    mov rdx, [rbp - CST_TMP]
    mov rcx, [rbp - CST_LINE]
    call cg_emit

    mov rdi, rbx
    mov rsi, [rbp - CST_TMP2]
    call ast_at
    mov byte [rax + AstNode.subkind], CTX_STORE
    mov rdi, rbx
    mov rsi, r12
    mov rdx, [rbp - CST_TMP2]
    call cg_store
    jmp .ret
.fail:
    xor eax, eax
.ret:
    pop r13
    pop r12
    pop rbx
    leave
    ret
END_FUNC cg_s_augassign

;; cg_s_annassign - `x: T` and `x: T = v`
;; The annotation is evaluated for its side effects and discarded: apython has
;; no module __annotations__ yet, and evaluating it is what makes a bad
;; annotation raise where CPython raises.
DEF_FUNC_LOCAL cg_s_annassign, CST_FRAME
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
    mov [rbp - CST_LINE], rcx
    mov ecx, [rax + AstNode.c]
    mov [rbp - CST_TMP], rcx
    test ecx, ecx
    jz .no_value

    mov edx, ecx
    mov rdi, rbx
    mov rsi, r12
    call cg_expr
    test eax, eax
    jz .fail
    mov rdi, rbx
    mov rsi, r13
    call ast_at
    mov edx, [rax + AstNode.a]
    mov rdi, rbx
    mov rsi, r12
    call cg_store
    test eax, eax
    jz .fail
.no_value:
    mov eax, 1
.fail:
    pop r13
    pop r12
    pop rbx
    leave
    ret
END_FUNC cg_s_annassign

;; cg_s_delete - `del a, b[0]`
DEF_FUNC_LOCAL cg_s_delete, CST_FRAME
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
    mov [rbp - CST_N], rcx
    mov qword [rbp - CST_I], 0
.loop:
    mov rax, [rbp - CST_I]
    cmp rax, [rbp - CST_N]
    jae .done
    mov rdi, rbx
    mov rsi, r13
    call ast_at
    mov rsi, rax
    mov rdx, [rbp - CST_I]
    mov rdi, rbx
    call ast_child
    mov rdx, rax
    mov rdi, rbx
    mov rsi, r12
    call cg_delete_target
    test eax, eax
    jz .fail
    inc qword [rbp - CST_I]
    jmp .loop
.done:
    mov eax, 1
.fail:
    pop r13
    pop r12
    pop rbx
    leave
    ret
END_FUNC cg_s_delete

;; cg_delete_target(Comp *c, CompUnit *u, uint32_t target) -> 1 ok, 0 error
DEF_FUNC cg_delete_target, CST_FRAME
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
    mov [rbp - CST_LINE], rcx
    movzx eax, byte [rax + AstNode.kind]
    cmp eax, AST_NAME
    je .name
    cmp eax, AST_ATTRIBUTE
    je .attribute
    cmp eax, AST_SUBSCRIPT
    je .subscript
    jmp .bad
.name:
    mov rdi, rbx
    mov rsi, r13
    call ast_at
    mov esi, [rax + AstNode.a]
    mov rdi, rbx
    call ast_obj_at
    mov rdi, r12
    mov rsi, rax
    call cg_name
    mov rdx, rax
    mov rdi, r12
    mov esi, OP_DELETE_NAME
    mov rcx, [rbp - CST_LINE]
    call cg_emit
    jmp .ok
.attribute:
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
    mov esi, [rax + AstNode.b]
    mov rdi, rbx
    call ast_obj_at
    mov rdi, r12
    mov rsi, rax
    call cg_name
    mov rdx, rax
    mov rdi, r12
    mov esi, OP_DELETE_ATTR
    mov rcx, [rbp - CST_LINE]
    call cg_emit
    jmp .ok
.subscript:
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
    mov rdi, r12
    mov esi, OP_DELETE_SUBSCR
    xor edx, edx
    mov rcx, [rbp - CST_LINE]
    call cg_emit
    jmp .ok
.bad:
    mov rdi, rbx
    lea rsi, [rel exc_SyntaxError_type]
    CSTRING rdx, "cannot delete that expression"
    xor ecx, ecx
    xor r8d, r8d
    call comp_error
.fail:
    xor eax, eax
    jmp .ret
.ok:
    mov eax, 1
.ret:
    pop r13
    pop r12
    pop rbx
    leave
    ret
END_FUNC cg_delete_target

;; cg_s_scope - `global x` / `nonlocal x` emit nothing; they are declarations
;; the symbol table reads, not instructions.
DEF_FUNC_LOCAL cg_s_scope, CST_FRAME
    mov eax, 1
    leave
    ret
END_FUNC cg_s_scope

;; cg_s_assert - `assert test, msg`
;;     <test>; POP_JUMP_IF_TRUE end
;;     LOAD_ASSERTION_ERROR; [<msg>]; CALL 0/1; RAISE_VARARGS 1
;;   end:
DEF_FUNC_LOCAL cg_s_assert, CST_FRAME
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
    mov [rbp - CST_LINE], rcx

    mov rdi, r12
    call cg_label_new
    mov [rbp - CST_TMP], rax

    mov rdi, rbx
    mov rsi, r13
    call ast_at
    mov edx, [rax + AstNode.a]
    mov rdi, rbx
    mov rsi, r12
    call cg_expr
    test eax, eax
    jz .fail
    mov rdi, r12
    mov esi, OP_POP_JUMP_IF_TRUE
    mov rdx, [rbp - CST_TMP]
    mov rcx, [rbp - CST_LINE]
    call cg_emit_jump

    mov rdi, r12
    mov esi, OP_LOAD_ASSERTION_ERROR
    xor edx, edx
    mov rcx, [rbp - CST_LINE]
    call cg_emit

    mov rdi, rbx
    mov rsi, r13
    call ast_at
    mov ecx, [rax + AstNode.b]
    mov [rbp - CST_TMP2], rcx
    test ecx, ecx
    jz .no_msg
    mov edx, ecx
    mov rdi, rbx
    mov rsi, r12
    call cg_expr
    test eax, eax
    jz .fail
.no_msg:
    mov rdi, r12
    mov esi, OP_CALL
    xor edx, edx
    cmp qword [rbp - CST_TMP2], 0
    je .call_arity
    mov edx, 1
.call_arity:
    mov rcx, [rbp - CST_LINE]
    call cg_emit
    mov rdi, r12
    mov esi, OP_RAISE_VARARGS
    mov edx, 1
    mov rcx, [rbp - CST_LINE]
    call cg_emit

    mov rdi, r12
    mov rsi, [rbp - CST_TMP]
    call cg_label_bind
    mov eax, 1
.fail:
    pop r13
    pop r12
    pop rbx
    leave
    ret
END_FUNC cg_s_assert

;; cg_s_raise - `raise`, `raise E`, `raise E from F`
;; RAISE_VARARGS' oparg is simply how many operands were pushed.
DEF_FUNC_LOCAL cg_s_raise, CST_FRAME
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
    mov [rbp - CST_LINE], rcx
    mov qword [rbp - CST_N], 0

    mov ecx, [rax + AstNode.a]
    test ecx, ecx
    jz .emit
    mov edx, ecx
    mov rdi, rbx
    mov rsi, r12
    call cg_expr
    test eax, eax
    jz .fail
    mov qword [rbp - CST_N], 1

    mov rdi, rbx
    mov rsi, r13
    call ast_at
    mov ecx, [rax + AstNode.b]
    test ecx, ecx
    jz .emit
    mov edx, ecx
    mov rdi, rbx
    mov rsi, r12
    call cg_expr
    test eax, eax
    jz .fail
    mov qword [rbp - CST_N], 2
.emit:
    mov rdi, r12
    mov esi, OP_RAISE_VARARGS
    mov rdx, [rbp - CST_N]
    mov rcx, [rbp - CST_LINE]
    call cg_emit
    mov eax, 1
.fail:
    pop r13
    pop r12
    pop rbx
    leave
    ret
END_FUNC cg_s_raise


;; ============================================================================
;; cg_load_const_int(CompUnit *u, int64_t v, int line)
;; A small integer constant, for the import level and similar fixed operands.
;; ============================================================================
DEF_FUNC cg_load_const_int, 16          ; + 2 pushes = 32
    push rbx
    push r12
    mov rbx, rdi
    mov r12, rdx
    mov rax, rsi
    V_PACK_I64 rax, rcx
    mov rdi, rbx
    mov rsi, rax
    call cg_const
    mov rdx, rax
    mov rdi, rbx
    mov esi, OP_LOAD_CONST
    mov rcx, r12
    call cg_emit
    pop r12
    pop rbx
    leave
    ret
END_FUNC cg_load_const_int

;; ============================================================================
;; cg_load_none(CompUnit *u, int line)
;; ============================================================================
DEF_FUNC cg_load_none, 16          ; + 2 pushes = 32
    push rbx
    push r12
    mov rbx, rdi
    mov r12, rsi
    lea rsi, [rel none_singleton]
    INCREF rsi
    mov rdi, rbx
    call cg_const
    mov rdx, rax
    mov rdi, rbx
    mov esi, OP_LOAD_CONST
    mov rcx, r12
    call cg_emit
    pop r12
    pop rbx
    leave
    ret
END_FUNC cg_load_none

;; ============================================================================
;; cg_s_import - `import a.b as c`
;;
;;     LOAD_CONST 0        ; the relative level: absolute
;;     LOAD_CONST None     ; no fromlist
;;     IMPORT_NAME "a.b"
;;     STORE_NAME ...
;;
;; Without an `as`, a dotted import binds the TOP package -- `import a.b` binds
;; `a`, not `a.b` -- because the submodule is reached through the attribute.
;; With an `as` it binds the final module, which needs an IMPORT_FROM walk.
;; ============================================================================
CIM_COMP  equ 8
CIM_UNIT  equ 16
CIM_NODE  equ 24
CIM_LINE  equ 32
CIM_I     equ 40
CIM_N     equ 48
CIM_ALIAS equ 56
CIM_FRAME equ 72          ; + 3 pushes = 96
DEF_FUNC_LOCAL cg_s_import, CIM_FRAME
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
    mov [rbp - CIM_LINE], rcx
    mov ecx, [rax + AstNode.nchild]
    mov [rbp - CIM_N], rcx
    mov qword [rbp - CIM_I], 0

.loop:
    mov rax, [rbp - CIM_I]
    cmp rax, [rbp - CIM_N]
    jae .done
    mov rdi, rbx
    mov rsi, r13
    call ast_at
    mov rsi, rax
    mov rdx, [rbp - CIM_I]
    mov rdi, rbx
    call ast_child
    mov [rbp - CIM_ALIAS], rax

    mov rdi, r12
    xor esi, esi
    mov rdx, [rbp - CIM_LINE]
    call cg_load_const_int
    mov rdi, r12
    mov rsi, [rbp - CIM_LINE]
    call cg_load_none

    mov rdi, rbx
    mov rsi, [rbp - CIM_ALIAS]
    call ast_at
    mov esi, [rax + AstNode.a]
    mov rdi, rbx
    call ast_obj_at
    mov rdi, r12
    mov rsi, rax
    call cg_name
    mov rdx, rax
    mov rdi, r12
    mov esi, OP_IMPORT_NAME
    mov rcx, [rbp - CIM_LINE]
    call cg_emit

    mov rdi, rbx
    mov rsi, [rbp - CIM_ALIAS]
    call ast_at
    mov ecx, [rax + AstNode.b]
    test ecx, ecx
    jnz .with_as

    ; No `as`: bind the first component of the dotted name.
    mov esi, [rax + AstNode.a]
    mov rdi, rbx
    call ast_obj_at
    mov rdi, r12
    mov rsi, rax
    call cg_name_first_component
    mov rdx, rax
    mov rdi, r12
    mov esi, OP_STORE_NAME
    mov rcx, [rbp - CIM_LINE]
    call cg_emit
    jmp .next

.with_as:
    mov esi, ecx
    mov rdi, rbx
    call ast_obj_at
    mov rdi, r12
    mov rsi, rax
    call cg_name
    mov rdx, rax
    mov rdi, r12
    mov esi, OP_STORE_NAME
    mov rcx, [rbp - CIM_LINE]
    call cg_emit
.next:
    inc qword [rbp - CIM_I]
    jmp .loop
.done:
    mov eax, 1
    pop r13
    pop r12
    pop rbx
    leave
    ret
END_FUNC cg_s_import

;; ============================================================================
;; cg_name_first_component(CompUnit *u, PyStrObject *dotted) -> co_names index
;; `import a.b` binds `a`; this interns the leading component of the name.
;; ============================================================================
DEF_FUNC cg_name_first_component, 16          ; + 2 pushes = 32
    push rbx
    push r12
    mov rbx, rdi
    mov r12, rsi
    lea rdi, [r12 + PyStrObject.data]
    mov rcx, [r12 + PyStrObject.ob_size]
    xor edx, edx
.scan:
    cmp rdx, rcx
    jae .whole
    cmp byte [rdi + rdx], '.'
    je .found
    inc rdx
    jmp .scan
.whole:
    ; No dot: the name is already its own first component.
    mov rdi, rbx
    mov rsi, r12
    call cg_name
    pop r12
    pop rbx
    leave
    ret
.found:
    mov rsi, rdx
    call comp_intern                     ; rdi already points at the text
    test rax, rax
    jz .fail
    mov rdi, rbx
    mov rsi, rax
    push rax
    call cg_name
    pop rdi
    push rax
    call obj_decref                      ; cg_name kept its own reference
    pop rax
    pop r12
    pop rbx
    leave
    ret
.fail:
    xor eax, eax
    pop r12
    pop rbx
    leave
    ret
END_FUNC cg_name_first_component

;; ============================================================================
;; cg_s_importfrom - `from a import b as c` and `from . import x`
;;
;;     LOAD_CONST <level>
;;     LOAD_CONST ('b', ...)      ; the fromlist, as a constant tuple
;;     IMPORT_NAME "a"
;;     IMPORT_FROM b ; STORE_NAME c   (repeated)
;;     POP_TOP                    ; drop the module itself
;;
;; `from m import *` is CALL_INTRINSIC_1 INTRINSIC_IMPORT_STAR instead, which
;; consumes the module rather than leaving it.
;; ============================================================================
CIF_COMP  equ 8
CIF_UNIT  equ 16
CIF_NODE  equ 24
CIF_LINE  equ 32
CIF_I     equ 40
CIF_N     equ 48
CIF_ALIAS equ 56
CIF_TUP   equ 64
CIF_FRAME equ 72          ; + 3 pushes = 96
DEF_FUNC_LOCAL cg_s_importfrom, CIF_FRAME
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
    mov [rbp - CIF_LINE], rcx
    mov ecx, [rax + AstNode.nchild]
    mov [rbp - CIF_N], rcx

    ; level
    movzx esi, byte [rax + AstNode.subkind]
    mov rdi, r12
    mov rdx, [rbp - CIF_LINE]
    call cg_load_const_int

    ; Is this the star form?
    mov rdi, rbx
    mov rsi, r13
    call ast_at
    mov rsi, rax
    xor edx, edx
    mov rdi, rbx
    call ast_child
    mov rdi, rbx
    mov rsi, rax
    call ast_at
    movzx eax, byte [rax + AstNode.subkind]
    cmp eax, 1
    je .star

    ; The fromlist, as one constant tuple of names.
    mov rdi, rbx
    mov rsi, r13
    call cg_fromlist_tuple
    test rax, rax
    jz .fail
    mov rdi, r12
    mov rsi, rax
    call cg_const
    mov rdx, rax
    mov rdi, r12
    mov esi, OP_LOAD_CONST
    mov rcx, [rbp - CIF_LINE]
    call cg_emit
    jmp .import_name

.star:
    ; CPython still passes a fromlist of ('*',) so the import machinery knows.
    mov rdi, rbx
    mov rsi, r13
    call cg_fromlist_star
    test rax, rax
    jz .fail
    mov rdi, r12
    mov rsi, rax
    call cg_const
    mov rdx, rax
    mov rdi, r12
    mov esi, OP_LOAD_CONST
    mov rcx, [rbp - CIF_LINE]
    call cg_emit

.import_name:
    mov rdi, rbx
    mov rsi, r13
    call ast_at
    mov ecx, [rax + AstNode.a]
    test ecx, ecx
    jz .empty_module
    mov esi, ecx
    mov rdi, rbx
    call ast_obj_at
    jmp .have_module
.empty_module:
    ; `from . import x` has no module name; IMPORT_NAME still wants one.
    mov rdi, rbx
    call comp_empty_string
.have_module:
    mov rdi, r12
    mov rsi, rax
    call cg_name
    mov rdx, rax
    mov rdi, r12
    mov esi, OP_IMPORT_NAME
    mov rcx, [rbp - CIF_LINE]
    call cg_emit

    mov rdi, rbx
    mov rsi, r13
    call ast_at
    mov rsi, rax
    xor edx, edx
    mov rdi, rbx
    call ast_child
    mov rdi, rbx
    mov rsi, rax
    call ast_at
    movzx eax, byte [rax + AstNode.subkind]
    cmp eax, 1
    jne .each_name

    mov rdi, r12
    mov esi, OP_CALL_INTRINSIC_1
    mov edx, INTRINSIC_IMPORT_STAR
    mov rcx, [rbp - CIF_LINE]
    call cg_emit
    mov eax, 1
    jmp .ret

.each_name:
    mov qword [rbp - CIF_I], 0
.loop:
    mov rax, [rbp - CIF_I]
    cmp rax, [rbp - CIF_N]
    jae .finish
    mov rdi, rbx
    mov rsi, r13
    call ast_at
    mov rsi, rax
    mov rdx, [rbp - CIF_I]
    mov rdi, rbx
    call ast_child
    mov [rbp - CIF_ALIAS], rax

    mov rdi, rbx
    mov rsi, rax
    call ast_at
    mov esi, [rax + AstNode.a]
    mov rdi, rbx
    call ast_obj_at
    mov rdi, r12
    mov rsi, rax
    call cg_name
    mov rdx, rax
    mov rdi, r12
    mov esi, OP_IMPORT_FROM
    mov rcx, [rbp - CIF_LINE]
    call cg_emit

    ; Bind under the `as` name when there is one, else its own.
    mov rdi, rbx
    mov rsi, [rbp - CIF_ALIAS]
    call ast_at
    mov ecx, [rax + AstNode.b]
    test ecx, ecx
    jnz .bind
    mov ecx, [rax + AstNode.a]
.bind:
    mov esi, ecx
    mov rdi, rbx
    call ast_obj_at
    mov rdi, r12
    mov rsi, rax
    call cg_name
    mov rdx, rax
    mov rdi, r12
    mov esi, OP_STORE_NAME
    mov rcx, [rbp - CIF_LINE]
    call cg_emit
    inc qword [rbp - CIF_I]
    jmp .loop

.finish:
    mov rdi, r12
    mov esi, OP_POP_TOP
    xor edx, edx
    mov rcx, [rbp - CIF_LINE]
    call cg_emit
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
END_FUNC cg_s_importfrom


;; ============================================================================
;; cg_fromlist_tuple(Comp *c, uint32_t node) -> rax = an owned tuple, or 0
;; The names a `from ... import ...` asks for, as one constant.
;; ============================================================================
FT_COMP  equ 8
FT_NODE  equ 16
FT_TUP   equ 24
FT_I     equ 32
FT_N     equ 40
FT_FRAME equ 40           ; + 3 pushes = 64
DEF_FUNC cg_fromlist_tuple, FT_FRAME
    push rbx
    push r12
    push r13
    mov rbx, rdi
    mov r13, rsi
    mov rdi, rbx
    mov rsi, r13
    call ast_at
    mov ecx, [rax + AstNode.nchild]
    mov [rbp - FT_N], rcx
    mov rdi, rcx
    call tuple_new
    test rax, rax
    jz .fail
    mov [rbp - FT_TUP], rax
    mov r12, [rax + PyTupleObject.ob_item]
    mov qword [rbp - FT_I], 0
.loop:
    mov rax, [rbp - FT_I]
    cmp rax, [rbp - FT_N]
    jae .done
    mov rdi, rbx
    mov rsi, r13
    call ast_at
    mov rsi, rax
    mov rdx, [rbp - FT_I]
    mov rdi, rbx
    call ast_child
    mov rdi, rbx
    mov rsi, rax
    call ast_at
    mov esi, [rax + AstNode.a]
    mov rdi, rbx
    call ast_obj_at
    mov rcx, [rbp - FT_I]
    mov [r12 + rcx*8], rax
    INCREF_V rax, rdx
    inc qword [rbp - FT_I]
    jmp .loop
.done:
    mov rax, [rbp - FT_TUP]
.fail:
    pop r13
    pop r12
    pop rbx
    leave
    ret
END_FUNC cg_fromlist_tuple

;; ============================================================================
;; cg_fromlist_star(Comp *c, uint32_t node) -> rax = the tuple ('*',)
;; The import machinery keys off this to know a star import was asked for.
;; ============================================================================
DEF_FUNC cg_fromlist_star, 16          ; + 2 pushes = 32
    push rbx
    push r12
    mov rbx, rdi
    mov edi, 1
    call tuple_new
    test rax, rax
    jz .fail
    mov r12, rax
    lea rdi, [rel cg_star_name]
    call str_from_cstr_heap
    mov rdx, [r12 + PyTupleObject.ob_item]
    mov [rdx], rax
    mov rax, r12
.fail:
    pop r12
    pop rbx
    leave
    ret
END_FUNC cg_fromlist_star

section .rodata
;; AST kind -> statement emitter.  A zero entry is reported by cg_stmt as
;; unsupported rather than silently emitting nothing.
align 8
cg_stmt_table:
    dq 0                ;  0 AST_NULL
    dq 0                ;  1 AST_CONST
    dq 0                ;  2 AST_NAME
    dq 0                ;  3 AST_BINOP
    dq 0                ;  4 AST_UNARYOP
    dq 0                ;  5 AST_SHIFT
    dq 0                ;  6 AST_COMPARE
    dq 0                ;  7 AST_IFEXP
    dq 0                ;  8 AST_LAMBDA
    dq 0                ;  9 AST_TUPLE
    dq 0                ; 10 AST_LIST
    dq 0                ; 11 AST_SET
    dq 0                ; 12 AST_DICT
    dq 0                ; 13 AST_CALL
    dq 0                ; 14 AST_ATTRIBUTE
    dq 0                ; 15 AST_SUBSCRIPT
    dq 0                ; 16 AST_SLICE
    dq 0                ; 17 AST_STARRED
    dq 0                ; 18 AST_DOUBLESTARRED
    dq 0                ; 19 AST_KEYWORD
    dq 0                ; 20 AST_NAMEDEXPR
    dq 0                ; 21 AST_YIELD
    dq 0                ; 22 AST_YIELDFROM
    dq 0                ; 23 AST_AWAIT
    dq 0                ; 24 AST_JOINEDSTR
    dq 0                ; 25 AST_FORMATTEDVALUE
    dq 0                ; 26 AST_LISTCOMP
    dq 0                ; 27 AST_SETCOMP
    dq 0                ; 28 AST_DICTCOMP
    dq 0                ; 29 AST_GENEXP
    dq 0                ; 30 AST_COMPREHENSION
    dq 0                ; 31 
    dq 0                ; 32 
    dq 0                ; 33 
    dq 0                ; 34 
    dq 0                ; 35 
    dq 0                ; 36 
    dq 0                ; 37 
    dq 0                ; 38 
    dq 0                ; 39 
    dq 0                ; 40 AST_MODULE
    dq 0                ; 41 AST_EXPRESSION
    dq cg_s_expr        ; 42 AST_EXPR_STMT
    dq cg_s_assign      ; 43 AST_ASSIGN
    dq cg_s_augassign   ; 44 AST_AUGASSIGN
    dq cg_s_annassign   ; 45 AST_ANNASSIGN
    dq 0                ; 46 AST_IF
    dq 0                ; 47 AST_WHILE
    dq 0                ; 48 AST_FOR
    dq 0                ; 49 AST_BLOCK
    dq cg_s_pass        ; 50 AST_PASS
    dq 0                ; 51 AST_BREAK
    dq 0                ; 52 AST_CONTINUE
    dq 0                ; 53 AST_RETURN
    dq cg_s_delete      ; 54 AST_DELETE
    dq cg_s_raise       ; 55 AST_RAISE
    dq cg_s_assert      ; 56 AST_ASSERT
    dq cg_s_scope       ; 57 AST_GLOBAL
    dq cg_s_scope       ; 58 AST_NONLOCAL
    dq cg_s_import      ; 59 AST_IMPORT
    dq cg_s_importfrom  ; 60 AST_IMPORTFROM
    dq 0                ; 61 AST_ALIAS
    dq 0                ; 62 AST_FUNCTIONDEF
    dq 0                ; 63 AST_CLASSDEF
    dq 0                ; 64 AST_TRY
    dq 0                ; 65 AST_HANDLER
    dq 0                ; 66 AST_WITH
    dq 0                ; 67 AST_WITHITEM
    dq 0                ; 68 AST_ARGUMENTS
    dq 0                ; 69 AST_ARG
    dq 0                ; 70 AST_MATCH
    dq 0                ; 71 AST_EXTRA
    dq 0                ; 72 
    dq 0                ; 73 
    dq 0                ; 74 
    dq 0                ; 75 
    dq 0                ; 76 
    dq 0                ; 77 
    dq 0                ; 78 
    dq 0                ; 79 

cg_star_name: db "*", 0


ASM_INIT
