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
%include "value.inc"
%include "opcodes.inc"
%include "compiler.inc"

extern ast_at
extern ast_child
extern ast_obj_at
extern sym_at
extern comp_intern_cstr
extern cg_const
extern comp_keep
extern cg_emit
extern cg_emit_jump
extern cg_emit_jump_back
extern cg_loop_pop
extern cg_loop_push
extern cg_loop_top
extern cg_expr
extern cg_label_bind
extern cg_label_new
extern cg_name
extern cg_nameop
extern cg_unwind_finallys
extern cg_s_asyncfor
extern cg_s_match
extern cg_s_classdef
extern cg_s_decorated
extern cg_s_try
extern cg_s_with
extern cg_s_functiondef
extern cg_s_return
extern comp_error

extern comp_empty_string
extern none_singleton
extern str_from_cstr_heap
extern tuple_new

extern exc_SyntaxError_type

; --- Named frame-layout constants ---
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
    mov [rbx + Comp.cur_unit], r12
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
    mov rdx, rax
    mov rcx, [rbp - CSV_LINE]
    mov [r12 + CompUnit.curline], ecx
    mov rdi, rbx
    mov rsi, r12
    mov ecx, CTX_STORE
    xor r8d, r8d
    call cg_nameop
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
;;
;; The target is evaluated ONCE.  A name just loads and stores, but a subscript
;; or an attribute has an object expression -- and an index expression -- that
;; must not run twice: `d[next(it)] += 5` drew two values from the iterator,
;; and `obj().n += 5` called obj() twice.  CPython duplicates the pieces it has
;; already evaluated with COPY and puts them back in the order the store wants
;; with SWAP:
;;
;;     obj; idx; COPY 2; COPY 2; BINARY_SUBSCR; rhs; BINARY_OP; SWAP 3; SWAP 2;
;;     STORE_SUBSCR
;;     obj; COPY 1; LOAD_ATTR n; rhs; BINARY_OP; SWAP 2; STORE_ATTR n
;; ============================================================================
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

    mov rdi, rbx
    mov rsi, [rbp - CST_TMP2]
    call ast_at
    movzx ecx, byte [rax + AstNode.kind]
    cmp ecx, AST_ATTRIBUTE
    je .aug_attr
    cmp ecx, AST_SUBSCRIPT
    je .aug_subscr

    ; A plain name: load, combine, store.  It was marked CTX_STORE by the
    ; parser, so read it as a load by temporarily treating it as an
    ; expression.
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

    call .aug_rhs_and_op
    test eax, eax
    jz .fail

    mov rdi, rbx
    mov rsi, [rbp - CST_TMP2]
    call ast_at
    mov byte [rax + AstNode.subkind], CTX_STORE
    mov rdi, rbx
    mov rsi, r12
    mov rdx, [rbp - CST_TMP2]
    call cg_store
    jmp .ret

.aug_attr:
    ; obj; COPY 1; LOAD_ATTR n; rhs; BINARY_OP; SWAP 2; STORE_ATTR n
    mov edx, [rax + AstNode.a]
    mov rdi, rbx
    mov rsi, r12
    call cg_expr
    test eax, eax
    jz .fail
    mov rdi, r12
    mov esi, OP_COPY
    mov edx, 1
    mov rcx, [rbp - CST_LINE]
    call cg_emit

    call .aug_attr_name
    mov rdx, rax
    add rdx, rdx                        ; index << 1: not the method form
    mov rdi, r12
    mov esi, OP_LOAD_ATTR
    mov rcx, [rbp - CST_LINE]
    call cg_emit

    call .aug_rhs_and_op
    test eax, eax
    jz .fail

    mov rdi, r12
    mov esi, OP_SWAP
    mov edx, 2
    mov rcx, [rbp - CST_LINE]
    call cg_emit
    call .aug_attr_name
    mov rdx, rax
    mov rdi, r12
    mov esi, OP_STORE_ATTR
    mov rcx, [rbp - CST_LINE]
    call cg_emit
    mov eax, 1
    jmp .ret

.aug_subscr:
    ; obj; idx; COPY 2; COPY 2; BINARY_SUBSCR; rhs; BINARY_OP; SWAP 3; SWAP 2;
    ; STORE_SUBSCR
    mov ecx, [rax + AstNode.b]
    mov [rbp - CST_I], rcx              ; the index expression
    mov edx, [rax + AstNode.a]
    mov rdi, rbx
    mov rsi, r12
    call cg_expr
    test eax, eax
    jz .fail
    mov rdi, rbx
    mov rsi, r12
    mov rdx, [rbp - CST_I]
    call cg_expr
    test eax, eax
    jz .fail

    mov rdi, r12
    mov esi, OP_COPY
    mov edx, 2
    mov rcx, [rbp - CST_LINE]
    call cg_emit
    mov rdi, r12
    mov esi, OP_COPY
    mov edx, 2
    mov rcx, [rbp - CST_LINE]
    call cg_emit
    mov rdi, r12
    mov esi, OP_BINARY_SUBSCR
    xor edx, edx
    mov rcx, [rbp - CST_LINE]
    call cg_emit

    call .aug_rhs_and_op
    test eax, eax
    jz .fail

    mov rdi, r12
    mov esi, OP_SWAP
    mov edx, 3
    mov rcx, [rbp - CST_LINE]
    call cg_emit
    mov rdi, r12
    mov esi, OP_SWAP
    mov edx, 2
    mov rcx, [rbp - CST_LINE]
    call cg_emit
    mov rdi, r12
    mov esi, OP_STORE_SUBSCR
    xor edx, edx
    mov rcx, [rbp - CST_LINE]
    call cg_emit
    mov eax, 1
    jmp .ret

; Local: the right-hand side and the in-place operator.
.aug_rhs_and_op:
    sub rsp, 8
    mov rdi, rbx
    mov rsi, r13
    call ast_at
    mov edx, [rax + AstNode.b]
    mov rdi, rbx
    mov rsi, r12
    call cg_expr
    test eax, eax
    jz .rhs_fail
    mov rdi, r12
    mov esi, OP_BINARY_OP
    mov rdx, [rbp - CST_TMP]
    mov rcx, [rbp - CST_LINE]
    call cg_emit
    mov eax, 1
    add rsp, 8
    ret
.rhs_fail:
    xor eax, eax
    add rsp, 8
    ret

; Local: the co_names index of the attribute the target names.
.aug_attr_name:
    sub rsp, 8
    mov rdi, rbx
    mov rsi, [rbp - CST_TMP2]
    call ast_at
    mov esi, [rax + AstNode.b]
    mov rdi, rbx
    call ast_obj_at
    mov rdi, r12
    mov rsi, rax
    call cg_name
    add rsp, 8
    ret
.fail:
    xor eax, eax
.ret:
    pop r13
    pop r12
    pop rbx
    leave
    ret
END_FUNC cg_s_augassign

;; ============================================================================
;; cg_has_annotation(Comp *c, uint32_t node) -> rax = 1 if the block contains
;; an annotated assignment
;;
;; SETUP_ANNOTATIONS goes at the top of a module or class body that has one
;; anywhere in it, including inside an `if` or a loop -- but not inside a
;; nested def or class, which get their own.
;; ============================================================================
HA_I     equ 8
HA_N     equ 16
HA_C     equ 24
HA_K     equ 32           ; the node's kind, for the AST_FOR fix-up
HA_FRAME equ 48           ; + 2 pushes = 64
;; The body form skips the kind test on the node itself: a class body is the
;; classdef's own child list, and the node would otherwise stop the walk dead.
;; The caller must therefore pass a node whose clist really IS a statement list
;; -- a classdef or a module.  Handed an AST_GLOBAL it would walk that node's
;; object indices as node indices, which is the trap cg_has_annotation below
;; refuses by allow-list.
DEF_FUNC cg_has_annotation_body, HA_FRAME
    push rbx
    push r12
    mov rbx, rdi
    mov r12, rsi
    test r12, r12
    jz .hb_no
    mov rdi, rbx
    mov rsi, r12
    call ast_at
    mov ecx, [rax + AstNode.nchild]
    mov [rbp - HA_N], rcx
    mov qword [rbp - HA_I], 0
.hb_loop:
    mov rax, [rbp - HA_I]
    cmp rax, [rbp - HA_N]
    jae .hb_no
    mov rdi, rbx
    mov rsi, r12
    call ast_at
    mov rsi, rax
    mov rdx, [rbp - HA_I]
    mov rdi, rbx
    call ast_child
    mov rsi, rax
    mov rdi, rbx
    call cg_has_annotation
    test eax, eax
    jnz .hb_yes
    inc qword [rbp - HA_I]
    jmp .hb_loop
.hb_yes:
    mov eax, 1
    pop r12
    pop rbx
    leave
    ret
.hb_no:
    xor eax, eax
    pop r12
    pop rbx
    leave
    ret
END_FUNC cg_has_annotation_body

DEF_FUNC cg_has_annotation, HA_FRAME
    push rbx
    push r12
    mov rbx, rdi
    mov r12, rsi
    test r12, r12
    jz .ha_no
    mov rdi, rbx
    mov rsi, r12
    call ast_at
    movzx ecx, byte [rax + AstNode.kind]
    mov [rbp - HA_K], rcx
    cmp ecx, AST_ANNASSIGN
    je .ha_yes
    cmp ecx, AST_FUNCTIONDEF
    je .ha_no
    cmp ecx, AST_CLASSDEF
    je .ha_no
    cmp ecx, AST_LAMBDA
    je .ha_no

    ; Only the compound statements have nested blocks in a/b/c.  Following
    ; a/b/c for every kind walks an *object* index as a node index -- the two
    ; arenas overlap -- and lands anywhere at all.
    cmp ecx, AST_IF
    je .ha_fields
    cmp ecx, AST_WHILE
    je .ha_fields
    cmp ecx, AST_FOR
    je .ha_fields
    cmp ecx, AST_TRY
    je .ha_fields
    cmp ecx, AST_WITH
    je .ha_fields
    cmp ecx, AST_MATCH
    je .ha_fields
    cmp ecx, AST_CASE
    je .ha_fields
    cmp ecx, AST_HANDLER
    je .ha_children_only
    cmp ecx, AST_BLOCK
    je .ha_children_only
    cmp ecx, AST_MODULE
    je .ha_children_only
    cmp ecx, AST_DECORATED
    je .ha_no
    ; Everything else: an allow-list, not a default-recurse.  A child list is
    ; only a list of NODES for the kinds above.  AST_GLOBAL and AST_NONLOCAL
    ; hold object indices there, and the two arenas overlap freely, so walking
    ; one as a node reads whatever sits at that index -- `global a` at module
    ; level recursed about a hundred thousand deep and segfaulted the compiler.
    ; AST_COMPARE is the same trap one size smaller: its child list interleaves
    ; raw CMPOP_* codes with node indices.
    jmp .ha_no

.ha_fields:
    mov ecx, [rax + AstNode.a]
    mov [rbp - HA_I], rcx
    mov ecx, [rax + AstNode.b]
    mov [rbp - HA_N], rcx
    mov ecx, [rax + AstNode.c]
    mov [rbp - HA_C], rcx

    mov rdi, rbx
    mov rsi, [rbp - HA_I]
    call cg_has_annotation
    test eax, eax
    jnz .ha_yes
    mov rdi, rbx
    mov rsi, [rbp - HA_N]
    call cg_has_annotation
    test eax, eax
    jnz .ha_yes
    mov rdi, rbx
    mov rsi, [rbp - HA_C]
    call cg_has_annotation
    test eax, eax
    jnz .ha_yes

    ; AST_FOR keeps its else block in clist with nchild at 0 -- a is the
    ; target, b the iterable and c the body -- so the child walk below never
    ; reaches it, and `for i in []: pass / else: x: int = 1` never emitted
    ; SETUP_ANNOTATIONS.  sym_visit has the same fix-up for the same reason.
    cmp qword [rbp - HA_K], AST_FOR
    jne .ha_children_only
    mov rdi, rbx
    mov rsi, r12
    call ast_at
    mov esi, [rax + AstNode.clist]
    mov rdi, rbx
    call cg_has_annotation
    test eax, eax
    jnz .ha_yes

.ha_children_only:

    mov rdi, rbx
    mov rsi, r12
    call ast_at
    mov ecx, [rax + AstNode.nchild]
    mov [rbp - HA_N], rcx
    mov qword [rbp - HA_I], 0
.ha_loop:
    mov rax, [rbp - HA_I]
    cmp rax, [rbp - HA_N]
    jae .ha_no
    mov rdi, rbx
    mov rsi, r12
    call ast_at
    mov rsi, rax
    mov rdx, [rbp - HA_I]
    mov rdi, rbx
    call ast_child
    mov rsi, rax
    mov rdi, rbx
    call cg_has_annotation
    test eax, eax
    jnz .ha_yes
    inc qword [rbp - HA_I]
    jmp .ha_loop
.ha_yes:
    mov eax, 1
    pop r12
    pop rbx
    leave
    ret
.ha_no:
    xor eax, eax
    pop r12
    pop rbx
    leave
    ret
END_FUNC cg_has_annotation

;; cg_s_annassign - `x: T` and `x: T = v`
;;
;; At module and class scope the annotation is evaluated and recorded in
;; __annotations__, which is what makes `x: Undefined` a NameError and what
;; dataclasses reads.  Inside a function it is not evaluated at all, as
;; CPython does.  Nothing was evaluated anywhere, so a bad annotation was
;; silently accepted and __annotations__ never existed.
;;
;;     <annotation>; LOAD_NAME __annotations__; LOAD_CONST 'x'; STORE_SUBSCR
;; ============================================================================
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

    ; Only a module or class body evaluates the annotation.
    mov rdi, rbx
    mov esi, [r12 + CompUnit.scope]     ; a dword field
    call sym_at
    cmp dword [rax + Scope.kind], SCOPE_FUNCTION
    je .ann_done

    mov rdi, rbx
    mov rsi, r13
    call ast_at
    mov edx, [rax + AstNode.b]          ; the annotation expression
    test edx, edx
    jz .ann_done
    mov rdi, rbx
    mov rsi, r12
    call cg_expr
    test eax, eax
    jz .fail

    ; A simple name is recorded; anything else is evaluated and dropped.  A
    ; PARENTHESISED name is not simple either -- `(x): int = 1` records
    ; nothing in CPython -- and the parser leaves that bit in the node's
    ; subkind, since (x) and x are otherwise the same Name node.
    mov rdi, rbx
    mov rsi, r13
    call ast_at
    cmp byte [rax + AstNode.subkind], 0
    jne .ann_discard
    mov edx, [rax + AstNode.a]
    mov rdi, rbx
    mov rsi, rdx
    push rdx
    call ast_at
    pop rdx
    movzx ecx, byte [rax + AstNode.kind]
    cmp ecx, AST_NAME
    jne .ann_discard

    mov esi, [rax + AstNode.a]
    mov rdi, rbx
    call ast_obj_at
    mov rdi, r12
    mov rsi, rax
    call cg_const
    mov [rbp - CST_I], rax

    mov rdi, rbx
    lea rsi, [rel cg_annotations_dunder]
    call comp_intern_cstr
    test rax, rax
    jz .fail
    mov rdi, r12
    mov rsi, rax
    call cg_name
    mov rdx, rax
    mov rdi, r12
    mov esi, OP_LOAD_NAME
    mov rcx, [rbp - CST_LINE]
    call cg_emit

    mov rdi, r12
    mov esi, OP_LOAD_CONST
    mov rdx, [rbp - CST_I]
    mov rcx, [rbp - CST_LINE]
    call cg_emit
    mov rdi, r12
    mov esi, OP_STORE_SUBSCR
    xor edx, edx
    mov rcx, [rbp - CST_LINE]
    call cg_emit
    jmp .ann_done

.ann_discard:
    mov rdi, r12
    mov esi, OP_POP_TOP
    xor edx, edx
    mov rcx, [rbp - CST_LINE]
    call cg_emit

.ann_done:
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
    cmp eax, AST_TUPLE
    je .sequence
    cmp eax, AST_LIST
    je .sequence
    jmp .bad

.sequence:
    ; `del (a, b)` and `del [a, b]` delete each element: the parentheses are
    ; grouping, not a target of their own.  (rax holds the kind by now, not
    ; the node.)
    mov rdi, rbx
    mov rsi, r13
    call ast_at
    mov ecx, [rax + AstNode.nchild]
    mov [rbp - CST_N], rcx
    mov qword [rbp - CST_I], 0
.seq_loop:
    mov rax, [rbp - CST_I]
    cmp rax, [rbp - CST_N]
    jae .ok
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
    jmp .seq_loop

.name:
    mov rdi, rbx
    mov rsi, r13
    call ast_at
    mov esi, [rax + AstNode.a]
    mov rdi, rbx
    call ast_obj_at
    mov rdx, rax
    mov rcx, [rbp - CST_LINE]
    mov [r12 + CompUnit.curline], ecx
    mov rdi, rbx
    mov rsi, r12
    mov ecx, CTX_DEL
    xor r8d, r8d
    call cg_nameop
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
;;     LOAD_ASSERTION_ERROR; [<msg>; CALL 0]; RAISE_VARARGS 1
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
    ; With a message, LOAD_ASSERTION_ERROR sits in the callable slot and the
    ; message in the one a bound method's self would occupy, so CALL's oparg
    ; counts zero further arguments.  Without one there is nothing to call:
    ; RAISE_VARARGS instantiates the class itself.
    mov rdi, r12
    mov esi, OP_CALL
    xor edx, edx
    mov rcx, [rbp - CST_LINE]
    call cg_emit
.no_msg:
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
CIM_LINE  equ 32
CIM_I     equ 40
CIM_N     equ 48
CIM_ALIAS equ 56
CIM_TARGET equ 64         ; the `as` name's object index
CIM_J     equ 72          ; which dotted component the walk is on
CIM_FRAME equ 88          ; + 3 pushes = 112
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

    ; The name this import binds, through the symbol table like any other
    ; assignment.  STORE_NAME unconditionally wrote into a locals mapping that
    ; a function frame does not have, so `def f(): import sys` left sys empty.
    mov esi, [rax + AstNode.c]
    mov rdi, rbx
    call ast_obj_at
    mov rdx, rax
    mov rcx, [rbp - CIM_LINE]
    mov [r12 + CompUnit.curline], ecx
    mov rdi, rbx
    mov rsi, r12
    mov ecx, CTX_STORE
    xor r8d, r8d
    call cg_nameop
    test eax, eax
    jz .fail
    jmp .next

.with_as:
    ; `import a.b.c as n` binds the *submodule*, not the top package: after
    ; IMPORT_NAME leaves `a` on the stack, each remaining component is walked
    ; with IMPORT_FROM.  Storing the IMPORT_NAME result straight into `n` gave
    ; `import os.path as p` the `os` module.
    mov [rbp - CIM_TARGET], rcx
    mov qword [rbp - CIM_J], 1
.from_loop:
    mov rdi, rbx
    mov rsi, [rbp - CIM_ALIAS]
    call ast_at
    mov esi, [rax + AstNode.a]
    mov rdi, rbx
    call ast_obj_at
    mov rdi, rbx
    mov rsi, r12
    mov rdx, rax
    mov rcx, [rbp - CIM_J]
    call cg_name_component
    cmp rax, -1
    je .from_done
    push rax
    ; Everything but the first walk step leaves the previous module behind.
    cmp qword [rbp - CIM_J], 1
    je .no_shift
    mov rdi, r12
    mov esi, OP_SWAP
    mov edx, 2
    mov rcx, [rbp - CIM_LINE]
    call cg_emit
    mov rdi, r12
    mov esi, OP_POP_TOP
    xor edx, edx
    mov rcx, [rbp - CIM_LINE]
    call cg_emit
.no_shift:
    pop rdx
    mov rdi, r12
    mov esi, OP_IMPORT_FROM
    mov rcx, [rbp - CIM_LINE]
    call cg_emit
    inc qword [rbp - CIM_J]
    jmp .from_loop
.from_done:

    mov rsi, [rbp - CIM_TARGET]
    mov rdi, rbx
    call ast_obj_at
    mov rdx, rax
    mov rcx, [rbp - CIM_LINE]
    mov [r12 + CompUnit.curline], ecx
    mov rdi, rbx
    mov rsi, r12
    mov ecx, CTX_STORE
    xor r8d, r8d
    call cg_nameop
    test eax, eax
    jz .fail
    ; The package IMPORT_NAME left behind, if the walk ever ran.
    cmp qword [rbp - CIM_J], 1
    je .next
    mov rdi, r12
    mov esi, OP_POP_TOP
    xor edx, edx
    mov rcx, [rbp - CIM_LINE]
    call cg_emit
.next:
    inc qword [rbp - CIM_I]
    jmp .loop
.done:
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
END_FUNC cg_s_import

;; ============================================================================
;; cg_name_component(Comp *c, CompUnit *u, PyStrObject *dotted, uint64_t i)
;;   -> co_names index of the i-th dot-separated component, or -1 if there is
;;      no such component.  `import a.b.c as n` walks 1, 2, ... with it.
;; ============================================================================
CNC_COMP  equ 8
CNC_UNIT  equ 16
CNC_I     equ 24
CNC_FRAME equ 48          ; + 2 pushes = 56
DEF_FUNC cg_name_component, CNC_FRAME
    push rbx
    push r12
    mov [rbp - CNC_COMP], rdi
    mov [rbp - CNC_UNIT], rsi
    mov [rbp - CNC_I], rcx
    lea rbx, [rdx + PyStrObject.data]
    mov r12, [rdx + PyStrObject.ob_size]

    xor ecx, ecx                        ; byte position
    xor r8d, r8d                        ; component number
.find_start:
    cmp r8, [rbp - CNC_I]
    jae .have_start
.scan_dot:
    cmp rcx, r12
    jae .none
    cmp byte [rbx + rcx], '.'
    je .past_dot
    inc rcx
    jmp .scan_dot
.past_dot:
    inc rcx
    inc r8
    jmp .find_start
.have_start:
    cmp rcx, r12
    jae .none
    mov r9, rcx
.scan_end:
    cmp rcx, r12
    jae .have_end
    cmp byte [rbx + rcx], '.'
    je .have_end
    inc rcx
    jmp .scan_end
.have_end:
    ; comp_intern_keep, not comp_intern: CompUnit.names holds a borrowed
    ; pointer, so the string has to outlive this call.
    mov rdi, [rbp - CNC_COMP]
    lea rsi, [rbx + r9]
    mov rdx, rcx
    sub rdx, r9
    extern comp_intern_keep
    call comp_intern_keep
    test rax, rax
    jz .none
    mov rdi, [rbp - CNC_UNIT]
    mov rsi, rax
    call cg_name
    pop r12
    pop rbx
    leave
    ret
.none:
    mov rax, -1
    pop r12
    pop rbx
    leave
    ret
END_FUNC cg_name_component

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
CIF_LINE  equ 32
CIF_I     equ 40
CIF_N     equ 48
CIF_ALIAS equ 56
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
    ; The tuple is owned and cg_const stores a borrowed reference, so hand it
    ; to the arena: comp_free releases it however the compilation ended.
    mov rdi, rbx
    mov rsi, rax
    call comp_keep
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
    ; The tuple is owned and cg_const stores a borrowed reference, so hand it
    ; to the arena: comp_free releases it however the compilation ended.
    mov rdi, rbx
    mov rsi, rax
    call comp_keep
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
    ; The intrinsic is net zero -- it consumes the module and pushes None --
    ; so the module has to be popped.  Without this every `from m import *`
    ; grew the stack by one, and inside a loop the depth worklist grew
    ; without bound.
    mov rdi, r12
    mov esi, OP_POP_TOP
    xor edx, edx
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
    mov rdx, rax
    mov rcx, [rbp - CIF_LINE]
    mov [r12 + CompUnit.curline], ecx
    mov rdi, rbx
    mov rsi, r12
    mov ecx, CTX_STORE
    xor r8d, r8d
    call cg_nameop
    test eax, eax
    jz .fail
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

;; ============================================================================
;; cg_block(Comp *c, CompUnit *u, uint32_t block) -> rax = 1 ok, 0 error
;; An absent clause is node 0 and emits nothing, which is how a missing `else`
;; stays distinct from an empty one.
;; ============================================================================
DEF_FUNC cg_block               ; no frame: it uses no slot, and one here
                                ; would misalign rsp at the cg_body call
    test rdx, rdx
    jz .empty
    call cg_body
    leave
    ret
.empty:
    mov eax, 1
    leave
    ret
END_FUNC cg_block

;; ============================================================================
;; cg_s_if - if / elif / else
;;
;;     <test>; POP_JUMP_IF_FALSE orelse
;;     <body>; JUMP_FORWARD end
;;   orelse:
;;     <else>
;;   end:
;;
;; An `elif` arrives here as an `if` nested in the else block, so a chain of
;; any length needs nothing extra.
;; ============================================================================
DEF_FUNC_LOCAL cg_s_if, CST_FRAME
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
    mov [rbp - CST_TMP], rax            ; orelse
    mov rdi, r12
    call cg_label_new
    mov [rbp - CST_TMP2], rax           ; end

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
    mov esi, OP_POP_JUMP_IF_FALSE
    mov rdx, [rbp - CST_TMP]
    mov rcx, [rbp - CST_LINE]
    call cg_emit_jump

    mov rdi, rbx
    mov rsi, r13
    call ast_at
    mov edx, [rax + AstNode.c]          ; the body block
    mov rdi, rbx
    mov rsi, r12
    call cg_block
    test eax, eax
    jz .fail

    mov rdi, rbx
    mov rsi, r13
    call ast_at
    mov ecx, [rax + AstNode.b]
    test ecx, ecx
    jz .no_else                         ; nothing to jump over
    mov rdi, r12
    mov esi, OP_JUMP_FORWARD
    mov rdx, [rbp - CST_TMP2]
    mov rcx, [rbp - CST_LINE]
    call cg_emit_jump
.no_else:
    mov rdi, r12
    mov rsi, [rbp - CST_TMP]
    call cg_label_bind

    mov rdi, rbx
    mov rsi, r13
    call ast_at
    mov edx, [rax + AstNode.b]
    mov rdi, rbx
    mov rsi, r12
    call cg_block
    test eax, eax
    jz .fail

    mov rdi, r12
    mov rsi, [rbp - CST_TMP2]
    call cg_label_bind
    mov eax, 1
.fail:
    pop r13
    pop r12
    pop rbx
    leave
    ret
END_FUNC cg_s_if

;; ============================================================================
;; cg_s_while - while / else
;;
;;   top:  <test>; POP_JUMP_IF_FALSE orelse
;;         <body>; JUMP_BACKWARD top
;;   orelse: <else>
;;   end:
;;
;; The `else` clause runs when the condition finally fails, NOT when the loop
;; is broken out of -- which is why `break` targets `end` and the falling-off
;; path targets `orelse`.  CPython additionally duplicates the test to rotate
;; the loop; that is an optimization, and this shape is the one it optimizes.
;; ============================================================================
DEF_FUNC_LOCAL cg_s_while, CST_FRAME
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
    mov [rbp - CST_I], rax              ; top
    mov rdi, r12
    call cg_label_new
    mov [rbp - CST_TMP], rax            ; orelse
    mov rdi, r12
    call cg_label_new
    mov [rbp - CST_TMP2], rax           ; end

    mov rdi, r12
    mov rsi, [rbp - CST_I]
    call cg_label_bind

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
    mov esi, OP_POP_JUMP_IF_FALSE
    mov rdx, [rbp - CST_TMP]
    mov rcx, [rbp - CST_LINE]
    call cg_emit_jump

    mov rdi, r12
    mov rsi, [rbp - CST_TMP2]           ; break
    mov rdx, [rbp - CST_I]              ; continue
    xor ecx, ecx                        ; nothing to pop
    call cg_loop_push

    mov rdi, rbx
    mov rsi, r13
    call ast_at
    mov edx, [rax + AstNode.c]
    mov rdi, rbx
    mov rsi, r12
    call cg_block
    push rax
    mov rdi, r12
    call cg_loop_pop
    pop rax
    test eax, eax
    jz .fail

    mov rdi, r12
    mov esi, OP_JUMP_BACKWARD
    mov rdx, [rbp - CST_I]
    mov rcx, [rbp - CST_LINE]
    call cg_emit_jump_back

    mov rdi, r12
    mov rsi, [rbp - CST_TMP]
    call cg_label_bind
    mov rdi, rbx
    mov rsi, r13
    call ast_at
    mov edx, [rax + AstNode.b]
    mov rdi, rbx
    mov rsi, r12
    call cg_block
    test eax, eax
    jz .fail

    mov rdi, r12
    mov rsi, [rbp - CST_TMP2]
    call cg_label_bind
    mov eax, 1
.fail:
    pop r13
    pop r12
    pop rbx
    leave
    ret
END_FUNC cg_s_while

;; ============================================================================
;; cg_s_for - for / else
;;
;;         <iter>; GET_ITER
;;   top:  FOR_ITER exit
;;         <store target>; <body>; JUMP_BACKWARD top
;;   exit: END_FOR
;;         <else>
;;   end:
;;
;; FOR_ITER's target is the END_FOR itself, not what follows it: the
;; interpreter adds one to the delta so the exhausted path lands past it.  The
;; iterator stays on the stack across the body, so `break` has to drop it while
;; `continue` -- which jumps back to a FOR_ITER that still wants it -- does not.
;; ============================================================================
DEF_FUNC_LOCAL cg_s_for, CST_FRAME
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

    ; `async for` shares nothing with this loop but its AST node: its exit edge
    ; is an exception, not a sentinel.
    cmp byte [rax + AstNode.subkind], 0
    je .sync
    mov rdi, rbx
    mov rsi, r12
    mov rdx, r13
    call cg_s_asyncfor
    jmp .fail
.sync:

    mov edx, [rax + AstNode.b]          ; the iterable
    mov rdi, rbx
    mov rsi, r12
    call cg_expr
    test eax, eax
    jz .fail
    mov rdi, r12
    mov esi, OP_GET_ITER
    xor edx, edx
    mov rcx, [rbp - CST_LINE]
    call cg_emit

    mov rdi, r12
    call cg_label_new
    mov [rbp - CST_I], rax              ; top
    mov rdi, r12
    call cg_label_new
    mov [rbp - CST_TMP], rax            ; exit, at the END_FOR
    mov rdi, r12
    call cg_label_new
    mov [rbp - CST_TMP2], rax           ; end, past the else clause

    mov rdi, r12
    mov rsi, [rbp - CST_I]
    call cg_label_bind
    mov rdi, r12
    mov esi, OP_FOR_ITER
    mov rdx, [rbp - CST_TMP]
    mov rcx, [rbp - CST_LINE]
    call cg_emit_jump

    mov rdi, rbx
    mov rsi, r13
    call ast_at
    mov edx, [rax + AstNode.a]          ; the loop target
    mov rdi, rbx
    mov rsi, r12
    call cg_store
    test eax, eax
    jz .fail

    mov rdi, r12
    mov rsi, [rbp - CST_TMP2]           ; break
    mov rdx, [rbp - CST_I]              ; continue
    mov ecx, 1                          ; break must drop the iterator
    call cg_loop_push

    mov rdi, rbx
    mov rsi, r13
    call ast_at
    mov edx, [rax + AstNode.c]
    mov rdi, rbx
    mov rsi, r12
    call cg_block
    push rax
    mov rdi, r12
    call cg_loop_pop
    pop rax
    test eax, eax
    jz .fail

    mov rdi, r12
    mov esi, OP_JUMP_BACKWARD
    mov rdx, [rbp - CST_I]
    mov rcx, [rbp - CST_LINE]
    call cg_emit_jump_back

    mov rdi, r12
    mov rsi, [rbp - CST_TMP]
    call cg_label_bind
    mov rdi, r12
    mov esi, OP_END_FOR
    xor edx, edx
    mov rcx, [rbp - CST_LINE]
    call cg_emit

    mov rdi, rbx
    mov rsi, r13
    call ast_at
    mov edx, [rax + AstNode.clist]      ; the else block
    mov rdi, rbx
    mov rsi, r12
    call cg_block
    test eax, eax
    jz .fail

    mov rdi, r12
    mov rsi, [rbp - CST_TMP2]
    call cg_label_bind
    mov eax, 1
.fail:
    pop r13
    pop r12
    pop rbx
    leave
    ret
END_FUNC cg_s_for

;; ============================================================================
;; cg_s_break / cg_s_continue
;; ============================================================================
DEF_FUNC_LOCAL cg_s_break, CST_FRAME
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
    call cg_loop_top
    test rax, rax
    jz .outside
    ; A break or continue leaving a try/finally inside the loop has to run that
    ; finally body on its way out, exactly as a return does.
    mov ecx, [rax + LoopFrame.fdepth]
    mov [rbp - CST_N], rcx
    mov rdi, rbx
    mov rsi, r12
    mov rdx, rcx
    xor ecx, ecx                        ; break and continue carry no value
    xor r8d, r8d                        ; the loop's own items are popped below
    call cg_unwind_finallys
    test eax, eax
    jz .fail_unwind
    mov rdi, r12
    call cg_loop_top
    mov ecx, [rax + LoopFrame.npop]
    mov [rbp - CST_I], rcx
    mov ecx, [rax + LoopFrame.brk]
    mov [rbp - CST_TMP], rcx
.pop_loop:
    cmp qword [rbp - CST_I], 0
    je .jump
    mov rdi, r12
    mov esi, OP_POP_TOP
    xor edx, edx
    mov rcx, [rbp - CST_LINE]
    call cg_emit
    dec qword [rbp - CST_I]
    jmp .pop_loop
.jump:
    mov rdi, r12
    mov esi, OP_JUMP_FORWARD
    mov rdx, [rbp - CST_TMP]
    mov rcx, [rbp - CST_LINE]
    call cg_emit_jump
    mov eax, 1
    jmp .ret
.fail_unwind:
    xor eax, eax
    jmp .ret
.outside:
    mov rdi, rbx
    lea rsi, [rel exc_SyntaxError_type]
    CSTRING rdx, "'break' outside loop"
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
END_FUNC cg_s_break

DEF_FUNC_LOCAL cg_s_continue, CST_FRAME
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
    call cg_loop_top
    test rax, rax
    jz .outside
    mov ecx, [rax + LoopFrame.fdepth]
    mov [rbp - CST_N], rcx
    mov rdi, rbx
    mov rsi, r12
    mov rdx, rcx
    xor ecx, ecx                        ; break and continue carry no value
    xor r8d, r8d                        ; the loop's own items are popped below
    call cg_unwind_finallys
    test eax, eax
    jz .fail_unwind
    mov rdi, r12
    call cg_loop_top
    mov ecx, [rax + LoopFrame.cont]
    mov rdi, r12
    mov esi, OP_JUMP_BACKWARD
    mov rdx, rcx
    mov rcx, [rbp - CST_LINE]
    call cg_emit_jump_back
    mov eax, 1
    jmp .ret
.fail_unwind:
    xor eax, eax
    jmp .ret
.outside:
    mov rdi, rbx
    lea rsi, [rel exc_SyntaxError_type]
    CSTRING rdx, "'continue' not properly in loop"
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
END_FUNC cg_s_continue

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
    dq cg_s_if                         ; 46 AST_IF
    dq cg_s_while                      ; 47 AST_WHILE
    dq cg_s_for                        ; 48 AST_FOR
    dq cg_body                         ; 49 AST_BLOCK
    dq cg_s_pass        ; 50 AST_PASS
    dq cg_s_break                      ; 51 AST_BREAK
    dq cg_s_continue                   ; 52 AST_CONTINUE
    dq cg_s_return                     ; 53 AST_RETURN
    dq cg_s_delete      ; 54 AST_DELETE
    dq cg_s_raise       ; 55 AST_RAISE
    dq cg_s_assert      ; 56 AST_ASSERT
    dq cg_s_scope       ; 57 AST_GLOBAL
    dq cg_s_scope       ; 58 AST_NONLOCAL
    dq cg_s_import      ; 59 AST_IMPORT
    dq cg_s_importfrom  ; 60 AST_IMPORTFROM
    dq 0                ; 61 AST_ALIAS
    dq cg_s_functiondef                ; 62 AST_FUNCTIONDEF
    dq cg_s_classdef                   ; 63 AST_CLASSDEF
    dq cg_s_try                        ; 64 AST_TRY
    dq 0                ; 65 AST_HANDLER
    dq cg_s_with                       ; 66 AST_WITH
    dq 0                ; 67 AST_WITHITEM
    dq 0                ; 68 AST_ARGUMENTS
    dq 0                ; 69 AST_ARG
    dq cg_s_match       ; 70 AST_MATCH
    dq 0                ; 71 AST_EXTRA
    dq cg_s_decorated   ; 72 AST_DECORATED
    dq 0                ; 73 
    dq 0                ; 74 
    dq 0                ; 75 
    dq 0                ; 76 
    dq 0                ; 77 
    dq 0                ; 78 
    dq 0                ; 79 

cg_star_name: db "*", 0

section .rodata
cg_annotations_dunder: db "__annotations__", 0

ASM_INIT
