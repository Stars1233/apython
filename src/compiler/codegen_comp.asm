; codegen_comp.asm - Comprehensions and generator expressions
;
; All four comprehension forms compile to a nested function taking the
; outermost iterable as its single argument, called `.0` -- the pre-3.12 shape.
; 3.12 inlines the list, set and dict forms with LOAD_FAST_AND_CLEAR, but a
; generator expression cannot be inlined at all, so that machinery would have
; to exist alongside this rather than instead of it.  One mechanism for all
; four is smaller and gets the scoping right for free: the loop variable does
; not leak because it lives in a different frame, rather than because anything
; here arranges for it not to.
;
; The observable difference is a `<listcomp>` frame in a traceback where CPython
; 3.12 has none.

%include "macros.inc"
%include "object.inc"
%include "value.inc"
%include "opcodes.inc"
%include "compiler.inc"
extern buf_init
extern buf_free
extern buf_push_u32
extern ast_obj_at
extern sym_lp_index

extern asm_assemble
extern ast_at
extern ast_child
extern cg_const
extern ast_obj
extern cg_emit
extern cg_emit_jump
extern cg_emit_jump_back
extern cg_expr
extern cg_label_bind
extern cg_label_new
extern cg_nameop
extern cg_store
extern cg_unit_free
extern cg_unit_init
extern comp_error
extern comp_intern_cstr
extern obj_decref
extern str_from_cstr_heap
extern sym_at
extern sym_finalize
extern cg_cell_prologue
extern cg_closure_tuple
extern cg_send_loop
extern cg_await_value
extern cg_push_handler
extern cg_pop_handler
extern none_singleton


; --- Named frame-layout constants ---
CM2_LINE  equ 32
CM2_SCOPE equ 40
CM2_CODE  equ 48
CM2_NAME  equ 56
CM2_KIND  equ 64
CM2_AITER equ 72
CM2_UNIT2 equ 80 + CompUnit_size
CM2_FRAME equ ((CM2_UNIT2 + 15) / 16) * 16 + 8      ; + 3 pushes = 16-aligned

section .text

;; ============================================================================
;; cg_e_comprehension(Comp *c, CompUnit *u, uint32_t node) -> 1 ok, 0 error
;;
;;     PUSH_NULL; LOAD_CONST <code>; MAKE_FUNCTION
;;     <outermost iterable>; GET_ITER; CALL 1
;; ============================================================================
DEF_FUNC cg_e_comprehension, CM2_FRAME
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
    mov [rbp - CM2_LINE], rcx
    mov [r12 + CompUnit.curline], ecx
    movzx ecx, word [rax + AstNode.flags]
    mov [rbp - CM2_SCOPE], rcx
    movzx ecx, byte [rax + AstNode.kind]
    mov [rbp - CM2_KIND], rcx

    ; Settle the comprehension scope's layout before emitting into it.
    mov rax, [rbp - CM2_SCOPE]
    mov [rbx + Comp.cur_scope], eax
    mov rdi, rbx
    mov rsi, [rbp - CM2_SCOPE]
    xor edx, edx
    call sym_finalize
    test eax, eax
    jz .fail

    mov rdi, rbx
    lea rsi, [rbp - CM2_UNIT2]
    mov rdx, r13
    call cg_comp_body
    mov [rbp - CM2_CODE], rax
    test rax, rax
    jz .fail
    ; CompUnit.consts holds a BORROWED pointer, so the arena has to own this
    ; reference -- otherwise nothing ever releases it and every comprehension
    ; leaks its code object.
    mov rdi, rbx
    mov rsi, rax
    call ast_obj

    ; Back in the enclosing scope for the iterable and the call.
    mov eax, [r12 + CompUnit.scope]
    mov [rbx + Comp.cur_scope], eax

    mov rdi, r12
    mov esi, OP_PUSH_NULL
    xor edx, edx
    mov rcx, [rbp - CM2_LINE]
    call cg_emit

    ; A comprehension that captured anything needs a closure like any other
    ; nested function -- `[x + n for x in it]` inside a function reads n.
    mov rdi, rbx
    mov rsi, r12
    mov rdx, [rbp - CM2_SCOPE]
    mov rcx, [rbp - CM2_LINE]
    call cg_closure_tuple
    cmp rax, -1
    je .fail
    mov [rbp - CM2_NAME], rax

    mov rdi, r12
    mov rsi, [rbp - CM2_CODE]
    call cg_const
    mov rdx, rax
    mov rdi, r12
    mov esi, OP_LOAD_CONST
    mov rcx, [rbp - CM2_LINE]
    call cg_emit
    mov rdi, r12
    mov esi, OP_MAKE_FUNCTION
    mov rdx, [rbp - CM2_NAME]
    mov rcx, [rbp - CM2_LINE]
    call cg_emit

    ; The outermost iterable is evaluated here, not inside: that is why
    ; `[x for x in undefined]` raises at the comprehension rather than lazily.
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
    movzx ecx, byte [rax + AstNode.subkind]
    mov [rbp - CM2_AITER], rcx
    mov edx, [rax + AstNode.b]
    mov rdi, rbx
    mov rsi, r12
    call cg_expr
    test eax, eax
    jz .fail
    mov esi, OP_GET_ITER
    cmp qword [rbp - CM2_AITER], 0
    je .sync_outer
    mov esi, OP_GET_AITER
.sync_outer:
    mov rdi, r12
    xor edx, edx
    mov rcx, [rbp - CM2_LINE]
    call cg_emit
    mov rdi, r12
    mov esi, OP_CALL
    mov edx, 1
    mov rcx, [rbp - CM2_LINE]
    call cg_emit

    ; A list, set or dict comprehension that had to become a coroutine hands
    ; back an awaitable rather than the container, so the await belongs here.
    ; A generator expression does not: `(x async for x in y)` IS the async
    ; generator, and awaiting it would consume it.
    cmp qword [rbp - CM2_KIND], AST_GENEXP
    je .no_await
    mov rdi, rbx
    mov rsi, [rbp - CM2_SCOPE]
    call sym_at
    test dword [rax + Scope.flags], SCF_COROUTINE
    jz .no_await
    mov rdi, r12
    mov rsi, [rbp - CM2_LINE]
    xor edx, edx
    call cg_await_value
    test eax, eax
    jz .fail
.no_await:
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
END_FUNC cg_e_comprehension

;; ============================================================================
;; cg_comp_body(Comp *c, CompUnit *u, uint32_t node) -> PyCodeObject*, or 0
;;
;;     BUILD_LIST 0                (or BUILD_SET / BUILD_MAP; nothing for a
;;                                  generator expression)
;;     LOAD_FAST .0
;;   top: FOR_ITER exit
;;     <store target>
;;     [<condition>; POP_JUMP_IF_FALSE top]  for each condition
;;     ... inner clauses, nested the same way ...
;;     <element>; LIST_APPEND n     (or SET_ADD / MAP_ADD / YIELD_VALUE)
;;     JUMP_BACKWARD top
;;   exit: END_FOR
;;     RETURN_VALUE                (RETURN_CONST None for a generator)
;;
;; The accumulator sits beneath every loop's iterator, so the LIST_APPEND oparg
;; counts back past them -- one per enclosing clause.
;; ============================================================================
CB2_LINE  equ 32
CB2_SCOPE equ 40
CB2_KIND  equ 48
CB2_NAME  equ 56
CB2_CODE  equ 64
CB2_ASYNC equ 72
CB2_FRAME equ 88          ; + 3 pushes = 112
DEF_FUNC cg_comp_body, CB2_FRAME
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
    mov [rbp - CB2_LINE], rcx
    movzx ecx, word [rax + AstNode.flags]
    mov [rbp - CB2_SCOPE], rcx
    movzx ecx, byte [rax + AstNode.kind]
    mov [rbp - CB2_KIND], rcx

    ; A name for tracebacks, matching CPython's.
    lea rdi, [rel cm_listcomp]
    cmp qword [rbp - CB2_KIND], AST_SETCOMP
    jne .not_set
    lea rdi, [rel cm_setcomp]
    jmp .have_name
.not_set:
    cmp qword [rbp - CB2_KIND], AST_DICTCOMP
    jne .not_dict
    lea rdi, [rel cm_dictcomp]
    jmp .have_name
.not_dict:
    cmp qword [rbp - CB2_KIND], AST_GENEXP
    jne .have_name
    lea rdi, [rel cm_genexpr]
.have_name:
    call str_from_cstr_heap
    mov [rbp - CB2_NAME], rax

    mov rdi, r12
    mov rsi, [rbx + Comp.filename]
    mov rdx, [rbp - CB2_NAME]
    call cg_unit_init
    mov rax, [rbp - CB2_SCOPE]
    mov [r12 + CompUnit.scope], eax
    mov [r12 + CompUnit.comp], rbx
    mov rax, [rbp - CB2_LINE]
    mov [r12 + CompUnit.firstline], eax
    mov [r12 + CompUnit.curline], eax
    mov dword [r12 + CompUnit.flags], CO_OPTIMIZED | CO_NEWLOCALS | CO_NESTED
    mov dword [r12 + CompUnit.argcount], 1      ; the implicit .0

    ; A comprehension containing `async for` or `await` is a coroutine (or, if
    ; it is also a generator expression, an async generator).  Which of the
    ; three it is decides both the flag and whether the caller awaits the call.
    mov rdi, rbx
    mov rsi, [rbp - CB2_SCOPE]
    call sym_at
    mov edx, [rax + Scope.flags]
    and edx, SCF_COROUTINE
    mov [rbp - CB2_ASYNC], rdx
    xor ecx, ecx
    cmp qword [rbp - CB2_KIND], AST_GENEXP
    jne .not_gen
    mov ecx, CO_GENERATOR
    test rdx, rdx
    jz .not_gen
    mov ecx, CO_ASYNC_GENERATOR
    jmp .not_gen
.not_gen:
    test rdx, rdx
    jz .have_flag
    test ecx, ecx
    jnz .have_flag
    mov ecx, CO_COROUTINE
.have_flag:
    or dword [r12 + CompUnit.flags], ecx

    mov rdi, rbx
    mov rsi, r12
    mov rdx, [rbp - CB2_SCOPE]
    call cg_cell_prologue
    test eax, eax
    jz .fail

    ; A generator expression must announce itself before RESUME, or the body
    ; runs eagerly on the first call rather than producing a generator.
    cmp qword [rbp - CB2_KIND], AST_GENEXP
    je .gen_prologue
    cmp qword [rbp - CB2_ASYNC], 0
    je .no_gen_prologue
.gen_prologue:
    mov rdi, r12
    mov esi, OP_RETURN_GENERATOR
    xor edx, edx
    xor ecx, ecx
    call cg_emit
    or byte [rax + Instr.flags], IF_NOLINE
    mov rdi, r12
    mov esi, OP_POP_TOP
    xor edx, edx
    xor ecx, ecx
    call cg_emit
    or byte [rax + Instr.flags], IF_NOLINE
.no_gen_prologue:

    mov rdi, r12
    mov esi, OP_RESUME
    xor edx, edx
    xor ecx, ecx
    call cg_emit
    or byte [rax + Instr.flags], IF_NOLINE

    ; The accumulator, except for a generator, which yields instead.
    cmp qword [rbp - CB2_KIND], AST_GENEXP
    je .no_accum
    mov esi, OP_BUILD_LIST
    cmp qword [rbp - CB2_KIND], AST_SETCOMP
    jne .not_set2
    mov esi, OP_BUILD_SET
    jmp .emit_accum
.not_set2:
    cmp qword [rbp - CB2_KIND], AST_DICTCOMP
    jne .emit_accum
    mov esi, OP_BUILD_MAP
.emit_accum:
    mov rdi, r12
    xor edx, edx
    mov rcx, [rbp - CB2_LINE]
    call cg_emit
.no_accum:

    ; The clauses, nested innermost-last.  This is the nested-function form,
    ; so the outermost iterator is the parameter rather than the stack.
    mov rdi, rbx
    mov rsi, r12
    mov rdx, r13
    xor ecx, ecx
    xor r8d, r8d
    call cg_comp_clause
    test eax, eax
    jz .fail

    cmp qword [rbp - CB2_KIND], AST_GENEXP
    je .gen_return
    mov rdi, r12
    mov esi, OP_RETURN_VALUE
    xor edx, edx
    mov rcx, [rbp - CB2_LINE]
    call cg_emit
    jmp .assemble
.gen_return:
    lea rsi, [rel none_singleton]
    INCREF rsi
    mov rdi, r12
    call cg_const
    mov rdx, rax
    mov rdi, r12
    mov esi, OP_RETURN_CONST
    xor ecx, ecx
    call cg_emit
    or byte [rax + Instr.flags], IF_NOLINE

.assemble:
    mov rdi, rbx
    mov rsi, r12
    call asm_assemble
    mov [rbp - CB2_CODE], rax
    mov rdi, r12
    call cg_unit_free
    mov rdi, [rbp - CB2_NAME]
    call obj_decref
    mov rax, [rbp - CB2_CODE]
    jmp .ret
.fail:
    mov rdi, r12
    call cg_unit_free
    mov rdi, [rbp - CB2_NAME]
    call obj_decref
    xor eax, eax
.ret:
    pop r13
    pop r12
    pop rbx
    leave
    ret
END_FUNC cg_comp_body

;; ============================================================================
;; cg_comp_clause(Comp *c, CompUnit *u, uint32_t node, uint64_t i,
;;                int on_stack) -> 1 ok, 0 error
;; One `for` clause, with the remaining ones -- and finally the element --
;; nested inside it.
;;
;; `on_stack` says where the OUTERMOST iterator comes from: a nested
;; comprehension is a function and takes it as the parameter `.0`, an inlined
;; one has it on the stack already.  It only matters for clause 0.
;; ============================================================================
CC5_I     equ 32
CC5_LINE  equ 40
CC5_TOP   equ 48
CC5_EXIT  equ 56
CC5_CL    equ 64
CC5_N     equ 72
CC5_J     equ 80
CC5_KIND  equ 88
CC5_CONT  equ 96
CC5_ASYNC equ 104
CC5_STACK equ 112         ; is the outermost iterator already on the stack?
CC5_FRAME equ 120         ; + 3 pushes = 144
DEF_FUNC cg_comp_clause, CC5_FRAME
    push rbx
    push r12
    push r13
    mov rbx, rdi
    mov r12, rsi
    mov r13, rdx
    mov [rbp - CC5_I], rcx
    mov [rbp - CC5_STACK], r8

    mov rdi, rbx
    mov rsi, r13
    call ast_at
    mov ecx, [rax + AstNode.lineno]
    mov [rbp - CC5_LINE], rcx
    movzx ecx, byte [rax + AstNode.kind]
    mov [rbp - CC5_KIND], rcx
    mov ecx, [rax + AstNode.nchild]
    mov [rbp - CC5_N], rcx

    mov rax, [rbp - CC5_I]
    cmp rax, [rbp - CC5_N]
    jb .one_clause

    ; Past the last clause: emit the element and accumulate it.
    mov rdi, rbx
    mov rsi, r12
    mov rdx, r13
    mov rcx, [rbp - CC5_N]
    call cg_comp_element
    jmp .ret

.one_clause:
    mov rdi, rbx
    mov rsi, r13
    call ast_at
    mov rsi, rax
    mov rdx, [rbp - CC5_I]
    mov rdi, rbx
    call ast_child
    mov [rbp - CC5_CL], rax
    mov rdi, rbx
    mov rsi, rax
    call ast_at
    movzx ecx, byte [rax + AstNode.subkind]
    mov [rbp - CC5_ASYNC], rcx

    ; The outermost iterable arrives as the parameter -- or is already on the
    ; stack, for an inlined comprehension; the rest are evaluated here, one
    ; loop deeper each time.
    cmp qword [rbp - CC5_I], 0
    jne .inner_iter
    cmp qword [rbp - CC5_STACK], 0
    jne .loop_top
    mov rdi, rbx
    lea rsi, [rel cm_dot_zero]
    call comp_intern_cstr
    mov rdx, rax
    mov rdi, rbx
    mov rsi, r12
    mov ecx, CTX_LOAD
    xor r8d, r8d
    call cg_nameop
    test eax, eax
    jz .fail
    jmp .loop_top
.inner_iter:
    mov rdi, rbx
    mov rsi, [rbp - CC5_CL]
    call ast_at
    mov edx, [rax + AstNode.b]
    mov rdi, rbx
    mov rsi, r12
    call cg_expr
    test eax, eax
    jz .fail
    mov esi, OP_GET_ITER
    cmp qword [rbp - CC5_ASYNC], 0
    je .sync_iter
    mov esi, OP_GET_AITER
.sync_iter:
    mov rdi, r12
    xor edx, edx
    mov rcx, [rbp - CC5_LINE]
    call cg_emit

.loop_top:
    mov rdi, r12
    call cg_label_new
    mov [rbp - CC5_TOP], rax
    mov rdi, r12
    call cg_label_new
    mov [rbp - CC5_EXIT], rax
    mov rdi, r12
    call cg_label_new
    mov [rbp - CC5_CONT], rax
    mov rdi, r12
    mov rsi, [rbp - CC5_TOP]
    call cg_label_bind
    cmp qword [rbp - CC5_ASYNC], 0
    jne .async_head
    mov rdi, r12
    mov esi, OP_FOR_ITER
    mov rdx, [rbp - CC5_EXIT]
    mov rcx, [rbp - CC5_LINE]
    call cg_emit_jump
    jmp .have_item
.async_head:
    ; `async for` leaves the loop by raising StopAsyncIteration, so the exit
    ; edge is an exception edge and the head has to sit in a protected region.
    mov rdi, r12
    mov rsi, [rbp - CC5_EXIT]
    xor edx, edx
    call cg_push_handler
    mov rdi, r12
    mov esi, OP_GET_ANEXT
    xor edx, edx
    mov rcx, [rbp - CC5_LINE]
    call cg_emit
    lea rsi, [rel none_singleton]
    INCREF rsi
    mov rdi, r12
    call cg_const
    mov rdx, rax
    mov rdi, r12
    mov esi, OP_LOAD_CONST
    mov rcx, [rbp - CC5_LINE]
    call cg_emit
    mov rdi, r12
    mov rsi, [rbp - CC5_LINE]
    mov edx, 3
    call cg_send_loop
    test eax, eax
    jz .fail
    mov rdi, r12
    call cg_pop_handler
.have_item:

    mov rdi, rbx
    mov rsi, [rbp - CC5_CL]
    call ast_at
    mov edx, [rax + AstNode.a]
    mov rdi, rbx
    mov rsi, r12
    call cg_store
    test eax, eax
    jz .fail

    ; Conditions: any that fails goes straight back round the loop.
    mov qword [rbp - CC5_J], 0
.cond_loop:
    mov rdi, rbx
    mov rsi, [rbp - CC5_CL]
    call ast_at
    mov ecx, [rax + AstNode.nchild]
    cmp [rbp - CC5_J], rcx
    jae .nest
    mov rsi, rax
    mov rdx, [rbp - CC5_J]
    mov rdi, rbx
    call ast_child
    mov rdx, rax
    mov rdi, rbx
    mov rsi, r12
    call cg_expr
    test eax, eax
    jz .fail
    ; A failed condition jumps FORWARD to the loop's tail, which then jumps
    ; back: POP_JUMP_IF_FALSE only ever adds its delta, so it cannot target
    ; something already emitted.
    mov rdi, r12
    mov esi, OP_POP_JUMP_IF_FALSE
    mov rdx, [rbp - CC5_CONT]
    mov rcx, [rbp - CC5_LINE]
    call cg_emit_jump
    inc qword [rbp - CC5_J]
    jmp .cond_loop

.nest:
    mov rdi, rbx
    mov rsi, r12
    mov rdx, r13
    mov rcx, [rbp - CC5_I]
    inc rcx
    mov r8, [rbp - CC5_STACK]
    call cg_comp_clause
    test eax, eax
    jz .fail

    mov rdi, r12
    mov rsi, [rbp - CC5_CONT]
    call cg_label_bind
    mov rdi, r12
    mov esi, OP_JUMP_BACKWARD
    mov rdx, [rbp - CC5_TOP]
    mov rcx, [rbp - CC5_LINE]
    call cg_emit_jump_back
    mov rdi, r12
    mov rsi, [rbp - CC5_EXIT]
    call cg_label_bind
    mov esi, OP_END_FOR
    cmp qword [rbp - CC5_ASYNC], 0
    je .sync_end
    mov esi, OP_END_ASYNC_FOR
.sync_end:
    mov rdi, r12
    xor edx, edx
    mov rcx, [rbp - CC5_LINE]
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
END_FUNC cg_comp_clause

;; ============================================================================
;; cg_comp_element(Comp *c, CompUnit *u, uint32_t node, uint64_t depth)
;;   -> 1 ok, 0 error
;; The element, accumulated into the container `depth` iterators below.
;; ============================================================================
CE4_DEPTH equ 32
CE4_LINE  equ 40
CE4_KIND  equ 48
CE4_FRAME equ 56          ; + 3 pushes = 80
DEF_FUNC cg_comp_element, CE4_FRAME
    push rbx
    push r12
    push r13
    mov rbx, rdi
    mov r12, rsi
    mov r13, rdx
    mov [rbp - CE4_DEPTH], rcx

    mov rdi, rbx
    mov rsi, r13
    call ast_at
    mov ecx, [rax + AstNode.lineno]
    mov [rbp - CE4_LINE], rcx
    movzx ecx, byte [rax + AstNode.kind]
    mov [rbp - CE4_KIND], rcx

    ; A dict comprehension pushes key then value; everything else one element.
    cmp qword [rbp - CE4_KIND], AST_DICTCOMP
    jne .single
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
    mov esi, OP_MAP_ADD
    mov rdx, [rbp - CE4_DEPTH]
    add rdx, 1
    mov rcx, [rbp - CE4_LINE]
    call cg_emit
    jmp .ok

.single:
    mov rdi, rbx
    mov rsi, r13
    call ast_at
    mov edx, [rax + AstNode.a]
    mov rdi, rbx
    mov rsi, r12
    call cg_expr
    test eax, eax
    jz .fail

    cmp qword [rbp - CE4_KIND], AST_GENEXP
    jne .accumulate
    ; A generator yields the element and resumes where it left off.
    test dword [r12 + CompUnit.flags], CO_ASYNC_GENERATOR
    jz .plain_yield
    mov rdi, r12
    mov esi, OP_CALL_INTRINSIC_1
    mov edx, INTRINSIC_ASYNC_GEN_WRAP
    mov rcx, [rbp - CE4_LINE]
    call cg_emit
.plain_yield:
    mov rdi, r12
    mov esi, OP_YIELD_VALUE
    xor edx, edx
    mov rcx, [rbp - CE4_LINE]
    call cg_emit
    mov rdi, r12
    mov esi, OP_RESUME
    mov edx, 1
    xor ecx, ecx
    call cg_emit
    or byte [rax + Instr.flags], IF_NOLINE
    mov rdi, r12
    mov esi, OP_POP_TOP
    xor edx, edx
    mov rcx, [rbp - CE4_LINE]
    call cg_emit
    jmp .ok

.accumulate:
    mov esi, OP_LIST_APPEND
    cmp qword [rbp - CE4_KIND], AST_SETCOMP
    jne .have_add
    mov esi, OP_SET_ADD
.have_add:
    mov rdi, r12
    mov rdx, [rbp - CE4_DEPTH]
    add rdx, 1
    mov rcx, [rbp - CE4_LINE]
    call cg_emit
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
END_FUNC cg_comp_element

;; ============================================================================
;; cg_e_yield(Comp *c, CompUnit *u, uint32_t node) -> 1 ok, 0 error
;;
;;     <value or None>; YIELD_VALUE; RESUME 1
;;
;; The value the generator is resumed with is left on the stack, which is what
;; makes `x = yield v` receive from send().  A statement-level yield discards
;; it through the POP_TOP an expression statement already emits.
;; ============================================================================
CY_NODE  equ 24
CY_LINE  equ 32
CY_FRAME equ 40           ; + 3 pushes = 64
DEF_FUNC cg_e_yield, CY_FRAME
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
    mov [rbp - CY_LINE], rcx
    mov ecx, [rax + AstNode.a]
    test ecx, ecx
    jz .bare
    mov edx, ecx
    mov rdi, rbx
    mov rsi, r12
    call cg_expr
    test eax, eax
    jz .fail
    jmp .emit
.bare:
    lea rsi, [rel none_singleton]
    INCREF rsi
    mov rdi, r12
    call cg_const
    mov rdx, rax
    mov rdi, r12
    mov esi, OP_LOAD_CONST
    mov rcx, [rbp - CY_LINE]
    call cg_emit
.emit:
    ; An async generator yields through an __anext__ awaitable rather than
    ; straight to the caller, so the value is boxed first; the interpreter's
    ; asend/athrow machinery unwraps it on the other side.
    test dword [r12 + CompUnit.flags], CO_ASYNC_GENERATOR
    jz .plain_yield
    mov rdi, r12
    mov esi, OP_CALL_INTRINSIC_1
    mov edx, INTRINSIC_ASYNC_GEN_WRAP
    mov rcx, [rbp - CY_LINE]
    call cg_emit
.plain_yield:
    mov rdi, r12
    mov esi, OP_YIELD_VALUE
    xor edx, edx
    mov rcx, [rbp - CY_LINE]
    call cg_emit
    mov rdi, r12
    mov esi, OP_RESUME
    mov edx, 1
    xor ecx, ecx
    call cg_emit
    or byte [rax + Instr.flags], IF_NOLINE
    mov eax, 1
.fail:
    pop r13
    pop r12
    pop rbx
    leave
    ret
END_FUNC cg_e_yield

;; ============================================================================
;; cg_e_yieldfrom(Comp *c, CompUnit *u, uint32_t node) -> 1 ok, 0 error
;;
;;     <iterable>; GET_YIELD_FROM_ITER; LOAD_CONST None
;;   top: SEND end
;;     YIELD_VALUE; RESUME 2
;;     JUMP_BACKWARD_NO_INTERRUPT top
;;   end: END_SEND
;;
;; SEND drives the sub-iterator and jumps out when it is exhausted, leaving its
;; return value; the loop in between is what forwards send() and throw()
;; through to it.
;; ============================================================================
DEF_FUNC cg_e_yieldfrom, CY_FRAME
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
    mov [rbp - CY_LINE], rcx
    mov edx, [rax + AstNode.a]
    mov rdi, rbx
    mov rsi, r12
    call cg_expr
    test eax, eax
    jz .fail

    mov rdi, r12
    mov esi, OP_GET_YIELD_FROM_ITER
    xor edx, edx
    mov rcx, [rbp - CY_LINE]
    call cg_emit
    lea rsi, [rel none_singleton]
    INCREF rsi
    mov rdi, r12
    call cg_const
    mov rdx, rax
    mov rdi, r12
    mov esi, OP_LOAD_CONST
    mov rcx, [rbp - CY_LINE]
    call cg_emit

    mov rdi, r12
    call cg_label_new
    mov r13, rax                        ; top
    mov rdi, r12
    call cg_label_new
    mov [rbp - CY_NODE], rax            ; end

    mov rdi, r12
    mov rsi, r13
    call cg_label_bind
    mov rdi, r12
    mov esi, OP_SEND
    mov rdx, [rbp - CY_NODE]
    mov rcx, [rbp - CY_LINE]
    call cg_emit_jump
    mov rdi, r12
    mov esi, OP_YIELD_VALUE
    xor edx, edx
    mov rcx, [rbp - CY_LINE]
    call cg_emit
    mov rdi, r12
    mov esi, OP_RESUME
    mov edx, 2
    xor ecx, ecx
    call cg_emit
    or byte [rax + Instr.flags], IF_NOLINE
    mov rdi, r12
    mov esi, OP_JUMP_BACKWARD_NO_INTERRUPT
    mov rdx, r13
    mov rcx, [rbp - CY_LINE]
    call cg_emit_jump_back
    mov rdi, r12
    mov rsi, [rbp - CY_NODE]
    call cg_label_bind
    mov rdi, r12
    mov esi, OP_END_SEND
    xor edx, edx
    mov rcx, [rbp - CY_LINE]
    call cg_emit
    mov eax, 1
.fail:
    pop r13
    pop r12
    pop rbx
    leave
    ret
END_FUNC cg_e_yieldfrom

;; ============================================================================
;; cg_e_joinedstr(Comp *c, CompUnit *u, uint32_t node) -> 1 ok, 0 error
;; Each piece in order, then one BUILD_STRING over all of them.
;; ============================================================================
CJ2_LINE  equ 32
CJ2_I     equ 40
CJ2_N     equ 48
CJ2_FRAME equ 56          ; + 3 pushes = 80
DEF_FUNC cg_e_joinedstr, CJ2_FRAME
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
    mov [rbp - CJ2_LINE], rcx
    mov ecx, [rax + AstNode.nchild]
    mov [rbp - CJ2_N], rcx
    mov qword [rbp - CJ2_I], 0
.loop:
    mov rax, [rbp - CJ2_I]
    cmp rax, [rbp - CJ2_N]
    jae .join
    mov rdi, rbx
    mov rsi, r13
    call ast_at
    mov rsi, rax
    mov rdx, [rbp - CJ2_I]
    mov rdi, rbx
    call ast_child
    mov rdx, rax
    mov rdi, rbx
    mov rsi, r12
    call cg_expr
    test eax, eax
    jz .fail
    inc qword [rbp - CJ2_I]
    jmp .loop
.join:
    mov rdi, r12
    mov esi, OP_BUILD_STRING
    mov rdx, [rbp - CJ2_N]
    mov rcx, [rbp - CJ2_LINE]
    call cg_emit
    mov eax, 1
.fail:
    pop r13
    pop r12
    pop rbx
    leave
    ret
END_FUNC cg_e_joinedstr

;; ============================================================================
;; cg_e_formattedvalue(Comp *c, CompUnit *u, uint32_t node) -> 1 ok, 0 error
;;
;;     <value>; [<spec>]; FORMAT_VALUE conv | (spec ? 4 : 0)
;;
;; The conversion is in the low two bits (0 none, 1 !s, 2 !r, 3 !a) and bit 2
;; says a format spec was pushed as well.
;; ============================================================================
DEF_FUNC cg_e_formattedvalue, CJ2_FRAME
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
    mov [rbp - CJ2_LINE], rcx
    movzx ecx, byte [rax + AstNode.subkind]
    mov [rbp - CJ2_N], rcx              ; the conversion
    mov edx, [rax + AstNode.a]
    mov rdi, rbx
    mov rsi, r12
    call cg_expr
    test eax, eax
    jz .fail

    mov rdi, rbx
    mov rsi, r13
    call ast_at
    mov ecx, [rax + AstNode.b]
    test ecx, ecx
    jz .no_spec
    mov edx, ecx
    mov rdi, rbx
    mov rsi, r12
    call cg_expr
    test eax, eax
    jz .fail
    or qword [rbp - CJ2_N], 4
.no_spec:
    mov rdi, r12
    mov esi, OP_FORMAT_VALUE
    mov rdx, [rbp - CJ2_N]
    mov rcx, [rbp - CJ2_LINE]
    call cg_emit
    mov eax, 1
.fail:
    pop r13
    pop r12
    pop rbx
    leave
    ret
END_FUNC cg_e_formattedvalue

section .rodata
cm_listcomp: db "<listcomp>", 0
cm_setcomp:  db "<setcomp>", 0
cm_dictcomp: db "<dictcomp>", 0
cm_genexpr:  db "<genexpr>", 0
cm_dot_zero: db ".0", 0

ASM_INIT

section .text

