; codegen_async.asm - await, async for, async with
;
; Everything here is built out of one shape: the *send loop*, which drives a
; sub-iterator to exhaustion while yielding whatever it yields up to the caller.
; `await x` is that loop over x.__await__(); `async for` is that loop over each
; __anext__(); `async with` is that loop over __aenter__() and again over each
; __aexit__().  So the loop is written once, in cg_send_loop, and the four
; constructs differ only in what they push before calling it.
;
; The loop's exception table entry is the part that is easy to leave out and
; impossible to notice afterwards: it covers the YIELD_VALUE alone, and routes
; a throw() into a CLEANUP_THROW that converts a StopIteration back into the
; sub-iterator's return value.  Without it, `gen.throw()` through a delegation
; escapes to the caller instead of resuming the awaited object.

%include "macros.inc"
%include "object.inc"
%include "opcodes.inc"
%include "value.inc"
%include "compiler.inc"

extern ast_at
extern ast_child
extern cg_block
extern cg_const
extern cg_emit
extern cg_emit_jump
extern cg_emit_jump_back
extern cg_expr
extern cg_label_bind
extern cg_label_new
extern cg_loop_pop
extern cg_loop_push
extern cg_pop_handler
extern cg_push_handler
extern cg_store
extern none_singleton

global cg_send_loop
global cg_e_await
global cg_s_asyncfor
global cg_await_value

section .text

;; ============================================================================
;; cg_send_loop(CompUnit *u, uint64_t line, uint64_t resume_arg) -> 1 ok, 0 err
;;
;; Stack in:  ... receiver, sent_value       (the value is normally None)
;; Stack out: ... result
;;
;;   top:   SEND end
;;          [handler -> throw, covering the YIELD_VALUE only]
;;          YIELD_VALUE
;;          RESUME resume_arg
;;          JUMP_BACKWARD_NO_INTERRUPT top
;;   throw: CLEANUP_THROW
;;   end:   END_SEND
;;
;; CPython puts the CLEANUP_THROW out of line and jumps back to `end`; putting
;; it immediately above `end` and letting it fall through is the same code with
;; the jump removed.  Nothing reaches it by fallthrough, because
;; JUMP_BACKWARD_NO_INTERRUPT does not fall through.
;; ============================================================================
SL_UNIT   equ 8
SL_LINE   equ 16
SL_RESUME equ 24
SL_TOP    equ 32
SL_END    equ 40
SL_THROW  equ 48
SL_FRAME  equ 56          ; + 1 push = 64
DEF_FUNC cg_send_loop, SL_FRAME
    push rbx
    mov rbx, rdi
    mov [rbp - SL_LINE], rsi
    mov [rbp - SL_RESUME], rdx

    mov rdi, rbx
    call cg_label_new
    mov [rbp - SL_TOP], rax
    mov rdi, rbx
    call cg_label_new
    mov [rbp - SL_END], rax
    mov rdi, rbx
    call cg_label_new
    mov [rbp - SL_THROW], rax

    mov rdi, rbx
    mov rsi, [rbp - SL_TOP]
    call cg_label_bind
    mov rdi, rbx
    mov esi, OP_SEND
    mov rdx, [rbp - SL_END]
    mov rcx, [rbp - SL_LINE]
    call cg_emit_jump

    ; The region opens here, with the receiver and the sent value on the stack;
    ; that is the depth CLEANUP_THROW expects to find beneath the exception.
    mov rdi, rbx
    mov rsi, [rbp - SL_THROW]
    xor edx, edx                        ; no lasti
    call cg_push_handler
    mov rdi, rbx
    mov esi, OP_YIELD_VALUE
    xor edx, edx
    mov rcx, [rbp - SL_LINE]
    call cg_emit
    mov rdi, rbx
    call cg_pop_handler

    mov rdi, rbx
    mov esi, OP_RESUME
    mov rdx, [rbp - SL_RESUME]
    xor ecx, ecx
    call cg_emit
    or byte [rax + Instr.flags], IF_NOLINE
    mov rdi, rbx
    mov esi, OP_JUMP_BACKWARD_NO_INTERRUPT
    mov rdx, [rbp - SL_TOP]
    mov rcx, [rbp - SL_LINE]
    call cg_emit_jump_back

    mov rdi, rbx
    mov rsi, [rbp - SL_THROW]
    call cg_label_bind
    mov rdi, rbx
    mov esi, OP_CLEANUP_THROW
    xor edx, edx
    mov rcx, [rbp - SL_LINE]
    call cg_emit

    mov rdi, rbx
    mov rsi, [rbp - SL_END]
    call cg_label_bind
    mov rdi, rbx
    mov esi, OP_END_SEND
    xor edx, edx
    mov rcx, [rbp - SL_LINE]
    call cg_emit
    mov eax, 1
    pop rbx
    leave
    ret
END_FUNC cg_send_loop

;; ============================================================================
;; cg_await_value(CompUnit *u, uint64_t line, uint64_t which) -> 1 ok, 0 err
;; Await whatever is on top of the stack.  `which` is GET_AWAITABLE's oparg,
;; which only affects the message in the TypeError it raises: 0 for a plain
;; `await`, 1 for __aenter__, 2 for __aexit__.
;; ============================================================================
AV_UNIT  equ 8
AV_LINE  equ 16
AV_FRAME equ 24           ; + 1 push = 32
DEF_FUNC cg_await_value, AV_FRAME
    push rbx
    mov rbx, rdi
    mov [rbp - AV_LINE], rsi

    mov rdi, rbx
    mov esi, OP_GET_AWAITABLE
    ; rdx already holds `which`
    mov rcx, [rbp - AV_LINE]
    call cg_emit

    lea rsi, [rel none_singleton]
    INCREF rsi
    mov rdi, rbx
    call cg_const
    mov rdx, rax
    mov rdi, rbx
    mov esi, OP_LOAD_CONST
    mov rcx, [rbp - AV_LINE]
    call cg_emit

    mov rdi, rbx
    mov rsi, [rbp - AV_LINE]
    mov edx, 3
    call cg_send_loop
    pop rbx
    leave
    ret
END_FUNC cg_await_value

;; ============================================================================
;; cg_e_await(Comp *c, CompUnit *u, uint32_t node) -> 1 ok, 0 error
;; ============================================================================
EA_COMP  equ 8
EA_UNIT  equ 16
EA_LINE  equ 24
EA_FRAME equ 40           ; + 3 pushes = 64
DEF_FUNC cg_e_await, EA_FRAME
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
    mov [rbp - EA_LINE], rcx
    mov edx, [rax + AstNode.a]
    mov rdi, rbx
    mov rsi, r12
    call cg_expr
    test eax, eax
    jz .fail

    mov rdi, r12
    mov rsi, [rbp - EA_LINE]
    xor edx, edx
    call cg_await_value
.fail:
    pop r13
    pop r12
    pop rbx
    leave
    ret
END_FUNC cg_e_await

;; ============================================================================
;; cg_s_asyncfor(Comp *c, CompUnit *u, uint32_t node) -> 1 ok, 0 error
;;
;;         <iter>; GET_AITER
;;   top:  [handler -> exc, depth = just the iterator]
;;         GET_ANEXT; LOAD_CONST None; <send loop>
;;         [pop]
;;         <store target>; <body>; JUMP_BACKWARD top
;;   exc:  END_ASYNC_FOR
;;         <else>
;;   end:
;;
;; The protected region is what makes the loop terminate: __anext__ raises
;; StopAsyncIteration rather than returning a sentinel, so the exit edge is an
;; exception edge.  END_ASYNC_FOR swallows exactly that exception, drops the
;; iterator with it, and re-raises anything else.  The region deliberately ends
;; before the target store, so an exception from the body is the caller's.
;; ============================================================================
AF_COMP  equ 8
AF_UNIT  equ 16
AF_NODE  equ 24
AF_LINE  equ 32
AF_TOP   equ 40
AF_EXC   equ 48
AF_END   equ 56
AF_FRAME equ 72           ; + 3 pushes = 96
DEF_FUNC cg_s_asyncfor, AF_FRAME
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
    mov [rbp - AF_LINE], rcx

    mov edx, [rax + AstNode.b]          ; the iterable
    mov rdi, rbx
    mov rsi, r12
    call cg_expr
    test eax, eax
    jz .fail
    mov rdi, r12
    mov esi, OP_GET_AITER
    xor edx, edx
    mov rcx, [rbp - AF_LINE]
    call cg_emit

    mov rdi, r12
    call cg_label_new
    mov [rbp - AF_TOP], rax
    mov rdi, r12
    call cg_label_new
    mov [rbp - AF_EXC], rax
    mov rdi, r12
    call cg_label_new
    mov [rbp - AF_END], rax

    mov rdi, r12
    mov rsi, [rbp - AF_TOP]
    call cg_label_bind
    mov rdi, r12
    mov rsi, [rbp - AF_EXC]
    xor edx, edx                        ; no lasti
    call cg_push_handler

    mov rdi, r12
    mov esi, OP_GET_ANEXT
    xor edx, edx
    mov rcx, [rbp - AF_LINE]
    call cg_emit
    lea rsi, [rel none_singleton]
    INCREF rsi
    mov rdi, r12
    call cg_const
    mov rdx, rax
    mov rdi, r12
    mov esi, OP_LOAD_CONST
    mov rcx, [rbp - AF_LINE]
    call cg_emit
    mov rdi, r12
    mov rsi, [rbp - AF_LINE]
    mov edx, 3
    call cg_send_loop
    test eax, eax
    jz .fail
    mov rdi, r12
    call cg_pop_handler

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
    mov rsi, [rbp - AF_END]             ; break
    mov rdx, [rbp - AF_TOP]             ; continue
    mov ecx, 1                          ; break must drop the iterator
    call cg_loop_push

    mov rdi, rbx
    mov rsi, r13
    call ast_at
    mov edx, [rax + AstNode.c]          ; the body
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
    mov rdx, [rbp - AF_TOP]
    mov rcx, [rbp - AF_LINE]
    call cg_emit_jump_back

    mov rdi, r12
    mov rsi, [rbp - AF_EXC]
    call cg_label_bind
    mov rdi, r12
    mov esi, OP_END_ASYNC_FOR
    xor edx, edx
    mov rcx, [rbp - AF_LINE]
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
    mov rsi, [rbp - AF_END]
    call cg_label_bind
    mov eax, 1
.fail:
    pop r13
    pop r12
    pop rbx
    leave
    ret
END_FUNC cg_s_asyncfor

ASM_INIT
