; opcodes/call_spec.asm - the specialized call
;
; CALL had no specialization at all where CPython 3.12 has seventeen, and a
; profile of a call-heavy benchmark put roughly half its instructions in the
; call machinery: op_call 12%, func_call 8%, frame_new and frame_free 15%,
; eval_frame and eval_return 17%.
;
; This takes the first two.  A call whose callable is a plain Python function
; and whose arguments exactly fill its parameters needs none of op_call's
; 104-byte frame, its tp_call indirection, its profiling gate or its cleanup
; loop, and none of func_call's prologue, keyword handling or six binder
; phases -- func.asm already isolates exactly this predicate and calls it "the
; shape almost every call has".
;
; Both call shapes are handled, because the second is the one that matters for
; object-oriented code: LOAD_ATTR_METHOD leaves [method, self] on the stack
; with self already contiguous with the arguments, so a method call is the
; plain case with one more argument and the callable one slot deeper.  That
; layout exists precisely so no argument array has to be built.
;
; What is NOT taken: eval_frame and eval_return, and the frame allocation.
; Those need the frame pushed without a C-level call, which is a change to how
; the interpreter is structured rather than a new handler.
;
; A generator, coroutine or async generator is refused at the guard.  Such a
; call returns with the frame still live -- op_return_generator hands it to the
; generator object and func_call reads instr_ptr to decide not to free it --
; and this handler frees unconditionally.

%include "macros.inc"
%include "object.inc"
%include "opcodes.inc"

extern eval_saved_rbx
extern eval_saved_r13
extern opcode_dispatch_table
extern eval_frame
extern eval_exception_unwind
extern frame_new
extern frame_free
extern func_type
extern builtins_dict_global
extern kw_names_pending
extern current_exception
extern obj_dealloc
extern op_call

section .text

;; ============================================================================
;; op_call_py_exact (241) -> nothing; replaces the call sequence with its result
;;
;; Guards, in the order that rejects soonest:
;;   - no keyword names pending
;;   - the callable is exactly a Python function
;;   - its parameters are exactly filled: argcount matches, no keyword-only
;;     parameters, no *args or **kwargs
;;   - it is not a generator, coroutine or async generator
;;
;; CALL carries an oparg, so the deopt jumps to op_call with ecx intact rather
;; than rewinding rbx.  Nothing is popped before the last guard.
;;
;; Stack, top last:  func_or_null, callable_or_self, arg0 .. argN-1
;; A NULL in the first slot means the second is the function and the arguments
;; are the N; a non-NULL means the first is the method, the second is self, and
;; the arguments are self plus the N.  Either way N+2 slots go and one comes
;; back, and the values to release are the arguments plus the callable -- which
;; is the slot immediately below them in both shapes.
;; ============================================================================
CPE_NARGS equ 8
CPE_TOTAL equ 16                ; the arguments the callee actually receives
CPE_FUNC  equ 24
CPE_ARGS  equ 32                ; where they start on the value stack
CPE_RET   equ 40
CPE_FRAME equ 56                ; a handler is entered with rsp 16-aligned, so
                                ; push rbp + 56 brings it back to aligned.
                                ; 56 and not 48 although CPE_IDX is gone: the
                                ; parity is what the number is for.

DEF_FUNC op_call_py_exact, CPE_FRAME
    ; ecx is the oparg and must survive to .cpe_deopt, so nothing before the
    ; last guard touches rcx.
    cmp qword [rel kw_names_pending], 0
    jne .cpe_deopt

    mov r8d, ecx
    add r8d, 2
    neg r8
    mov r9, [r13 + r8*8]                ; func_or_null
    mov r10d, ecx                       ; the callee's argument count
    test r9, r9
    jz .cpe_plain
    inc r10d                            ; self is an argument too
    mov rdi, r9                         ; and the method is the callable
    jmp .cpe_have_callable
.cpe_plain:
    mov r8d, ecx
    inc r8d
    neg r8
    mov rdi, [r13 + r8*8]
.cpe_have_callable:
    V_TEST_PTR rdi, rax
    ja .cpe_deopt
    lea rax, [rel func_type]
    cmp [rdi + PyObject.ob_type], rax
    jne .cpe_deopt

    mov rax, [rdi + PyFuncObject.func_code]
    cmp r10d, [rax + PyCodeObject.co_argcount]
    jne .cpe_deopt
    cmp dword [rax + PyCodeObject.co_kwonlyargcount], 0
    jne .cpe_deopt
    test dword [rax + PyCodeObject.co_flags], \
         CO_VARARGS | CO_VARKEYWORDS | CO_GENERATOR | CO_COROUTINE | \
         CO_ASYNC_GENERATOR
    jnz .cpe_deopt

    ; Past the last guard.
    mov [rbp - CPE_NARGS], rcx
    mov [rbp - CPE_TOTAL], r10
    mov [rbp - CPE_FUNC], rdi
    mov r8, r10
    neg r8
    lea r8, [r13 + r8*8]
    mov [rbp - CPE_ARGS], r8

    ; frame_new(code, globals, builtins, locals = NULL)
    mov rdi, rax
    mov rsi, [rbp - CPE_FUNC]
    mov rsi, [rsi + PyFuncObject.func_globals]
    mov rdx, [rel builtins_dict_global]
    xor ecx, ecx
    call frame_new
    mov r15, rax                        ; the register convention leaves r15
                                        ; free, and eval_frame preserves it
    mov rcx, [rbp - CPE_FUNC]
    mov [r15 + PyFrame.func_obj], rcx

    ; Every parameter has an argument, so the bind is a copy.  Defaults need no
    ; test: a default could not apply to a parameter that already has one.
    mov rcx, [rbp - CPE_TOTAL]
    test ecx, ecx
    jz .cpe_run
    mov r8, [rbp - CPE_ARGS]
    xor eax, eax
.cpe_bind:
    mov rdx, [r8 + rax*8]
    INCREF_V rdx, r9
    mov [r15 + PyFrame.localsplus + rax*8], rdx
    inc eax
    cmp eax, ecx
    jb .cpe_bind

.cpe_run:
    mov rdi, r15
    call eval_frame
    mov [rbp - CPE_RET], rax
    mov rdi, r15
    call frame_free

    ; Release the arguments and the callable.  They are contiguous: the
    ; callable is the slot immediately below the arguments in both shapes.
    ; A pointer walking down, not an index in the frame.  This used to reload
    ; the argument base AND read back the counter it had just written, on
    ; every iteration -- four memory accesses per released argument for two
    ; values that fit in registers.  r15 is free: the frame it held has just
    ; been given back, and it survives DECREF_V, which is the whole
    ; difficulty.
    mov rax, [rbp - CPE_TOTAL]
    mov r15, [rbp - CPE_ARGS]
    lea r15, [r15 + rax*8]              ; one past the last argument
.cpe_release:
    sub r15, 8
    mov rdi, [r15]
    DECREF_V rdi, rdx
    cmp r15, [rbp - CPE_ARGS]
    jae .cpe_release                    ; ...down to the callable below them

    ; N+2 slots go, whichever shape this was.
    mov rcx, [rbp - CPE_NARGS]
    add rcx, 2
    shl rcx, 3
    sub r13, rcx

    mov rax, [rbp - CPE_RET]
    test rax, rax
    jz .cpe_propagate
    VPUSH rax
    add rbx, 6                          ; skip 3 CACHE entries
    leave
    DISPATCH

.cpe_propagate:
    ; The callee raised.  As op_call's own propagate: rbx is not advanced,
    ; because the unwinder reads the current IP from eval_saved_rbx, which
    ; DISPATCH set.
    leave
    mov [rel eval_saved_r13], r13
    jmp eval_exception_unwind

.cpe_deopt:
    mov byte [rbx - 2], OP_CALL
    leave
    jmp op_call
END_FUNC op_call_py_exact
