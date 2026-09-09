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
; The frame is now pushed WITHOUT a C-level call: the handler builds it, writes
; PyFrame.entry_kind, and jumps into eval_frame rather than calling it.
; eval_return reads that field while the frame is still current, restores the
; caller exactly as it always did, and jumps to eval_inline_resume below
; instead of returning.  What is still shared, unchanged, is eval_frame's whole
; prologue and eval_return's whole restore -- the 19-word state block, the
; handled_exception swap, prev_frame linking, eval_base_rsp, the recursion
; count and the trace hooks.
;
; The frame allocation is still frame_new's.
;
; A generator, coroutine or async generator is refused at the guard.  Such a
; call returns with the frame still live -- op_return_generator hands it to the
; generator object and func_call reads instr_ptr to decide not to free it --
; and this handler frees unconditionally.  That guard is load-bearing twice
; over: the unconditional frame_free is also what lets the arguments' stack
; references be MOVED into the frame rather than copied.

%include "macros.inc"
%include "object.inc"
%include "opcodes.inc"

extern eval_saved_rbx
extern eval_saved_r13
extern opcode_dispatch_table
extern eval_frame
extern eval_inline_ret_bug
extern eval_dispatch
extern eval_exception_unwind
extern frame_new
extern frame_free
extern func_type
extern builtins_dict_global
extern kw_names_pending
extern current_exception
extern obj_dealloc
extern obj_decref
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
    ;
    ; And it is a copy in the ownership sense too: the stack slot's reference
    ; MOVES into the frame, which is what CPython's CALL_PY_EXACT_ARGS does.
    ; The slots below are popped and never read again, and frame_free releases
    ; every localsplus entry unconditionally -- which this handler's own guards
    ; guarantee runs, because they exclude the generator, coroutine and async
    ; generator shapes, the only ones that return with the frame still live.
    ; So the pair of refcount operations per argument becomes none, on the path
    ; every Python call takes.
    ;
    ; Nothing sees the slots twice in the window between: neither the value
    ; stack nor localsplus is reached by any tp_traverse -- an interpreter
    ; frame is pool-allocated and untracked, and frameobj_traverse walks
    ; f_back, f_globals, f_locals and f_trace -- so the collector cannot
    ; subtract two references where only one exists.
    mov rcx, [rbp - CPE_TOTAL]
    test ecx, ecx
    jz .cpe_run
    mov r8, [rbp - CPE_ARGS]
    xor eax, eax
.cpe_bind:
    mov rdx, [r8 + rax*8]
    mov [r15 + PyFrame.localsplus + rax*8], rdx
    inc eax
    cmp eax, ecx
    jb .cpe_bind

.cpe_run:
    ; Hand the frame to eval_frame WITHOUT calling it.
    ;
    ; Two words say everything the far side needs.  entry_kind tells
    ; eval_return that this frame has no return address to go back to, so that
    ; after its restore -- which is unchanged, and which is what puts rbx, r12,
    ; r13 and every eval global back to the caller's -- it jumps to
    ; eval_inline_resume instead of returning.  entry_slots is what the resume
    ; cannot work out for itself once the frame is gone: how many value-stack
    ; slots this call consumed.
    mov dword [r15 + PyFrame.entry_kind], FRAME_ENTRY_INLINE
    mov ecx, [rbp - CPE_NARGS]
    add ecx, 2                          ; N+2 slots, whichever shape this was
    mov dword [r15 + PyFrame.entry_slots], ecx

    mov rdi, r15
    leave                               ; this handler's own C frame is gone;
                                        ; rbp is the caller frame's again, and
                                        ; eval_frame will push it as its own
    ; The word where a return address would be.  It is a real address, not
    ; padding: if entry_kind is ever wrong, a `ret` lands on a named
    ; fatal_error rather than in the middle of the value stack.  It also
    ; supplies the parity -- a handler is entered with rsp 16-aligned, `call`
    ; would have left rsp 8 past that, and eval_frame's push list is counted
    ; from there.  Get this wrong and every libc call inside the callee is
    ; misaligned.
    push eval_inline_ret_bug    ; one instruction: -no-pie puts .text below
                                ; 2**31, so the immediate sign-extends to
                                ; itself
    ;
    ; rbx is deliberately NOT advanced past the CACHE entries here.  The
    ; unwinder reads the current IP from eval_saved_rbx, which DISPATCH set to
    ; this CALL's own address, and a propagate out of the callee has to find it
    ; there -- exactly as .cpe_propagate relied on.  The resume advances it.
    jmp eval_frame

.cpe_deopt:
    mov byte [rbx - 2], OP_CALL
    leave
    jmp op_call
END_FUNC op_call_py_exact

;; ============================================================================
;; eval_inline_resume(r9 = the frame just left, r11 = a deferred exception or 0,
;;                    rax:rdx = the returned Value) -> nothing; dispatches on
;;                    in the caller, or unwinds
;;
;; The far side of the jump above.
;;
;; eval_return jumps here in place of returning, having already put back
;; everything a `ret` would have: rbx, r12, r13, r14, rbp and all thirteen
;; scoped globals.  So this runs exactly where the instruction after
;; `call eval_frame` used to, and does exactly what stood there.
;;
;;   r9  = the frame just left, to free
;;   r11 = an exception whose release eval_return deferred to here, or 0
;;   rax:rdx = the returned Value; rax == 0 means the callee is unwinding
;;
;; The order below is the order that code had, and two parts of it are not
;; free to move.  The deferred release comes before frame_free because that is
;; where it was -- eval_return did it last, the caller did frame_free first --
;; and its whole point is to run a __del__ where the caller's next opcode
;; would.  entry_slots is read before frame_free because after it the frame is
;; back in the pool.
;; ============================================================================
DEF_FUNC_BARE eval_inline_resume
    ; The stack top first, while the frame is still there to be asked.  r13 is
    ; callee-saved, so lowering it now means nothing below has to carry the
    ; count across a call -- and the slots are still physically there, which is
    ; what makes finding the callable below work.
    mov r10d, dword [r9 + PyFrame.entry_slots]
    shl r10, 3
    sub r13, r10                        ; N+2 slots go, whichever shape it was

    ; rdx is DEAD: it is the fat-pair tag, and the code this replaces clobbered
    ; it in its own DECREF before pushing rax.  So only the payload has to
    ; survive the two calls below, and r15 -- free by the register convention,
    ; and between frames here -- carries it instead of a stack slot.
    mov r15, rax

    ; The exception eval_return deferred, released before the frame is freed,
    ; which is the order it had: eval_return did this last and the caller did
    ; frame_free first.  Its point is that a __del__ runs where the caller's
    ; next opcode would, and that is here.
    test r11, r11
    jz .eir_no_deferred
    push r9
    push r9                             ; twice: rsp keeps its alignment
    mov rdi, r11
    call obj_decref
    pop r9
    pop r9
.eir_no_deferred:
    mov rdi, r9
    call frame_free

    ; Release the callable, and only the callable.  The arguments' references
    ; went into the frame at .cpe_bind and frame_free has just given them back;
    ; releasing them here as well would be one DECREF too many.
    ;
    ; Which slot it is depends on the call's shape, and the stack still says
    ; which: the deepest of the consumed slots is the method in one shape and a
    ; NULL in the other, and in the second the callable is the slot above it.
    mov rdi, [r13]
    test rdi, rdi
    jnz .eir_have_callable
    mov rdi, [r13 + 8]
.eir_have_callable:
    DECREF_V rdi, rcx

    mov rax, r15
    test rax, rax
    jz .eir_propagate
    VPUSH rax
    add rbx, 6                          ; skip 3 CACHE entries
    DISPATCH

.eir_propagate:
    ; The callee raised and found no handler.  rbx is still the CALL's own
    ; address, which is what the unwinder reads out of eval_saved_rbx.
    mov [rel eval_saved_r13], r13
    jmp eval_exception_unwind
END_FUNC eval_inline_resume
