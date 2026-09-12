; generator.asm - Generator, coroutine, and async generator object implementation
; Phase 10: suspendable frames via RETURN_GENERATOR / YIELD_VALUE
; Phase 11: async/await — coro_type, async_gen_type, gen_throw

%include "macros.inc"
%include "object.inc"

extern bool_true
extern bool_false
extern ap_malloc
extern gc_alloc
extern gc_track
extern gc_dealloc
extern ap_free
extern obj_decref
extern obj_incref
extern eval_frame
extern frame_free
extern none_singleton
extern str_from_cstr
extern obj_dealloc
extern type_type
extern ap_strcmp
extern raise_exception
extern raise_exception_obj
extern exc_new
extern exc_TypeError_type
extern exc_StopIteration_type
extern set_exception
extern exc_RuntimeError_type
extern exc_GeneratorExit_type
extern sys_write
extern exc_StopAsyncIteration_type
extern method_new
extern builtin_func_new
extern current_exception
extern throw_pending

; The awaitables live in asyncgen.asm; these are what this file reaches for.
extern _get_agen_aclose_builtin
extern _get_agen_athrow_builtin
extern _get_agen_asend_builtin
extern agw_dealloc
extern ags_dealloc
extern ags_iter_self
extern ags_iternext
extern agt_iternext

;; ============================================================================
;; gen_new(PyFrame *frame) -> PyGenObject*
;; Create a new generator object that owns the given frame.
;; rdi = frame (ownership transfers to generator)
;; ============================================================================
DEF_FUNC gen_new
    push rbx
    push r12

    mov rbx, rdi               ; rbx = frame

    mov edi, PyGenObject_size
    lea rsi, [rel gen_type]
    call gc_alloc
    mov r12, rax               ; r12 = gen object (ob_refcnt=1, ob_type set)

    mov [r12 + PyGenObject.gi_frame], rbx
    mov [rbx + PyFrame.gen_owner], r12   ; borrowed; frame.clear() needs it
    mov qword [r12 + PyGenObject.gi_running], 0

    ; Copy code from frame and INCREF it
    mov rdx, [rbx + PyFrame.code]
    mov [r12 + PyGenObject.gi_code], rdx
    mov rdi, rdx
    call obj_incref

    ; gi_name = NULL (not critical)
    mov qword [r12 + PyGenObject.gi_name], 0

    ; gi_return_value = NULL (no return value yet)
    mov qword [r12 + PyGenObject.gi_return_value], 0

    ; The two PEP 525 words, which only an ASYNC generator ever uses.  All
    ; three constructors allocate at PyGenObject_size, so the words are there
    ; for an ordinary generator too -- and gc_alloc does not zero, so leaving
    ; them meant gen_traverse and gen_dealloc read allocator leftovers as an
    ; owned pointer the moment they started consulting the field.
    mov qword [r12 + PyGenObject.ag_hooks_done], 0
    mov qword [r12 + PyGenObject.ag_finalizer], 0

    mov rdi, r12
    call gc_track

    mov rax, r12               ; return gen object
    mov edx, TAG_PTR             ; return tag
    pop r12
    pop rbx
    leave
    ret
END_FUNC gen_new

;; ============================================================================
;; coro_new(PyFrame *frame) -> PyGenObject* (coroutine)
;; Same as gen_new but ob_type = coro_type.
;; rdi = frame (ownership transfers to coroutine)
;; ============================================================================
DEF_FUNC coro_new
    push rbx
    push r12

    mov rbx, rdi               ; rbx = frame

    mov edi, PyGenObject_size
    lea rsi, [rel coro_type]
    call gc_alloc
    mov r12, rax               ; r12 = coro object (ob_refcnt=1, ob_type set)

    mov [r12 + PyGenObject.gi_frame], rbx
    mov [rbx + PyFrame.gen_owner], r12   ; borrowed; frame.clear() needs it
    mov qword [r12 + PyGenObject.gi_running], 0

    ; Copy code from frame and INCREF it
    mov rdx, [rbx + PyFrame.code]
    mov [r12 + PyGenObject.gi_code], rdx
    mov rdi, rdx
    call obj_incref

    mov qword [r12 + PyGenObject.gi_name], 0
    mov qword [r12 + PyGenObject.gi_return_value], 0
    mov qword [r12 + PyGenObject.ag_hooks_done], 0   ; see gen_new
    mov qword [r12 + PyGenObject.ag_finalizer], 0

    mov rdi, r12
    call gc_track

    mov rax, r12               ; return coro object
    mov edx, TAG_PTR
    pop r12
    pop rbx
    leave
    ret
END_FUNC coro_new

;; ============================================================================
;; async_gen_new(PyFrame *frame) -> PyGenObject* (async generator)
;; Same as gen_new but ob_type = async_gen_type.
;; rdi = frame (ownership transfers to async generator)
;; ============================================================================
DEF_FUNC async_gen_new
    push rbx
    push r12

    mov rbx, rdi               ; rbx = frame

    mov edi, PyGenObject_size
    lea rsi, [rel async_gen_type]
    call gc_alloc
    mov r12, rax               ; ob_refcnt=1, ob_type set

    mov [r12 + PyGenObject.gi_frame], rbx
    mov [rbx + PyFrame.gen_owner], r12   ; borrowed; frame.clear() needs it
    mov qword [r12 + PyGenObject.gi_running], 0

    mov rdx, [rbx + PyFrame.code]
    mov [r12 + PyGenObject.gi_code], rdx
    mov rdi, rdx
    call obj_incref

    mov qword [r12 + PyGenObject.gi_name], 0
    mov qword [r12 + PyGenObject.gi_return_value], 0
    mov qword [r12 + PyGenObject.ag_hooks_done], 0
    mov qword [r12 + PyGenObject.ag_finalizer], 0

    mov rdi, r12
    call gc_track

    mov rax, r12
    mov edx, TAG_PTR
    pop r12
    pop rbx
    leave
    ret
END_FUNC async_gen_new

;; ============================================================================
;; gen_iternext(PyGenObject *self) -> rax = Value or NULL
;; Resume the generator. Push None as sent value, call eval_frame.
;; Returns yielded value, or NULL if generator is exhausted.
;; rdi = generator
;; ============================================================================
DEF_FUNC gen_iternext
    push rbx
    push r12

    mov rbx, rdi               ; rbx = generator

    ; Check if generator is exhausted
    mov r12, [rbx + PyGenObject.gi_frame]
    test r12, r12
    jz .exhausted

    ; Check if already running (recursive call)
    cmp qword [rbx + PyGenObject.gi_running], 1
    je .running_error

    ; Mark as running
    mov qword [rbx + PyGenObject.gi_running], 1

    ; Push None as the "sent" value onto the frame's value stack
    FRAME_PUSH_NONE r12, rax

    ; Resume execution
    mov rdi, r12
    ; A generator is its own execution context, and two globals say so in
    ; different ways.  handled_exception -- what an except block installed --
    ; is swapped between the global and PyFrame.exc_state by eval_frame and
    ; eval_return, so a generator suspended inside a handler carries that
    ; state across the suspension and the caller gets its own back.  Nothing
    ; here has to do anything about it.
    ;
    ; current_exception is the other one: an exception in FLIGHT.  It is
    ; cleared for the duration of the resume so that a NULL result means the
    ; body raised and nothing else, and put back if it did not.
    push rax
    mov rax, [rel current_exception]
    push rax
    mov qword [rel current_exception], 0
    call eval_frame
    pop rcx
    test rax, rax
    jz .gs_gen_raised
    mov [rel current_exception], rcx    ; takes over the saved reference
    jmp .gs_exc_settled
.gs_gen_raised:
    ; The body raised, and that exception is the result.  The caller's, if it
    ; had one, is ours to drop: the global gave up its reference above.
    test rcx, rcx
    jz .gs_exc_settled
    push rax
    push rdx
    mov rdi, rcx
    call obj_decref
    pop rdx
    pop rax
.gs_exc_settled:
    add rsp, 8
    V_UNPACK rax, rdx           ; eval_frame returns a Value
    ; rax = yielded/returned value payload, rdx = tag

    mov r12, rax               ; save return value payload
    push rdx                   ; save return value tag

    ; Mark as not running
    mov qword [rbx + PyGenObject.gi_running], 0

    ; Check if generator returned (vs yielded)
    ; If frame->instr_ptr == 0, generator returned (exhausted)
    mov rdi, [rbx + PyGenObject.gi_frame]
    cmp qword [rdi + PyFrame.instr_ptr], 0
    jne .yielded

    ; Generator is exhausted.  Clear gi_frame BEFORE freeing it: frame_free
    ; releases localsplus, and a __del__ down there that touches this
    ; generator must not find gi_frame naming a frame being torn down.
    mov qword [rbx + PyGenObject.gi_frame], 0
    call frame_free

    ; Store return value in gi_return_value (for StopIteration.value)
    pop rax                    ; rax = return value tag
    V_PACK r12, rax
    mov [rbx + PyGenObject.gi_return_value], r12

    ; Return NULL to signal StopIteration
    RET_NULL
    pop r12
    pop rbx
    leave
    ret

.yielded:
    ; Return the yielded value
    mov rax, r12
    pop rdx                    ; restore result tag
    V_PACK rax, rdx            ; eval_frame still returns a fat pair
    pop r12
    pop rbx
    leave
    ret

.exhausted:
    RET_NULL
    pop r12
    pop rbx
    leave
    ret

.running_error:
    RET_NULL
    pop r12
    pop rbx
    leave
    ret
END_FUNC gen_iternext

;; ============================================================================
;; async_gen_init_hooks(rdi = an async generator) -> nothing
;;
;; PEP 525's firstiter, fired once per generator, and the finalizer snapshotted
;; beside it.  Anything the hook raises is left pending: the caller is building
;; an asend wrapper and has no way to report, and CPython's own
;; async_gen_init_hooks propagates it from the same place.
;; ============================================================================
AGIH_GEN   equ 8
AGIH_FRAME equ 16           ; 0 pushes, 16-aligned
DEF_FUNC_LOCAL async_gen_init_hooks, AGIH_FRAME
    cmp qword [rdi + PyGenObject.ag_hooks_done], 0
    jne .agih_done
    mov qword [rdi + PyGenObject.ag_hooks_done], 1
    mov [rbp - AGIH_GEN], rdi

    extern asyncgen_finalizer_hook
    mov rax, [rel asyncgen_finalizer_hook]
    test rax, rax
    jz .agih_no_final
    mov [rdi + PyGenObject.ag_finalizer], rax
    mov rdi, rax
    call obj_incref
.agih_no_final:

    extern asyncgen_firstiter_hook
    mov rax, [rel asyncgen_firstiter_hook]
    test rax, rax
    jz .agih_done
    sub rsp, 16
    mov rcx, [rbp - AGIH_GEN]
    mov [rsp], rcx
    mov rdi, rax
    mov rsi, rsp
    mov edx, 1
    extern obj_call_n
    call obj_call_n
    add rsp, 16
    test rax, rax
    jz .agih_done               ; the hook raised; the exception stays pending
    DECREF_V rax, rcx
.agih_done:
    leave
    ret
END_FUNC async_gen_init_hooks

;; ============================================================================
;; async_gen_iternext(PyGenObject *self) -> fat value (AsyncGenASend wrapper)
;; Called by GET_ANEXT. Creates an AsyncGenASend wrapper that, when iterated
;; by SEND, actually resumes the async generator.
;; rdi = async generator
;; Returns: (rax=AsyncGenASend*, edx=TAG_PTR)
;; ============================================================================


DEF_FUNC async_gen_iternext, 8            ; 1 pushes, so rsp is 16-aligned
    push rbx
    mov rbx, rdi               ; rbx = async generator

    ; PEP 525: the first time an async generator is used, the event loop's
    ; firstiter hook is told about it -- that is how asyncio builds the set
    ; shutdown_asyncgens() closes -- and the finalizer in force at that moment
    ; is kept on the generator, so one collected later is still closed by the
    ; loop that started it.
    call async_gen_init_hooks

    ; Allocate AsyncGenASend wrapper
    mov edi, AsyncGenASend_size
    call ap_malloc
    ; rax = wrapper

    mov qword [rax + PyObject.ob_refcnt], 1
    lea rcx, [rel async_gen_asend_type]
    mov [rax + PyObject.ob_type], rcx
    mov [rax + AsyncGenASend.ags_gen], rbx
    mov dword [rax + AsyncGenASend.ags_state], 0   ; initial
    mov dword [rax + AsyncGenASend.ags_aclose], 0  ; athrow's flag too
    mov qword [rax + AsyncGenASend.ags_sendval], 0 ; resume with None
    mov dword [rax + AsyncGenASend.ags_started], 0 ; athrow's flag
    mov qword [rax + AsyncGenASend.gi_return_value], 0

    ; INCREF the async generator (wrapper holds a ref)
    inc qword [rbx + PyObject.ob_refcnt]

    pop rbx
    leave
    ret
END_FUNC async_gen_iternext



;; ============================================================================
;; gen_dealloc(PyObject *self)
;; Free generator: free frame if still held, DECREF code.
;; ============================================================================

;; ============================================================================
;; gen_dealloc_close(rdi = a generator with a live, suspended frame)
;;
;; gen_close, minus the two arms that leave through the unwinder.  A dealloc
;; is called from arbitrary depth -- including from inside
;; eval_exception_unwind's own release loop -- and unwinding from there
;; abandons a C stack that is in the middle of something.  So a cleanup that
;; raises leaves its exception pending for gen_dealloc to report, the way
;; instance_dealloc reports one from __del__, and nothing jumps.
;; ============================================================================
GDC_GEN   equ 8
GDC_FRAME equ 24            ; + 1 push = 32, 16-aligned
DEF_FUNC_LOCAL gen_dealloc_close, GDC_FRAME
    push rbx
    mov rbx, rdi
    mov [rbp - GDC_GEN], rbx

    mov rdi, [rbx + PyGenObject.gi_frame]
    test rdi, rdi
    jz .gdc_free

    mov rdi, rbx
    lea rsi, [rel exc_GeneratorExit_type]
    call gen_throw
    test edx, edx
    jz .gdc_settle

    ; It yielded instead of finishing.  Python reports that; here it is
    ; recorded rather than raised, because there is nowhere to raise to.
    SET_EXC exc_RuntimeError_type, "generator ignored GeneratorExit"
    jmp .gdc_free

.gdc_settle:
    ; GeneratorExit and StopIteration on the way out are the expected
    ; outcomes; anything else is left pending for the caller to report.
    mov rax, [rel current_exception]
    test rax, rax
    jz .gdc_free
    mov rcx, [rax + PyObject.ob_type]
    lea rdx, [rel exc_GeneratorExit_type]
    cmp rcx, rdx
    je .gdc_swallow
    lea rdx, [rel exc_StopIteration_type]
    cmp rcx, rdx
    jne .gdc_free
.gdc_swallow:
    mov rdi, [rel current_exception]
    mov qword [rel current_exception], 0
    call obj_decref

.gdc_free:
    mov rbx, [rbp - GDC_GEN]
    mov rdi, [rbx + PyGenObject.gi_frame]
    test rdi, rdi
    jz .gdc_done
    mov qword [rbx + PyGenObject.gi_frame], 0
    call frame_free
.gdc_done:
    pop rbx
    leave
    ret
END_FUNC gen_dealloc_close

GD_EXC   equ 8
GD_FRAME equ 16             ; + 2 pushes = 32, 16-aligned
DEF_FUNC gen_dealloc, GD_FRAME
    push rbx
    push r12

    mov rbx, rdi

    ; A generator suspended inside a try/finally has cleanup still to run, and
    ; CPython runs it when the generator is collected.  Doing it here means
    ; running Python inside a dealloc, which needs three things:
    ;
    ;   - a resurrection bump, because the cleanup can take and drop
    ;     references to the generator and would otherwise re-enter this at
    ;     refcount 0;
    ;   - the pending exception saved and put back, because a dealloc runs at
    ;     arbitrary points -- including in the middle of somebody else's raise
    ;     -- and the close both reads and clears current_exception;
    ;   - and nothing that unwinds, which is what gen_dealloc_close is for.
    ;
    ; A frame with instr_ptr 0 has either never started or already finished:
    ; there is nothing suspended in it, and CPython runs no cleanup for one
    ; either.
    mov rdi, [rbx + PyGenObject.gi_frame]
    test rdi, rdi
    jz .no_frame
    cmp qword [rdi + PyFrame.instr_ptr], 0
    je .gd_just_free

    inc qword [rbx + PyObject.ob_refcnt]
    ; The pending exception is held in a register across the cleanup, and
    ; DUNDER_EXC_SAVE only borrows.  raise_exception_obj takes over its
    ; caller's reference rather than adding one, so the global's is often the
    ; only one there is -- and gen_dealloc_close runs arbitrary Python, which
    ; can free anything nothing else holds.  Take a reference for the
    ; register's own copy; .gd_close_done gives it back to the global.
    DUNDER_EXC_SAVE r12
    test r12, r12
    jz .gd_no_pending
    mov rdi, r12
    call obj_incref
.gd_no_pending:
    mov qword [rel current_exception], 0

    ; An async generator with a finalizer is HANDED OVER rather than closed.
    ; Closing it here would run its `await`s with no loop to run them on;
    ; the hook exists so that the loop which started the generator schedules
    ; an aclose of its own, and CPython's gen_finalize makes the same choice.
    ; Only an async generator ever has this field set.
    cmp qword [rbx + PyGenObject.ag_finalizer], 0
    jne .gd_hand_over

    mov rdi, rbx
    call gen_dealloc_close
    jmp .gd_closed

.gd_hand_over:
    sub rsp, 16
    mov [rsp], rbx
    mov rdi, [rbx + PyGenObject.ag_finalizer]
    mov rsi, rsp
    mov edx, 1
    extern obj_call_n
    call obj_call_n
    add rsp, 16
    test rax, rax
    jz .gd_handed              ; it raised; reported below, as a close would be
    DECREF_V rax, rcx
.gd_handed:
    ; Once.  The hook owns whatever it kept, and a second pass through this
    ; dealloc -- after an aclose the hook scheduled, say -- must close rather
    ; than hand the same generator over again.
    mov rdi, [rbx + PyGenObject.ag_finalizer]
    mov qword [rbx + PyGenObject.ag_finalizer], 0
    call obj_decref
.gd_closed:

    ; Anything the cleanup raised is reported and dropped, as an exception
    ; from __del__ is: there is no caller left to hand it to.
    mov rdi, [rel current_exception]
    test rdi, rdi
    jz .gd_close_done
    mov qword [rel current_exception], 0
    ; The full report, as CPython's does: the generator itself, then the
    ; traceback of where inside the cleanup it happened.  One line naming
    ; neither was all this used to print.
    push rdi
    mov rsi, rbx
    extern traceback_print_unraisable
    call traceback_print_unraisable
    pop rdi
    call obj_decref
.gd_close_done:
    ; Hand the reference taken above back to the global.
    mov [rel current_exception], r12
    dec qword [rbx + PyObject.ob_refcnt]
    jnz .gd_resurrected         ; someone kept it: free nothing

.gd_just_free:
    ; Free the frame if the close did not.
    mov rdi, [rbx + PyGenObject.gi_frame]
    test rdi, rdi
    jz .no_frame
    mov qword [rbx + PyGenObject.gi_frame], 0
    call frame_free
.no_frame:

    ; XDECREF gi_return_value (tag-aware)
    mov rdi, [rbx + PyGenObject.gi_return_value]
    V_UNPACK rdi, rsi
    XDECREF_VAL rdi, rsi

    ; DECREF code object
    mov rdi, [rbx + PyGenObject.gi_code]
    call obj_decref

    ; The snapshotted finalizer, if it was never handed over -- a generator
    ; that ran to the end keeps one and is freed without ever consulting it.
    ; Owned since firstiter, and released by nobody until now: five started and
    ; dropped generators took their finalizer's refcount from 3 to 8.
    mov rdi, [rbx + PyGenObject.ag_finalizer]
    test rdi, rdi
    jz .gd_no_final
    mov qword [rbx + PyGenObject.ag_finalizer], 0
    call obj_decref
.gd_no_final:

    ; Free self (GC-aware)
    mov rdi, rbx
    call gc_dealloc

    pop r12
    pop rbx
    leave
    ret

.gd_resurrected:
    ; The cleanup, or the finalizer hook, took a reference to the generator and
    ; still holds it.  Nothing here may be freed -- and it goes back into the
    ; collector's lists, because obj_dealloc untracked it on the way in and a
    ; cycle through a live object the collector cannot see is a leak.
    mov rdi, rbx
    extern gc_track
    call gc_track
    pop r12
    pop rbx
    leave
    ret
END_FUNC gen_dealloc

;; ============================================================================
;; gen_iter_self(PyObject *self) -> self with INCREF
;; tp_iter for generator: return self
;; ============================================================================
DEF_FUNC_BARE gen_iter_self
    inc qword [rdi + PyObject.ob_refcnt]
    mov rax, rdi
    ret
END_FUNC gen_iter_self

;; ============================================================================
;; gen_repr / coro_repr / async_gen_repr (PyObject *self) -> PyStrObject*
;;
;; "<generator object NAME at 0x...>", with the qualified name off the code
;; object -- which is how a generator expression says "<genexpr>" and a
;; generator function says its own name.  All three used to be a fixed string
;; naming only the kind.
;; ============================================================================
GRP_KIND  equ 8
GRP_FRAME equ 24            ; + 1 push = 32, 16-aligned
DEF_FUNC_LOCAL gen_repr_kind, GRP_FRAME
    push rbx
    mov rbx, rdi
    mov [rbp - GRP_KIND], rsi
    mov rax, [rbx + PyGenObject.gi_code]
    test rax, rax
    jz .grp_gi_name
    mov rax, [rax + PyCodeObject.co_qualname]
    test rax, rax
    jnz .grp_have_name
.grp_gi_name:
    mov rax, [rbx + PyGenObject.gi_name]
    test rax, rax
    jz .grp_no_name
.grp_have_name:
    lea rdx, [rax + PyStrObject.data]
    jmp .grp_build
.grp_no_name:
    xor edx, edx
.grp_build:
    mov rdi, rbx
    mov rsi, [rbp - GRP_KIND]
    extern obj_repr_named_at
    call obj_repr_named_at
    pop rbx
    leave
    ret
END_FUNC gen_repr_kind

DEF_FUNC_BARE gen_repr
    lea rsi, [rel gen_repr_str]
    jmp gen_repr_kind
END_FUNC gen_repr

DEF_FUNC_BARE coro_repr
    lea rsi, [rel coro_repr_str]
    jmp gen_repr_kind
END_FUNC coro_repr

DEF_FUNC_BARE async_gen_repr
    lea rsi, [rel async_gen_repr_str]
    jmp gen_repr_kind
END_FUNC async_gen_repr

;; ============================================================================
;; gen_send(PyGenObject *gen, PyObject *value) -> PyObject*
;; Resume generator with a sent value. Returns yielded value or NULL.
;; rdi = generator, rsi = value to send
;; ============================================================================
DEF_FUNC gen_send
    V_UNPACK rsi, rdx           ; sent Value -> (payload, tag)
    ; rdi = generator, rsi = value, edx = value_tag
    push rbx
    push r12
    push r13
    push r14

    mov rbx, rdi               ; rbx = generator
    mov r13, rsi               ; r13 = value to send
    mov r14d, edx              ; r14d = value tag

    ; Check if generator is exhausted
    mov r12, [rbx + PyGenObject.gi_frame]
    test r12, r12
    jz .gs_exhausted

    ; Check if already running
    cmp qword [rbx + PyGenObject.gi_running], 1
    je .gs_error

    ; Mark as running
    mov qword [rbx + PyGenObject.gi_running], 1

    ; INCREF the sent value while its tag is still around, then pack it
    INCREF_VAL r13, r14
    V_PACK r13, r14

    ; Push the sent Value onto the frame's value stack
    FRAME_PUSH_VALUE r12, r13, rax

    ; Resume execution
    mov rdi, r12
    ; A generator is its own execution context, and two globals say so in
    ; different ways.  handled_exception -- what an except block installed --
    ; is swapped between the global and PyFrame.exc_state by eval_frame and
    ; eval_return, so a generator suspended inside a handler carries that
    ; state across the suspension and the caller gets its own back.  Nothing
    ; here has to do anything about it.
    ;
    ; current_exception is the other one: an exception in FLIGHT.  It is
    ; cleared for the duration of the resume so that a NULL result means the
    ; body raised and nothing else, and put back if it did not.
    push rax
    mov rax, [rel current_exception]
    push rax
    mov qword [rel current_exception], 0
    call eval_frame
    pop rcx
    ; If the generator body raised, that exception is the result and must not
    ; be overwritten by the caller's saved one -- gen_iternext was fixed for
    ; this; the identical block here was not, so send() after a raise gave
    ; StopIteration instead of the exception.
    test rax, rax
    jz .gsend_raised
    mov [rel current_exception], rcx
    jmp .gsend_settled
.gsend_raised:
    test rcx, rcx
    jz .gsend_settled
    push rax
    push rdx
    mov rdi, rcx
    call obj_decref
    pop rdx
    pop rax
.gsend_settled:
    add rsp, 8
    V_UNPACK rax, rdx           ; eval_frame returns a Value
    mov r12, rax               ; save return value payload
    mov r13, rdx               ; save return value tag (sent value no longer needed)

    ; Mark as not running
    mov qword [rbx + PyGenObject.gi_running], 0

    ; Check if exhausted
    mov rdi, [rbx + PyGenObject.gi_frame]
    cmp qword [rdi + PyFrame.instr_ptr], 0
    jne .gs_yielded

    ; Exhausted.  Clear before freeing, as gen_iternext does.
    mov qword [rbx + PyGenObject.gi_frame], 0
    call frame_free

    ; Store return value in gi_return_value (for StopIteration.value)
    V_PACK r12, r13
    mov [rbx + PyGenObject.gi_return_value], r12

    ; Return NULL to signal StopIteration
    RET_NULL
    pop r14
    pop r13
    pop r12
    pop rbx
    leave
    V_PACK rax, rdx             ; return one Value
    ret

.gs_yielded:
    mov rax, r12
    mov rdx, r13               ; restore result tag
    pop r14
    pop r13
    pop r12
    pop rbx
    leave
    V_PACK rax, rdx             ; return one Value
    ret

.gs_exhausted:
    RET_NULL
    pop r14
    pop r13
    pop r12
    pop rbx
    leave
    V_PACK rax, rdx             ; return one Value
    ret

.gs_error:
    RET_NULL
    pop r14
    pop r13
    pop r12
    pop rbx
    leave
    V_PACK rax, rdx             ; return one Value
    ret
END_FUNC gen_send

;; ============================================================================
;; gen_throw(PyGenObject *gen, PyObject *exc_type) -> fat value
;; Throw an exception into a generator/coroutine.
;; If gi_frame == NULL → re-raise (generator exhausted).
;; Sets current_exception, pushes dummy value onto frame stack,
;; resumes eval_frame → exception unwind finds handler or propagates.
;; On yield: return yielded value. On exhaustion: return NULL.
;; rdi = generator, rsi = exc_type (PyTypeObject*)
;; ============================================================================
GT_SAVED_EXC equ 24    ; the caller's pending exception, put aside
GT_FRAME equ 40            ; + 3 pushes = 64, 16-aligned
DEF_FUNC gen_throw, GT_FRAME
    push rbx
    push r12
    push r13

    mov rbx, rdi               ; rbx = generator
    mov r12, rsi               ; r12 = exc_type

    ; Check if generator is exhausted
    mov r13, [rbx + PyGenObject.gi_frame]
    test r13, r13
    jz .gt_exhausted

    ; Check if already running
    cmp qword [rbx + PyGenObject.gi_running], 1
    je .gt_error

    ; Mark as running
    mov qword [rbx + PyGenObject.gi_running], 1

    ; Set the thrown exception as current.  The caller's is put aside rather
    ; than released: it belongs to the caller, and DECREFing it here freed an
    ; exception that was still being handled out there.
    mov rax, [rel current_exception]
    mov [rbp - GT_SAVED_EXC], rax
    ; throw() takes either an exception class or an already-built instance.
    ; This always called exc_new on it, so g.throw(ValueError("x")) built an
    ; exception whose type was the *instance* -- and `except ValueError`
    ; inside the generator never matched, so every throw came back out as a
    ; re-raise.
    mov rax, [r12 + PyObject.ob_type]
    extern exc_metatype
    lea rcx, [rel exc_metatype]
    cmp rax, rcx
    je .gt_from_class
    extern user_type_metatype
    lea rcx, [rel user_type_metatype]
    cmp rax, rcx
    je .gt_from_class
    extern type_type
    lea rcx, [rel type_type]
    cmp rax, rcx
    je .gt_from_class

    ; Already an instance: take a reference and use it as-is.
    mov rdi, r12
    call obj_incref
    mov rax, r12
    jmp .gt_have_exc

.gt_from_class:
    mov rdi, r12               ; exc_type
    xor esi, esi               ; no message
    xor edx, edx               ; TAG_NULL
    call exc_new

.gt_have_exc:
    mov [rel current_exception], rax

    ; Push dummy value onto frame stack (eval_frame expects TOS after YIELD_VALUE)
    FRAME_PUSH_NONE r13, rax

    ; Back up instr_ptr by 2 bytes so it points to YIELD_VALUE itself
    ; (not the CACHE entry after it). The exception table covers YIELD_VALUE's
    ; offset but NOT the CACHE entry's offset.
    mov rax, [r13 + PyFrame.instr_ptr]
    sub rax, 2
    mov [r13 + PyFrame.instr_ptr], rax

    ; Set throw_pending so eval_frame resume immediately unwinds
    mov byte [rel throw_pending], 1

    ; Resume execution — eval_frame will see throw_pending and unwind
    mov rdi, r13
    ; The exception being thrown is handed in through current_exception, so
    ; it must survive the resume; the caller's was put aside above and goes
    ; back below, once it is known whether the generator handled it.
    call eval_frame
    V_UNPACK rax, rdx           ; eval_frame returns a Value
    mov r12, rax               ; save result payload
    mov r13, rdx               ; save result tag

    ; Mark as not running
    mov qword [rbx + PyGenObject.gi_running], 0

    ; Check if exhausted
    mov rdi, [rbx + PyGenObject.gi_frame]
    cmp qword [rdi + PyFrame.instr_ptr], 0
    jne .gt_yielded

    ; Exhausted.  If something is still pending, the generator did not
    ; handle the throw and it propagates -- which is what CPython does, and
    ; raising StopIteration over the top of it freed it twice.  Otherwise
    ; the caller's own pending exception goes back.
    cmp qword [rel current_exception], 0
    jne .gt_exhausted_propagating
    mov rcx, [rbp - GT_SAVED_EXC]
    mov [rel current_exception], rcx
.gt_exhausted_propagating:

    ; Exhausted.  Clear before freeing, as gen_iternext does.
    mov rdi, [rbx + PyGenObject.gi_frame]
    mov qword [rbx + PyGenObject.gi_frame], 0
    call frame_free
    V_PACK r12, r13
    mov [rbx + PyGenObject.gi_return_value], r12

    ; Return NULL to signal StopIteration
    RET_NULL
    pop r13
    pop r12
    pop rbx
    leave
    ret

.gt_yielded:
    ; The generator is suspended.  Whatever it is handling went into its
    ; frame's exc_state on the way out of eval_return; what goes back here is
    ; the caller's in-flight exception, put aside before the throw.
    mov rcx, [rbp - GT_SAVED_EXC]
    mov [rel current_exception], rcx
    mov rax, r12
    mov rdx, r13
    pop r13
    pop r12
    pop rbx
    leave
    ret

.gt_exhausted:
    ; Generator is exhausted -- re-raise the exception.
    ;
    ; throw() takes a class OR an already-built instance, and this arm used
    ; to call exc_new on both.  Handed an instance that made an exception
    ; whose exc_type was the INSTANCE, and the next `except ValueError` read
    ; tp_base off it -- a segfault in type_mro_next, several frames away.
    ; The live path above learned this; this one had not.
    mov rax, [r12 + PyObject.ob_type]
    lea rcx, [rel exc_metatype]
    cmp rax, rcx
    je .gt_exh_from_class
    lea rcx, [rel user_type_metatype]
    cmp rax, rcx
    je .gt_exh_from_class
    lea rcx, [rel type_type]
    cmp rax, rcx
    je .gt_exh_from_class
    ; Already an instance: raise it as it stands.
    mov rdi, r12
    call obj_incref
    mov rdi, r12
    jmp .gt_exh_raise
.gt_exh_from_class:
    mov rdi, r12               ; exc_type
    xor esi, esi
    xor edx, edx
    call exc_new
    mov rdi, rax
.gt_exh_raise:
    call raise_exception_obj
    RET_NULL
    pop r13
    pop r12
    pop rbx
    leave
    ret

.gt_error:
    RET_NULL
    pop r13
    pop r12
    pop rbx
    leave
    ret
END_FUNC gen_throw

;; ============================================================================
;; gen_close(PyGenObject *gen) -> None
;; Close the generator by marking it as exhausted.
;; rdi = generator
;; ============================================================================
GC_GEN   equ 8
GC_FRAME equ 24            ; + 1 push = 32, 16-aligned
DEF_FUNC gen_close, GC_FRAME
    push rbx
    mov rbx, rdi
    mov [rbp - GC_GEN], rbx

    mov rdi, [rbx + PyGenObject.gi_frame]
    test rdi, rdi
    jz .gc_done                     ; already exhausted: nothing to unwind

    ; Throw GeneratorExit in, so that finally blocks and context managers
    ; run.  This used to free the frame outright, so a generator's finally
    ; simply never executed.
    extern exc_GeneratorExit_type
    mov rdi, rbx
    lea rsi, [rel exc_GeneratorExit_type]
    call gen_throw
    test edx, edx
    jnz .gc_ignored_exit

    ; The generator finished.  GeneratorExit and StopIteration coming back
    ; out are the expected outcomes and are swallowed; anything else is a
    ; real error from the cleanup and propagates.
    mov rax, [rel current_exception]
    test rax, rax
    jz .gc_done
    mov rcx, [rax + PyObject.ob_type]
    lea rdx, [rel exc_GeneratorExit_type]
    cmp rcx, rdx
    je .gc_swallow
    extern exc_StopIteration_type
    lea rdx, [rel exc_StopIteration_type]
    cmp rcx, rdx
    jne .gc_propagate

.gc_swallow:
    mov rdi, [rel current_exception]
    mov qword [rel current_exception], 0
    call obj_decref

.gc_done:
    ; Make sure the frame is gone even if the generator never started.
    mov rbx, [rbp - GC_GEN]
    mov rdi, [rbx + PyGenObject.gi_frame]
    test rdi, rdi
    jz .gc_no_frame
    mov qword [rbx + PyGenObject.gi_frame], 0
    call frame_free
.gc_no_frame:

    lea rax, [rel none_singleton]
    mov rdi, rax
    push rax
    call obj_incref
    pop rax
    mov edx, TAG_PTR             ; None is a heap pointer

    pop rbx
    leave
    ret

.gc_ignored_exit:
    ; It yielded instead of finishing, which Python reports.
    extern exc_RuntimeError_type
    RAISE exc_RuntimeError_type, "generator ignored GeneratorExit"

.gc_propagate:
    pop rbx
    leave
    mov [rel eval_saved_r13], r13
    jmp eval_exception_unwind
END_FUNC gen_close

;; ============================================================================
;; gen_getattr(PyGenObject *self, PyObject *name) -> rax = Value
;; Attribute lookup for generators: handles send, close, throw
;; ============================================================================
DEF_FUNC gen_getattr
    push rbx
    push r12

    mov rbx, rdi               ; self
    mov r12, rsi               ; name

    lea rdi, [r12 + PyStrObject.data]

    ; Check "send"
    CSTRING rsi, "send"
    call ap_strcmp
    test eax, eax
    jz .gga_send

    ; Check "close"
    lea rdi, [r12 + PyStrObject.data]
    CSTRING rsi, "close"
    call ap_strcmp
    test eax, eax
    jz .gga_close

    ; Check "throw"
    lea rdi, [r12 + PyStrObject.data]
    CSTRING rsi, "throw"
    call ap_strcmp
    test eax, eax
    jz .gga_throw

    ; Not found
    RET_NULL
    pop r12
    pop rbx
    leave
    V_PACK rax, rdx             ; return one Value
    ret

.gga_send:
    ; A *bound* method: returning the raw builtin left self to LOAD_ATTR's
    ; method fast path, so `f = gen.send; f(None)` and `gen.send(*args)` both
    ; called it with no generator.
    call _get_gen_send_builtin
    mov rdi, rax
    mov rsi, rbx
    extern method_new
    call method_new
    mov edx, TAG_PTR
    pop r12
    pop rbx
    leave
    V_PACK rax, rdx             ; return one Value
    ret

.gga_close:
    ; A *bound* method: returning the raw builtin left self to LOAD_ATTR's
    ; method fast path, so `f = gen.send; f(None)` and `gen.send(*args)` both
    ; called it with no generator.
    call _get_gen_close_builtin
    mov rdi, rax
    mov rsi, rbx
    extern method_new
    call method_new
    mov edx, TAG_PTR
    pop r12
    pop rbx
    leave
    V_PACK rax, rdx             ; return one Value
    ret

.gga_throw:
    ; A *bound* method: returning the raw builtin left self to LOAD_ATTR's
    ; method fast path, so `f = gen.send; f(None)` and `gen.send(*args)` both
    ; called it with no generator.
    call _get_gen_throw_builtin
    mov rdi, rax
    mov rsi, rbx
    extern method_new
    call method_new
    mov edx, TAG_PTR
    pop r12
    pop rbx
    leave
    V_PACK rax, rdx             ; return one Value
    ret
END_FUNC gen_getattr

;; ============================================================================
;; The generator protocol, by name.
;;
;; gen_type had no tp_dict at all, so `hasattr(gen, "__next__")` was False and
;; `it.__next__` an AttributeError -- and `_counter = _count(1).__next__` is
;; line 838 of CPython's threading.py, which is as far as it got.  The four
;; getters are what a repr and the async machinery read.
;; ============================================================================
%macro DEF_GEN_GETTER 2         ; %1 = the exposed name, %2 = the field
DEF_FUNC gen_get_%1
    mov rax, [rdi + PyGenObject.%2]
    test rax, rax
    jnz %%have
    LOAD_NONE rax
    leave
    ret
%%have:
    INCREF_V rax, rdx
    leave
    ret
END_FUNC gen_get_%1
%endmacro
DEF_GEN_GETTER name,    gi_name
DEF_GEN_GETTER code,    gi_code

;; ============================================================================
;; gen_get_frame(rdi = the generator) -> rax = its frame object, or None
;;
;; gi_frame, and cr_frame under the other spelling.  It used to be left out
;; deliberately: "a PyFrame is pooled and recycled and is not an object with a
;; type, so there is nothing to hand back."  That stopped being true when
;; frameobj_for arrived -- it hands out an OWNED frame object for a live
;; pooled PyFrame, and sys._getframe has been built on it ever since.
;;
;; Its absence is not only a missing name.  frame.clear() on a suspended
;; generator is the one thing that closes one from the outside, and without
;; gi_frame there is no way to reach the frame to call it.
;; ============================================================================
DEF_FUNC gen_get_frame
    mov rax, [rdi + PyGenObject.gi_frame]
    test rax, rax
    jz .ggf_none
    mov rdi, rax
    extern frameobj_for
    call frameobj_for
    test rax, rax
    jz .ggf_none
    leave
    ret
.ggf_none:
    LOAD_NONE rax
    leave
    ret
END_FUNC gen_get_frame

;; gi_running is a plain flag, not an object.
DEF_FUNC gen_get_running
    mov rax, [rdi + PyGenObject.gi_running]
    test rax, rax
    jz .ggr_false
    lea rax, [rel bool_true]
    jmp .ggr_out
.ggr_false:
    lea rax, [rel bool_false]
.ggr_out:
    INCREF rax
    leave
    ret
END_FUNC gen_get_running

;; ============================================================================
;; coro_getattr(PyGenObject *self, PyObject *name) -> rax = Value
;; Attribute lookup for coroutines: send, close, throw, cr_await, cr_running
;; ============================================================================
DEF_FUNC coro_getattr
    push rbx
    push r12

    mov rbx, rdi               ; self
    mov r12, rsi               ; name

    lea rdi, [r12 + PyStrObject.data]

    ; Check "send"
    CSTRING rsi, "send"
    call ap_strcmp
    test eax, eax
    jz .cga_send

    lea rdi, [r12 + PyStrObject.data]
    CSTRING rsi, "close"
    call ap_strcmp
    test eax, eax
    jz .cga_close

    lea rdi, [r12 + PyStrObject.data]
    CSTRING rsi, "throw"
    call ap_strcmp
    test eax, eax
    jz .cga_throw

    lea rdi, [r12 + PyStrObject.data]
    CSTRING rsi, "cr_running"
    call ap_strcmp
    test eax, eax
    jz .cga_cr_running

    ; Not found
    RET_NULL
    pop r12
    pop rbx
    leave
    V_PACK rax, rdx             ; return one Value
    ret

.cga_send:
    call _get_gen_send_builtin
    mov rdi, rax
    call obj_incref
    mov edx, TAG_PTR
    pop r12
    pop rbx
    leave
    V_PACK rax, rdx             ; return one Value
    ret

.cga_close:
    call _get_gen_close_builtin
    mov rdi, rax
    call obj_incref
    mov edx, TAG_PTR
    pop r12
    pop rbx
    leave
    V_PACK rax, rdx             ; return one Value
    ret

.cga_throw:
    call _get_gen_throw_builtin
    mov rdi, rax
    call obj_incref
    mov edx, TAG_PTR
    pop r12
    pop rbx
    leave
    V_PACK rax, rdx             ; return one Value
    ret

.cga_cr_running:
    ; Return bool for cr_running
    mov rax, [rbx + PyGenObject.gi_running]
    RET_BOOL_RAX
    pop r12
    pop rbx
    leave
    V_PACK rax, rdx             ; return one Value
    ret
END_FUNC coro_getattr

;; ============================================================================
;; async_gen_getattr(PyGenObject *self, PyObject *name) -> rax = Value
;; Attribute lookup for async generators: asend, aclose, athrow
;; Also supports send, close, throw (same underlying operations)
;; ============================================================================
DEF_FUNC async_gen_getattr
    push rbx
    push r12

    mov rbx, rdi
    mov r12, rsi

    lea rdi, [r12 + PyStrObject.data]
    CSTRING rsi, "asend"
    call ap_strcmp
    test eax, eax
    jz .aga_asend

    lea rdi, [r12 + PyStrObject.data]
    CSTRING rsi, "send"
    call ap_strcmp
    test eax, eax
    jz .aga_send

    lea rdi, [r12 + PyStrObject.data]
    CSTRING rsi, "aclose"
    call ap_strcmp
    test eax, eax
    jz .aga_aclose

    lea rdi, [r12 + PyStrObject.data]
    CSTRING rsi, "close"
    call ap_strcmp
    test eax, eax
    jz .aga_close

    lea rdi, [r12 + PyStrObject.data]
    CSTRING rsi, "athrow"
    call ap_strcmp
    test eax, eax
    jz .aga_athrow

    lea rdi, [r12 + PyStrObject.data]
    CSTRING rsi, "throw"
    call ap_strcmp
    test eax, eax
    jz .aga_throw

    RET_NULL
    pop r12
    pop rbx
    leave
    V_PACK rax, rdx             ; return one Value
    ret

.aga_aclose:
    ; aclose is NOT close, for the reason asend is not send: `await` needs an
    ; awaitable, and close answers None.
    call _get_agen_aclose_builtin
    mov rdi, rax
    mov rsi, rbx
    call method_new
    mov edx, TAG_PTR
    pop r12
    pop rbx
    leave
    V_PACK rax, rdx             ; return one Value
    ret

.aga_athrow:
    call _get_agen_athrow_builtin
    mov rdi, rax
    mov rsi, rbx
    call method_new
    mov edx, TAG_PTR
    pop r12
    pop rbx
    leave
    V_PACK rax, rdx             ; return one Value
    ret

.aga_asend:
    ; asend is NOT send: it answers the awaitable, and `send` resumes.  They
    ; shared an arm, so `await agen.asend(v)` awaited the wrapped yield the
    ; sync path had already produced and v never reached the generator.
    call _get_agen_asend_builtin
    mov rdi, rax
    mov rsi, rbx
    call method_new
    mov edx, TAG_PTR
    pop r12
    pop rbx
    leave
    V_PACK rax, rdx             ; return one Value
    ret

.aga_send:
    ; A *bound* method: returning the raw builtin left self to LOAD_ATTR's
    ; method fast path, so `f = gen.send; f(None)` and `gen.send(*args)` both
    ; called it with no generator.
    call _get_gen_send_builtin
    mov rdi, rax
    mov rsi, rbx
    extern method_new
    call method_new
    mov edx, TAG_PTR
    pop r12
    pop rbx
    leave
    V_PACK rax, rdx             ; return one Value
    ret

.aga_close:
    ; A *bound* method: returning the raw builtin left self to LOAD_ATTR's
    ; method fast path, so `f = gen.send; f(None)` and `gen.send(*args)` both
    ; called it with no generator.
    call _get_gen_close_builtin
    mov rdi, rax
    mov rsi, rbx
    extern method_new
    call method_new
    mov edx, TAG_PTR
    pop r12
    pop rbx
    leave
    V_PACK rax, rdx             ; return one Value
    ret

.aga_throw:
    ; A *bound* method: returning the raw builtin left self to LOAD_ATTR's
    ; method fast path, so `f = gen.send; f(None)` and `gen.send(*args)` both
    ; called it with no generator.
    call _get_gen_throw_builtin
    mov rdi, rax
    mov rsi, rbx
    extern method_new
    call method_new
    mov edx, TAG_PTR
    pop r12
    pop rbx
    leave
    V_PACK rax, rdx             ; return one Value
    ret
END_FUNC async_gen_getattr

;; ============================================================================
;; Builtin implementations for gen.send(), gen.close(), gen.throw()
;; These follow the builtin calling convention: (args, nargs)
;; args[0] = self (generator), remaining args follow
;; ============================================================================

;; _gen_send_impl(args, nargs) — gen.send(value)
global _gen_send_impl
DEF_FUNC _gen_send_impl, 8            ; 1 pushes, so rsp is 16-aligned
    push rbx

    cmp rsi, 2
    jne .gsi_error

    mov rax, rdi               ; save args ptr
    mov rbx, [rax]            ; rbx = gen (save for return value access)
    mov rsi, [rax + 8]       ; value = args[1].payload
    V_UNPACK rsi, rdx       ; args[1]
    mov rdi, rbx              ; gen = args[0].payload
    V_PACK rsi, rdx           ; gen_send takes a Value
    call gen_send
    V_UNPACK rax, rdx         ; gen_send returns a Value
    test edx, edx             ; check tag, not payload (SmallInt-0 vs NULL)
    jnz .gsi_ret

    ; A NULL result means exhaustion only when nothing is pending; the
    ; generator body may have raised, and turning that into StopIteration
    ; swallowed it.
    cmp qword [rel current_exception], 0
    jne .gsi_propagate

    ; StopIteration — raise with actual return value from generator
    lea rdi, [rel exc_StopIteration_type]
    mov rsi, [rbx + PyGenObject.gi_return_value]   ; already a Value
    test rsi, rsi
    jz .gsi_no_val
    ; A generator that returns None raises a bare StopIteration in CPython,
    ; so str(e) is "" rather than "None".
    lea rax, [rel none_singleton]
    cmp rsi, rax
    jne .gsi_have_val
.gsi_no_val:
    xor esi, esi
.gsi_have_val:
    call exc_new
    mov rdi, rax
    call raise_exception_obj

.gsi_propagate:
    extern eval_exception_unwind
    leave
    jmp eval_exception_unwind

.gsi_ret:
    pop rbx
    leave
    V_PACK rax, rdx             ; builtins return one Value
    ret

.gsi_error:
    RAISE exc_TypeError_type, "send() takes exactly one argument"
END_FUNC _gen_send_impl


;; _gen_close_impl(args, nargs) — gen.close()
global _gen_close_impl
DEF_FUNC _gen_close_impl
    mov rdi, [rdi]             ; gen = args[0]
    call gen_close
    leave
    V_PACK rax, rdx             ; builtins return one Value
    ret
END_FUNC _gen_close_impl

;; _gen_throw_impl(args, nargs) — gen.throw(exc_type)
global _gen_throw_impl
DEF_FUNC _gen_throw_impl, 8            ; 1 pushes, so rsp is 16-aligned
    push rbx

    cmp rsi, 2
    jne .gti_error

    mov rax, rdi               ; save args ptr
    mov rbx, [rax]            ; rbx = gen
    mov rsi, [rax + 8]       ; exc_type = args[1].payload
    mov rdi, rbx              ; gen = args[0].payload
    call gen_throw
    test edx, edx
    jnz .gti_ret

    ; The generator did not handle it: in Python the exception propagates
    ; out of throw().  Raising StopIteration instead also DECREF'd the
    ; still-pending exception a second time, which is where the double free
    ; came from.
    cmp qword [rel current_exception], 0
    jne .gti_propagate

    ; Exhausted — raise StopIteration with return value
    lea rdi, [rel exc_StopIteration_type]
    mov rsi, [rbx + PyGenObject.gi_return_value]   ; already a Value
    test rsi, rsi
    jz .gti_no_val
    ; A generator that returns None raises a BARE StopIteration in CPython --
    ; args is (), so str(e) is "" and the traceback says "StopIteration" and
    ; not "StopIteration: None".  gi_return_value holds the None singleton
    ; for such a generator, which is not the same as holding nothing.
    lea rax, [rel none_singleton]
    cmp rsi, rax
    jne .gti_have_val
    xor esi, esi
.gti_no_val:
    xor esi, esi
.gti_have_val:
    call exc_new
    mov rdi, rax
    call raise_exception_obj

.gti_ret:
    pop rbx
    leave
    V_PACK rax, rdx             ; builtins return one Value
    ret

.gti_propagate:
    extern eval_exception_unwind
    extern eval_saved_r13
    pop rbx
    leave
    mov [rel eval_saved_r13], r13
    jmp eval_exception_unwind

.gti_error:
    RAISE exc_TypeError_type, "throw() takes exactly one argument"
END_FUNC _gen_throw_impl

;; Lazy-init helpers for gen method builtins
;; ============================================================================

DEF_FUNC_LOCAL _get_gen_send_builtin
    mov rax, [rel _gen_send_cache]
    test rax, rax
    jnz .ret
    lea rdi, [rel _gen_send_impl]
    CSTRING rsi, "send"
    call builtin_func_new
    mov [rel _gen_send_cache], rax
.ret:
    leave
    ret
END_FUNC _get_gen_send_builtin

DEF_FUNC_LOCAL _get_gen_close_builtin
    mov rax, [rel _gen_close_cache]
    test rax, rax
    jnz .ret
    lea rdi, [rel _gen_close_impl]
    CSTRING rsi, "close"
    call builtin_func_new
    mov [rel _gen_close_cache], rax
.ret:
    leave
    ret
END_FUNC _get_gen_close_builtin

DEF_FUNC_LOCAL _get_gen_throw_builtin
    mov rax, [rel _gen_throw_cache]
    test rax, rax
    jnz .ret
    lea rdi, [rel _gen_throw_impl]
    CSTRING rsi, "throw"
    call builtin_func_new
    mov [rel _gen_throw_cache], rax
.ret:
    leave
    ret
END_FUNC _get_gen_throw_builtin

;; ============================================================================
;; Data section
;; ============================================================================
section .data

gen_name_str:       db "generator", 0
gen_repr_str:       db "generator object", 0
coro_name_str:      db "coroutine", 0
coro_repr_str:      db "coroutine object", 0
async_gen_name_str: db "async_generator", 0
async_gen_repr_str: db "async_generator object", 0

; Cached builtin singletons for gen methods
align 8
_gen_send_cache: dq 0
_gen_close_cache: dq 0
_gen_throw_cache: dq 0

align 8
global gen_type
gen_type:
    dq 1                        ; ob_refcnt (immortal)
    dq type_type                ; ob_type
    dq gen_name_str             ; tp_name
    dq PyGenObject_size         ; tp_basicsize
    dq gen_dealloc              ; tp_dealloc
    dq gen_repr                 ; tp_repr
    dq gen_repr                 ; tp_str
    dq 0                        ; tp_hash
    dq 0                        ; tp_call
    dq gen_getattr              ; tp_getattr (.send, .close, .throw)
    dq 0                        ; tp_setattr
    dq 0                        ; tp_richcompare
    dq gen_iter_self            ; tp_iter (return self)
    dq gen_iternext             ; tp_iternext
    dq 0                        ; tp_init
    dq 0                        ; tp_new
    dq 0                        ; tp_as_number
    dq 0                        ; tp_as_sequence
    dq 0                        ; tp_as_mapping
    dq 0                        ; tp_base
    dq 0                        ; tp_dict
    dq 0                        ; tp_mro
    dq TYPE_FLAG_HAVE_GC                        ; tp_flags
    dq 0                        ; tp_bases
    dq gen_traverse                        ; tp_traverse
    dq gen_clear                        ; tp_clear
    dq 0      ; tp_dictoffset
    dq 0                        ; tp_tailslots
    dq 0                        ; tp_as_buffer

align 8
global coro_type
coro_type:
    dq 1                        ; ob_refcnt (immortal)
    dq type_type                ; ob_type
    dq coro_name_str            ; tp_name
    dq PyGenObject_size         ; tp_basicsize
    dq gen_dealloc              ; tp_dealloc
    dq coro_repr                ; tp_repr
    dq coro_repr                ; tp_str
    dq 0                        ; tp_hash
    dq 0                        ; tp_call
    dq coro_getattr             ; tp_getattr
    dq 0                        ; tp_setattr
    dq 0                        ; tp_richcompare
    dq gen_iter_self            ; tp_iter (return self)
    dq gen_iternext             ; tp_iternext
    dq 0                        ; tp_init
    dq 0                        ; tp_new
    dq 0                        ; tp_as_number
    dq 0                        ; tp_as_sequence
    dq 0                        ; tp_as_mapping
    dq 0                        ; tp_base
    dq 0                        ; tp_dict
    dq 0                        ; tp_mro
    dq TYPE_FLAG_HAVE_GC                        ; tp_flags
    dq 0                        ; tp_bases
    dq gen_traverse                        ; tp_traverse
    dq gen_clear                        ; tp_clear
    dq 0      ; tp_dictoffset
    dq 0                        ; tp_tailslots
    dq 0                        ; tp_as_buffer

align 8
global async_gen_type
async_gen_type:
    dq 1                        ; ob_refcnt (immortal)
    dq type_type                ; ob_type
    dq async_gen_name_str       ; tp_name
    dq PyGenObject_size         ; tp_basicsize
    dq gen_dealloc              ; tp_dealloc
    dq async_gen_repr           ; tp_repr
    dq async_gen_repr           ; tp_str
    dq 0                        ; tp_hash
    dq 0                        ; tp_call
    dq async_gen_getattr        ; tp_getattr
    dq 0                        ; tp_setattr
    dq 0                        ; tp_richcompare
    dq gen_iter_self            ; tp_iter (return self)
    dq async_gen_iternext       ; tp_iternext
    dq 0                        ; tp_init
    dq 0                        ; tp_new
    dq 0                        ; tp_as_number
    dq 0                        ; tp_as_sequence
    dq 0                        ; tp_as_mapping
    dq 0                        ; tp_base
    dq 0                        ; tp_dict
    dq 0                        ; tp_mro
    dq TYPE_FLAG_HAVE_GC                        ; tp_flags
    dq 0                        ; tp_bases
    dq gen_traverse                        ; tp_traverse
    dq gen_clear                        ; tp_clear
    dq 0      ; tp_dictoffset
    dq 0                        ; tp_tailslots
    dq 0                        ; tp_as_buffer

ags_name_str: db "async_generator_asend", 0

align 8
agw_name_str: db "async_generator_wrapped_value", 0
align 8
global async_gen_wrapped_type
async_gen_wrapped_type:
    dq 1                        ; ob_refcnt (immortal)
    dq type_type                ; ob_type
    dq agw_name_str             ; tp_name
    dq AsyncGenWrapped_size     ; tp_basicsize
    dq agw_dealloc              ; tp_dealloc
    times 24 dq 0               ; the rest: this box is never used as a value

align 8
global async_gen_asend_type
async_gen_asend_type:
    dq 1                        ; ob_refcnt (immortal)
    dq type_type                ; ob_type
    dq ags_name_str             ; tp_name
    dq AsyncGenASend_size       ; tp_basicsize
    dq ags_dealloc              ; tp_dealloc
    dq 0                        ; tp_repr
    dq 0                        ; tp_str
    dq 0                        ; tp_hash
    dq 0                        ; tp_call
    dq 0                        ; tp_getattr
    dq 0                        ; tp_setattr
    dq 0                        ; tp_richcompare
    dq ags_iter_self            ; tp_iter (return self)
    dq ags_iternext             ; tp_iternext
    dq 0                        ; tp_init
    dq 0                        ; tp_new
    dq 0                        ; tp_as_number
    dq 0                        ; tp_as_sequence
    dq 0                        ; tp_as_mapping
    dq 0                        ; tp_base
    dq 0                        ; tp_dict
    dq 0                        ; tp_mro
    dq TYPE_FLAG_AWAITABLE      ; tp_flags -- tp_iter IS am_await here
    dq 0                        ; tp_bases
    dq 0                        ; tp_traverse
    dq 0                        ; tp_clear
    dq 0 ; tp_dictoffset
    dq 0                        ; tp_tailslots
    dq 0                        ; tp_as_buffer

agt_name_str: db "async_generator_athrow", 0
align 8
global async_gen_athrow_type
async_gen_athrow_type:
    dq 1                        ; ob_refcnt (immortal)
    dq type_type                ; ob_type
    dq agt_name_str             ; tp_name
    dq AsyncGenASend_size       ; tp_basicsize -- the same box as asend
    dq ags_dealloc              ; tp_dealloc
    dq 0                        ; tp_repr
    dq 0                        ; tp_str
    dq 0                        ; tp_hash
    dq 0                        ; tp_call
    dq 0                        ; tp_getattr
    dq 0                        ; tp_setattr
    dq 0                        ; tp_richcompare
    dq ags_iter_self            ; tp_iter (return self)
    dq agt_iternext             ; tp_iternext
    dq 0                        ; tp_init
    dq 0                        ; tp_new
    dq 0                        ; tp_as_number
    dq 0                        ; tp_as_sequence
    dq 0                        ; tp_as_mapping
    dq 0                        ; tp_base
    dq 0                        ; tp_dict
    dq 0                        ; tp_mro
    dq TYPE_FLAG_AWAITABLE      ; tp_flags -- tp_iter IS am_await here
    dq 0                        ; tp_bases
    dq 0                        ; tp_traverse
    dq 0                        ; tp_clear
    dq 0 ; tp_dictoffset
    dq 0                        ; tp_tailslots
    dq 0                        ; tp_as_buffer

section .text

;; ============================================================================
;; GC traverse and clear.  These lived in gc.asm, which left the collector
;; holding the reference graph of every type in the system; a type's own
;; file is the only place that knows which of its fields are owned.
;; ============================================================================

;; ============================================================================
;; ---- gen_traverse / gen_clear ----
;; ============================================================================
DEF_FUNC gen_traverse, 8        ; rsp 16-aligned at the call the macros below expand to
    push rbx
    push r12
    push r13

    mov rbx, rdi

    ; Visit code
    mov rdi, [rbx + PyGenObject.gi_code]
    VISIT_PTR rdi

    ; Visit name
    mov rdi, [rbx + PyGenObject.gi_name]
    VISIT_PTR rdi

    ; Visit return value (fat)
    mov rdi, [rbx + PyGenObject.gi_return_value]

    VISIT_V rdi, rsi

    ; The finalizer an async generator snapshotted at firstiter, which the
    ; generator OWNS.  An event loop's finalizer is a bound method of the loop,
    ; and the loop holds the generators started under it -- so this edge closes
    ; a cycle, and without it nothing could break one.
    mov rdi, [rbx + PyGenObject.ag_finalizer]
    VISIT_PTR rdi

    ; Traverse frame localsplus if frame exists
    mov r12, [rbx + PyGenObject.gi_frame]
    test r12, r12
    jz .done

    ; Get nlocalsplus from code object
    mov rax, [rbx + PyGenObject.gi_code]
    mov r13d, [rax + PyCodeObject.co_nlocalsplus]
    test r13d, r13d
    jz .visit_stack

    lea r12, [r12 + PyFrame.localsplus]  ; start of the Value array
.frame_loop:
    dec r13d
    mov rdi, [r12 + r13*8]
    VISIT_V rdi, rsi
    test r13d, r13d
    jnz .frame_loop

.visit_stack:
    ; And the frame's VALUE STACK, which this stopped short of: a suspended
    ; generator's live values are exactly there -- the iterator a `for` was
    ; walking above all -- so a cycle through one was invisible to the
    ; collector.  Only a suspended frame has a meaningful stack_ptr; see
    ; frame_free for why instr_ptr is the test.
    mov r12, [rbx + PyGenObject.gi_frame]
    test r12, r12
    jz .done
    cmp qword [r12 + PyFrame.instr_ptr], 0
    je .done
    ; ...and only while it is SUSPENDED.  stack_ptr is written by
    ; YIELD_VALUE and by nothing else, so in a running generator it records
    ; the depth of the previous suspension: slots already popped and
    ; released.  Visiting those subtracts gc_refs for references that no
    ; longer exist, which can make a live object look unreachable.  A
    ; running generator's stack needs no visiting anyway -- it holds owned
    ; references that no tp_traverse accounts for, which is exactly what
    ; makes the interpreter stack a root.
    cmp qword [rbx + PyGenObject.gi_running], 0
    jne .done
    mov r13, [r12 + PyFrame.stack_ptr]
    test r13, r13
    jz .done
    mov r12, [r12 + PyFrame.stack_base]
.stack_visit_loop:
    cmp r13, r12
    jbe .done
    sub r13, 8
    mov rdi, [r13]
    VISIT_V rdi, rsi
    jmp .stack_visit_loop

.done:
    pop r13
    pop r12
    pop rbx
    leave
    ret
END_FUNC gen_traverse

DEF_FUNC gen_clear, 8            ; 1 pushes, so rsp is 16-aligned
    push rbx
    mov rbx, rdi

    ; The snapshotted finalizer, which gen_traverse hands over: whatever this
    ; breaks the cycle through has to be released here too.
    mov rdi, [rbx + PyGenObject.ag_finalizer]
    mov qword [rbx + PyGenObject.ag_finalizer], 0
    test rdi, rdi
    jz .gc_no_final
    call obj_decref
.gc_no_final:

    ; Clear return value
    mov rdi, [rbx + PyGenObject.gi_return_value]
    V_UNPACK rdi, rsi
    mov qword [rbx + PyGenObject.gi_return_value], 0
    XDECREF_VAL rdi, rsi

    ; Free frame if held (frame_free DECREFs localsplus)
    mov rdi, [rbx + PyGenObject.gi_frame]
    mov qword [rbx + PyGenObject.gi_frame], 0
    test rdi, rdi
    jz .done
    call frame_free

.done:
    pop rbx
    leave
    ret
END_FUNC gen_clear

section .rodata
gd_ignored_msg: db "Exception ignored in: generator cleanup", 10, 0
gd_ignored_len  equ $ - gd_ignored_msg - 1

section .text

;; ============================================================================
;; async_gen_dunder_aiter(rdi = args Value[], rsi = nargs)
;;   -> (rax = args[0], rdx = TAG_PTR) -- an async generator is its own
;;      async iterator
;;
;; CPython's async_generator carries __aiter__ and __anext__ by name, and
;; aiter()/anext() and `async for` all ask for them.  This type had a getattr
;; of its own and no tp_dict at all, so `hasattr(g, "__aiter__")` was False
;; and aiter(g) refused a genuine async generator.
;; ============================================================================
global async_gen_dunder_aiter
DEF_FUNC async_gen_dunder_aiter
    test rsi, rsi
    jz .agda_error
    mov rax, [rdi]
    push rax
    mov rdi, rax
    call obj_incref
    pop rax
    mov edx, TAG_PTR
    leave
    V_PACK rax, rdx
    ret
.agda_error:
    RAISE exc_TypeError_type, "__aiter__() takes exactly one argument"
END_FUNC async_gen_dunder_aiter

;; ============================================================================
;; async_gen_dunder_anext(rdi = args Value[], rsi = nargs)
;;   -> the awaitable the next value comes from, as tp_iternext gives it
;; ============================================================================
global async_gen_dunder_anext
DEF_FUNC async_gen_dunder_anext
    test rsi, rsi
    jz .agdn_error
    mov rdi, [rdi]
    call async_gen_iternext
    leave
    V_PACK rax, rdx
    ret
.agdn_error:
    RAISE exc_TypeError_type, "__anext__() takes exactly one argument"
END_FUNC async_gen_dunder_anext
