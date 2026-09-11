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
;; async_gen_iternext(PyGenObject *self) -> fat value (AsyncGenASend wrapper)
;; Called by GET_ANEXT. Creates an AsyncGenASend wrapper that, when iterated
;; by SEND, actually resumes the async generator.
;; rdi = async generator
;; Returns: (rax=AsyncGenASend*, edx=TAG_PTR)
;; ============================================================================
DEF_FUNC async_gen_iternext, 8            ; 1 pushes, so rsp is 16-aligned
    push rbx
    mov rbx, rdi               ; rbx = async generator

    ; Allocate AsyncGenASend wrapper
    mov edi, AsyncGenASend_size
    call ap_malloc
    ; rax = wrapper

    mov qword [rax + PyObject.ob_refcnt], 1
    lea rcx, [rel async_gen_asend_type]
    mov [rax + PyObject.ob_type], rcx
    mov [rax + AsyncGenASend.ags_gen], rbx
    mov dword [rax + AsyncGenASend.ags_state], 0   ; initial
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
;; ags_iternext(AsyncGenASend *self) -> one Value, or 0
;; Called by SEND loop. Drives the async generator.
;;
;; State machine:
;;   0 (initial)  → resume async gen, return yielded value, go to state 1
;;   1 (yielded)  → return NULL (SEND sees exhausted, reads gi_return_value)
;;   2 (closed)   → raise StopAsyncIteration
;;
;; rdi = AsyncGenASend wrapper
;; Returns: a Value in rax, or 0 for exhaustion.  tp_iternext's contract is one
;; Value; every producer packs and every consumer unpacks exactly once.
;; ============================================================================
DEF_FUNC ags_iternext
    push rbx
    push r12

    mov rbx, rdi               ; rbx = wrapper

    ; Check state
    mov eax, [rbx + AsyncGenASend.ags_state]
    cmp eax, 1
    je .agsi_second_call
    cmp eax, 2
    jae .agsi_closed

    ; State 0: initial — resume the async generator
    mov r12, [rbx + AsyncGenASend.ags_gen] ; r12 = async generator

    ; Check if generator is exhausted
    mov rdi, [r12 + PyGenObject.gi_frame]
    test rdi, rdi
    jz .agsi_gen_exhausted

    ; Check if already running
    cmp qword [r12 + PyGenObject.gi_running], 1
    je .agsi_error

    ; Mark as running
    mov qword [r12 + PyGenObject.gi_running], 1

    ; Push the sent value onto the generator's frame stack.  None unless
    ; asend.send(v) put one there, in which case the reference it took moves
    ; onto the frame with it.
    mov rdi, [r12 + PyGenObject.gi_frame]
    mov rax, [rbx + AsyncGenASend.ags_sendval]
    test rax, rax
    jz .agsi_send_none
    mov qword [rbx + AsyncGenASend.ags_sendval], 0
    FRAME_PUSH_VALUE rdi, rax, rcx
    jmp .agsi_sent
.agsi_send_none:
    FRAME_PUSH_NONE rdi, rax
.agsi_sent:

    ; Resume execution of the async generator
    mov rdi, [r12 + PyGenObject.gi_frame]
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
    ; An exception the async generator body raised is the result, and
    ; restoring over it ended the iteration silently.
    test rax, rax
    jz .agsend_raised
    mov [rel current_exception], rcx
    jmp .agsend_settled
.agsend_raised:
    test rcx, rcx
    jz .agsi_body_raised
    push rax
    push rdx
    mov rdi, rcx
    call obj_decref
    pop rdx
    pop rax
    jmp .agsi_body_raised
.agsend_settled:
    add rsp, 8
    V_UNPACK rax, rdx           ; eval_frame returns a Value
    ; rax = result payload, rdx = result tag
    push rax
    push rdx

    ; Mark as not running
    mov qword [r12 + PyGenObject.gi_running], 0

    ; Check if exhausted (instr_ptr == 0)
    mov rdi, [r12 + PyGenObject.gi_frame]
    cmp qword [rdi + PyFrame.instr_ptr], 0
    jne .agsi_yielded

    ; Async gen returned (exhausted).  Clear before freeing, as gen_iternext
    ; does and for the same reason.
    mov qword [r12 + PyGenObject.gi_frame], 0
    call frame_free
    pop rdx                    ; result tag
    pop rax                    ; result payload
    V_PACK rax, rdx
    mov [r12 + PyGenObject.gi_return_value], rax

    ; Mark wrapper as closed
    mov dword [rbx + AsyncGenASend.ags_state], 2

    ; Raise StopAsyncIteration
    lea rdi, [rel exc_StopAsyncIteration_type]
    xor esi, esi
    xor edx, edx
    call exc_new
    mov rdi, rax
    call raise_exception_obj
    RET_NULL
    pop r12
    pop rbx
    leave
    ret

;; --- the body raised ---------------------------------------------------
;; Not a result, and above all not the end of the iteration.  The arm below
;; decides "exhausted or yielded" from instr_ptr, and a body that raised
;; leaves the frame finished -- so it read as exhausted and manufactured a
;; StopAsyncIteration, whose raise_exception_obj then DECREF'd the real
;; exception away.  `async for x in ag()` over a generator that raises ended
;; the loop cleanly and lost the exception outright: not caught by an
;; enclosing except, not reported at exit, the program simply carried on past
;; a `raise`.
;;
;; The exception is pending and already owns its reference.  Do the same
;; bookkeeping the exhausted arm does -- the generator is finished either way
;; -- and unwind with it, which is how that arm reaches END_ASYNC_FOR too.
.agsi_body_raised:
    extern eval_exception_unwind
    extern eval_saved_r13
    add rsp, 8                  ; the scratch pushed before eval_frame
    mov qword [r12 + PyGenObject.gi_running], 0
    mov rdi, [r12 + PyGenObject.gi_frame]
    test rdi, rdi
    jz .agsi_body_raised_closed
    mov qword [r12 + PyGenObject.gi_frame], 0
    call frame_free
.agsi_body_raised_closed:
    mov dword [rbx + AsyncGenASend.ags_state], 2
    pop r12
    pop rbx
    leave
    mov [rel eval_saved_r13], r13
    jmp eval_exception_unwind

.agsi_yielded:
    pop rdx                    ; result tag
    pop rax                    ; result payload
    V_PACK rax, rdx

    ; Only a boxed value is this generator's own item.  Anything else is an
    ; `await` inside the body yielding outward, and has to reach the event loop
    ; unchanged -- with the state left at 0, so the next SEND resumes the
    ; generator rather than reporting it exhausted.
    V_TEST_PTR rax, rcx
    ja .agsi_passthrough
    test rax, rax
    jz .agsi_passthrough
    mov rcx, [rax + PyObject.ob_type]
    lea rdx, [rel async_gen_wrapped_type]
    cmp rcx, rdx
    jne .agsi_passthrough

    ; The item: unwrap it, hand it to SEND through the exhaustion path, and
    ; drop the box.  gi_return_value owns its reference (ags_dealloc frees it),
    ; so the value is INCREF'd before the box that held it goes away.
    mov rcx, [rax + AsyncGenWrapped.agw_value]
    INCREF_V rcx, rdx
    mov [rbx + AsyncGenASend.gi_return_value], rcx
    mov rdi, rax
    DECREF_REG rdi
    mov dword [rbx + AsyncGenASend.ags_state], 1
    RET_NULL
    pop r12
    pop rbx
    leave
    ret

.agsi_passthrough:
    ; .agsi_yielded already packed this, and tp_iternext's contract is one
    ; Value -- op_send unpacks what it gets back.  Decoding here as well left a
    ; pointer intact but stripped the tag off the SLEEP and IO_WAIT sentinels,
    ; so the event loop read a nanosecond count as an object pointer.
    pop r12
    pop rbx
    leave
    ret

.agsi_second_call:
    ; State 1: already yielded — return NULL to signal SEND exhaustion
    ; SEND will read gi_return_value from the wrapper (at offset +48)
    ; which holds the yielded value stored during state 0.
    RET_NULL
    pop r12
    pop rbx
    leave
    ret

.agsi_gen_exhausted:
    ; Generator already exhausted
    mov dword [rbx + AsyncGenASend.ags_state], 2
    lea rdi, [rel exc_StopAsyncIteration_type]
    xor esi, esi
    xor edx, edx
    call exc_new
    mov rdi, rax
    call raise_exception_obj
    RET_NULL
    pop r12
    pop rbx
    leave
    ret

.agsi_closed:
    ; State 2: closed.  CPython refuses a REUSE rather than reporting
    ; exhaustion -- an asend is one step of one iteration, and driving the
    ; same one twice is a bug in the caller, not the end of the generator.
    ; `async for` never reuses one: GET_ANEXT builds a fresh wrapper per turn.
    RAISE exc_RuntimeError_type, "cannot reuse already awaited __anext__()/asend()"

.agsi_closed_unused:
    lea rdi, [rel exc_StopAsyncIteration_type]
    xor esi, esi
    xor edx, edx
    call exc_new
    mov rdi, rax
    call raise_exception_obj
    RET_NULL
    pop r12
    pop rbx
    leave
    ret

.agsi_error:
    RET_NULL
    pop r12
    pop rbx
    leave
    ret
END_FUNC ags_iternext

;; ============================================================================
;; ags_iter_self(AsyncGenASend *self) -> self with INCREF
;; tp_iter for AsyncGenASend: return self (it IS the iterator)
;; ============================================================================
DEF_FUNC_BARE ags_iter_self
    inc qword [rdi + PyObject.ob_refcnt]
    mov rax, rdi
    mov edx, TAG_PTR
    ret
END_FUNC ags_iter_self

;; ============================================================================
;; async_gen_wrap_value(Value v) -> PyObject*, stealing v
;; INTRINSIC_ASYNC_GEN_WRAP.  See AsyncGenWrapped in object.inc for why the box
;; exists at all.
;; ============================================================================
DEF_FUNC async_gen_wrap_value, 8            ; 1 pushes, so rsp is 16-aligned
    push rbx
    mov rbx, rdi
    mov edi, AsyncGenWrapped_size
    call ap_malloc
    mov qword [rax + PyObject.ob_refcnt], 1
    lea rcx, [rel async_gen_wrapped_type]
    mov [rax + PyObject.ob_type], rcx
    mov [rax + AsyncGenWrapped.agw_value], rbx
    pop rbx
    leave
    ret
END_FUNC async_gen_wrap_value

DEF_FUNC agw_dealloc, 8            ; 1 pushes, so rsp is 16-aligned
    push rbx
    mov rbx, rdi
    mov rdi, [rbx + AsyncGenWrapped.agw_value]
    DECREF_V rdi, rsi
    mov rdi, rbx
    call ap_free
    pop rbx
    leave
    ret
END_FUNC agw_dealloc

;; ============================================================================
;; ags_dealloc(AsyncGenASend *self)
;; Free the wrapper: DECREF stored value and async generator, then free.
;; ============================================================================
DEF_FUNC ags_dealloc, 8            ; 1 pushes, so rsp is 16-aligned
    push rbx
    mov rbx, rdi

    ; DECREF stored return value if present
    mov rdi, [rbx + AsyncGenASend.gi_return_value]
    V_UNPACK rdi, rsi
    DECREF_VAL rdi, rsi

    ; ...and the value the awaitable was going to resume with, which is
    ; asend's argument or athrow's exception.  Both are owned, and neither
    ; was released when the awaitable was dropped unawaited.
    mov rdi, [rbx + AsyncGenASend.ags_sendval]
    mov qword [rbx + AsyncGenASend.ags_sendval], 0
    DECREF_V rdi, rsi

    ; DECREF the async generator
    mov rdi, [rbx + AsyncGenASend.ags_gen]
    call obj_decref

    ; Free the wrapper itself
    mov rdi, rbx
    call ap_free

    pop rbx
    leave
    ret
END_FUNC ags_dealloc

;; ============================================================================
;; agt_iternext(AsyncGenASend *self) -> one Value, or 0
;;
;; What `await agen.aclose()` and `await agen.athrow(exc)` drive.  Both used
;; to be aliases for the SYNCHRONOUS gen_close/gen_throw, which answered None
;; and the raw yielded box respectively -- so `await agen.aclose()` awaited
;; None and raised TypeError, and every `async with` over an
;; @asynccontextmanager generator failed there.  CPython answers an awaitable
;; from both, and this is it: one object, told apart by whether ags_sendval
;; carries an exception.
;;
;; The operation happens on the FIRST drive and its result reaches `await`
;; through the exhaustion path -- tp_iternext answers 0 with gi_return_value
;; set, which is what SEND reads.  A generator that awaits inside its own
;; except or finally passes that outward instead, and ags_started is what
;; makes the next drive resume it rather than throw again.
;; ============================================================================
AGT_EXC   equ 8             ; the exception, across gen_throw
AGT_FRAME equ 16            ; + 2 pushes = 32, 16-aligned
DEF_FUNC agt_iternext, AGT_FRAME
    push rbx
    push r12
    mov rbx, rdi

    ; An aclose or an athrow is ONE awaitable: it completes on the drive that
    ; delivers its result, and driving it again is the caller's bug.  asend
    ; parks at state 1 instead because SEND reads gi_return_value on the same
    ; call and its own by-name path is what marks it used.
    mov eax, [rbx + AsyncGenASend.ags_state]
    cmp eax, 1
    jae .agt_reuse

    cmp dword [rbx + AsyncGenASend.ags_started], 0
    jne .agt_resume

    mov dword [rbx + AsyncGenASend.ags_started], 1
    mov r12, [rbx + AsyncGenASend.ags_gen]

    ; The exception is consumed here: gen_throw takes its own reference, and
    ; leaving it in place would make .agt_resume push it as a SENT value.
    mov rsi, [rbx + AsyncGenASend.ags_sendval]
    mov qword [rbx + AsyncGenASend.ags_sendval], 0
    test rsi, rsi
    jz .agt_close

    mov [rbp - AGT_EXC], rsi    ; a frame slot, not a push: a lone push would
                                ; leave gen_throw -- and every finally body it
                                ; runs -- called eight bytes out
    mov rdi, r12
    call gen_throw
    push rax
    push rdx
    mov rdi, [rbp - AGT_EXC]
    DECREF_V rdi, rsi
    pop rdx
    pop rax

    test edx, edx
    jz .agt_throw_done

    ; A value came out.  The generator's own yield is a wrapped box; anything
    ; else is an `await` inside the handler passing outward, and reaches the
    ; loop unchanged with ags_state left at 0 so the next drive resumes.
    V_PACK rax, rdx
    V_TEST_PTR rax, rcx
    ja .agt_out
    test rax, rax
    jz .agt_out
    mov rcx, [rax + PyObject.ob_type]
    lea rdx, [rel async_gen_wrapped_type]
    cmp rcx, rdx
    jne .agt_out

    mov rcx, [rax + AsyncGenWrapped.agw_value]
    INCREF_V rcx, rdx
    mov [rbx + AsyncGenASend.gi_return_value], rcx
    mov rdi, rax
    DECREF_REG rdi
    mov dword [rbx + AsyncGenASend.ags_state], 2
    jmp .agt_null

.agt_throw_done:
    ; The generator finished.  If it did not handle the throw its exception
    ; is pending and belongs to the awaiting frame; if it swallowed it and
    ; returned, the athrow ends the way an exhausted iteration does.
    mov dword [rbx + AsyncGenASend.ags_state], 2
    cmp qword [rel current_exception], 0
    jne .agt_null
    lea rdi, [rel exc_StopAsyncIteration_type]
    xor esi, esi
    xor edx, edx
    call exc_new
    mov rdi, rax
    call raise_exception_obj
    jmp .agt_null

.agt_close:
    ; aclose().  gen_close runs the finally blocks and answers None; it
    ; leaves through the unwinder itself if one of them raises.
    mov rdi, r12
    call gen_close
    V_PACK rax, rdx
    mov [rbx + AsyncGenASend.gi_return_value], rax
    mov dword [rbx + AsyncGenASend.ags_state], 2
    jmp .agt_null

.agt_out:
    pop r12
    pop rbx
    leave
    ret

.agt_null:
    xor eax, eax
    pop r12
    pop rbx
    leave
    ret

.agt_resume:
    ; The throw has been delivered and the generator suspended on an await of
    ; its own; from here it is an ordinary resume, which is asend's job.
    pop r12
    pop rbx
    leave
    jmp ags_iternext

.agt_reuse:
    RAISE exc_RuntimeError_type, "cannot reuse already awaited aclose()/athrow()"
END_FUNC agt_iternext

;; ============================================================================
;; async_gen_athrow_new(rdi = the async generator, rsi = the exception Value
;;                      or 0 for aclose) -> rax = the awaitable, owned
;; ============================================================================
DEF_FUNC async_gen_athrow_new               ; 2 pushes, so rsp is 16-aligned
    push rbx
    push r12
    mov rbx, rdi
    mov r12, rsi

    mov edi, AsyncGenASend_size
    call ap_malloc

    mov qword [rax + PyObject.ob_refcnt], 1
    lea rcx, [rel async_gen_athrow_type]
    mov [rax + PyObject.ob_type], rcx
    mov [rax + AsyncGenASend.ags_gen], rbx
    mov dword [rax + AsyncGenASend.ags_state], 0
    mov [rax + AsyncGenASend.ags_sendval], r12
    mov dword [rax + AsyncGenASend.ags_started], 0
    mov qword [rax + AsyncGenASend.gi_return_value], 0

    inc qword [rbx + PyObject.ob_refcnt]
    INCREF_V r12, rcx

    pop r12
    pop rbx
    leave
    ret
END_FUNC async_gen_athrow_new

;; ============================================================================
;; _agen_aclose_impl(rdi = args, rsi = nargs) -> rax = Value
;;
;; agen.aclose().  Answers the awaitable above rather than doing the work,
;; which is what makes `await` on it legal.
;; ============================================================================
DEF_FUNC_LOCAL _agen_aclose_impl
    cmp rsi, 1
    jne .arity
    mov rdi, [rdi]
    call _agen_check_receiver
    xor esi, esi                ; no exception: this is a close
    call async_gen_athrow_new
    leave
    ret
.arity:
    RAISE exc_TypeError_type, "aclose() takes no arguments"
END_FUNC _agen_aclose_impl

;; ============================================================================
;; _agen_athrow_impl(rdi = args, rsi = nargs) -> rax = Value
;;
;; agen.athrow(exc), the other half of aclose above: the same awaitable,
;; carrying the exception rather than the GeneratorExit aclose implies.
;; ============================================================================
AAT_EXC   equ 8             ; the exception, across the receiver check
AAT_OWNED equ 16            ; non-zero when AAT_EXC is a reference of ours
AAT_N     equ 24            ; the argument count, likewise
AAT_ARGS  equ 32            ; and the array
AAT_FRAME equ 48            ; + 0 pushes = 48, 16-aligned
DEF_FUNC_LOCAL _agen_athrow_impl, AAT_FRAME
    ; CPython takes athrow(typ[, val[, tb]]): the two- and three-argument
    ; spellings are deprecated but its own test_asyncgen uses them, and the
    ; value is what the type is CALLED with.  The traceback is accepted and
    ; ignored, as it is nowhere else in this tree either.
    cmp rsi, 2
    jl .arity
    cmp rsi, 4
    jg .arity
    mov qword [rbp - AAT_OWNED], 0
    cmp rsi, 2
    jle .not_deprecated
    ; CPython warns before it does anything else, and a suite that turns
    ; warnings into errors reads that as the deprecation firing.
    mov [rbp - AAT_EXC], rdi
    lea rdi, [rel aat_deprecated]
    extern deprecation_warn
    call deprecation_warn
    mov rdi, [rbp - AAT_EXC]
    test eax, eax
    jz .raised                  ; a filter made it an error
.not_deprecated:
    mov [rbp - AAT_N], rsi      ; the count, across every call below
    mov rax, [rdi + 8]          ; the exception or its type, borrowed
    mov [rbp - AAT_EXC], rax    ; a frame slot: a lone push left both calls
                                ; below, and ap_malloc under the second, eight
                                ; bytes out
    mov [rbp - AAT_ARGS], rdi

    ; It has to BE an exception class or instance.  gen_throw is going to
    ; raise it, and the type_call below would otherwise jump through whatever
    ; sits at tp_call in an object of another shape -- `athrow(5, 6)` was a
    ; segfault from ordinary Python.
    V_TEST_PTR rax, rcx
    ja .not_an_exception
    test rax, rax
    jz .not_an_exception
    mov rcx, [rax + PyObject.ob_type]
    extern type_type
    lea rdx, [rel type_type]
    cmp rcx, rdx
    je .athrow_class
    extern exc_metatype
    lea rdx, [rel exc_metatype]
    cmp rcx, rdx
    je .athrow_class
    extern user_type_metatype
    lea rdx, [rel user_type_metatype]
    cmp rcx, rdx
    je .athrow_class

    ; An INSTANCE: its type must derive from BaseException, and a separate
    ; value is refused rather than used.
    mov rdi, rcx
    extern type_is_exc_subclass
    call type_is_exc_subclass
    test eax, eax
    jz .not_an_exception
    cmp qword [rbp - AAT_N], 3
    jl .have_exc
    mov rdi, [rbp - AAT_ARGS]
    mov rdx, [rdi + 16]
    lea rcx, [rel none_singleton]
    cmp rdx, rcx
    je .have_exc
    test rdx, rdx
    jz .have_exc
    RAISE exc_TypeError_type, "instance exception may not have a separate value"

.athrow_class:
    mov rdi, rax
    call type_is_exc_subclass
    test eax, eax
    jz .not_an_exception
    cmp qword [rbp - AAT_N], 3
    jl .have_exc
    mov rdi, [rbp - AAT_ARGS]
    mov rdx, [rdi + 16]         ; the value
    lea rcx, [rel none_singleton]
    cmp rdx, rcx
    je .have_exc
    test rdx, rdx
    jz .have_exc

    ; A value that is ALREADY an instance of the class is the exception, not
    ; an argument to build one from -- _PyErr_SetObject's rule, and without it
    ; `athrow(ValueError, ValueError('z'))` raised ValueError(ValueError('z')).
    V_TEST_PTR rdx, rcx
    ja .athrow_build
    test rdx, rdx
    jz .athrow_build
    push rdx
    sub rsp, 8
    mov rdi, [rdx + PyObject.ob_type]
    mov rsi, [rbp - AAT_EXC]
    extern type_is_subtype
    call type_is_subtype
    add rsp, 8
    pop rdx
    test eax, eax
    jz .athrow_build
    mov [rbp - AAT_EXC], rdx
    jmp .have_exc

.athrow_build:
    ; typ(val), which is what CPython's _PyErr_CreateException does with the
    ; pair.  Its reference is this function's until async_gen_athrow_new takes
    ; one of its own.
    mov rdi, [rbp - AAT_EXC]
    lea rsi, [rbp - AAT_ARGS]
    mov rsi, [rsi]
    add rsi, 16
    mov edx, 1
    extern type_call
    call type_call
    test rax, rax
    jz .raised
    mov [rbp - AAT_EXC], rax
    mov qword [rbp - AAT_OWNED], 1

.have_exc:
    mov rdi, [rbp - AAT_ARGS]
    mov rdi, [rdi]
    call _agen_check_receiver
    mov rsi, [rbp - AAT_EXC]
    call async_gen_athrow_new
    cmp qword [rbp - AAT_OWNED], 0
    je .out
    push rax
    sub rsp, 8
    mov rdi, [rbp - AAT_EXC]
    call obj_decref
    add rsp, 8
    pop rax
.out:
    leave
    ret

.raised:
    ; The type refused the value; its exception is the caller's.
    extern eval_exception_unwind
    leave
    jmp eval_exception_unwind

.not_an_exception:
    mov rsi, [rbp - AAT_EXC]
    CSTRING rdi, `exceptions must be classes or instances deriving from BaseException, not \x01`
    extern raise_type_error_with_name
    jmp raise_type_error_with_name      ; does not return

.arity:
    ; _PyArg_CheckPositional's wording, which names the count it got.
    dec rsi                     ; self is not one of them
    lea rdi, [rel aat_buf]
    push rsi
    sub rsp, 8
    cmp rsi, 1
    jl .too_few
    CSTRING rsi, "athrow expected at most 3 arguments, got "
    jmp .say
.too_few:
    CSTRING rsi, "athrow expected at least 1 argument, got "
.say:
    extern rbt_append_cstr
    call rbt_append_cstr
    add rsp, 8
    pop rsi
    mov rdi, rax
    extern msg_append_i64
    call msg_append_i64
    lea rdi, [rel exc_TypeError_type]
    lea rsi, [rel aat_buf]
    call raise_exception        ; does not return
END_FUNC _agen_athrow_impl

section .rodata
aat_deprecated: db "the (type, exc, tb) signature of athrow() is deprecated, use the single-arg signature instead.", 0
section .bss
aat_buf: resb 96
section .text

;; ============================================================================
;; _agen_check_receiver(rdi = the candidate) -> rax = it, unchanged
;;
;; Raises rather than returning when it is not an async generator, so every
;; caller can ignore the failing case.
;; ============================================================================
DEF_FUNC_LOCAL _agen_check_receiver
    V_TEST_PTR rdi, rcx
    ja .bad
    test rdi, rdi
    jz .bad
    mov rcx, [rdi + PyObject.ob_type]
    lea rdx, [rel async_gen_type]
    cmp rcx, rdx
    jne .bad
    mov rax, rdi
    leave
    ret
.bad:
    RAISE exc_TypeError_type, "descriptor requires an async_generator object"
END_FUNC _agen_check_receiver

;; ============================================================================
;; _get_agen_aclose_builtin() -> rax = the `aclose` builtin, borrowed
;; ============================================================================
DEF_FUNC_LOCAL _get_agen_aclose_builtin
    mov rax, [rel _agen_aclose_cache]
    test rax, rax
    jnz .ret
    lea rdi, [rel _agen_aclose_impl]
    CSTRING rsi, "aclose"
    call builtin_func_new
    mov [rel _agen_aclose_cache], rax
.ret:
    leave
    ret
END_FUNC _get_agen_aclose_builtin

;; ============================================================================
;; _get_agen_athrow_builtin() -> rax = the `athrow` builtin, borrowed
;; ============================================================================
DEF_FUNC_LOCAL _get_agen_athrow_builtin
    mov rax, [rel _agen_athrow_cache]
    test rax, rax
    jnz .ret
    lea rdi, [rel _agen_athrow_impl]
    CSTRING rsi, "athrow"
    call builtin_func_new
    mov [rel _agen_athrow_cache], rax
.ret:
    leave
    ret
END_FUNC _get_agen_athrow_builtin

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

    mov rdi, rbx
    call gen_dealloc_close

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

    ; Free self (GC-aware)
    mov rdi, rbx
    call gc_dealloc

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

;; ============================================================================
;; _ags_send_impl(rdi = args, rsi = nargs) -> rax = Value
;;
;; asend.send(value) and asend.__next__(), which are one function in CPython
;; too.  The asend object had a live tp_iternext and no tp_dict at all, so
;; `agen.__anext__().send(None)` -- the only way to drive an async generator
;; without an event loop, and exactly what CPython's own test_asyncgen does
;; -- raised AttributeError, and the whole "compare a sync generator with an
;; async one" half of that file with it.
;;
;; ags_iternext answers NULL for the generator's own item and leaves it in
;; gi_return_value, because that is the shape op_send's exhaustion path
;; reads.  By NAME the protocol is the other one: StopIteration carrying the
;; value, the same as a generator's own send().
;;
;; send() wants its argument and __next__() refuses one, so the arity check
;; cannot be shared even though everything after it is.
;; ============================================================================
AGSS_SELF  equ 8
AGSS_VAL   equ 16           ; the item, held across exc_new so it can be freed
AGSS_FRAME equ 32           ; + 0 pushes = 32, 16-aligned

global _ags_send_impl
DEF_FUNC_BARE _ags_send_impl
    cmp rsi, 2
    jne .send_arity
    jmp ags_send_core
.send_arity:
    RAISE exc_TypeError_type, "send() takes exactly one argument"
END_FUNC _ags_send_impl

;; ============================================================================
;; _ags_next_impl(rdi = args, rsi = nargs) -> rax = Value
;;
;; asend.__next__(), which is send(None) under the name a for-loop uses.
;; ============================================================================
global _ags_next_impl
DEF_FUNC_BARE _ags_next_impl
    cmp rsi, 1
    jne .next_arity
    jmp ags_send_core
.next_arity:
    RAISE exc_TypeError_type, "__next__() takes no arguments"
END_FUNC _ags_next_impl

;; ============================================================================
;; ags_send_core(rdi = args, rsi = nargs) -> rax = Value
;;
;; Everything send() and __next__() share, past the arity check that is all
;; that separates them.  Drives the awaitable through its own tp_iternext:
;; the aclose/athrow object shares this method and has a different one.
;; ============================================================================
DEF_FUNC_LOCAL ags_send_core, AGSS_FRAME
    mov rax, [rdi]              ; self
    V_TEST_PTR rax, rcx
    ja .agss_receiver
    test rax, rax
    jz .agss_receiver
    mov rcx, [rax + PyObject.ob_type]
    lea rdx, [rel async_gen_asend_type]
    cmp rcx, rdx
    je .agss_asend
    lea rdx, [rel async_gen_athrow_type]
    cmp rcx, rdx
    jne .agss_receiver

    ; --- the aclose/athrow awaitable ---
    ; ags_sendval is NOT a resume value here: it is the exception agt_iternext
    ; is going to throw.  Storing the sent value over it handed gen_throw a
    ; plain int to raise -- a segfault from ordinary Python -- and dropped the
    ; exception's reference on the way.  CPython refuses the send instead.
    mov [rbp - AGSS_SELF], rax
    cmp rsi, 2
    jne .agss_go
    mov rdx, [rdi + 8]
    lea rcx, [rel none_singleton]
    cmp rdx, rcx
    je .agss_go
    cmp dword [rax + AsyncGenASend.ags_started], 0
    jne .agss_store             ; already thrown: this is an ordinary resume
    RAISE exc_RuntimeError_type, "can't send non-None value to a just-started coroutine"

.agss_asend:
    ; The sent value.  ags_iternext resumes with None; anything else is
    ; stashed for it to push instead, and the reference goes with it.
    mov [rbp - AGSS_SELF], rax
    cmp rsi, 2
    jne .agss_go
    mov rdx, [rdi + 8]
    lea rcx, [rel none_singleton]
    cmp rdx, rcx
    je .agss_go

.agss_store:
    ; asend(v).send(w) is CPython's "w wins", so what asend() left here is
    ; released rather than leaked.
    push rdx
    sub rsp, 8                  ; a pad: XDECREF_V expands to a call, and a
                                ; lone push leaves it eight out
    mov rdi, [rax + AsyncGenASend.ags_sendval]
    mov qword [rax + AsyncGenASend.ags_sendval], 0
    XDECREF_V rdi, rcx
    add rsp, 8
    pop rdx
    mov rax, [rbp - AGSS_SELF]
    INCREF_V rdx, rcx
    mov [rax + AsyncGenASend.ags_sendval], rdx

.agss_go:
    ; Through the slot, not through ags_iternext: the athrow awaitable shares
    ; this method and has a tp_iternext of its own.
    mov rdi, [rbp - AGSS_SELF]
    mov rax, [rdi + PyObject.ob_type]
    call [rax + PyTypeObject.tp_iternext]
    test rax, rax
    jnz .agss_out               ; an `await` passing outward: hand it back

    ; NULL means the item is ready, unless the body raised or the generator
    ; is finished -- ags_iternext raises StopAsyncIteration for the second,
    ; and a pending exception is the caller's either way.
    cmp qword [rel current_exception], 0
    jne .agss_propagate

    ; The item, and with it the end of this awaitable: a second send() on the
    ; same asend is the reuse CPython refuses.  Only the name path marks it,
    ; because op_send reads state 1 and gi_return_value for its own
    ; exhaustion arm and must keep finding them.
    mov rcx, [rbp - AGSS_SELF]
    mov rsi, [rcx + AsyncGenASend.gi_return_value]
    mov qword [rcx + AsyncGenASend.gi_return_value], 0   ; the reference moves
    mov [rbp - AGSS_VAL], rsi   ; ...to here, and is released below
    mov dword [rcx + AsyncGenASend.ags_state], 2
    lea rdi, [rel exc_StopIteration_type]
    test rsi, rsi
    jz .agss_no_val
    ; A bare StopIteration for None, as a generator returning None raises,
    ; so str(e) is "" rather than "None".
    lea rax, [rel none_singleton]
    cmp rsi, rax
    jne .agss_have_val
.agss_no_val:
    xor esi, esi
.agss_have_val:
    call exc_new
    ; exc_new took a reference of its own, so the one moved out of the
    ; wrapper is this function's to drop.  Leaving it held leaked the yielded
    ; item once per drive -- five objects for five turns of a hand-driven
    ; `async for`, invisible to every gate because nothing crashed.
    push rax
    sub rsp, 8                  ; a pad, as above
    mov rdi, [rbp - AGSS_VAL]
    XDECREF_V rdi, rcx
    add rsp, 8
    pop rdi
    call raise_exception_obj

.agss_propagate:
    extern eval_exception_unwind
    leave
    jmp eval_exception_unwind

.agss_out:
    leave
    ret                         ; ags_iternext already answers one Value

.agss_receiver:
    RAISE exc_TypeError_type, "send() requires an async_generator_asend"
END_FUNC ags_send_core

;; ============================================================================
;; _ags_close_impl(rdi = args, rsi = nargs) -> rax = Value
;;
;; asend.close().  CPython's does nothing but mark the awaitable closed --
;; the generator behind it is not touched, because closing THAT is aclose()'s
;; job and this object is one step of one iteration.
;; ============================================================================
global _ags_close_impl
DEF_FUNC _ags_close_impl
    cmp rsi, 1
    jne .agsc_arity
    mov rax, [rdi]
    V_TEST_PTR rax, rcx
    ja .agsc_receiver
    test rax, rax
    jz .agsc_receiver
    mov rcx, [rax + PyObject.ob_type]
    lea rdx, [rel async_gen_asend_type]
    cmp rcx, rdx
    je .agsc_ok
    lea rdx, [rel async_gen_athrow_type]
    cmp rcx, rdx
    jne .agsc_receiver
.agsc_ok:
    mov dword [rax + AsyncGenASend.ags_state], 2
    RET_NONE
    leave
    V_PACK rax, rdx             ; builtins return one Value
    ret
.agsc_receiver:
    RAISE exc_TypeError_type, "close() requires an async_generator_asend"
.agsc_arity:
    RAISE exc_TypeError_type, "close() takes no arguments"
END_FUNC _ags_close_impl

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
;; _agen_asend_impl(rdi = args, rsi = nargs) -> rax = Value (AsyncGenASend*)
;;
;; agen.asend(value).  It answers an AWAITABLE, the same object
;; `agen.__anext__()` answers, carrying the value the generator will resume
;; with -- it does not resume anything itself.  It used to be an alias for
;; the sync `send`, which resumed the generator immediately and handed back
;; the wrapped yield, so `await agen.asend(v)` awaited an
;; async_generator_wrapped_value and the value never reached the generator
;; at all.
;; ============================================================================
DEF_FUNC_LOCAL _agen_asend_impl
    cmp rsi, 2
    jne .arity
    mov rax, [rdi]              ; the async generator
    V_TEST_PTR rax, rcx
    ja .receiver
    test rax, rax
    jz .receiver
    mov rcx, [rax + PyObject.ob_type]
    lea rdx, [rel async_gen_type]
    cmp rcx, rdx
    jne .receiver

    push rbx
    push r12
    mov rbx, rax
    mov r12, [rdi + 8]          ; the value to resume with, borrowed

    mov rdi, rbx
    call async_gen_iternext     ; the wrapper, with ags_sendval clear

    lea rcx, [rel none_singleton]
    cmp r12, rcx
    je .done                    ; None is what the wrapper already resumes with
    INCREF_V r12, rcx
    mov [rax + AsyncGenASend.ags_sendval], r12
.done:
    pop r12
    pop rbx
    leave
    ret

.receiver:
    RAISE exc_TypeError_type, "asend() requires an async_generator"
.arity:
    RAISE exc_TypeError_type, "asend() takes exactly one argument"
END_FUNC _agen_asend_impl

;; ============================================================================
;; _get_agen_asend_builtin() -> rax = the `asend` builtin, borrowed
;; Built once and cached, as every other generator method is.
;; ============================================================================
DEF_FUNC_LOCAL _get_agen_asend_builtin
    mov rax, [rel _agen_asend_cache]
    test rax, rax
    jnz .ret
    lea rdi, [rel _agen_asend_impl]
    CSTRING rsi, "asend"
    call builtin_func_new
    mov [rel _agen_asend_cache], rax
.ret:
    leave
    ret
END_FUNC _get_agen_asend_builtin

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
_agen_asend_cache: dq 0
_agen_aclose_cache: dq 0
_agen_athrow_cache: dq 0

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
    times 23 dq 0               ; the rest: this box is never used as a value

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
