; opcodes_async.asm - Async/await opcode handlers
;
; Implements: GET_AWAITABLE (131), GET_AITER (50), GET_ANEXT (51),
;             BEFORE_ASYNC_WITH (52), END_ASYNC_FOR (54), CLEANUP_THROW (55)
;
; Register convention (callee-saved, preserved across handlers):
;   rbx = bytecode instruction pointer (IP into co_code[])
;   r12 = current frame pointer (PyFrame*)
;   r13 = value stack payload top pointer
;   r14 = locals_tag_base pointer (frame's tag sidecar for localsplus[])
;   r15 = value stack tag top pointer
;
; ecx = opcode argument on handler entry (set by eval_dispatch)
; rbx has already been advanced past the 2-byte instruction word.
;
; CRITICAL: Opcode handlers are jumped to (not called) from eval_dispatch.
; They must end with DISPATCH (never leave/ret) and must preserve
; callee-saved registers rbx, r12-r15.

%include "macros.inc"
%include "object.inc"
%include "types.inc"
%include "opcodes.inc"
%include "frame.inc"
%include "errcodes.inc"

section .text

extern eval_dispatch
extern eval_saved_rbx
extern eval_saved_r13
extern eval_saved_r15
extern trace_opcodes
extern opcode_table
extern obj_dealloc
extern obj_incref
extern obj_decref
extern gen_type
extern coro_type
extern async_gen_type
extern raise_exception
extern raise_exception_obj
extern exc_TypeError_type
extern exc_StopAsyncIteration_type
extern exc_StopIteration_type
extern exc_isinstance
extern exc_new
extern none_singleton
extern str_from_cstr_heap
extern dict_get
extern method_new
extern current_exception
extern eval_exception_unwind

;; ============================================================================
;; op_get_awaitable - GET_AWAITABLE (131)
;;
;; TOS = object to await.
;; If it's a coroutine, leave it. Otherwise call __await__ (tp_iter).
;; Reject plain generators. Accept coroutines and objects with __await__.
;; ============================================================================
DEF_FUNC_BARE op_get_awaitable
    ; TOS = object to await
    VPEEK rdi                  ; rdi = TOS payload (don't pop yet)

    ; Must be TAG_PTR to check ob_type
    cmp byte [r15 - 1], TAG_PTR
    jne .gaw_error

    ; Check if it's a coroutine — already awaitable
    mov rax, [rdi + PyObject.ob_type]
    lea rcx, [rel coro_type]
    cmp rax, rcx
    je .gaw_done               ; coroutine: leave on stack

    ; Check if it's a generator (plain generators are NOT awaitable)
    lea rcx, [rel gen_type]
    cmp rax, rcx
    je .gaw_gen_error

    ; Try calling __await__ via tp_iter
    mov rax, [rdi + PyObject.ob_type]
    mov rax, [rax + PyTypeObject.tp_iter]
    test rax, rax
    jz .gaw_error

    ; Fall through to call tp_iter
    ; Pop TOS, save it, call tp_iter
    VPOP rdi
    push rdi                   ; save for DECREF later

    mov rax, [rdi + PyObject.ob_type]
    mov rax, [rax + PyTypeObject.tp_iter]
    call rax                   ; tp_iter(obj) -> rax = iterator ptr (or NULL)
    push rax                   ; save result

    ; DECREF original
    mov rdi, [rsp + 8]        ; saved original
    call obj_decref

    pop rax                    ; restore result
    add rsp, 8                ; discard saved original

    ; Check for NULL return (tp_iter failed)
    test rax, rax
    jz .gaw_error

    VPUSH_PTR rax

.gaw_done:
    DISPATCH

.gaw_error:
    lea rdi, [rel exc_TypeError_type]
    CSTRING rsi, "object can't be used in 'await' expression"
    call raise_exception

.gaw_gen_error:
    lea rdi, [rel exc_TypeError_type]
    CSTRING rsi, "cannot 'await' a generator (use 'yield from' instead)"
    call raise_exception
END_FUNC op_get_awaitable

;; ============================================================================
;; op_get_aiter - GET_AITER (50)
;;
;; Pop TOS, call __aiter__ (tp_iter), push result.
;; The result should be an async iterator.
;; ============================================================================
DEF_FUNC_BARE op_get_aiter
    ; Pop TOS = async iterable
    VPOP_VAL rdi, rsi

    ; Must be TAG_PTR to dereference
    cmp esi, TAG_PTR
    jne .gai_error

    ; Get __aiter__ from type — for async generators, tp_iter returns self
    mov rax, [rdi + PyObject.ob_type]
    mov rax, [rax + PyTypeObject.tp_iter]
    test rax, rax
    jz .gai_error_noattr

    ; Save original for DECREF after call
    push rdi
    call rax                   ; tp_iter(obj) -> rax = async iterator (ptr)
    push rax                   ; save result

    ; DECREF original object
    mov rdi, [rsp + 8]
    call obj_decref

    pop rax                    ; restore result
    add rsp, 8                ; discard saved original

    ; tp_iter returns a pointer — validate not NULL
    test rax, rax
    jz .gai_iter_error

    VPUSH_PTR rax              ; tp_iter returns pointer, use TAG_PTR

    DISPATCH

.gai_error:
    ; Non-TAG_PTR value: DECREF via tag-aware macro
    DECREF_VAL rdi, rsi
    jmp .gai_raise
.gai_error_noattr:
    ; TAG_PTR but no tp_iter: DECREF the pointer
    call obj_decref
.gai_raise:
.gai_iter_error:
    lea rdi, [rel exc_TypeError_type]
    CSTRING rsi, "'async for' requires an object with __aiter__ method"
    call raise_exception
END_FUNC op_get_aiter

;; ============================================================================
;; op_get_anext - GET_ANEXT (51)
;;
;; Peek TOS (async iterator), call __anext__ on it, then push the result.
;; The result needs to be awaitable (SEND loop follows in bytecode).
;; ============================================================================
DEF_FUNC_BARE op_get_anext
    ; Peek TOS = async iterator (don't pop — async for loop needs it)
    VPEEK rdi

    ; Must be TAG_PTR
    cmp byte [r15 - 1], TAG_PTR
    jne .gan_error

    ; Call tp_iternext on the async iterator
    mov rax, [rdi + PyObject.ob_type]
    mov rax, [rax + PyTypeObject.tp_iternext]
    test rax, rax
    jz .gan_error

    call rax                   ; tp_iternext(aiter) -> awaitable or NULL
    ; rax = result payload, edx = tag
    test edx, edx
    jz .gan_stop               ; NULL = exhausted

    ; Push result (the awaitable from __anext__)
    VPUSH_VAL rax, rdx
    DISPATCH

.gan_stop:
    ; Raise StopAsyncIteration
    lea rdi, [rel exc_StopAsyncIteration_type]
    xor esi, esi
    xor edx, edx
    call exc_new
    mov rdi, rax
    call raise_exception_obj

.gan_error:
    lea rdi, [rel exc_TypeError_type]
    CSTRING rsi, "'async for' requires an object with __anext__ method"
    call raise_exception
END_FUNC op_get_anext

;; ============================================================================
;; op_before_async_with - BEFORE_ASYNC_WITH (52)
;;
;; Clone of BEFORE_WITH but looks up __aexit__/__aenter__ instead.
;; Stack: ... | mgr  ->  ... | bound_aexit | result_of___aenter__()
;;
;; Uses DEF_FUNC with rbp-relative locals (same pattern as op_before_with).
;; ============================================================================
BAW_RETTAG equ 8
BAW_MGR    equ 16
BAW_EXIT   equ 24
BAW_ENTER  equ 32
BAW_FRAME  equ 32

DEF_FUNC op_before_async_with, BAW_FRAME
    push rbx
    push r12

    ; Pop mgr (must be TAG_PTR to dereference)
    VPOP_VAL rax, rdx
    cmp edx, TAG_PTR
    jne .baw_not_ptr
    mov [rbp - BAW_MGR], rax
    mov rbx, rax               ; rbx = mgr

    ; Look up __aexit__ on mgr's type
    mov rax, [rbx + PyObject.ob_type]
    mov rax, [rax + PyTypeObject.tp_dict]
    test rax, rax
    jz .baw_no_exit

    ; Get "__aexit__" from type dict
    lea rdi, [rel baw_str_aexit]
    call str_from_cstr_heap
    mov r12, rax               ; r12 = aexit name str
    mov rdi, [rbx + PyObject.ob_type]
    mov rdi, [rdi + PyTypeObject.tp_dict]
    mov rsi, r12
    mov edx, TAG_PTR
    call dict_get
    ; rax = value payload, edx = tag
    test edx, edx
    jz .baw_no_exit_decref_name
    cmp edx, TAG_PTR
    jne .baw_no_exit_decref_name

    ; Got __aexit__ function — create bound method
    mov [rbp - BAW_EXIT], rax
    mov rdi, r12
    call obj_decref            ; DECREF aexit name str

    mov rdi, [rbp - BAW_EXIT]  ; func
    mov rsi, [rbp - BAW_MGR]   ; self = mgr
    call method_new
    mov [rbp - BAW_EXIT], rax

    ; Push bound __aexit__ method
    VPUSH_PTR rax

    ; Now look up __aenter__ on mgr's type
    mov rdi, [rbx + PyObject.ob_type]
    mov rdi, [rdi + PyTypeObject.tp_dict]
    test rdi, rdi
    jz .baw_no_enter

    lea rdi, [rel baw_str_aenter]
    call str_from_cstr_heap
    mov r12, rax               ; r12 = aenter name str
    mov rdi, [rbx + PyObject.ob_type]
    mov rdi, [rdi + PyTypeObject.tp_dict]
    mov rsi, r12
    mov edx, TAG_PTR
    call dict_get
    test edx, edx
    jz .baw_no_enter_decref_name
    cmp edx, TAG_PTR
    jne .baw_no_enter_decref_name

    ; Got __aenter__ function — call it with mgr as self
    push rax                   ; save aenter func
    mov rdi, r12
    call obj_decref            ; DECREF aenter name str
    pop rax                    ; restore aenter func

    ; Call __aenter__(mgr): tp_call(aenter_func, &mgr, 1)
    mov rcx, [rax + PyObject.ob_type]
    mov rcx, [rcx + PyTypeObject.tp_call]
    test rcx, rcx
    jz .baw_no_enter

    ; Build fat arg on stack
    mov r8, [rbp - BAW_MGR]
    SPUSH_PTR r8               ; args[0] = mgr
    mov rdi, rax               ; callable = __aenter__
    mov rsi, rsp               ; args ptr
    mov rdx, 1                 ; nargs = 1
    call rcx
    add rsp, 16                ; pop fat arg
    mov [rbp - BAW_ENTER], rax
    mov edx, edx               ; zero-extend 32-bit tag to 64-bit
    mov [rbp - BAW_RETTAG], rdx

    ; DECREF mgr
    mov rdi, [rbp - BAW_MGR]
    call obj_decref

    ; Push __aenter__ result
    mov rax, [rbp - BAW_ENTER]
    mov rdx, [rbp - BAW_RETTAG]
    VPUSH_VAL rax, rdx

    pop r12
    pop rbx
    leave
    DISPATCH

.baw_no_exit_decref_name:
    mov rdi, r12
    call obj_decref
.baw_no_exit:
    lea rdi, [rel exc_TypeError_type]
    CSTRING rsi, "'async with' requires __aexit__ method"
    call raise_exception

.baw_not_ptr:
    DECREF_VAL rax, rdx
    lea rdi, [rel exc_TypeError_type]
    CSTRING rsi, "'async with' requires a context manager object"
    call raise_exception
.baw_no_enter_decref_name:
    mov rdi, r12
    call obj_decref
.baw_no_enter:
    lea rdi, [rel exc_TypeError_type]
    CSTRING rsi, "'async with' requires __aenter__ method"
    call raise_exception
END_FUNC op_before_async_with

;; ============================================================================
;; op_end_async_for - END_ASYNC_FOR (54)
;;
;; Exception handler for async for loops.
;; If the exception is StopAsyncIteration: pop TOS + TOS1, jump forward by arg.
;; Otherwise: re-raise.
;;
;; Stack: ... | aiter | exc_val | (after except push)
;; arg = jump offset
;; ============================================================================
DEF_FUNC_BARE op_end_async_for
    ; ecx = arg (jump offset in instructions)

    ; TOS = exc_val (the exception that was raised)
    VPEEK rdi                  ; peek at exc payload
    movzx rsi, byte [r15 - 1]  ; exc tag

    ; Check if it's StopAsyncIteration
    cmp rsi, TAG_PTR
    jne .eaf_reraise

    ; Get exception type
    mov rax, [rdi + PyObject.ob_type]
    lea rdx, [rel exc_StopAsyncIteration_type]
    cmp rax, rdx
    je .eaf_stop

    ; Check via isinstance (for subclasses)
    push rcx                   ; save arg
    mov rsi, rdx
    call exc_isinstance
    pop rcx                    ; restore arg
    test eax, eax
    jnz .eaf_stop

.eaf_reraise:
    ; Not StopAsyncIteration — re-raise original exception
    VPOP_VAL rdi, rsi          ; exc payload+tag
    ; The exception is already a pointer; store as current_exception
    ; and re-raise via eval_exception_unwind (which handles unwind).
    ; raise_exception_obj INCREFs + XDECREFs old current_exception for us.
    call raise_exception_obj

.eaf_stop:
    ; StopAsyncIteration — pop exc and aiter, jump forward
    ; Pop exc
    VPOP_VAL rdi, rsi
    push rcx                   ; save arg
    DECREF_VAL rdi, rsi

    ; Pop aiter
    VPOP_VAL rdi, rsi
    DECREF_VAL rdi, rsi
    pop rcx                    ; restore arg

    ; Jump forward by arg instructions (each = 2 bytes)
    shl ecx, 1
    add rbx, rcx

    DISPATCH
END_FUNC op_end_async_for

;; ============================================================================
;; op_cleanup_throw - CLEANUP_THROW (55)
;;
;; Called after a throw into a subgenerator/coroutine via eval_exception_unwind.
;; eval_exception_unwind pushes the exception onto TOS and clears current_exception.
;; If the exception is StopIteration: extract .value and replace TOS.
;; Otherwise: re-raise by setting current_exception and re-entering unwind.
;;
;; Stack after eval_exception_unwind: ... | sub_iter(depth-preserved) | exception(TOS)
;; ============================================================================
DEF_FUNC_BARE op_cleanup_throw
    ; TOS = exception (pushed by eval_exception_unwind)
    ; Below TOS: sub-iterator (preserved to handler depth)
    ; current_exception = 0 (cleared by eval_exception_unwind)

    ; Read exception from TOS
    mov rax, [r13 - 8]          ; exception payload
    test rax, rax
    jz .ct_no_exc

    ; Is it StopIteration?
    mov rcx, [rax + PyObject.ob_type]
    lea rdx, [rel exc_StopIteration_type]
    cmp rcx, rdx
    jne .ct_reraise

    ; === StopIteration: sub-iterator returned normally ===
    ; Extract .value (exc_args[0] or None)
    mov rdi, [rax + PyExceptionObject.exc_args]
    test rdi, rdi
    jz .ct_si_none
    cmp qword [rdi + PyTupleObject.ob_size], 0
    je .ct_si_none
    ; value = args[0] (fat value in 16-byte slot)
    mov r8, [rdi + PyTupleObject.ob_item]       ; payloads
    mov r9, [rdi + PyTupleObject.ob_item_tags]  ; tags
    mov r8, [r8]                                ; value payload
    movzx r9d, byte [r9]                        ; value tag
    INCREF_VAL r8, r9
    jmp .ct_si_got_val
.ct_si_none:
    lea r8, [rel none_singleton]
    INCREF r8
    mov r9d, TAG_PTR
.ct_si_got_val:
    ; Save extracted value across DECREFs
    push r8
    push r9
    ; Pop and DECREF the StopIteration exception (TOS)
    VPOP rdi
    call obj_decref
    ; Pop and DECREF_VAL the sub-iterator
    VPOP_VAL rdi, rsi
    DECREF_VAL rdi, rsi
    ; Restore extracted value and push
    pop r9
    pop r8
    VPUSH_VAL r8, r9
    DISPATCH

.ct_no_exc:
    ; No exception — just dispatch
    DISPATCH

.ct_reraise:
    ; Not StopIteration — re-raise
    ; INCREF for current_exception (stack still owns its ref;
    ; eval_exception_unwind will restore r13 from saved value and
    ; XDECREF_VAL items when adjusting to the next handler's depth)
    INCREF rax
    mov [rel current_exception], rax
    jmp eval_exception_unwind
END_FUNC op_cleanup_throw

;; ============================================================================
;; Data section
;; ============================================================================
section .rodata
baw_str_aexit:  db "__aexit__", 0
baw_str_aenter: db "__aenter__", 0
section .text
