; eventloop.asm - Async event loop core
;
; Provides:
;   - EventLoop singleton with ready queue
;   - task_new(coro) -> AsyncTask*
;   - task_step(task) -> resume coro, dispatch result
;   - task_wake_waiters(task) -> wake tasks waiting on completion
;   - ready_enqueue(task) / ready_dequeue() -> task
;   - eventloop_run(root_task) -> fat value (main loop)
;   - eventloop_init() / eventloop_teardown()
;   - task_type — Python type for AsyncTask
;
; Uses IOBackend vtable for I/O abstraction (poll or io_uring).

%include "macros.inc"
%include "object.inc"
%include "types.inc"
%include "errcodes.inc"
%include "eventloop.inc"

extern ap_malloc
extern ap_free
extern obj_incref
extern obj_decref
extern obj_dealloc
extern gen_send
extern gen_throw
extern none_singleton
extern str_from_cstr
extern type_type
extern ap_strcmp
extern raise_exception
extern raise_exception_obj
extern exc_new
extern exc_TypeError_type
extern exc_RuntimeError_type
extern exc_CancelledError_type
extern exc_StopIteration_type
extern coro_type
extern builtin_func_new

; Poll backend (always available)
extern poll_backend

; io_uring backend (may fail at runtime)
extern uring_backend
extern uring_init

;; ============================================================================
;; eventloop_init() -> 0 ok, -1 fail
;; Initialize the event loop. Try io_uring first, fall back to poll.
;; ============================================================================
DEF_FUNC eventloop_init
    ; Try io_uring first
    call uring_init
    test eax, eax
    js .try_poll

    ; io_uring available
    lea rax, [rel uring_backend]
    mov [rel eventloop + EventLoop.backend], rax
    jmp .init_done

.try_poll:
    ; Use poll backend
    lea rax, [rel poll_backend]
    mov [rel eventloop + EventLoop.backend], rax

    ; Call poll init
    mov rax, [rax + IOBackend.init]
    call rax

.init_done:
    ; Initialize ready queue
    mov qword [rel eventloop + EventLoop.ready_head], 0
    mov qword [rel eventloop + EventLoop.ready_tail], 0
    mov dword [rel eventloop + EventLoop.running], 1
    mov qword [rel eventloop + EventLoop.root_task], 0

    xor eax, eax               ; return 0 = success
    leave
    ret
END_FUNC eventloop_init

;; ============================================================================
;; eventloop_teardown()
;; Shut down the event loop and backend.
;; ============================================================================
DEF_FUNC eventloop_teardown
    mov rax, [rel eventloop + EventLoop.backend]
    test rax, rax
    jz .td_done
    mov rax, [rax + IOBackend.teardown]
    call rax
.td_done:
    mov dword [rel eventloop + EventLoop.running], 0
    leave
    ret
END_FUNC eventloop_teardown

;; ============================================================================
;; task_new(PyGenObject *coro) -> AsyncTask*
;; Allocate and initialize a new async task.
;; ============================================================================
DEF_FUNC task_new
    push rbx

    mov rbx, rdi               ; save coro

    mov edi, AsyncTask_size
    call ap_malloc

    mov qword [rax + AsyncTask.ob_refcnt], 1
    lea rcx, [rel task_type]
    mov [rax + AsyncTask.ob_type], rcx
    mov [rax + AsyncTask.coro], rbx
    ; INCREF coro
    mov rdi, rbx
    push rax
    call obj_incref
    pop rax

    mov qword [rax + AsyncTask.result], 0
    mov qword [rax + AsyncTask.result_tag], 0
    mov qword [rax + AsyncTask.exception], 0
    ; send_value starts as None
    lea rcx, [rel none_singleton]
    mov [rax + AsyncTask.send_value], rcx
    mov qword [rax + AsyncTask.send_tag], TAG_PTR
    mov dword [rax + AsyncTask.done], 0
    mov dword [rax + AsyncTask.cancelling], 0
    mov dword [rax + AsyncTask.n_waiters], 0
    mov qword [rax + AsyncTask.waiters], 0
    mov dword [rax + AsyncTask.waiters_cap], 0
    mov qword [rax + AsyncTask.next], 0

    pop rbx
    leave
    ret
END_FUNC task_new

;; ============================================================================
;; task_dealloc(AsyncTask *self)
;; ============================================================================
DEF_FUNC task_dealloc
    push rbx
    mov rbx, rdi

    ; DECREF coro
    mov rdi, [rbx + AsyncTask.coro]
    test rdi, rdi
    jz .td_no_coro
    call obj_decref
.td_no_coro:

    ; XDECREF result
    mov rdi, [rbx + AsyncTask.result]
    mov rsi, [rbx + AsyncTask.result_tag]
    XDECREF_VAL rdi, rsi

    ; XDECREF exception
    mov rdi, [rbx + AsyncTask.exception]
    test rdi, rdi
    jz .td_no_exc
    call obj_decref
.td_no_exc:

    ; Free waiters array
    mov rdi, [rbx + AsyncTask.waiters]
    test rdi, rdi
    jz .td_no_waiters
    call ap_free
.td_no_waiters:

    ; Free self
    mov rdi, rbx
    call ap_free

    pop rbx
    leave
    ret
END_FUNC task_dealloc

;; ============================================================================
;; ready_enqueue(AsyncTask *task)
;; O(1) append to ready queue tail.
;; ============================================================================
DEF_FUNC_BARE ready_enqueue
    mov qword [rdi + AsyncTask.next], 0
    mov rax, [rel eventloop + EventLoop.ready_tail]
    test rax, rax
    jz .re_empty

    ; Append to tail
    mov [rax + AsyncTask.next], rdi
    mov [rel eventloop + EventLoop.ready_tail], rdi
    ret

.re_empty:
    ; Queue was empty
    mov [rel eventloop + EventLoop.ready_head], rdi
    mov [rel eventloop + EventLoop.ready_tail], rdi
    ret
END_FUNC ready_enqueue

;; ============================================================================
;; ready_dequeue() -> AsyncTask* or NULL
;; O(1) pop from ready queue head.
;; ============================================================================
DEF_FUNC_BARE ready_dequeue
    mov rax, [rel eventloop + EventLoop.ready_head]
    test rax, rax
    jz .rd_empty

    ; Advance head
    mov rcx, [rax + AsyncTask.next]
    mov [rel eventloop + EventLoop.ready_head], rcx
    test rcx, rcx
    jnz .rd_done
    ; Queue now empty
    mov qword [rel eventloop + EventLoop.ready_tail], 0
.rd_done:
    mov qword [rax + AsyncTask.next], 0
.rd_empty:
    ret
END_FUNC ready_dequeue

;; ============================================================================
;; task_step(AsyncTask *task)
;; Resume the task's coroutine via gen_send. Dispatch on result tag.
;; ============================================================================
TS_TASK  equ 8
TS_FRAME equ 8
DEF_FUNC task_step, TS_FRAME
    push rbx
    push r12

    mov rbx, rdi               ; rbx = task
    mov [rbp - TS_TASK], rdi

    ; Check if cancelled
    cmp dword [rbx + AsyncTask.cancelling], 1
    je .ts_cancel

    ; gen_send(coro, send_value, send_tag)
    mov rdi, [rbx + AsyncTask.coro]
    mov rsi, [rbx + AsyncTask.send_value]
    mov edx, [rbx + AsyncTask.send_tag]
    call gen_send
    ; rax = result payload, edx = tag

    ; Check for exhaustion (NULL tag = coroutine returned)
    test edx, edx
    jz .ts_finished

    ; Dispatch on result tag
    cmp edx, TAG_SLEEP
    je .ts_sleep
    cmp edx, TAG_IO_WAIT
    je .ts_io_wait
    cmp edx, TAG_TASK
    je .ts_await_task

    ; Unknown yield value — coroutine yielded a regular value
    ; For asyncio: this means the coroutine is waiting
    ; Re-enqueue to retry (shouldn't normally happen with proper awaitables)
    mov rdi, rbx
    call ready_enqueue
    jmp .ts_ret

.ts_sleep:
    ; rax = delay_ns
    mov rdi, [rel eventloop + EventLoop.backend]
    mov rdi, [rdi + IOBackend.submit_timeout]
    mov rsi, rbx               ; task
    mov rdx, rax               ; delay_ns
    ; swap args: submit_timeout(task, delay_ns)
    mov rdi, rbx
    mov rsi, rax
    mov rax, [rel eventloop + EventLoop.backend]
    call [rax + IOBackend.submit_timeout]
    jmp .ts_ret

.ts_io_wait:
    ; rax = fd | (direction << 32)
    mov r12, rax
    mov edi, eax               ; fd = low 32 bits
    shr r12, 32
    mov esi, r12d              ; events (POLLIN=1, POLLOUT=4)
    mov rdx, rbx               ; task
    ; submit_poll_fd(task, fd, events)
    mov rdi, rbx
    mov esi, eax               ; fd
    mov edx, r12d              ; events
    mov rax, [rel eventloop + EventLoop.backend]
    call [rax + IOBackend.submit_poll_fd]
    jmp .ts_ret

.ts_await_task:
    ; rax = AsyncTask* being awaited
    mov r12, rax               ; r12 = awaited task

    ; If the awaited task is already done, immediately wake us
    cmp dword [r12 + AsyncTask.done], 1
    je .ts_await_done

    ; Add current task as waiter on the awaited task
    ; Grow waiters array if needed
    mov eax, [r12 + AsyncTask.n_waiters]
    cmp eax, [r12 + AsyncTask.waiters_cap]
    jb .ts_add_waiter

    ; Grow: new_cap = old_cap ? old_cap*2 : 4
    mov ecx, [r12 + AsyncTask.waiters_cap]
    test ecx, ecx
    jz .ts_init_cap
    shl ecx, 1
    jmp .ts_grow
.ts_init_cap:
    mov ecx, 4
.ts_grow:
    mov [r12 + AsyncTask.waiters_cap], ecx
    ; Allocate new array
    lea edi, [ecx * 8]
    push rcx
    call ap_malloc
    pop rcx
    ; Copy old entries
    mov rsi, [r12 + AsyncTask.waiters]
    test rsi, rsi
    jz .ts_no_copy
    mov rdi, rax
    mov edx, [r12 + AsyncTask.n_waiters]
    shl edx, 3
    push rax
    ; memcpy manually (small count)
    xor ecx, ecx
.ts_copy_loop:
    cmp ecx, edx
    jge .ts_copy_done
    mov r8, [rsi + rcx]
    mov [rdi + rcx], r8
    add ecx, 8
    jmp .ts_copy_loop
.ts_copy_done:
    ; Free old array
    mov rdi, rsi
    call ap_free
    pop rax
.ts_no_copy:
    mov [r12 + AsyncTask.waiters], rax

.ts_add_waiter:
    mov eax, [r12 + AsyncTask.n_waiters]
    mov rcx, [r12 + AsyncTask.waiters]
    mov [rcx + rax*8], rbx     ; waiters[n_waiters] = current task
    inc dword [r12 + AsyncTask.n_waiters]
    jmp .ts_ret

.ts_await_done:
    ; Awaited task already done — set send_value to its result, re-enqueue
    mov rax, [r12 + AsyncTask.result]
    mov rdx, [r12 + AsyncTask.result_tag]
    mov [rbx + AsyncTask.send_value], rax
    mov [rbx + AsyncTask.send_tag], rdx
    mov rdi, rbx
    call ready_enqueue
    jmp .ts_ret

.ts_finished:
    ; Coroutine returned (StopIteration) — task is done
    ; The return value is in gen.gi_return_value
    mov rdi, [rbx + AsyncTask.coro]
    mov rax, [rdi + PyGenObject.gi_return_value]
    mov rdx, [rdi + PyGenObject.gi_return_tag]
    mov [rbx + AsyncTask.result], rax
    mov [rbx + AsyncTask.result_tag], rdx
    INCREF_VAL rax, rdx
    mov dword [rbx + AsyncTask.done], 1

    ; Wake waiters
    mov rdi, rbx
    call task_wake_waiters
    jmp .ts_ret

.ts_cancel:
    ; Throw CancelledError into coroutine
    mov rdi, [rbx + AsyncTask.coro]
    lea rsi, [rel exc_CancelledError_type]
    call gen_throw
    ; Mark as done regardless
    mov dword [rbx + AsyncTask.done], 1
    mov dword [rbx + AsyncTask.cancelling], 0
    ; Wake waiters
    mov rdi, rbx
    call task_wake_waiters

.ts_ret:
    pop r12
    pop rbx
    leave
    ret
END_FUNC task_step

;; ============================================================================
;; task_wake_waiters(AsyncTask *task)
;; Iterate waiters, set their send_value to task's result, enqueue them.
;; ============================================================================
DEF_FUNC task_wake_waiters
    push rbx
    push r12
    push r13

    mov rbx, rdi               ; task

    mov r12d, [rbx + AsyncTask.n_waiters]
    test r12d, r12d
    jz .tw_done

    mov r13, [rbx + AsyncTask.waiters]
    xor ecx, ecx

.tw_loop:
    cmp ecx, r12d
    jge .tw_done

    push rcx
    mov rdi, [r13 + rcx*8]    ; waiter task

    ; Set send_value = task's result
    mov rax, [rbx + AsyncTask.result]
    mov rdx, [rbx + AsyncTask.result_tag]
    mov [rdi + AsyncTask.send_value], rax
    mov [rdi + AsyncTask.send_tag], rdx

    ; Enqueue waiter
    call ready_enqueue
    pop rcx
    inc ecx
    jmp .tw_loop

.tw_done:
    ; Clear waiters
    mov dword [rbx + AsyncTask.n_waiters], 0

    pop r13
    pop r12
    pop rbx
    leave
    ret
END_FUNC task_wake_waiters

;; ============================================================================
;; eventloop_run(AsyncTask *root_task) -> fat value
;; Main event loop: drain ready queue, step tasks, wait for I/O.
;; Returns when root_task completes.
;; ============================================================================
ER_ROOT equ 8
ER_FRAME equ 8
DEF_FUNC eventloop_run, ER_FRAME
    push rbx
    push r12

    mov rbx, rdi               ; root task
    mov [rel eventloop + EventLoop.root_task], rdi
    mov [rbp - ER_ROOT], rdi

    ; Enqueue root task
    mov rdi, rbx
    call ready_enqueue

.er_loop:
    ; Process all ready tasks
.er_drain:
    call ready_dequeue
    test rax, rax
    jz .er_wait

    mov rdi, rax
    call task_step
    jmp .er_drain

.er_wait:
    ; Check if root task is done
    mov rbx, [rbp - ER_ROOT]
    cmp dword [rbx + AsyncTask.done], 1
    je .er_done

    ; Wait for I/O events
    mov rax, [rel eventloop + EventLoop.backend]
    call [rax + IOBackend.wait_and_drain]

    jmp .er_loop

.er_done:
    ; Return root task's result
    mov rax, [rbx + AsyncTask.result]
    mov rdx, [rbx + AsyncTask.result_tag]
    INCREF_VAL rax, rdx

    pop r12
    pop rbx
    leave
    ret
END_FUNC eventloop_run

;; ============================================================================
;; task_getattr — attribute lookup for task_type
;; Supports: done, result, cancel, cancelled
;; ============================================================================
DEF_FUNC task_getattr
    push rbx
    push r12

    mov rbx, rdi               ; self (AsyncTask*)
    mov r12, rsi               ; name

    lea rdi, [r12 + PyStrObject.data]
    CSTRING rsi, "done"
    call ap_strcmp
    test eax, eax
    jz .ta_done_method

    lea rdi, [r12 + PyStrObject.data]
    CSTRING rsi, "result"
    call ap_strcmp
    test eax, eax
    jz .ta_result_method

    lea rdi, [r12 + PyStrObject.data]
    CSTRING rsi, "cancel"
    call ap_strcmp
    test eax, eax
    jz .ta_cancel_method

    lea rdi, [r12 + PyStrObject.data]
    CSTRING rsi, "cancelled"
    call ap_strcmp
    test eax, eax
    jz .ta_cancelled_method

    RET_NULL
    pop r12
    pop rbx
    leave
    ret

.ta_done_method:
    call _get_task_done_builtin
    mov rdi, rax
    call obj_incref
    mov edx, TAG_PTR
    pop r12
    pop rbx
    leave
    ret

.ta_result_method:
    call _get_task_result_builtin
    mov rdi, rax
    call obj_incref
    mov edx, TAG_PTR
    pop r12
    pop rbx
    leave
    ret

.ta_cancel_method:
    call _get_task_cancel_builtin
    mov rdi, rax
    call obj_incref
    mov edx, TAG_PTR
    pop r12
    pop rbx
    leave
    ret

.ta_cancelled_method:
    call _get_task_cancelled_builtin
    mov rdi, rax
    call obj_incref
    mov edx, TAG_PTR
    pop r12
    pop rbx
    leave
    ret
END_FUNC task_getattr

;; ============================================================================
;; task_iter_self — tp_iter for task: return self
;; ============================================================================
task_iter_self:
    inc qword [rdi + PyObject.ob_refcnt]
    mov rax, rdi
    ret
END_FUNC task_iter_self

;; ============================================================================
;; task_iternext — tp_iternext for task: yield TAG_TASK or stop
;; When awaited, yields itself as TAG_TASK so event loop can track dependency.
;; ============================================================================
task_iternext:
    ; If done, return NULL (signals StopIteration to SEND)
    cmp dword [rdi + AsyncTask.done], 1
    je .ti_done

    ; Not done — yield self as TAG_TASK
    inc qword [rdi + PyObject.ob_refcnt]
    mov rax, rdi
    mov edx, TAG_TASK
    ret

.ti_done:
    ; Return the result
    mov rax, [rdi + AsyncTask.result]
    mov rdx, [rdi + AsyncTask.result_tag]
    INCREF_VAL rax, rdx
    ; Return NULL to signal completion
    RET_NULL
    ret
END_FUNC task_iternext

;; ============================================================================
;; task_repr
;; ============================================================================
task_repr:
    lea rdi, [rel task_repr_str]
    jmp str_from_cstr
END_FUNC task_repr

;; ============================================================================
;; Builtin method implementations for task
;; ============================================================================

;; task.done() -> bool
DEF_FUNC _task_done_impl
    mov rax, [rdi]             ; self = args[0]
    mov eax, [rax + AsyncTask.done]
    mov edx, TAG_BOOL
    leave
    ret
END_FUNC _task_done_impl

;; task.result() -> value
DEF_FUNC _task_result_impl
    mov rax, [rdi]
    cmp dword [rax + AsyncTask.done], 0
    je .tr_not_done
    ; Check for exception
    mov rcx, [rax + AsyncTask.exception]
    test rcx, rcx
    jnz .tr_exception
    ; Return result
    mov rdx, [rax + AsyncTask.result_tag]
    mov rax, [rax + AsyncTask.result]
    INCREF_VAL rax, rdx
    leave
    ret

.tr_not_done:
    lea rdi, [rel exc_RuntimeError_type]
    CSTRING rsi, "Result is not ready"
    call raise_exception

.tr_exception:
    mov rdi, rcx
    call raise_exception_obj
END_FUNC _task_result_impl

;; task.cancel() -> True
DEF_FUNC _task_cancel_impl
    mov rax, [rdi]
    mov dword [rax + AsyncTask.cancelling], 1
    mov eax, 1
    mov edx, TAG_BOOL
    leave
    ret
END_FUNC _task_cancel_impl

;; task.cancelled() -> bool
DEF_FUNC _task_cancelled_impl
    mov rax, [rdi]
    mov eax, [rax + AsyncTask.cancelling]
    mov edx, TAG_BOOL
    leave
    ret
END_FUNC _task_cancelled_impl

;; Lazy-init builtin caches
DEF_FUNC_LOCAL _get_task_done_builtin
    mov rax, [rel _task_done_cache]
    test rax, rax
    jnz .ret
    lea rdi, [rel _task_done_impl]
    CSTRING rsi, "done"
    call builtin_func_new
    mov [rel _task_done_cache], rax
.ret:
    leave
    ret
END_FUNC _get_task_done_builtin

DEF_FUNC_LOCAL _get_task_result_builtin
    mov rax, [rel _task_result_cache]
    test rax, rax
    jnz .ret
    lea rdi, [rel _task_result_impl]
    CSTRING rsi, "result"
    call builtin_func_new
    mov [rel _task_result_cache], rax
.ret:
    leave
    ret
END_FUNC _get_task_result_builtin

DEF_FUNC_LOCAL _get_task_cancel_builtin
    mov rax, [rel _task_cancel_cache]
    test rax, rax
    jnz .ret
    lea rdi, [rel _task_cancel_impl]
    CSTRING rsi, "cancel"
    call builtin_func_new
    mov [rel _task_cancel_cache], rax
.ret:
    leave
    ret
END_FUNC _get_task_cancel_builtin

DEF_FUNC_LOCAL _get_task_cancelled_builtin
    mov rax, [rel _task_cancelled_cache]
    test rax, rax
    jnz .ret
    lea rdi, [rel _task_cancelled_impl]
    CSTRING rsi, "cancelled"
    call builtin_func_new
    mov [rel _task_cancelled_cache], rax
.ret:
    leave
    ret
END_FUNC _get_task_cancelled_builtin

;; ============================================================================
;; Data section
;; ============================================================================
section .data

task_name_str:  db "Task", 0
task_repr_str:  db "<Task>", 0

_task_done_cache: dq 0
_task_result_cache: dq 0
_task_cancel_cache: dq 0
_task_cancelled_cache: dq 0

align 8
global task_type
task_type:
    dq 1                        ; ob_refcnt (immortal)
    dq type_type                ; ob_type
    dq task_name_str            ; tp_name
    dq AsyncTask_size           ; tp_basicsize
    dq task_dealloc             ; tp_dealloc
    dq task_repr                ; tp_repr
    dq task_repr                ; tp_str
    dq 0                        ; tp_hash
    dq 0                        ; tp_call
    dq task_getattr             ; tp_getattr
    dq 0                        ; tp_setattr
    dq 0                        ; tp_richcompare
    dq task_iter_self           ; tp_iter
    dq task_iternext            ; tp_iternext
    dq 0                        ; tp_init
    dq 0                        ; tp_new
    dq 0                        ; tp_as_number
    dq 0                        ; tp_as_sequence
    dq 0                        ; tp_as_mapping
    dq 0                        ; tp_base
    dq 0                        ; tp_dict
    dq 0                        ; tp_mro
    dq 0                        ; tp_flags
    dq 0                        ; tp_bases

section .bss
align 8
global eventloop
eventloop: resb EventLoop_size

section .data
