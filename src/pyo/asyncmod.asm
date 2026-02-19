; asyncmod.asm - asyncio builtin module
;
; Provides: asyncio.run, asyncio.sleep, asyncio.create_task, asyncio.gather,
;           asyncio.wait_for, asyncio.get_running_loop
;
; asyncio.run(coro) — main entry point
; asyncio.sleep(delay) — suspend for delay seconds
; asyncio.create_task(coro) — wrap coro in a Task and schedule it
; asyncio.gather(*coros) — run multiple coros concurrently

%include "macros.inc"
%include "object.inc"
%include "types.inc"
%include "builtins.inc"
%include "errcodes.inc"
%include "eventloop.inc"

extern ap_malloc
extern ap_free
extern obj_decref
extern obj_incref
extern obj_dealloc
extern str_from_cstr_heap
extern float_from_f64
extern dict_new
extern dict_set
extern module_new
extern builtin_func_new
extern none_singleton
extern raise_exception
extern raise_exception_obj
extern exc_TypeError_type
extern exc_RuntimeError_type
extern exc_TimeoutError_type
extern exc_CancelledError_type
extern exc_new
extern type_type
extern coro_type
extern task_new
extern task_type
extern eventloop_init
extern eventloop_teardown
extern eventloop_run
extern eventloop
extern ready_enqueue
extern task_add_waiter
extern str_from_cstr
extern list_new
extern list_append
extern asyncio_open_connection_func
extern asyncio_start_server_func
extern stream_reader_type
extern stream_writer_type

; SleepAwaitable type methods

;; ============================================================================
;; asyncio_run(args, nargs) — asyncio.run(coro)
;; Main event loop entry point.
;; ============================================================================
AR_CORO  equ 8
AR_TASK  equ 16
AR_FRAME equ 16
DEF_FUNC asyncio_run_func, AR_FRAME
    push rbx
    push r12

    cmp rsi, 1
    jne .ar_error

    ; Get coroutine from args[0]
    mov rax, [rdi]             ; coro payload
    mov edx, [rdi + 8]        ; coro tag
    cmp edx, TAG_PTR
    jne .ar_type_error

    mov rbx, rax               ; rbx = coro

    ; Check if loop already running
    cmp dword [rel eventloop + EventLoop.running], 1
    je .ar_reentrant

    ; Initialize event loop
    call eventloop_init

    ; Create root task
    mov rdi, rbx
    call task_new
    mov r12, rax               ; r12 = root task

    ; Run the event loop
    mov rdi, r12
    call eventloop_run
    ; rax = result payload, edx = tag
    push rdx
    push rax

    ; Teardown event loop
    call eventloop_teardown

    ; DECREF root task
    mov rdi, r12
    call obj_decref

    pop rax
    pop rdx

    pop r12
    pop rbx
    leave
    ret

.ar_error:
    lea rdi, [rel exc_TypeError_type]
    CSTRING rsi, "asyncio.run() takes exactly 1 argument"
    call raise_exception

.ar_type_error:
    lea rdi, [rel exc_TypeError_type]
    CSTRING rsi, "asyncio.run() requires a coroutine"
    call raise_exception

.ar_reentrant:
    lea rdi, [rel exc_RuntimeError_type]
    CSTRING rsi, "asyncio.run() cannot be called from a running event loop"
    call raise_exception
END_FUNC asyncio_run_func

;; ============================================================================
;; asyncio_sleep(args, nargs) — asyncio.sleep(delay)
;; Returns a SleepAwaitable.
;; ============================================================================
DEF_FUNC asyncio_sleep_func
    push rbx

    cmp rsi, 1
    jne .as_error

    ; Get delay from args[0]
    mov rax, [rdi]             ; payload
    mov edx, [rdi + 8]        ; tag

    ; Convert to nanoseconds
    ; Supports: float (seconds), int (seconds)
    cmp edx, TAG_FLOAT
    je .as_float
    cmp edx, TAG_SMALLINT
    je .as_int
    jmp .as_type_error

.as_float:
    ; payload = IEEE 754 double bits representing seconds
    movq xmm0, rax
    ; Multiply by 1e9 to get nanoseconds
    movsd xmm1, [rel async_1e9]
    mulsd xmm0, xmm1
    cvttsd2si rbx, xmm0       ; rbx = delay_ns
    jmp .as_create

.as_int:
    ; payload = seconds as integer
    mov rbx, rax
    imul rbx, 1000000000      ; rbx = delay_ns

.as_create:
    ; Allocate SleepAwaitable
    mov edi, SleepAwaitable_size
    call ap_malloc
    mov qword [rax + SleepAwaitable.ob_refcnt], 1
    lea rcx, [rel sleep_awaitable_type]
    mov [rax + SleepAwaitable.ob_type], rcx
    mov [rax + SleepAwaitable.delay_ns], rbx
    mov dword [rax + SleepAwaitable.yielded], 0

    mov edx, TAG_PTR
    pop rbx
    leave
    ret

.as_error:
    lea rdi, [rel exc_TypeError_type]
    CSTRING rsi, "asyncio.sleep() takes exactly 1 argument"
    call raise_exception

.as_type_error:
    lea rdi, [rel exc_TypeError_type]
    CSTRING rsi, "asyncio.sleep() delay must be a number"
    call raise_exception
END_FUNC asyncio_sleep_func

;; ============================================================================
;; sleep_awaitable_iter_self — tp_iter for SleepAwaitable (return self)
sleep_awaitable_iter_self:
    inc qword [rdi + PyObject.ob_refcnt]
    mov rax, rdi
    ret
END_FUNC sleep_awaitable_iter_self

;; sleep_awaitable_iternext — tp_iternext for SleepAwaitable
;; First call: yield (delay_ns, TAG_SLEEP). Second call: return NULL (done).
;; ============================================================================
sleep_awaitable_iternext:
    ; rdi = SleepAwaitable*
    cmp dword [rdi + SleepAwaitable.yielded], 0
    jne .sai_done

    ; First call: yield TAG_SLEEP with delay_ns
    mov dword [rdi + SleepAwaitable.yielded], 1
    mov rax, [rdi + SleepAwaitable.delay_ns]
    mov edx, TAG_SLEEP
    ret

.sai_done:
    ; Already yielded — done, return None via StopIteration
    RET_NULL
    ret
END_FUNC sleep_awaitable_iternext

;; ============================================================================
;; sleep_awaitable_dealloc
;; ============================================================================
sleep_awaitable_dealloc:
    ; Simple object with no refs to DECREF
    jmp ap_free                ; tail call
END_FUNC sleep_awaitable_dealloc

;; ============================================================================
;; asyncio_wait_for_func(args, nargs) — asyncio.wait_for(coro, timeout)
;; Creates inner task, wraps in WaitForAwaitable.
;; ============================================================================
WF_INNER equ 8
WF_DELAY equ 16
WF_FRAME equ 16
DEF_FUNC asyncio_wait_for_func, WF_FRAME
    push rbx

    cmp rsi, 2
    jne .wf_error

    ; args[0] = coro, args[1] = timeout
    push rdi                   ; save args

    ; Create inner task from coro
    mov rdi, [rdi]             ; coro = args[0] payload
    call task_new
    mov [rbp - WF_INNER], rax  ; save inner task

    ; Enqueue inner task on ready queue
    mov rdi, rax
    call ready_enqueue

    ; Convert timeout (args[1]) to nanoseconds
    pop rdi                    ; restore args
    mov rax, [rdi + 16]       ; args[1] payload
    mov edx, [rdi + 24]       ; args[1] tag

    cmp edx, TAG_FLOAT
    je .wf_float_timeout
    cmp edx, TAG_SMALLINT
    je .wf_int_timeout
    jmp .wf_type_error

.wf_float_timeout:
    movq xmm0, rax
    movsd xmm1, [rel async_1e9]
    mulsd xmm0, xmm1
    cvttsd2si rbx, xmm0       ; rbx = timeout_ns
    jmp .wf_create

.wf_int_timeout:
    mov rbx, rax
    imul rbx, 1000000000      ; rbx = timeout_ns

.wf_create:
    mov [rbp - WF_DELAY], rbx

    ; Allocate WaitForAwaitable
    mov edi, WaitForAwaitable_size
    call ap_malloc
    mov qword [rax + WaitForAwaitable.ob_refcnt], 1
    lea rcx, [rel wait_for_awaitable_type]
    mov [rax + WaitForAwaitable.ob_type], rcx
    mov rcx, [rbp - WF_INNER]
    mov [rax + WaitForAwaitable.inner_task], rcx  ; transfer ownership (task_new ref)
    mov rcx, [rbp - WF_DELAY]
    mov [rax + WaitForAwaitable.timeout_ns], rcx
    mov dword [rax + WaitForAwaitable.state], 0
    mov qword [rax + WaitForAwaitable.outer_task], 0
    mov qword [rax + WaitForAwaitable.gi_return_value], 0
    mov qword [rax + WaitForAwaitable.gi_return_tag], 0

    mov edx, TAG_PTR
    pop rbx
    leave
    ret

.wf_error:
    lea rdi, [rel exc_TypeError_type]
    CSTRING rsi, "asyncio.wait_for() takes exactly 2 arguments"
    call raise_exception

.wf_type_error:
    lea rdi, [rel exc_TypeError_type]
    CSTRING rsi, "asyncio.wait_for() timeout must be a number"
    call raise_exception
END_FUNC asyncio_wait_for_func

;; ============================================================================
;; wait_for_awaitable_iter_self — tp_iter for WaitForAwaitable (return self)
;; ============================================================================
wait_for_awaitable_iter_self:
    inc qword [rdi + PyObject.ob_refcnt]
    mov rax, rdi
    ret
END_FUNC wait_for_awaitable_iter_self

;; ============================================================================
;; wait_for_awaitable_iternext — tp_iternext for WaitForAwaitable
;; State 0: first call — yield self as TAG_WAIT_FOR for task_step to intercept.
;; State 1: resumed — check inner task, return result or raise TimeoutError.
;; State 2+: exhausted.
;; ============================================================================
wait_for_awaitable_iternext:
    ; rdi = WaitForAwaitable*
    mov eax, [rdi + WaitForAwaitable.state]

    cmp eax, 0
    je .wfai_first

    cmp eax, 1
    je .wfai_check

    ; State 2+: exhausted
    RET_NULL
    ret

.wfai_first:
    ; State 0 → 1: yield (self, TAG_WAIT_FOR) for task_step
    mov dword [rdi + WaitForAwaitable.state], 1
    INCREF rdi
    mov rax, rdi
    mov edx, TAG_WAIT_FOR
    ret

.wfai_check:
    ; State 1 → 2: check inner task
    mov dword [rdi + WaitForAwaitable.state], 2
    push rbx
    mov rbx, rdi              ; rbx = WaitForAwaitable

    ; Check if inner task completed
    mov rax, [rbx + WaitForAwaitable.inner_task]
    cmp dword [rax + AsyncTask.done], 1
    jne .wfai_timeout

    ; Inner task done — check for exception
    mov rax, [rbx + WaitForAwaitable.inner_task]
    cmp qword [rax + AsyncTask.exception], 0
    jne .wfai_inner_exc

    ; Copy result to gi_return_value for SEND exhaustion protocol
    mov rax, [rbx + WaitForAwaitable.inner_task]
    mov rcx, [rax + AsyncTask.result]
    mov rdx, [rax + AsyncTask.result_tag]
    mov [rbx + WaitForAwaitable.gi_return_value], rcx
    mov [rbx + WaitForAwaitable.gi_return_tag], rdx
    INCREF_VAL rcx, rdx

    RET_NULL
    pop rbx
    ret

.wfai_inner_exc:
    ; Inner task had exception — re-raise it
    mov rax, [rbx + WaitForAwaitable.inner_task]
    mov rdi, [rax + AsyncTask.exception]
    INCREF rdi
    call raise_exception_obj
    RET_NULL
    pop rbx
    ret

.wfai_timeout:
    ; Inner task not done — cancel it and raise TimeoutError
    mov rax, [rbx + WaitForAwaitable.inner_task]
    mov dword [rax + AsyncTask.cancelling], 1

    lea rdi, [rel exc_TimeoutError_type]
    CSTRING rsi, "asyncio.wait_for() timed out"
    call raise_exception
    RET_NULL
    pop rbx
    ret
END_FUNC wait_for_awaitable_iternext

;; ============================================================================
;; wait_for_awaitable_dealloc — tp_dealloc for WaitForAwaitable
;; ============================================================================
wait_for_awaitable_dealloc:
    push rdi                   ; save self
    ; DECREF inner_task
    mov rdi, [rdi + WaitForAwaitable.inner_task]
    test rdi, rdi
    jz .wfad_no_inner
    call obj_decref
.wfad_no_inner:
    pop rdi
    push rdi
    ; XDECREF_VAL gi_return_value
    mov rax, [rdi + WaitForAwaitable.gi_return_value]
    mov rdx, [rdi + WaitForAwaitable.gi_return_tag]
    XDECREF_VAL rax, rdx
    pop rdi
    jmp ap_free                ; tail call
END_FUNC wait_for_awaitable_dealloc

;; ============================================================================
;; asyncio_create_task(args, nargs) — asyncio.create_task(coro)
;; ============================================================================
ACT_TASK equ 8
ACT_FRAME equ 16
DEF_FUNC asyncio_create_task_func, ACT_FRAME
    cmp rsi, 1
    jne .act_error

    mov rdi, [rdi]             ; coro = args[0]
    call task_new
    mov [rbp - ACT_TASK], rax  ; save task (stack-aligned)

    ; Enqueue the new task
    mov rdi, rax
    call ready_enqueue

    mov rax, [rbp - ACT_TASK]
    mov edx, TAG_PTR
    leave
    ret

.act_error:
    lea rdi, [rel exc_TypeError_type]
    CSTRING rsi, "asyncio.create_task() takes exactly 1 argument"
    call raise_exception
END_FUNC asyncio_create_task_func

;; ============================================================================
;; asyncio_gather(args, nargs) — asyncio.gather(*coros)
;; Creates tasks for all args, returns a GatherAwaitable.
;; For simplicity: create tasks, return a list of results when all done.
;; Implementation: returns a coroutine that awaits all tasks.
;;
;; Since we can't easily create a gather coroutine in asm, we create all tasks
;; and return a special GatherAwaitable that the event loop recognizes.
;; For now: simply create tasks and return a list placeholder.
;; ============================================================================
DEF_FUNC asyncio_gather_func
    push rbx
    push r12
    push r13

    mov rbx, rdi               ; args
    mov r12, rsi               ; nargs

    ; Create a list to collect tasks
    call list_new
    mov r13, rax               ; r13 = result list

    ; Create a task for each coroutine arg
    xor ecx, ecx
.ag_loop:
    cmp rcx, r12
    jge .ag_done
    push rcx

    ; Get coro from args[i] (16 bytes per arg, scale by shifting)
    mov rdi, rcx
    shl rdi, 4                 ; * 16
    mov edx, [rbx + rdi + 8]  ; tag
    cmp edx, TAG_PTR
    jne .ag_type_error
    mov rdi, [rbx + rdi]      ; payload
    call task_new
    push rax

    ; Enqueue the task
    mov rdi, rax
    call ready_enqueue

    ; Add to result list
    pop rax
    push rax                   ; save task for DECREF
    mov rdi, r13
    mov rsi, rax
    mov edx, TAG_PTR
    call list_append

    ; DECREF task (list_append INCREFs, release our initial ref)
    pop rdi
    call obj_decref

    pop rcx
    inc rcx
    jmp .ag_loop

.ag_done:
    ; Return the task list
    ; TODO: Properly await all tasks and return results
    ; For now, return the list of tasks (caller must await each)
    mov rax, r13
    mov edx, TAG_PTR

    pop r13
    pop r12
    pop rbx
    leave
    ret

.ag_type_error:
    pop rcx                    ; restore loop counter from stack
    ; DECREF the partially-built list
    mov rdi, r13
    call obj_decref
    lea rdi, [rel exc_TypeError_type]
    CSTRING rsi, "asyncio.gather() arguments must be coroutines"
    call raise_exception
END_FUNC asyncio_gather_func

;; ============================================================================
;; asyncio_get_running_loop(args, nargs)
;; ============================================================================
DEF_FUNC asyncio_get_running_loop_func
    cmp dword [rel eventloop + EventLoop.running], 1
    jne .grl_error

    ; Return None as a placeholder for the loop object
    lea rax, [rel none_singleton]
    mov rdi, rax
    push rax
    call obj_incref
    pop rax
    mov edx, TAG_PTR
    leave
    ret

.grl_error:
    lea rdi, [rel exc_RuntimeError_type]
    CSTRING rsi, "no running event loop"
    call raise_exception
END_FUNC asyncio_get_running_loop_func

;; ============================================================================
;; asyncio_module_create() -> PyObject*
;; Creates and returns the asyncio module.
;; ============================================================================
global asyncio_module_create
DEF_FUNC asyncio_module_create
    push rbx
    push r12

    ; Create module dict
    call dict_new
    mov r12, rax

    ; Helper macro equivalent: add function to dict
    ; Pattern: builtin_func_new -> str_from_cstr_heap -> dict_set -> decref key+func

    ; asyncio.run
    lea rdi, [rel asyncio_run_func]
    lea rsi, [rel am_run]
    call builtin_func_new
    push rax
    lea rdi, [rel am_run]
    call str_from_cstr_heap
    push rax
    mov rdi, r12
    mov rsi, rax
    mov rdx, [rsp + 8]
    mov ecx, TAG_PTR
    mov r8d, TAG_PTR
    call dict_set
    pop rdi
    call obj_decref
    pop rdi
    call obj_decref

    ; asyncio.sleep
    lea rdi, [rel asyncio_sleep_func]
    lea rsi, [rel am_sleep]
    call builtin_func_new
    push rax
    lea rdi, [rel am_sleep]
    call str_from_cstr_heap
    push rax
    mov rdi, r12
    mov rsi, rax
    mov rdx, [rsp + 8]
    mov ecx, TAG_PTR
    mov r8d, TAG_PTR
    call dict_set
    pop rdi
    call obj_decref
    pop rdi
    call obj_decref

    ; asyncio.create_task
    lea rdi, [rel asyncio_create_task_func]
    lea rsi, [rel am_create_task]
    call builtin_func_new
    push rax
    lea rdi, [rel am_create_task]
    call str_from_cstr_heap
    push rax
    mov rdi, r12
    mov rsi, rax
    mov rdx, [rsp + 8]
    mov ecx, TAG_PTR
    mov r8d, TAG_PTR
    call dict_set
    pop rdi
    call obj_decref
    pop rdi
    call obj_decref

    ; asyncio.gather
    lea rdi, [rel asyncio_gather_func]
    lea rsi, [rel am_gather]
    call builtin_func_new
    push rax
    lea rdi, [rel am_gather]
    call str_from_cstr_heap
    push rax
    mov rdi, r12
    mov rsi, rax
    mov rdx, [rsp + 8]
    mov ecx, TAG_PTR
    mov r8d, TAG_PTR
    call dict_set
    pop rdi
    call obj_decref
    pop rdi
    call obj_decref

    ; asyncio.wait_for
    lea rdi, [rel asyncio_wait_for_func]
    lea rsi, [rel am_wait_for]
    call builtin_func_new
    push rax
    lea rdi, [rel am_wait_for]
    call str_from_cstr_heap
    push rax
    mov rdi, r12
    mov rsi, rax
    mov rdx, [rsp + 8]
    mov ecx, TAG_PTR
    mov r8d, TAG_PTR
    call dict_set
    pop rdi
    call obj_decref
    pop rdi
    call obj_decref

    ; asyncio.get_running_loop
    lea rdi, [rel asyncio_get_running_loop_func]
    lea rsi, [rel am_get_running_loop]
    call builtin_func_new
    push rax
    lea rdi, [rel am_get_running_loop]
    call str_from_cstr_heap
    push rax
    mov rdi, r12
    mov rsi, rax
    mov rdx, [rsp + 8]
    mov ecx, TAG_PTR
    mov r8d, TAG_PTR
    call dict_set
    pop rdi
    call obj_decref
    pop rdi
    call obj_decref

    ; asyncio.open_connection
    lea rdi, [rel asyncio_open_connection_func]
    lea rsi, [rel am_open_connection]
    call builtin_func_new
    push rax
    lea rdi, [rel am_open_connection]
    call str_from_cstr_heap
    push rax
    mov rdi, r12
    mov rsi, rax
    mov rdx, [rsp + 8]
    mov ecx, TAG_PTR
    mov r8d, TAG_PTR
    call dict_set
    pop rdi
    call obj_decref
    pop rdi
    call obj_decref

    ; asyncio.start_server
    lea rdi, [rel asyncio_start_server_func]
    lea rsi, [rel am_start_server]
    call builtin_func_new
    push rax
    lea rdi, [rel am_start_server]
    call str_from_cstr_heap
    push rax
    mov rdi, r12
    mov rsi, rax
    mov rdx, [rsp + 8]
    mov ecx, TAG_PTR
    mov r8d, TAG_PTR
    call dict_set
    pop rdi
    call obj_decref
    pop rdi
    call obj_decref

    ; asyncio.StreamReader (type)
    lea rdi, [rel am_stream_reader]
    call str_from_cstr_heap
    push rax
    mov rdi, r12
    mov rsi, rax
    lea rdx, [rel stream_reader_type]
    mov ecx, TAG_PTR
    mov r8d, TAG_PTR
    call dict_set
    pop rdi
    call obj_decref

    ; asyncio.StreamWriter (type)
    lea rdi, [rel am_stream_writer]
    call str_from_cstr_heap
    push rax
    mov rdi, r12
    mov rsi, rax
    lea rdx, [rel stream_writer_type]
    mov ecx, TAG_PTR
    mov r8d, TAG_PTR
    call dict_set
    pop rdi
    call obj_decref

    ; Create module object
    lea rdi, [rel am_asyncio]
    call str_from_cstr_heap
    mov rdi, rax
    mov rsi, r12
    call module_new

    pop r12
    pop rbx
    leave
    ret
END_FUNC asyncio_module_create

;; ============================================================================
;; Data section
;; ============================================================================
section .rodata
align 8
async_1e9: dq 0x41CDCD6500000000   ; 1e9 as IEEE 754 double

am_asyncio:          db "asyncio", 0
am_run:              db "run", 0
am_sleep:            db "sleep", 0
am_create_task:      db "create_task", 0
am_gather:           db "gather", 0
am_get_running_loop: db "get_running_loop", 0
am_open_connection:  db "open_connection", 0
am_start_server:     db "start_server", 0
am_wait_for:         db "wait_for", 0
am_stream_reader:    db "StreamReader", 0
am_stream_writer:    db "StreamWriter", 0

sleep_awaitable_name: db "SleepAwaitable", 0
wait_for_awaitable_name: db "WaitForAwaitable", 0

section .data

align 8
sleep_awaitable_type:
    dq 1                        ; ob_refcnt (immortal)
    dq type_type                ; ob_type
    dq sleep_awaitable_name     ; tp_name
    dq SleepAwaitable_size      ; tp_basicsize
    dq sleep_awaitable_dealloc  ; tp_dealloc
    dq 0                        ; tp_repr
    dq 0                        ; tp_str
    dq 0                        ; tp_hash
    dq 0                        ; tp_call
    dq 0                        ; tp_getattr
    dq 0                        ; tp_setattr
    dq 0                        ; tp_richcompare
    dq sleep_awaitable_iter_self ; tp_iter (return self — __await__ protocol)
    dq sleep_awaitable_iternext ; tp_iternext
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

align 8
wait_for_awaitable_type:
    dq 1                        ; ob_refcnt (immortal)
    dq type_type                ; ob_type
    dq wait_for_awaitable_name  ; tp_name
    dq WaitForAwaitable_size    ; tp_basicsize
    dq wait_for_awaitable_dealloc ; tp_dealloc
    dq 0                        ; tp_repr
    dq 0                        ; tp_str
    dq 0                        ; tp_hash
    dq 0                        ; tp_call
    dq 0                        ; tp_getattr
    dq 0                        ; tp_setattr
    dq 0                        ; tp_richcompare
    dq wait_for_awaitable_iter_self ; tp_iter
    dq wait_for_awaitable_iternext ; tp_iternext
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
