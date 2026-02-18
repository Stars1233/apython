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
;; asyncio_create_task(args, nargs) — asyncio.create_task(coro)
;; ============================================================================
DEF_FUNC asyncio_create_task_func
    cmp rsi, 1
    jne .act_error

    mov rdi, [rdi]             ; coro = args[0]
    call task_new
    push rax

    ; Enqueue the new task
    mov rdi, rax
    call ready_enqueue

    pop rax
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
am_stream_reader:    db "StreamReader", 0
am_stream_writer:    db "StreamWriter", 0

sleep_awaitable_name: db "SleepAwaitable", 0

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
