; timemod.asm - time module implementation
; Provides time.process_time() and time.monotonic()

%include "macros.inc"
%include "object.inc"

extern ap_malloc
extern obj_decref
extern obj_incref
extern str_from_cstr_heap
extern float_from_f64
extern dict_new
extern dict_set
extern module_new
extern builtin_func_new
extern none_singleton
extern raise_exception
extern exc_TypeError_type

; Clock IDs for clock_gettime
CLOCK_MONOTONIC          equ 1
CLOCK_PROCESS_CPUTIME_ID equ 2

;; ============================================================================
;; time_process_time_func(PyObject **args, int64_t nargs) -> rax = Value
;; Returns process CPU time as a float (seconds)
;; ============================================================================
; A struct timespec, filled by clock_gettime: tv_sec then tv_nsec.
TS_SEC    equ 16
TS_NSEC   equ 8
DEF_FUNC time_process_time_func, 16
    cmp rsi, 0
    jne .pt_error

    ; clock_gettime(CLOCK_PROCESS_CPUTIME_ID, &timespec)
    ; timespec is at [rbp - TS_SEC]: tv_sec at [rbp - TS_SEC], tv_nsec at [rbp - TS_NSEC]
    mov eax, 228            ; __NR_clock_gettime
    mov edi, CLOCK_PROCESS_CPUTIME_ID
    lea rsi, [rbp - TS_SEC]
    syscall

    ; Convert to float: seconds + nanoseconds/1e9
    ; tv_sec at [rbp - TS_SEC], tv_nsec at [rbp - TS_NSEC]
    cvtsi2sd xmm0, qword [rbp - TS_SEC]    ; seconds
    cvtsi2sd xmm1, qword [rbp - TS_NSEC]     ; nanoseconds
    movsd xmm2, [rel tm_1e9]
    divsd xmm1, xmm2
    addsd xmm0, xmm1

    call float_from_f64
    leave
    V_PACK rax, rdx             ; builtins return one Value
    ret

.pt_error:
    RAISE exc_TypeError_type, "process_time() takes no arguments"
END_FUNC time_process_time_func

;; ============================================================================
;; time_monotonic_func(PyObject **args, int64_t nargs) -> rax = Value
;; Returns monotonic clock as a float (seconds)
;; ============================================================================
DEF_FUNC time_monotonic_func, 16
    cmp rsi, 0
    jne .mono_error

    mov eax, 228            ; __NR_clock_gettime
    mov edi, CLOCK_MONOTONIC
    lea rsi, [rbp - TS_SEC]
    syscall

    cvtsi2sd xmm0, qword [rbp - TS_SEC]
    cvtsi2sd xmm1, qword [rbp - TS_NSEC]
    movsd xmm2, [rel tm_1e9]
    divsd xmm1, xmm2
    addsd xmm0, xmm1

    call float_from_f64
    leave
    V_PACK rax, rdx             ; builtins return one Value
    ret

.mono_error:
    RAISE exc_TypeError_type, "monotonic() takes no arguments"
END_FUNC time_monotonic_func

;; ============================================================================
;; time_module_create() -> PyObject*
;; Creates and returns the time module
;; ============================================================================
DEF_FUNC time_module_create
    push rbx
    push r12

    ; Create module dict
    call dict_new
    mov r12, rax            ; r12 = module dict

    ; Add process_time function
    lea rdi, [rel time_process_time_func]
    lea rsi, [rel tm_process_time]
    call builtin_func_new
    push rax
    lea rdi, [rel tm_process_time]
    call str_from_cstr_heap
    push rax
    mov rdi, r12
    mov rsi, rax
    mov rdx, [rsp + 8]
    call dict_set
    pop rdi
    call obj_decref
    pop rdi
    call obj_decref

    ; Add monotonic function
    lea rdi, [rel time_monotonic_func]
    lea rsi, [rel tm_monotonic]
    call builtin_func_new
    push rax
    lea rdi, [rel tm_monotonic]
    call str_from_cstr_heap
    push rax
    mov rdi, r12
    mov rsi, rax
    mov rdx, [rsp + 8]
    call dict_set
    pop rdi
    call obj_decref
    pop rdi
    call obj_decref

    ; Create module object
    lea rdi, [rel tm_time]
    call str_from_cstr_heap
    push rax                ; save name for DECREF
    mov rdi, rax
    mov rsi, r12
    call module_new
    mov rbx, rax            ; save module
    pop rdi                 ; DECREF name (module_new INCREF'd)
    call obj_decref
    mov rdi, r12            ; DECREF dict (module_new INCREF'd)
    call obj_decref
    mov rax, rbx            ; return module

    pop r12
    pop rbx
    leave
    ret
END_FUNC time_module_create

;; ============================================================================
;; Data
;; ============================================================================
section .rodata
align 8
tm_1e9: dq 0x41cdcd6500000000     ; 1e9 as IEEE 754 double

tm_time:         db "time", 0
tm_process_time: db "process_time", 0
tm_monotonic:    db "monotonic", 0
