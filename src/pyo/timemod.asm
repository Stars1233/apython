; timemod.asm - time module implementation
; Provides time.time(), time.sleep(), time.process_time() and time.monotonic()

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
CLOCK_REALTIME           equ 0
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
;; time_time_func(PyObject **args, int64_t nargs) -> rax = Value
;; Seconds since the epoch, as a float.  The wall clock, where monotonic is
;; the one that cannot go backwards.
;; ============================================================================
DEF_FUNC time_time_func, 16
    cmp rsi, 0
    jne .time_error

    mov eax, 228            ; __NR_clock_gettime
    mov edi, CLOCK_REALTIME
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

.time_error:
    RAISE exc_TypeError_type, "time() takes no arguments"
END_FUNC time_time_func

;; ============================================================================
;; time_sleep_func(PyObject **args, int64_t nargs) -> None
;; nanosleep, with the argument in seconds as an int or a float.
;; ============================================================================
TSL_SEC   equ 16
TSL_NSEC  equ 8
TSL_ARG   equ 24            ; the argument Value, kept for the overflow message
TSL_FRAME equ 32            ; + 0 pushes = 32
DEF_FUNC time_sleep_func, TSL_FRAME
    cmp rsi, 1
    jne .sleep_error

    mov rdi, [rdi]
    mov [rbp - TSL_ARG], rdi
    V_UNPACK rdi, rsi
    extern float_binop_accepts
    push rdi
    push rsi
    call float_binop_accepts
    pop rsi
    pop rdi
    test eax, eax
    jz .sleep_type_error
    extern float_to_f64
    call float_to_f64           ; seconds, as a double

    ; NaN is its own answer, and CPython gives it its own wording -- it is not
    ; "negative", which is what this used to call it.
    ucomisd xmm0, xmm0
    jp .sleep_nan

    ; The delay has to survive the trip through nanoseconds.  CPython's
    ; _PyTime_t is an int64 count of them, so it rejects anything whose
    ; product with 1e9 will not fit one, and that check has to come BEFORE the
    ; negative test: -inf and -1e300 are overflows in CPython, not "sleep
    ; length must be non-negative".
    ;
    ; Without it, cvttsd2si below answered INT64_MIN for any argument out of
    ; range -- the x86 "integer indefinite" -- and that went into tv_sec.  So
    ; time.sleep(float('inf')) returned at once instead of raising, and
    ; time.sleep(10**10) slept for three centuries.
    movsd xmm1, xmm0
    mulsd xmm1, [rel tm_1e9]        ; the delay in nanoseconds
    ucomisd xmm1, [rel tm_i64_max]
    jae .sleep_overflow
    ucomisd xmm1, [rel tm_i64_min]
    jb .sleep_overflow

    xorpd xmm1, xmm1
    ucomisd xmm0, xmm1
    jb .sleep_value_error

    ; Split into whole seconds and nanoseconds for struct timespec.
    roundsd xmm1, xmm0, 1       ; floor
    subsd xmm0, xmm1
    cvttsd2si rax, xmm1
    mov [rbp - TSL_SEC], rax
    mulsd xmm0, [rel tm_1e9]
    cvttsd2si rax, xmm0
    mov [rbp - TSL_NSEC], rax

    mov eax, 35                 ; __NR_nanosleep
    lea rdi, [rbp - TSL_SEC]
    xor esi, esi
    syscall

    extern none_singleton
    lea rax, [rel none_singleton]
    INCREF rax
    mov edx, TAG_PTR
    leave
    V_PACK rax, rdx
    ret

.sleep_type_error:
    RAISE exc_TypeError_type, "sleep() argument must be a number"
.sleep_nan:
    extern exc_ValueError_type
    RAISE exc_ValueError_type, "Invalid value NaN (not a number)"
.sleep_overflow:
    ; CPython reaches the same overflow by two roads and says so differently:
    ; an int is rejected converting to _PyTime_t, a float converting back out
    ; to the platform's timespec.  The wording is what a program greps for.
    extern exc_OverflowError_type
    mov rax, [rbp - TSL_ARG]
    V_IS_FLOAT rax, rcx
    jbe .sleep_overflow_float
    RAISE exc_OverflowError_type, \
          "timestamp too large to convert to C _PyTime_t"
.sleep_overflow_float:
    RAISE exc_OverflowError_type, \
          "timestamp out of range for platform time_t"
.sleep_value_error:
    RAISE exc_ValueError_type, "sleep length must be non-negative"
.sleep_error:
    RAISE exc_TypeError_type, "sleep() takes exactly one argument"
END_FUNC time_sleep_func

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

    ; Add time and sleep
    lea rdi, [rel time_time_func]
    lea rsi, [rel tm_time_name]
    call builtin_func_new
    push rax
    lea rdi, [rel tm_time_name]
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

    lea rdi, [rel time_sleep_func]
    lea rsi, [rel tm_sleep]
    call builtin_func_new
    push rax
    lea rdi, [rel tm_sleep]
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
; The int64 range a nanosecond count has to fit, as doubles.  (double)INT64_MAX
; rounds up to 2^63, so the positive test is >= rather than >; the only value
; that separates the two is 2^63 nanoseconds exactly, and refusing it is the
; safe side of a cast that would otherwise be undefined.
tm_i64_max: dq 0x43e0000000000000   ; 2^63
tm_i64_min: dq 0xc3e0000000000000   ; -2^63

tm_time:         db "time", 0
tm_process_time: db "process_time", 0
tm_time_name: db "time", 0
tm_sleep: db "sleep", 0
tm_monotonic:    db "monotonic", 0
