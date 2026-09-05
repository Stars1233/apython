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
extern exc_ValueError_type
extern exc_OverflowError_type
extern structseq_new
extern structseq_set
extern structseq_dealloc
extern structseq_repr
extern structseq_getattr
extern str_type
extern structseq_init_type
extern int_from_i64
extern str_new_heap
extern tuple_type
extern obj_as_index
extern type_type
extern localtime_r
extern gmtime_r
extern mktime
extern strftime
extern ap_free
extern ap_strlen
extern int_to_i64
extern obj_dealloc
extern tuple_new

;; ============================================================================
;; tm_add_int(rdi = the value, rsi = its name as a C string) -> void
;; One int in the module dict, with the dict in r12 as everything here has it.
;; ============================================================================
TAI_VAL   equ 8
TAI_NAME  equ 16
TAI_OBJ   equ 24
TAI_FRAME equ 32            ; + 0 pushes = 32
DEF_FUNC_LOCAL tm_add_int, TAI_FRAME
    mov [rbp - TAI_VAL], rdi
    mov [rbp - TAI_NAME], rsi
    extern int_from_i64
    call int_from_i64
    V_PACK rax, rdx
    mov [rbp - TAI_OBJ], rax
    mov rdi, [rbp - TAI_NAME]
    call str_from_cstr_heap
    push rax
    mov rdi, r12
    mov rsi, rax
    mov rdx, [rbp - TAI_OBJ]
    call dict_set
    pop rdi
    call obj_decref
    mov rdi, [rbp - TAI_OBJ]
    DECREF_V rdi, rsi
    leave
    ret
END_FUNC tm_add_int

;; TIME_ADD_FUNC impl, name -- the module dict is in r12, so this is
;; MODULE_ADD_FUNC by another name; it is spelled out here because the file
;; predates the macro and the other six registrations still use the long form.
%macro TIME_ADD_FUNC 2
    lea rdi, [rel %1]
    lea rsi, [rel %2]
    call builtin_func_new
    push rax
    lea rdi, [rel %2]
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
%endmacro

;; TIME_FIELD slot, tm_offset, bias -- one int field of the struct_time.
%macro TIME_FIELD 3
    mov rdi, [rbp - TFS_TM]
    mov eax, [rdi + %2]
    add eax, %3
    movsxd rdi, eax
    call int_from_i64
    V_PACK rax, rdx             ; it hands back a (payload, tag) pair
    mov rdx, rax
    mov rdi, [rbp - TFS_OBJ]
    mov esi, %1
    call structseq_set          ; one numbering across the tuple and the tail
%endmacro

;; TIME_READ slot, tm_offset, bias -- the same field, read back.
%macro TIME_READ 3
    mov rax, [rbp - TRS_SEQ]
    mov rax, [rax + PyTupleObject.ob_item]
    mov rdi, [rax + %1*8]
    V_UNPACK rdi, rdx
    call obj_as_index
    add rax, %3
    mov [rbx + %2], eax
%endmacro

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
;; time_perf_counter_func(PyObject **args, int64_t nargs) -> rax = Value
;; The highest-resolution clock, in seconds, with an undefined origin -- so
;; only differences mean anything.  On Linux that is CLOCK_MONOTONIC, the same
;; source monotonic() reads; CPython keeps them separate because on other
;; platforms they are not, and because timeit and every benchmark in the
;; stdlib ask for this name rather than that one.
;; ============================================================================
DEF_FUNC time_perf_counter_func, 16
    cmp rsi, 0
    jne .perf_error

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

.perf_error:
    RAISE exc_TypeError_type, "perf_counter() takes no arguments"
END_FUNC time_perf_counter_func

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

    ; --- localtime / gmtime / mktime / strftime, and struct_time ---
    ;
    ; logging, hashlib and random all reach time.localtime at import.  The
    ; calendar arithmetic and the format language are libc's -- localtime_r,
    ; gmtime_r and strftime are already linked for the compiler's strtod --
    ; so what is here is the conversion either way.
    TIME_ADD_FUNC time_localtime_func, tm_localtime
    TIME_ADD_FUNC time_gmtime_func,    tm_gmtime
    TIME_ADD_FUNC time_mktime_func,    tm_mktime
    TIME_ADD_FUNC time_strftime_func,  tm_strftime
    TIME_ADD_FUNC time_asctime_func,   tm_asctime
    TIME_ADD_FUNC time_ctime_func,     tm_ctime

    ; --- tzname / timezone / altzone / daylight ---
    ;
    ; The four glibc sets from $TZ, and _strptime reads tzname
    ; unconditionally at import.  tzset() is what fills them; localtime_r
    ; calls it too, but not before this runs.
    extern tzset
    call tzset
    extern tzname
    extern timezone
    extern daylight
    lea rax, [rel tzname]
    mov rdi, [rax]
    call str_from_cstr_heap
    push rax
    lea rax, [rel tzname]
    mov rdi, [rax + 8]
    call str_from_cstr_heap
    push rax
    mov edi, 2
    call tuple_new
    mov rcx, [rax + PyTupleObject.ob_item]
    pop rdx
    mov [rcx + 8], rdx
    pop rdx
    mov [rcx], rdx
    push rax
    lea rdi, [rel tm_tzname]
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

    ; The offset west of UTC in seconds, as CPython reports it: `timezone` is
    ; standard time, `altzone` is the same less an hour when the zone has DST,
    ; and `daylight` says whether it has any.
    lea rax, [rel timezone]
    mov rdi, [rax]
    lea rsi, [rel tm_timezone]
    call tm_add_int
    lea rax, [rel timezone]
    mov rdi, [rax]
    lea rcx, [rel daylight]
    mov ecx, [rcx]
    test ecx, ecx
    jz .ti_no_dst
    sub rdi, 3600
.ti_no_dst:
    lea rsi, [rel tm_altzone]
    call tm_add_int
    lea rax, [rel daylight]
    movsxd rdi, dword [rax]
    lea rsi, [rel tm_daylight]
    call tm_add_int

    lea rdi, [rel struct_time_type]
    call structseq_init_type
    lea rax, [rel struct_time_type]
    inc qword [rax + PyObject.ob_refcnt]
    push rax
    lea rdi, [rel tm_struct_time]
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

    ; Add perf_counter function
    lea rdi, [rel time_perf_counter_func]
    lea rsi, [rel tm_perf_counter]
    call builtin_func_new
    push rax
    lea rdi, [rel tm_perf_counter]
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
tm_perf_counter: db "perf_counter", 0
tm_localtime:    db "localtime", 0
tm_gmtime:       db "gmtime", 0
tm_mktime:       db "mktime", 0
tm_strftime:     db "strftime", 0
tm_asctime:      db "asctime", 0
tm_ctime:        db "ctime", 0
tm_tzname:       db "tzname", 0
tm_timezone:     db "timezone", 0
tm_altzone:      db "altzone", 0
tm_daylight:     db "daylight", 0
tm_struct_time:  db "struct_time", 0

; --- struct_time ---
st_name: db "time.struct_time", 0
st_f0: db "tm_year", 0
st_f1: db "tm_mon", 0
st_f2: db "tm_mday", 0
st_f3: db "tm_hour", 0
st_f4: db "tm_min", 0
st_f5: db "tm_sec", 0
st_f6: db "tm_wday", 0
st_f7: db "tm_yday", 0
st_f8: db "tm_isdst", 0
st_f9: db "tm_zone", 0
st_f10: db "tm_gmtoff", 0
align 8
st_fields:
    dq st_f0, 0
    dq st_f1, 1
    dq st_f2, 2
    dq st_f3, 3
    dq st_f4, 4
    dq st_f5, 5
    dq st_f6, 6
    dq st_f7, 7
    dq st_f8, 8
    dq st_f9, 9
    dq st_f10, 10
align 8
st_desc:
    dq 9                    ; n_in_sequence: the nine a tuple sees
    dq 11                   ; n_fields: plus tm_zone and tm_gmtoff by name
    dq st_fields

section .data
align 8
global struct_time_type
struct_time_type:
    dq 1                        ; ob_refcnt (immortal)
    dq type_type                ; ob_type
    dq st_name                  ; tp_name
    ; The header plus the named-ONLY tail: two of the eleven fields are not
    ; in the sequence, and the other nine live in ob_item.  This has to move
    ; with st_desc, or the last field is written past the end.
    dq PyTupleObject_size + 2*8 ; tp_basicsize
    dq structseq_dealloc        ; tp_dealloc
    dq structseq_repr           ; tp_repr
    dq structseq_repr           ; tp_str
    dq 0                        ; tp_hash          } copied from tuple_type by
    dq 0                        ; tp_call          } structseq_init_type
    dq structseq_getattr        ; tp_getattr
    dq 0                        ; tp_setattr
    dq 0                        ; tp_richcompare
    dq 0                        ; tp_iter
    dq 0                        ; tp_iternext
    dq 0                        ; tp_init
    dq 0                        ; tp_new
    dq 0                        ; tp_as_number
    dq 0                        ; tp_as_sequence
    dq 0                        ; tp_as_mapping
    dq 0                        ; tp_base
    dq 0                        ; tp_dict
    dq 0                        ; tp_mro
    dq TYPE_FLAG_TUPLE_SUBCLASS ; tp_flags
    dq 0                        ; tp_bases
    dq 0                        ; tp_traverse
    dq 0                        ; tp_clear
    dq 0                        ; tp_dictoffset
    dq 0                        ; tp_tailslots
    dq st_desc                  ; STRUCTSEQ_DESC

section .text

;; ============================================================================
;; struct tm, as libc lays it out on x86-64 Linux.  localtime_r and gmtime_r
;; fill one; mktime and strftime read one.
;; ============================================================================
TM_SEC    equ 0
TM_MIN    equ 4
TM_HOUR   equ 8
TM_MDAY   equ 12
TM_MON    equ 16
TM_YEAR   equ 20
TM_WDAY   equ 24
TM_YDAY   equ 28
TM_ISDST  equ 32
TM_GMTOFF equ 40
TM_ZONE   equ 48
TM_SIZE   equ 56

;; ============================================================================
;; time_fill_struct(rdi = a struct tm) -> rax = a struct_time, or 0
;;
;; CPython's field order is NOT libc's: the tuple is
;; (year, mon, mday, hour, min, sec, wday, yday, isdst), with the year offset
;; by 1900, the month 1-based, and Monday as weekday 0 where libc has Sunday.
;; ============================================================================
TFS_TM    equ 8
TFS_OBJ   equ 16
TFS_FRAME equ 40            ; + 1 push = 48, 16-aligned
DEF_FUNC_LOCAL time_fill_struct, TFS_FRAME
    push rbx
    mov [rbp - TFS_TM], rdi
    lea rdi, [rel struct_time_type]
    call structseq_new
    test rax, rax
    jz .tfs_fail
    mov [rbp - TFS_OBJ], rax

    mov rdi, [rbp - TFS_TM]
    mov eax, [rdi + TM_YEAR]
    add eax, 1900
    movsxd rdi, eax
    call int_from_i64
    V_PACK rax, rdx
    mov rdx, rax
    mov rdi, [rbp - TFS_OBJ]
    xor esi, esi
    call structseq_set

    TIME_FIELD 1, TM_MON, 1
    TIME_FIELD 2, TM_MDAY, 0
    TIME_FIELD 3, TM_HOUR, 0
    TIME_FIELD 4, TM_MIN, 0
    TIME_FIELD 5, TM_SEC, 0

    ; Monday is 0 in Python and 1 in libc, where Sunday is 0.
    mov rdi, [rbp - TFS_TM]
    mov eax, [rdi + TM_WDAY]
    add eax, 6
    cdq
    mov ecx, 7
    idiv ecx
    movsxd rdi, edx
    call int_from_i64
    V_PACK rax, rdx
    mov rdx, rax
    mov rdi, [rbp - TFS_OBJ]
    mov esi, 6
    call structseq_set

    TIME_FIELD 7, TM_YDAY, 1
    TIME_FIELD 8, TM_ISDST, 0

    ; The two reachable only by name.
    mov rdi, [rbp - TFS_TM]
    mov rdi, [rdi + TM_ZONE]
    test rdi, rdi
    jz .tfs_no_zone
    call str_from_cstr_heap
    jmp .tfs_zone_done
.tfs_no_zone:
    LOAD_NONE rax
    INCREF rax
.tfs_zone_done:
    ; A pointer IS its own Value, so both arms here need no packing.
    mov rdx, rax
    mov rdi, [rbp - TFS_OBJ]
    mov esi, 9
    call structseq_set

    mov rdi, [rbp - TFS_TM]
    mov rdi, [rdi + TM_GMTOFF]
    call int_from_i64
    V_PACK rax, rdx
    mov rdx, rax
    mov rdi, [rbp - TFS_OBJ]
    mov esi, 10
    call structseq_set

    mov rax, [rbp - TFS_OBJ]
    pop rbx
    leave
    ret
.tfs_fail:
    xor eax, eax
    pop rbx
    leave
    ret
END_FUNC time_fill_struct

;; ============================================================================
;; time_read_struct(rdi = a 9-or-more sequence, rsi = a struct tm to fill)
;;   -> eax = 1, or 0 with the exception pending
;; The inverse of the above, for mktime and strftime.
;; ============================================================================
TRS_SEQ   equ 8
TRS_TM    equ 16
TRS_I     equ 24
TRS_NAME  equ 32
TRS_FRAME equ 56            ; + 1 push = 64, 16-aligned
DEF_FUNC_LOCAL time_read_struct, TRS_FRAME
    push rbx
    mov [rbp - TRS_SEQ], rdi
    mov [rbp - TRS_TM], rsi
    mov [rbp - TRS_NAME], rdx
    mov rbx, rsi

    ; Zero it: tm_zone and tm_gmtoff are not in the sequence, and libc reads
    ; whatever is there.
    xor ecx, ecx
.trs_zero:
    cmp ecx, TM_SIZE
    jge .trs_zeroed
    mov qword [rbx + rcx], 0
    add ecx, 8
    jmp .trs_zero
.trs_zeroed:

    V_TEST_PTR rdi, rax
    ja .trs_bad
    test rdi, rdi
    jz .trs_bad
    mov rax, [rdi + PyObject.ob_type]
    lea rcx, [rel tuple_type]
    cmp rax, rcx
    je .trs_have
    test qword [rax + PyTypeObject.tp_flags], TYPE_FLAG_TUPLE_SUBCLASS
    jz .trs_bad
.trs_have:
    cmp qword [rdi + PyTupleObject.ob_size], 9
    jl .trs_short

    TIME_READ 0, TM_YEAR, -1900
    TIME_READ 1, TM_MON, -1
    TIME_READ 2, TM_MDAY, 0
    TIME_READ 3, TM_HOUR, 0
    TIME_READ 4, TM_MIN, 0
    TIME_READ 5, TM_SEC, 0
    TIME_READ 7, TM_YDAY, -1
    TIME_READ 8, TM_ISDST, 0

    ; Monday-0 back to Sunday-0.
    mov rax, [rbp - TRS_SEQ]
    mov rax, [rax + PyTupleObject.ob_item]
    mov rdi, [rax + 6*8]
    V_UNPACK rdi, rdx
    call obj_as_index
    add rax, 1
    cdq
    mov ecx, 7
    idiv ecx
    mov [rbx + TM_WDAY], edx

    mov eax, 1
    pop rbx
    leave
    ret
.trs_short:
    pop rbx
    ; CPython names the caller: "strftime(): illegal time tuple argument".
    ; time_read_struct is shared, so the name comes in as an argument.
    mov rdi, [rbp - TRS_NAME]
    call time_raise_bad_tuple
.trs_bad:
    pop rbx
    RAISE exc_TypeError_type, "Tuple or struct_time argument required"
END_FUNC time_read_struct

;; ============================================================================
;; time.localtime([secs]) and time.gmtime([secs])
;; ============================================================================
TLT_TM    equ 64             ; a struct tm, 56 bytes
TLT_SECS  equ 72
TLT_FRAME equ 80            ; + 0 pushes = 80
%macro DEF_TIME_CONVERT 2   ; %1 = the exposed name, %2 = the libc call
DEF_FUNC time_%1_func, TLT_FRAME
    call time_seconds_arg
    mov [rbp - TLT_SECS], rax
    lea rdi, [rbp - TLT_SECS]
    lea rsi, [rbp - TLT_TM]
    and rsp, -16                ; glibc uses aligned SSE
    call %2 wrt ..plt
    test rax, rax
    jz %%failed
    lea rdi, [rbp - TLT_TM]
    call time_fill_struct
    test rax, rax
    jz %%failed
    mov edx, TAG_PTR
    leave
    V_PACK rax, rdx             ; builtins return one Value
    ret
%%failed:
    RAISE exc_OverflowError_type, "timestamp out of range for platform time_t"
END_FUNC time_%1_func
%endmacro
DEF_TIME_CONVERT localtime, localtime_r
DEF_TIME_CONVERT gmtime,    gmtime_r

;; time_seconds_arg -- the optional first argument, truncated to whole
;; seconds, defaulting to now.  rdi = args, rsi = nargs on entry.
TSA_FRAME equ 32            ; + 0 pushes = 32
DEF_FUNC_LOCAL time_seconds_arg, TSA_FRAME
    test rsi, rsi
    jz .tsa_now
    mov rdi, [rdi]
    LOAD_NONE rax
    cmp rdi, rax
    je .tsa_now
    V_IS_FLOAT rdi, rax
    ja .tsa_int
    V_TO_F64 rdi
    movq xmm0, rdi
    cvttsd2si rax, xmm0
    leave
    ret
.tsa_int:
    V_UNPACK rdi, rdx
    call obj_as_index
    leave
    ret
.tsa_now:
    ; clock_gettime(CLOCK_REALTIME, &ts); only the seconds are wanted.
    mov eax, 228
    mov edi, CLOCK_REALTIME
    lea rsi, [rbp - TSA_FRAME]
    syscall
    mov rax, [rbp - TSA_FRAME]
    leave
    ret
END_FUNC time_seconds_arg

;; ============================================================================
;; time.mktime(t) -> the seconds since the epoch, as a float
;; ============================================================================
TMK_TM    equ 64
TMK_FRAME equ 80            ; + 0 pushes = 80
DEF_FUNC time_mktime_func, TMK_FRAME
    test rsi, rsi
    jz .tmk_args
    mov rdi, [rdi]
    lea rsi, [rbp - TMK_TM]
    CSTRING rdx, "mktime"
    call time_read_struct
    test eax, eax
    jz .tmk_failed
    ; mktime interprets tm_isdst < 0 as "work it out", which is what CPython
    ; passes through.
    lea rdi, [rbp - TMK_TM]
    and rsp, -16                ; glibc uses aligned SSE
    call mktime wrt ..plt
    cmp rax, -1
    je .tmk_range
    cvtsi2sd xmm0, rax
    call float_from_f64
    leave
    V_PACK rax, rdx
    ret
.tmk_range:
    RAISE exc_OverflowError_type, "mktime argument out of range"
.tmk_failed:
    xor eax, eax
    leave
    ret
.tmk_args:
    RAISE exc_TypeError_type, "mktime() takes exactly one argument"
END_FUNC time_mktime_func

;; ============================================================================
;; time.strftime(format[, t]) -> str
;; ============================================================================
TSF_TM    equ 64
TSF_BUF   equ 72
TSF_FMT   equ 80
TSF_FRAME equ 96            ; + 0 pushes = 96
STRFTIME_BUF equ 4096
DEF_FUNC time_strftime_func, TSF_FRAME
    test rsi, rsi
    jz .tsf_args
    push rdi
    push rsi
    mov rdi, [rdi]
    V_TEST_PTR rdi, rax
    ja .tsf_badfmt
    mov rax, [rdi + PyObject.ob_type]
    lea rcx, [rel str_type]
    cmp rax, rcx
    jne .tsf_badfmt
    lea rax, [rdi + PyStrObject.data]
    mov [rbp - TSF_FMT], rax
    pop rsi
    pop rdi

    cmp rsi, 2
    jl .tsf_now
    mov rdi, [rdi + 8]
    lea rsi, [rbp - TSF_TM]
    CSTRING rdx, "strftime"
    call time_read_struct
    test eax, eax
    jz .tsf_failed
    jmp .tsf_have_tm
.tsf_now:
    ; No time given: localtime(now), as CPython does.
    xor esi, esi
    call time_seconds_arg
    mov [rbp - TSF_BUF], rax
    lea rdi, [rbp - TSF_BUF]
    lea rsi, [rbp - TSF_TM]
    and rsp, -16                ; glibc uses aligned SSE
    call localtime_r wrt ..plt
    test rax, rax
    jz .tsf_failed

.tsf_have_tm:
    mov edi, STRFTIME_BUF
    call ap_malloc
    test rax, rax
    jz .tsf_failed
    mov [rbp - TSF_BUF], rax
    mov rdi, rax
    mov esi, STRFTIME_BUF
    mov rdx, [rbp - TSF_FMT]
    lea rcx, [rbp - TSF_TM]
    and rsp, -16                ; glibc uses aligned SSE
    call strftime wrt ..plt
    mov rdi, [rbp - TSF_BUF]
    mov rsi, rax
    call str_new_heap
    push rax
    sub rsp, 8
    mov rdi, [rbp - TSF_BUF]
    call ap_free
    add rsp, 8
    pop rax
    mov edx, TAG_PTR
    leave
    V_PACK rax, rdx
    ret

.tsf_badfmt:
    pop rsi                     ; nargs
    pop rdi                     ; args -- the two pushes are gone now
    mov rsi, [rdi]              ; args[0], the format
    CSTRING rdi, `strftime() argument 1 must be str, not \x01`
    extern raise_type_error_with_name
    call raise_type_error_with_name
.tsf_failed:
    xor eax, eax
    leave
    ret
.tsf_args:
    RAISE exc_TypeError_type, "strftime() takes at least 1 argument"
END_FUNC time_strftime_func

;; ============================================================================
;; time.asctime([t]) and time.ctime([secs])
;;
;; Both are strftime with CPython's fixed format; ctime converts first.
;; ============================================================================
TAC_TM    equ 64
TAC_SECS  equ 72
TAC_BUF   equ 80
TAC_FRAME equ 96            ; + 0 pushes = 96
DEF_FUNC time_asctime_func, TAC_FRAME
    test rsi, rsi
    jz .tac_now
    mov rdi, [rdi]
    LOAD_NONE rax
    cmp rdi, rax
    je .tac_now
    lea rsi, [rbp - TAC_TM]
    CSTRING rdx, "asctime"
    call time_read_struct
    test eax, eax
    jz .tac_failed
    jmp .tac_format
.tac_now:
    xor esi, esi
    call time_seconds_arg
    mov [rbp - TAC_SECS], rax
    lea rdi, [rbp - TAC_SECS]
    lea rsi, [rbp - TAC_TM]
    and rsp, -16                ; glibc uses aligned SSE
    call localtime_r wrt ..plt
    test rax, rax
    jz .tac_failed
.tac_format:
    mov edi, 128
    call ap_malloc
    test rax, rax
    jz .tac_failed
    mov [rbp - TAC_BUF], rax
    mov rdi, rax
    mov esi, 128
    lea rdx, [rel tm_asctime_fmt]
    lea rcx, [rbp - TAC_TM]
    and rsp, -16                ; glibc uses aligned SSE
    call strftime wrt ..plt
    mov rdi, [rbp - TAC_BUF]
    mov rsi, rax
    call str_new_heap
    push rax
    sub rsp, 8
    mov rdi, [rbp - TAC_BUF]
    call ap_free
    add rsp, 8
    pop rax
    mov edx, TAG_PTR
    leave
    V_PACK rax, rdx
    ret
.tac_failed:
    xor eax, eax
    leave
    ret
END_FUNC time_asctime_func

DEF_FUNC time_ctime_func, TAC_FRAME
    call time_seconds_arg
    mov [rbp - TAC_SECS], rax
    lea rdi, [rbp - TAC_SECS]
    lea rsi, [rbp - TAC_TM]
    and rsp, -16                ; glibc uses aligned SSE
    call localtime_r wrt ..plt
    test rax, rax
    jz .tct_failed
    mov edi, 128
    call ap_malloc
    test rax, rax
    jz .tct_failed
    mov [rbp - TAC_BUF], rax
    mov rdi, rax
    mov esi, 128
    lea rdx, [rel tm_asctime_fmt]
    lea rcx, [rbp - TAC_TM]
    and rsp, -16                ; glibc uses aligned SSE
    call strftime wrt ..plt
    mov rdi, [rbp - TAC_BUF]
    mov rsi, rax
    call str_new_heap
    push rax
    sub rsp, 8
    mov rdi, [rbp - TAC_BUF]
    call ap_free
    add rsp, 8
    pop rax
    mov edx, TAG_PTR
    leave
    V_PACK rax, rdx
    ret
.tct_failed:
    xor eax, eax
    leave
    ret
END_FUNC time_ctime_func

section .rodata
; CPython's asctime format: "Sun Jun 20 23:21:05 1993", with the day of the
; month space-padded rather than zero-padded.
tm_asctime_fmt: db "%a %b %e %H:%M:%S %Y", 0


section .text

;; time_raise_bad_tuple(rdi = the caller's name) -- does not return
TRB_NAME  equ 8
TRB_BUF   equ 176
TRB_FRAME equ 176           ; + 0 pushes = 176, 16-aligned
DEF_FUNC_LOCAL time_raise_bad_tuple, TRB_FRAME
    mov [rbp - TRB_NAME], rdi
    lea rdi, [rbp - TRB_BUF]
    mov rsi, [rbp - TRB_NAME]
    extern rbt_append_cstr
    call rbt_append_cstr
    mov rdi, rax
    CSTRING rsi, "(): illegal time tuple argument"
    call rbt_append_cstr
    lea rdi, [rel exc_TypeError_type]
    lea rsi, [rbp - TRB_BUF]
    call raise_exception
END_FUNC time_raise_bad_tuple
