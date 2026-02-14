; float.asm - Float type (IEEE 754 double precision)
;
; PyFloatObject layout:
;   +0  ob_refcnt (8 bytes)
;   +8  ob_type   (8 bytes)
;   +16 value     (8 bytes: double)
;   Total: 24 bytes

%include "macros.inc"
%include "object.inc"
%include "types.inc"

section .note.GNU-stack noalloc noexec nowrite progbits

section .text

extern ap_malloc
extern ap_free
extern str_from_cstr
extern str_from_data
extern bool_true
extern bool_false
extern none_singleton
extern int_from_i64
extern int_type
extern raise_exception
extern exc_TypeError_type
extern exc_ZeroDivisionError_type
extern exc_ValueError_type
extern obj_incref

; libc functions for float formatting
extern snprintf
extern strtod

; GMP for int-to-double conversion
extern __gmpz_get_d

;; ============================================================================
;; float_from_f64 - Create a float from a double in xmm0
;; Input:  xmm0 = double value
;; Output: rax = new PyFloatObject*
;; ============================================================================
global float_from_f64
float_from_f64:
    push rbp
    mov rbp, rsp
    sub rsp, 16

    movsd [rbp-8], xmm0       ; save double value

    mov edi, PyFloatObject_size
    call ap_malloc
    ; rax = new object

    mov qword [rax + PyObject.ob_refcnt], 1
    lea rcx, [rel float_type]
    mov [rax + PyObject.ob_type], rcx
    movsd xmm0, [rbp-8]
    movsd [rax + PyFloatObject.value], xmm0

    leave
    ret

;; ============================================================================
;; float_to_f64 - Convert numeric PyObject to double
;; Input:  rdi = PyObject* (float, SmallInt, or GMP int)
;; Output: xmm0 = double value
;; Clobbers: rax, rcx, rdx, rdi, rsi, r8-r11
;; ============================================================================
global float_to_f64
float_to_f64:
    ; Check SmallInt first (bit 63 set)
    test rdi, rdi
    js .from_smallint

    ; Check type
    mov rax, [rdi + PyObject.ob_type]
    lea rcx, [rel float_type]
    cmp rax, rcx
    je .from_float

    lea rcx, [rel int_type]
    cmp rax, rcx
    je .from_gmp_int

    ; Not a number - return 0.0
    xorpd xmm0, xmm0
    ret

.from_float:
    movsd xmm0, [rdi + PyFloatObject.value]
    ret

.from_smallint:
    mov rax, rdi
    shl rax, 1
    sar rax, 1                 ; sign-extend SmallInt decode
    cvtsi2sd xmm0, rax
    ret

.from_gmp_int:
    push rbp
    mov rbp, rsp
    and rsp, -16              ; ensure 16-byte alignment for GMP call
    lea rdi, [rdi + PyIntObject.mpz]
    call __gmpz_get_d wrt ..plt
    ; result in xmm0
    leave
    ret

;; ============================================================================
;; float_repr(PyObject *self) -> PyStrObject*
;; Uses shortest representation that round-trips.
;; ============================================================================
global float_repr
float_repr:
    push rbp
    mov rbp, rsp
    and rsp, -16              ; ensure 16-byte alignment for libc calls
    sub rsp, 80
    ; Stack layout:
    ;   [rbp-8]   = original double value (8 bytes)
    ;   [rbp-16]  = precision counter (8 bytes, only low 4 used)
    ;   [rbp-64]  = buffer (48 bytes: [rbp-64] to [rbp-17])

    movsd xmm0, [rdi + PyFloatObject.value]
    movsd [rbp-8], xmm0       ; save original value

    ; Check for NaN
    ucomisd xmm0, xmm0
    jp .is_nan

    ; Check for infinity
    movsd xmm1, [rel pos_inf]
    ucomisd xmm0, xmm1
    je .is_pos_inf
    movsd xmm1, [rel neg_inf]
    ucomisd xmm0, xmm1
    je .is_neg_inf

    ; General case: find shortest representation
    ; Try precision 1..17 with snprintf "%.*g"
    mov qword [rbp-16], 1     ; prec = 1

.repr_loop:
    lea rdi, [rbp-64]         ; buf
    mov esi, 48                ; bufsz
    lea rdx, [rel fmt_g]      ; "%.*g"
    mov ecx, [rbp-16]         ; prec
    movsd xmm0, [rbp-8]      ; value
    mov eax, 1                ; 1 xmm register used
    call snprintf wrt ..plt

    ; Round-trip check: strtod(buf, NULL) == val?
    lea rdi, [rbp-64]         ; buf
    xor esi, esi              ; endptr = NULL
    call strtod wrt ..plt
    ; xmm0 = reparsed value
    movsd xmm1, [rbp-8]      ; original
    ucomisd xmm0, xmm1
    je .repr_found             ; match! use this precision

    inc qword [rbp-16]
    cmp qword [rbp-16], 17
    jle .repr_loop

.repr_found:
    ; Check if buf needs ".0" appended (no '.', no 'e', no 'E')
    lea rdi, [rbp-64]
    xor ecx, ecx
.scan_dot:
    mov al, [rdi + rcx]
    test al, al
    jz .no_dot_found
    cmp al, '.'
    je .has_dot
    cmp al, 'e'
    je .has_dot
    cmp al, 'E'
    je .has_dot
    cmp al, 'n'               ; nan
    je .has_dot
    cmp al, 'i'               ; inf
    je .has_dot
    inc ecx
    jmp .scan_dot
.no_dot_found:
    ; Append ".0"
    mov byte [rdi + rcx], '.'
    mov byte [rdi + rcx + 1], '0'
    mov byte [rdi + rcx + 2], 0
.has_dot:
    lea rdi, [rbp-64]
    call str_from_cstr
    leave
    ret

.is_nan:
    lea rdi, [rel str_nan]
    call str_from_cstr
    leave
    ret

.is_pos_inf:
    lea rdi, [rel str_inf]
    call str_from_cstr
    leave
    ret

.is_neg_inf:
    lea rdi, [rel str_neg_inf]
    call str_from_cstr
    leave
    ret

;; ============================================================================
;; float_hash(PyObject *self) -> int64 in rax
;; ============================================================================
global float_hash
float_hash:
    mov rax, [rdi + PyFloatObject.value]  ; raw bits
    ; Ensure hash is never -1 (Python convention)
    cmp rax, -1
    jne .ok
    mov rax, -2
.ok:
    ret

;; ============================================================================
;; float_bool(PyObject *self) -> int (0 or 1) in eax
;; ============================================================================
global float_bool
float_bool:
    movsd xmm0, [rdi + PyFloatObject.value]
    xorpd xmm1, xmm1         ; xmm1 = 0.0
    ucomisd xmm0, xmm1
    je .is_zero
    mov eax, 1
    ret
.is_zero:
    ; Also check for -0.0 (which is also falsy)
    xor eax, eax
    ret

;; ============================================================================
;; float_dealloc(PyObject *self)
;; ============================================================================
global float_dealloc
float_dealloc:
    ; rdi = self
    jmp ap_free                ; tail call

;; ============================================================================
;; Binary arithmetic: float_add, float_sub, float_mul, float_truediv,
;;                    float_floordiv, float_mod, float_neg
;; All take (PyObject *a, PyObject *b) -> PyObject*
;; Convert both to double, perform operation, return new float.
;; ============================================================================

; Helper macro: convert both args to doubles
; Uses rbp frame with [rbp-8] = left double, [rbp-16] = right double
%macro FLOAT_BINOP_SETUP 0
    mov [rbp-24], rsi          ; save right operand
    call float_to_f64          ; rdi = left → xmm0
    movsd [rbp-8], xmm0
    mov rdi, [rbp-24]
    call float_to_f64          ; xmm0 = right as double
    movsd [rbp-16], xmm0
%endmacro

global float_add
float_add:
    push rbp
    mov rbp, rsp
    sub rsp, 32
    FLOAT_BINOP_SETUP
    movsd xmm0, [rbp-8]
    addsd xmm0, [rbp-16]
    call float_from_f64
    leave
    ret

global float_sub
float_sub:
    push rbp
    mov rbp, rsp
    sub rsp, 32
    FLOAT_BINOP_SETUP
    movsd xmm0, [rbp-8]
    subsd xmm0, [rbp-16]
    call float_from_f64
    leave
    ret

global float_mul
float_mul:
    push rbp
    mov rbp, rsp
    sub rsp, 32
    FLOAT_BINOP_SETUP
    movsd xmm0, [rbp-8]
    mulsd xmm0, [rbp-16]
    call float_from_f64
    leave
    ret

global float_truediv
float_truediv:
    push rbp
    mov rbp, rsp
    sub rsp, 32
    FLOAT_BINOP_SETUP

    ; Check for division by zero
    movsd xmm1, [rbp-16]
    xorpd xmm2, xmm2
    ucomisd xmm1, xmm2
    je .div_zero

    movsd xmm0, [rbp-8]
    divsd xmm0, xmm1
    call float_from_f64
    leave
    ret

.div_zero:
    lea rdi, [rel exc_ZeroDivisionError_type]
    CSTRING rsi, "float division by zero"
    call raise_exception

global float_floordiv
float_floordiv:
    push rbp
    mov rbp, rsp
    sub rsp, 32
    FLOAT_BINOP_SETUP

    ; Check for division by zero
    movsd xmm1, [rbp-16]
    xorpd xmm2, xmm2
    ucomisd xmm1, xmm2
    je .floordiv_zero

    movsd xmm0, [rbp-8]
    divsd xmm0, xmm1
    ; Floor: round toward negative infinity
    roundsd xmm0, xmm0, 1     ; 1 = floor
    call float_from_f64
    leave
    ret

.floordiv_zero:
    lea rdi, [rel exc_ZeroDivisionError_type]
    CSTRING rsi, "float floor division by zero"
    call raise_exception

global float_mod
float_mod:
    push rbp
    mov rbp, rsp
    sub rsp, 32
    FLOAT_BINOP_SETUP

    ; Check for division by zero
    movsd xmm1, [rbp-16]
    xorpd xmm2, xmm2
    ucomisd xmm1, xmm2
    je .mod_zero

    ; a % b = a - floor(a/b) * b
    movsd xmm0, [rbp-8]       ; a
    movsd xmm1, [rbp-16]      ; b
    movapd xmm2, xmm0         ; save a
    divsd xmm0, xmm1          ; a/b
    roundsd xmm0, xmm0, 1     ; floor(a/b)
    mulsd xmm0, xmm1          ; floor(a/b)*b
    subsd xmm2, xmm0          ; a - floor(a/b)*b
    movapd xmm0, xmm2
    call float_from_f64
    leave
    ret

.mod_zero:
    lea rdi, [rel exc_ZeroDivisionError_type]
    CSTRING rsi, "float modulo"
    call raise_exception

global float_neg
float_neg:
    push rbp
    mov rbp, rsp
    sub rsp, 16

    ; rdi = self (only one operand for unary ops)
    call float_to_f64
    ; Negate: XOR with sign bit
    movsd xmm1, [rel sign_mask]
    xorpd xmm0, xmm1
    call float_from_f64

    leave
    ret

;; ============================================================================
;; float_int(PyObject *self) -> PyIntObject*
;; Convert float to int by truncation.
;; ============================================================================
global float_int
float_int:
    push rbp
    mov rbp, rsp

    movsd xmm0, [rdi + PyFloatObject.value]

    ; Check for NaN/inf
    ucomisd xmm0, xmm0
    jp .not_finite

    movsd xmm1, [rel pos_inf]
    ucomisd xmm0, xmm1
    je .not_finite
    movsd xmm1, [rel neg_inf]
    ucomisd xmm0, xmm1
    je .not_finite

    ; Truncate to int64
    cvttsd2si rdi, xmm0
    call int_from_i64
    pop rbp
    ret

.not_finite:
    lea rdi, [rel exc_ValueError_type]
    CSTRING rsi, "cannot convert float NaN or infinity to integer"
    call raise_exception

;; ============================================================================
;; float_compare(PyObject *a, PyObject *b, int op) -> PyObject*
;; op: PY_LT=0, PY_LE=1, PY_EQ=2, PY_NE=3, PY_GT=4, PY_GE=5
;; Handles mixed int/float comparisons.
;; ============================================================================
global float_compare
float_compare:
    push rbp
    mov rbp, rsp
    sub rsp, 32
    mov [rbp-24], edx          ; save op

    ; Convert both to doubles
    mov [rbp-32], rsi          ; save right
    call float_to_f64          ; left → xmm0
    movsd [rbp-8], xmm0
    mov rdi, [rbp-32]
    call float_to_f64          ; right → xmm0
    movsd [rbp-16], xmm0

    ; Compare
    movsd xmm0, [rbp-8]
    ucomisd xmm0, [rbp-16]

    ; Handle NaN: unordered (PF set) → everything False except NE
    jp .unordered

    ; Save ucomisd result as three-way (-1=below, 0=equal, 1=above)
    ; Must do this BEFORE cmp instructions overwrite flags
    mov r8d, 0
    je .float_cmp_dispatch
    mov r8d, -1
    jb .float_cmp_dispatch
    mov r8d, 1

.float_cmp_dispatch:
    mov ecx, [rbp-24]          ; op
    cmp ecx, PY_LT
    je .do_lt
    cmp ecx, PY_LE
    je .do_le
    cmp ecx, PY_EQ
    je .do_eq
    cmp ecx, PY_NE
    je .do_ne
    cmp ecx, PY_GT
    je .do_gt
    ; PY_GE
    test r8d, r8d
    jge .ret_true
    jmp .ret_false

.do_lt:
    test r8d, r8d
    js .ret_true
    jmp .ret_false
.do_le:
    test r8d, r8d
    jle .ret_true
    jmp .ret_false
.do_eq:
    test r8d, r8d
    jz .ret_true
    jmp .ret_false
.do_ne:
    test r8d, r8d
    jnz .ret_true
    jmp .ret_false
.do_gt:
    test r8d, r8d
    jg .ret_true
    jmp .ret_false

.unordered:
    ; NaN comparisons: only NE returns True
    cmp ecx, PY_NE
    je .ret_true
    jmp .ret_false

.ret_true:
    lea rax, [rel bool_true]
    inc qword [rax + PyObject.ob_refcnt]
    leave
    ret
.ret_false:
    lea rax, [rel bool_false]
    inc qword [rax + PyObject.ob_refcnt]
    leave
    ret

;; ============================================================================
;; Data
;; ============================================================================
section .data

float_name_str: db "float", 0
str_nan: db "nan", 0
str_inf: db "inf", 0
str_neg_inf: db "-inf", 0
fmt_g: db "%.*g", 0

align 8
sign_mask: dq 0x8000000000000000
pos_inf:   dq 0x7FF0000000000000
neg_inf:   dq 0xFFF0000000000000

align 8
global float_number_methods
float_number_methods:
    dq float_add              ; nb_add          +0
    dq float_sub              ; nb_subtract     +8
    dq float_mul              ; nb_multiply     +16
    dq float_mod              ; nb_remainder    +24
    dq 0                      ; nb_divmod       +32
    dq 0                      ; nb_power        +40
    dq float_neg              ; nb_negative     +48
    dq 0                      ; nb_positive     +56
    dq 0                      ; nb_absolute     +64
    dq float_bool             ; nb_bool         +72
    dq 0                      ; nb_invert       +80
    dq 0                      ; nb_lshift       +88
    dq 0                      ; nb_rshift       +96
    dq 0                      ; nb_and          +104
    dq 0                      ; nb_xor          +112
    dq 0                      ; nb_or           +120
    dq float_int              ; nb_int          +128
    dq 0                      ; nb_float        +136
    dq float_floordiv         ; nb_floor_divide +144
    dq float_truediv          ; nb_true_divide  +152
    dq 0                      ; nb_index        +160

align 8
extern type_type

global float_type
float_type:
    dq 1                      ; ob_refcnt (immortal)
    dq type_type              ; ob_type
    dq float_name_str         ; tp_name
    dq PyFloatObject_size     ; tp_basicsize
    dq float_dealloc          ; tp_dealloc
    dq float_repr             ; tp_repr
    dq float_repr             ; tp_str (same as repr for float)
    dq float_hash             ; tp_hash
    dq 0                      ; tp_call
    dq 0                      ; tp_getattr
    dq 0                      ; tp_setattr
    dq float_compare          ; tp_richcompare
    dq 0                      ; tp_iter
    dq 0                      ; tp_iternext
    dq 0                      ; tp_init
    dq 0                      ; tp_new
    dq float_number_methods   ; tp_as_number
    dq 0                      ; tp_as_sequence
    dq 0                      ; tp_as_mapping
    dq 0                      ; tp_base
    dq 0                      ; tp_dict
    dq 0                      ; tp_mro
    dq 0                      ; tp_flags
    dq 0                      ; tp_bases
