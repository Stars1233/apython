; float.asm - Float type (IEEE 754 double precision)
;
; PyFloatObject layout:
;   +0  ob_refcnt (8 bytes)
;   +8  ob_type   (8 bytes)
;   +16 value     (8 bytes: double)
;   Total: 24 bytes

%include "macros.inc"
%include "object.inc"

extern int_promote_mpz
extern str_from_cstr
extern bool_true
extern bool_false
extern none_singleton
extern int_from_i64
extern int_type
extern raise_exception
extern exc_ZeroDivisionError_type
extern exc_ValueError_type
extern obj_incref

; libc functions for float formatting
extern snprintf
extern strtod

; GMP for int-to-double conversion
extern __gmpz_get_d

;; ============================================================================
;; float_from_f64 - Create an inline float from a double in xmm0
;; Input:  xmm0 = double value
;; Output: rax = raw double bits (payload), edx = TAG_FLOAT
;; ============================================================================
DEF_FUNC_BARE float_from_f64
    movq rax, xmm0
    mov edx, TAG_FLOAT
    ret
END_FUNC float_from_f64

;; ============================================================================
;; float_to_f64 - Convert numeric value to double
;; Input:  rdi = payload, esi = tag
;; Output: xmm0 = double value
;; Clobbers: rax, rcx, rdx, rdi, rsi, r8-r11
;; ============================================================================
DEF_FUNC_BARE float_to_f64
    ; rdi = payload, esi = tag
    cmp esi, TAG_FLOAT
    je .from_float

    cmp esi, TAG_SMALLINT
    je .from_smallint

    ; TAG_PTR: check for GMP int or bool singleton
    test rdi, rdi
    jz .ret_zero
    mov rax, [rdi + PyObject.ob_type]
    lea rcx, [rel int_type]
    cmp rax, rcx
    je .from_gmp_int
    extern bool_type
    lea rcx, [rel bool_type]
    cmp rax, rcx
    je .from_gmp_int           ; bool singletons have embedded mpz
    ; An int subclass wraps its value; unwrap and retry, or 1.0 == MyInt(1)
    ; came out False.
    mov rcx, [rax + PyTypeObject.tp_flags]
    test rcx, TYPE_FLAG_INT_SUBCLASS
    jz .ret_zero
    mov edx, esi
    extern int_unwrap
    call int_unwrap
    mov esi, edx
    cmp esi, TAG_SMALLINT
    je .from_smallint
    test rdi, rdi
    jz .ret_zero
    mov rax, [rdi + PyObject.ob_type]
    lea rcx, [rel int_type]
    cmp rax, rcx
    je .from_gmp_int

    ; Not a number - return 0.0
.ret_zero:
    xorpd xmm0, xmm0
    ret

.from_float:
    movq xmm0, rdi
    ret

.from_smallint:
    mov rax, rdi
    cvtsi2sd xmm0, rax
    ret

.from_gmp_int:
    push rbp
    mov rbp, rsp
    and rsp, -16              ; ensure 16-byte alignment for GMP call
    INT_NEED_MPZ rdi
    lea rdi, [rdi + PyIntObject.mpz]
    call __gmpz_get_d wrt ..plt
    ; result in xmm0
    leave
    ret
END_FUNC float_to_f64

;; ============================================================================
;; float_repr(rdi = raw double bits) -> PyStrObject*
;; Uses shortest representation that round-trips.
;; ============================================================================
; Named slots for the second buffer the notation choice needs.
FR_VAL   equ 8
FR_PREC  equ 16
FR_BUF   equ 64          ; 48 bytes, [rbp-64, rbp-16)
FR_EBUF  equ 128         ; 48 bytes, [rbp-128, rbp-80)
FR_EXP   equ 136

DEF_FUNC float_repr
    and rsp, -16              ; ensure 16-byte alignment for libc calls
    sub rsp, 160
    ; Stack layout:
    ;   [rbp-8]   = original double value (8 bytes)
    ;   [rbp-16]  = precision counter (8 bytes, only low 4 used)
    ;   [rbp-64]  = buffer (48 bytes: [rbp-64] to [rbp-17])

    movq xmm0, rdi
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
    lea rdi, [rbp - FR_BUF]   ; buf
    mov esi, 48                ; bufsz
    lea rdx, [rel fmt_g]      ; "%.*g"
    mov ecx, [rbp - FR_PREC]  ; prec
    movsd xmm0, [rbp - FR_VAL] ; value
    mov eax, 1                ; 1 xmm register used
    call snprintf wrt ..plt

    ; Round-trip check: strtod(buf, NULL) == val?
    lea rdi, [rbp - FR_BUF]   ; buf
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
    ; The loop above found the shortest digit count that round-trips, but it
    ; let %g pick the notation -- and %g goes exponential as soon as the
    ; exponent reaches the precision, so repr(100.0) came out as "1e+02".
    ; CPython chooses the digits first and the notation second: fixed when
    ; the decimal exponent is in [-4, 16), exponential otherwise.
    lea rdi, [rbp - FR_EBUF]
    mov esi, 48
    lea rdx, [rel fmt_e]
    mov ecx, [rbp - FR_PREC]
    dec ecx                   ; %e takes digits after the point
    movsd xmm0, [rbp - FR_VAL]
    mov eax, 1
    call snprintf wrt ..plt

    ; Read the exponent out of "d.dddde<sign>dd".
    lea rsi, [rbp - FR_EBUF]
    xor ecx, ecx
.fr_find_e:
    movzx eax, byte [rsi + rcx]
    test al, al
    jz .fr_use_e              ; no exponent: nothing to decide
    cmp al, 'e'
    je .fr_got_e
    inc rcx
    jmp .fr_find_e
.fr_got_e:
    inc rcx
    xor r8d, r8d              ; negative?
    movzx eax, byte [rsi + rcx]
    cmp al, '-'
    jne .fr_exp_sign_done
    mov r8d, 1
    inc rcx
    jmp .fr_exp_digits
.fr_exp_sign_done:
    cmp al, '+'
    jne .fr_exp_digits
    inc rcx
.fr_exp_digits:
    xor r9d, r9d
.fr_exp_loop:
    movzx eax, byte [rsi + rcx]
    cmp al, '0'
    jb .fr_exp_done
    cmp al, '9'
    ja .fr_exp_done
    imul r9, r9, 10
    sub rax, '0'
    add r9, rax
    inc rcx
    jmp .fr_exp_loop
.fr_exp_done:
    test r8d, r8d
    jz .fr_exp_positive
    neg r9
.fr_exp_positive:
    mov [rbp - FR_EXP], r9

    cmp r9, -4
    jl .fr_use_e
    cmp r9, 16
    jge .fr_use_e

    ; Fixed notation: digits after the point = (significant - 1) - exponent
    mov rcx, [rbp - FR_PREC]
    dec rcx
    sub rcx, r9
    jns .fr_fixed_prec_ok
    xor ecx, ecx
.fr_fixed_prec_ok:
    lea rdi, [rbp - FR_BUF]
    mov esi, 48
    lea rdx, [rel fmt_f]
    movsd xmm0, [rbp - FR_VAL]
    mov eax, 1
    call snprintf wrt ..plt
    jmp .fr_notation_done

.fr_use_e:
    ; Exponential: the %e rendering is already what CPython would print.
    lea rdi, [rbp - FR_BUF]
    lea rsi, [rbp - FR_EBUF]
    xor ecx, ecx
.fr_copy_e:
    movzx eax, byte [rsi + rcx]
    mov [rdi + rcx], al
    test al, al
    jz .fr_notation_done
    inc rcx
    cmp rcx, 47
    jl .fr_copy_e
    mov byte [rdi + rcx], 0

.fr_notation_done:
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
END_FUNC float_repr

;; ============================================================================
;; float_format_spec(rdi = raw double bits, rsi = spec data ptr, edx = spec length) -> PyStrObject*
;; Format float using a format spec string like ".2f", ".4e", etc.
;; ============================================================================
global float_format_spec
DEF_FUNC float_format_spec, 80
    and rsp, -16              ; ensure alignment

    movq xmm0, rdi
    movsd [rbp-8], xmm0      ; save value

    ; Parse spec: look for optional '.', digits, then type char (f/e/g)
    ; Simple parser: find precision and type
    mov [rbp-16], rsi         ; spec data
    mov [rbp-20], edx         ; spec len

    ; Default: precision=6, type='f'
    mov dword [rbp-24], 6     ; precision
    mov byte [rbp-25], 'g'    ; type

    ; Scan spec
    xor ecx, ecx              ; pos
    mov rsi, [rbp-16]

    ; Skip fill/align/sign/width for now — just look for '.' and type
.ffs_scan:
    cmp ecx, edx
    jge .ffs_have_spec
    movzx eax, byte [rsi + rcx]
    cmp al, '.'
    je .ffs_dot
    ; Check if it's a type char at the end
    cmp ecx, edx
    jge .ffs_have_spec
    inc ecx
    jmp .ffs_scan

.ffs_dot:
    ; Found '.': read precision digits
    inc ecx                   ; skip '.'
    xor eax, eax              ; precision = 0
.ffs_prec_loop:
    cmp ecx, edx
    jge .ffs_store_prec
    movzx edi, byte [rsi + rcx]
    sub edi, '0'
    cmp edi, 9
    ja .ffs_prec_done         ; not a digit
    imul eax, eax, 10
    add eax, edi
    inc ecx
    jmp .ffs_prec_loop
.ffs_prec_done:
    ; Next char should be type
    cmp ecx, edx
    jge .ffs_store_prec
    movzx edi, byte [rsi + rcx]
    mov [rbp-25], dil         ; type char
.ffs_store_prec:
    mov [rbp-24], eax

.ffs_have_spec:
    ; Format using snprintf with appropriate format string
    lea rdi, [rbp-76]         ; buffer (48 bytes)
    mov esi, 48               ; bufsz

    movzx eax, byte [rbp-25]  ; type char
    cmp al, 'f'
    je .ffs_use_f
    cmp al, 'e'
    je .ffs_use_e
    cmp al, 'E'
    je .ffs_use_E
    ; Default: use %.*g
    lea rdx, [rel fmt_g]
    jmp .ffs_do_snprintf
.ffs_use_f:
    lea rdx, [rel fmt_f]
    jmp .ffs_do_snprintf
.ffs_use_e:
    lea rdx, [rel fmt_e]
    jmp .ffs_do_snprintf
.ffs_use_E:
    lea rdx, [rel fmt_E]

.ffs_do_snprintf:
    mov ecx, [rbp-24]         ; precision
    movsd xmm0, [rbp-8]      ; value
    mov eax, 1                ; 1 xmm register
    call snprintf wrt ..plt

    lea rdi, [rbp-76]
    call str_from_cstr
    leave
    ret
END_FUNC float_format_spec

;; ============================================================================
;; float_hash(rdi = raw double bits) -> int64 in rax
;; For integer-valued floats, returns hash(int(x)) to match PEP requirement:
;;   hash(float(n)) == hash(n)
;; For non-integer floats, returns a hash derived from the raw bits.
;; ============================================================================
;; _Py_HashDouble, exactly.  The old code returned the truncated integer for
;; an integral float and an xor of the raw bits otherwise, so hash(1.5) bore
;; no relation to CPython's and hash(2**61) != hash(float(2**61)) -- an int
;; and an equal float landed in different dict slots.
FH_EXP   equ 8
FH_M     equ 16
FH_FRAME equ 32
DEF_FUNC float_hash, FH_FRAME
    push rbx
    push r12
    push r13
    movq xmm0, rdi

    ; Check NaN (unordered with itself)
    ucomisd xmm0, xmm0
    jp .fh_nan

    ; Check infinity
    movsd xmm1, [rel pos_inf]
    ucomisd xmm0, xmm1
    je .fh_pos_inf
    movsd xmm1, [rel neg_inf]
    ucomisd xmm0, xmm1
    je .fh_neg_inf

    ; sign, then frexp: v = m * 2^e with |m| in [0.5, 1)
    xor r13d, r13d                      ; sign = +1
    xorpd xmm1, xmm1
    ucomisd xmm0, xmm1
    jae .fh_positive
    mov r13d, 1                         ; sign = -1
    movsd xmm1, [rel fh_sign_mask]
    xorpd xmm0, xmm1                    ; m = -m
.fh_positive:
    lea rdi, [rbp - FH_EXP]
    extern frexp
    call frexp wrt ..plt
    movsd [rbp - FH_M], xmm0
    movsxd r12, dword [rbp - FH_EXP]    ; r12 = e

    xor ebx, ebx                        ; x = 0
    mov r10, PYHASH_MODULUS
.fh_loop:
    movsd xmm0, [rbp - FH_M]
    xorpd xmm1, xmm1
    ucomisd xmm0, xmm1
    jp .fh_loop_done
    je .fh_loop_done

    ; x = ((x << 28) & MODULUS) | (x >> 33)
    mov rax, rbx
    shl rax, 28
    and rax, r10
    mov rcx, rbx
    shr rcx, 33
    or rax, rcx
    mov rbx, rax

    ; m *= 2**28; e -= 28
    mulsd xmm0, [rel fh_two28]
    sub r12, 28

    ; y = (uint64)m; m -= y; x += y
    cvttsd2si rax, xmm0
    mov r11, rax
    cvtsi2sd xmm1, rax
    subsd xmm0, xmm1
    movsd [rbp - FH_M], xmm0
    add rbx, r11
    cmp rbx, r10
    jb .fh_loop
    sub rbx, r10
    jmp .fh_loop
.fh_loop_done:

    ; e mod 61, taken toward -infinity
    test r12, r12
    js .fh_neg_exp
    mov rax, r12
    xor edx, edx
    mov rcx, 61
    div rcx
    mov r12, rdx
    jmp .fh_rotate
.fh_neg_exp:
    mov rax, r12
    not rax                             ; -1 - e
    xor edx, edx
    mov rcx, 61
    div rcx
    mov r12, 60
    sub r12, rdx
.fh_rotate:
    ; x = ((x << e) & MODULUS) | (x >> (61 - e))
    mov rax, rbx
    mov rcx, r12
    shl rax, cl
    and rax, r10
    mov rdx, rbx
    mov rcx, 61
    sub rcx, r12
    shr rdx, cl
    or rax, rdx

    test r13d, r13d
    jz .fh_signed
    neg rax
.fh_signed:
    cmp rax, -1
    jne .fh_done
    mov rax, -2
.fh_done:
    pop r13
    pop r12
    pop rbx
    leave
    ret

.fh_nan:
    xor eax, eax              ; hash(nan) = 0
    pop r13
    pop r12
    pop rbx
    leave
    ret

.fh_pos_inf:
    mov rax, 314159            ; hash(inf) = 314159 (CPython convention)
    pop r13
    pop r12
    pop rbx
    leave
    ret

.fh_neg_inf:
    mov rax, -314159           ; hash(-inf) = -314159
    pop r13
    pop r12
    pop rbx
    leave
    ret
END_FUNC float_hash

section .rodata
align 16
fh_sign_mask: dq 0x8000000000000000, 0
align 8
fh_two28:     dq 0x41B0000000000000      ; 2.0**28
section .text

;; ============================================================================
;; float_bool(rdi = raw double bits) -> int (0 or 1) in eax
;; ============================================================================
DEF_FUNC_BARE float_bool
    movq xmm0, rdi
    xorpd xmm1, xmm1         ; xmm1 = 0.0
    ucomisd xmm0, xmm1
    je .is_zero
    mov eax, 1
    ret
.is_zero:
    ; Also check for -0.0 (which is also falsy)
    xor eax, eax
    ret
END_FUNC float_bool

;; float_dealloc removed — floats are inline (TAG_FLOAT), no heap allocation

;; ============================================================================
;; Binary arithmetic: float_add, float_sub, float_mul, float_truediv,
;;                    float_floordiv, float_mod, float_neg
;; All take (PyObject *a, PyObject *b) -> PyObject*
;; Convert both to double, perform operation, return new float.
;; ============================================================================

; Helper macro: convert both args to doubles
; Uses rbp frame with [rbp-8] = left double, [rbp-16] = right double
%macro FLOAT_BINOP_SETUP 0
    ; rdi=left, rsi=right, edx=left_tag, ecx=right_tag
    mov [rbp-24], rsi          ; save right operand
    mov dword [rbp-32], ecx    ; save right_tag
    mov esi, edx               ; esi = left_tag for float_to_f64
    call float_to_f64          ; rdi = left → xmm0
    movsd [rbp-8], xmm0
    mov rdi, [rbp-24]
    mov esi, dword [rbp-32]    ; esi = right_tag
    call float_to_f64          ; xmm0 = right as double
    movsd [rbp-16], xmm0
%endmacro

DEF_FUNC float_add, 32
    V_UNPACK rdi, rdx           ; left  Value -> (payload, tag)
    V_UNPACK rsi, rcx           ; right Value -> (payload, tag)
    FLOAT_BINOP_SETUP
    movsd xmm0, [rbp-8]
    addsd xmm0, [rbp-16]
    call float_from_f64
    leave
    V_PACK rax, rdx             ; return one Value
    ret
END_FUNC float_add

DEF_FUNC float_sub, 32
    V_UNPACK rdi, rdx           ; left  Value -> (payload, tag)
    V_UNPACK rsi, rcx           ; right Value -> (payload, tag)
    FLOAT_BINOP_SETUP
    movsd xmm0, [rbp-8]
    subsd xmm0, [rbp-16]
    call float_from_f64
    leave
    V_PACK rax, rdx             ; return one Value
    ret
END_FUNC float_sub

DEF_FUNC float_mul, 32
    V_UNPACK rdi, rdx           ; left  Value -> (payload, tag)
    V_UNPACK rsi, rcx           ; right Value -> (payload, tag)
    FLOAT_BINOP_SETUP
    movsd xmm0, [rbp-8]
    mulsd xmm0, [rbp-16]
    call float_from_f64
    leave
    V_PACK rax, rdx             ; return one Value
    ret
END_FUNC float_mul

DEF_FUNC float_truediv, 32
    V_UNPACK rdi, rdx           ; left  Value -> (payload, tag)
    V_UNPACK rsi, rcx           ; right Value -> (payload, tag)
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
    V_PACK rax, rdx             ; return one Value
    ret

.div_zero:
    lea rdi, [rel exc_ZeroDivisionError_type]
    CSTRING rsi, "float division by zero"
    call raise_exception
END_FUNC float_truediv

DEF_FUNC float_floordiv, 32
    V_UNPACK rdi, rdx           ; left  Value -> (payload, tag)
    V_UNPACK rsi, rcx           ; right Value -> (payload, tag)
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
    V_PACK rax, rdx             ; return one Value
    ret

.floordiv_zero:
    lea rdi, [rel exc_ZeroDivisionError_type]
    CSTRING rsi, "float floor division by zero"
    call raise_exception
END_FUNC float_floordiv

DEF_FUNC float_mod, 32
    V_UNPACK rdi, rdx           ; left  Value -> (payload, tag)
    V_UNPACK rsi, rcx           ; right Value -> (payload, tag)
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
    V_PACK rax, rdx             ; return one Value
    ret

.mod_zero:
    lea rdi, [rel exc_ZeroDivisionError_type]
    CSTRING rsi, "float modulo"
    call raise_exception
END_FUNC float_mod

DEF_FUNC_BARE float_neg
    ; rdi = raw double bits (payload), edx = tag
    ; Negate: flip sign bit (bit 63)
    btc rdi, 63
    mov rax, rdi
    mov edx, TAG_FLOAT
    ret
END_FUNC float_neg

;; ============================================================================
;; float_pos(rdi = left, rsi = right, edx = left_tag, ecx = right_tag)
;; Unary positive: identity for floats.
;; Note: called via nb_positive slot — only left operand matters.
;; ============================================================================
DEF_FUNC_BARE float_pos
    mov rax, rdi
    mov edx, TAG_FLOAT
    ret
END_FUNC float_pos

;; ============================================================================
;; float_pow(rdi = left, rsi = right, edx = left_tag, ecx = right_tag)
;; Compute left ** right, returning TAG_FLOAT result.
;; Both args are converted to double. Uses x87 fyl2x/f2xm1/fscale for
;; non-integer exponents, repeated squaring for integer exponents.
;; ============================================================================
DEF_FUNC float_pow, 32
    V_UNPACK rdi, rdx           ; left  Value -> (payload, tag)
    V_UNPACK rsi, rcx           ; right Value -> (payload, tag)
    FLOAT_BINOP_SETUP
    ; [rbp-8] = left double, [rbp-16] = right double

    movsd xmm0, [rbp-8]        ; base
    movsd xmm1, [rbp-16]       ; exp

    ; Fast path: exp == 0.5 → sqrtsd (~12 cycles vs ~100+ for general)
    movsd xmm2, [rel const_half_f]
    ucomisd xmm1, xmm2
    jne .not_sqrt
    jp .not_sqrt
    ; base >= 0 check (negative base → general path for complex/error)
    xorpd xmm3, xmm3
    ucomisd xmm0, xmm3
    jb .fpow_general
    sqrtsd xmm0, xmm0
    call float_from_f64
    leave
    V_PACK rax, rdx             ; return one Value
    ret
.not_sqrt:
    ; Fast path: exp == 2.0 → mulsd
    movsd xmm2, [rel const_two_f]
    ucomisd xmm1, xmm2
    jne .check_int_exp
    jp .check_int_exp
    mulsd xmm0, xmm0
    call float_from_f64
    leave
    V_PACK rax, rdx             ; return one Value
    ret

.check_int_exp:
    ; Check if exponent is an integer
    cvtsd2si rcx, xmm1
    cvtsi2sd xmm2, rcx
    ucomisd xmm1, xmm2
    jne .fpow_general           ; non-integer exp
    jp .fpow_general            ; NaN exp

    ; Integer exponent: repeated squaring
    test rcx, rcx
    js .fpow_neg

    ; Non-negative integer exponent
    mov rax, rcx                ; exponent
    movsd xmm2, [rel const_one_f] ; result = 1.0
.fpow_sq:
    test rax, rax
    jz .fpow_sq_done
    test rax, 1
    jz .fpow_sq_even
    mulsd xmm2, xmm0
.fpow_sq_even:
    mulsd xmm0, xmm0
    shr rax, 1
    jmp .fpow_sq
.fpow_sq_done:
    movapd xmm0, xmm2
    call float_from_f64
    leave
    V_PACK rax, rdx             ; return one Value
    ret

.fpow_neg:
    neg rcx
    mov rax, rcx
    movsd xmm2, [rel const_one_f] ; result = 1.0
.fpow_neg_sq:
    test rax, rax
    jz .fpow_neg_done
    test rax, 1
    jz .fpow_neg_even
    mulsd xmm2, xmm0
.fpow_neg_even:
    mulsd xmm0, xmm0
    shr rax, 1
    jmp .fpow_neg_sq
.fpow_neg_done:
    movsd xmm0, [rel const_one_f]
    divsd xmm0, xmm2
    call float_from_f64
    leave
    V_PACK rax, rdx             ; return one Value
    ret

.fpow_general:
    ; Non-integer exponent: x^y = 2^(y * log2(x))
    ; xmm0 = base, xmm1 = exp
    sub rsp, 16
    movsd [rsp], xmm1          ; exp on stack
    fld qword [rsp]             ; st(0) = exp
    movsd [rsp], xmm0          ; base on stack
    fld qword [rsp]             ; st(0) = base, st(1) = exp
    fyl2x                       ; st(0) = exp * log2(base)
    ; Compute 2^st(0): split into int + frac
    fld st0                     ; dup
    frndint                     ; st(0) = int part
    fsub st1, st0               ; st(1) = frac part
    fxch st1                    ; st(0) = frac, st(1) = int
    f2xm1                       ; st(0) = 2^frac - 1
    fld1
    faddp st1, st0              ; st(0) = 2^frac
    fscale                      ; st(0) = 2^frac * 2^int = result
    fstp st1                    ; pop int part
    fstp qword [rsp]            ; store result
    movsd xmm0, [rsp]
    add rsp, 16
    call float_from_f64
    leave
    V_PACK rax, rdx             ; return one Value
    ret
END_FUNC float_pow

;; ============================================================================
;; float_int(rdi = raw double bits) -> SmallInt or GMP int
;; Convert float to int by truncation.
;; ============================================================================
DEF_FUNC float_int

    movq xmm0, rdi

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
    leave
    ret

.not_finite:
    lea rdi, [rel exc_ValueError_type]
    CSTRING rsi, "cannot convert float NaN or infinity to integer"
    call raise_exception
END_FUNC float_int

;; ============================================================================
;; float_compare(PyObject *a, PyObject *b, int op) -> PyObject*
;; op: PY_LT=0, PY_LE=1, PY_EQ=2, PY_NE=3, PY_GT=4, PY_GE=5
;; Handles mixed int/float comparisons.
;; ============================================================================
DEF_FUNC float_compare, 40
    V_UNPACK rdi, rcx           ; left  Value -> (payload, tag)
    V_UNPACK rsi, r8            ; right Value -> (payload, tag)
    ; rdi=left, rsi=right, edx=op, ecx=left_tag, r8d=right_tag
    ; Validate both operands are numeric (float, int, or bool)
    ; Left tag
    cmp ecx, TAG_FLOAT
    je .fc_left_ok
    cmp ecx, TAG_SMALLINT
    je .fc_left_ok
    cmp ecx, TAG_PTR
    jne .fc_not_impl
    mov rax, [rdi + PyObject.ob_type]
    lea r9, [rel int_type]
    cmp rax, r9
    je .fc_left_ok
    lea r9, [rel float_type]
    cmp rax, r9
    je .fc_left_ok
    ; A bool is an int, and float_to_f64 already handles one; only this
    ; whitelist was missing it, so True < 2.5 raised and 1.0 == True was
    ; False -- which also put True and 1.0 in different dict slots.
    lea r9, [rel bool_type]
    cmp rax, r9
    je .fc_left_ok
    mov rax, [rax + PyTypeObject.tp_flags]
    test rax, TYPE_FLAG_INT_SUBCLASS
    jnz .fc_left_ok
    jmp .fc_not_impl
.fc_left_ok:
    ; Right tag
    cmp r8d, TAG_FLOAT
    je .fc_right_ok
    cmp r8d, TAG_SMALLINT
    je .fc_right_ok
    cmp r8d, TAG_PTR
    jne .fc_not_impl
    mov rax, [rsi + PyObject.ob_type]
    lea r9, [rel int_type]
    cmp rax, r9
    je .fc_right_ok
    lea r9, [rel float_type]
    cmp rax, r9
    je .fc_right_ok
    lea r9, [rel bool_type]
    cmp rax, r9
    je .fc_right_ok
    mov rax, [rax + PyTypeObject.tp_flags]
    test rax, TYPE_FLAG_INT_SUBCLASS
    jnz .fc_right_ok
    jmp .fc_not_impl
.fc_right_ok:
    mov [rbp-24], edx          ; save op (4 bytes)

    ; Convert both to doubles
    mov [rbp-40], rsi          ; save right (8 bytes)
    mov dword [rbp-28], r8d    ; save right_tag (4 bytes, no overlap)
    mov esi, ecx               ; left_tag for float_to_f64
    call float_to_f64          ; left → xmm0
    movsd [rbp-8], xmm0
    mov rdi, [rbp-40]
    mov esi, dword [rbp-28]    ; right_tag
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
    cmp dword [rbp-24], PY_NE
    je .ret_true
    jmp .ret_false

.fc_not_impl:
    ; Operand is not numeric — return NULL (NotImplemented)
    RET_NULL
    leave
    ret

.ret_true:
    lea rax, [rel bool_true]
    inc qword [rax + PyObject.ob_refcnt]
    mov edx, TAG_PTR
    leave
    ret
.ret_false:
    lea rax, [rel bool_false]
    inc qword [rax + PyObject.ob_refcnt]
    mov edx, TAG_PTR
    leave
    ret
END_FUNC float_compare

;; ============================================================================
;; Data
;; ============================================================================
section .data

float_name_str: db "float", 0
str_nan: db "nan", 0
str_inf: db "inf", 0
str_neg_inf: db "-inf", 0
fmt_g: db "%.*g", 0
fmt_f: db "%.*f", 0
fmt_e: db "%.*e", 0
fmt_E: db "%.*E", 0

align 8
pos_inf:     dq 0x7FF0000000000000
neg_inf:     dq 0xFFF0000000000000
const_one_f:  dq 0x3FF0000000000000   ; 1.0 in IEEE 754
const_half_f: dq 0x3FE0000000000000   ; 0.5
const_two_f:  dq 0x4000000000000000   ; 2.0

align 8
global float_number_methods
float_number_methods:
    dq float_add              ; nb_add          +0
    dq float_sub              ; nb_subtract     +8
    dq float_mul              ; nb_multiply     +16
    dq float_mod              ; nb_remainder    +24
    dq 0                      ; nb_divmod       +32
    dq float_pow              ; nb_power        +40
    dq float_neg              ; nb_negative     +48
    dq float_pos              ; nb_positive     +56
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
    dq 0                        ; nb_iadd         +168
    dq 0                        ; nb_isub         +176
    dq 0                        ; nb_imul         +184
    dq 0                        ; nb_irem         +192
    dq 0                        ; nb_ipow         +200
    dq 0                        ; nb_ilshift      +208
    dq 0                        ; nb_irshift      +216
    dq 0                        ; nb_iand         +224
    dq 0                        ; nb_ixor         +232
    dq 0                        ; nb_ior          +240
    dq 0                        ; nb_ifloor_divide +248
    dq 0                        ; nb_itrue_divide +256
    dq 0 ; nb_matmul
    dq 0 ; nb_imatmul

align 8
extern type_type

global float_type
float_type:
    dq 1                      ; ob_refcnt (immortal)
    dq type_type              ; ob_type
    dq float_name_str         ; tp_name
    dq PyFloatObject_size     ; tp_basicsize
    dq 0                      ; tp_dealloc (inline floats, no heap alloc)
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
    dq 0                        ; tp_traverse
    dq 0                        ; tp_clear
    dq 0 ; tp_dictoffset
