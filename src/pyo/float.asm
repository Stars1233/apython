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
    ; A float subclass stores its double inline at the same offset the base
    ; would, which is the whole reason it cannot come from instance_new.
    test rcx, TYPE_FLAG_FLOAT_SUBCLASS
    jnz .from_float_sub
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

.from_float_sub:
    movsd xmm0, [rdi + PyFloatObject.value]
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

FR_VALUE equ 8              ; the double being rendered
FR_PREC  equ 16             ; precision counter (low 4 bytes)
FR_BUF   equ 64             ; 48-byte render buffer
                            ; (the frame is built by hand below: `and rsp,-16`
                            ; then `sub rsp,160`, for libc's aligned SSE)
DEF_FUNC float_repr
    ; A float subclass arrives as a pointer with its double inline, and only
    ; the tag can say so: a subnormal's bit pattern is a small integer, which
    ; is exactly what a pointer looks like.  Every caller supplies edx.
    cmp edx, TAG_PTR
    jne .fr_have_bits
    mov rdi, [rdi + PyFloatObject.value]
.fr_have_bits:
    and rsp, -16              ; ensure 16-byte alignment for libc calls
    sub rsp, 160
    ; Stack layout:
    ;   [rbp - FR_VALUE]   = original double value (8 bytes)
    ;   [rbp - FR_PREC]  = precision counter (8 bytes, only low 4 used)
    ;   [rbp - FR_BUF]  = the 48-byte render buffer

    movq xmm0, rdi
    movsd [rbp - FR_VALUE], xmm0       ; save original value

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
    mov qword [rbp - FR_PREC], 1     ; prec = 1

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
    movsd xmm1, [rbp - FR_VALUE]      ; original
    ucomisd xmm0, xmm1
    je .repr_found             ; match! use this precision

    inc qword [rbp - FR_PREC]
    cmp qword [rbp - FR_PREC], 17
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
    lea rdi, [rbp - FR_BUF]
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
    lea rdi, [rbp - FR_BUF]
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
FS_VALUE   equ 8            ; the double being formatted
FS_SPEC    equ 16           ; spec data pointer
FS_SPECLEN equ 20           ; spec length (4 bytes)
FS_PREC    equ 24           ; precision (4 bytes)
FS_TYPE    equ 25           ; the type character
FS_BUF     equ 76           ; 48-byte render buffer
FS_FRAME   equ 80           ; + 0 pushes = 80
DEF_FUNC float_format_spec, FS_FRAME
    and rsp, -16              ; ensure alignment

    movq xmm0, rdi
    movsd [rbp - FS_VALUE], xmm0      ; save value

    ; Parse spec: look for optional '.', digits, then type char (f/e/g)
    ; Simple parser: find precision and type
    mov [rbp - FS_SPEC], rsi         ; spec data
    mov [rbp - FS_SPECLEN], edx         ; spec len

    ; Default: precision=6, type='f'
    mov dword [rbp - FS_PREC], 6     ; precision
    mov byte [rbp - FS_TYPE], 'g'    ; type

    ; Scan spec
    xor ecx, ecx              ; pos
    mov rsi, [rbp - FS_SPEC]

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
    mov [rbp - FS_TYPE], dil         ; type char
.ffs_store_prec:
    mov [rbp - FS_PREC], eax

.ffs_have_spec:
    ; Format using snprintf with appropriate format string
    lea rdi, [rbp - FS_BUF]         ; buffer (48 bytes)
    mov esi, 48               ; bufsz

    ; Each of the six letters has its own conversion.  The uppercase ones are
    ; not cosmetic: C99's %F and %G spell a non-finite result INF and NAN,
    ; which is the whole difference CPython draws between 'f' and 'F'.  'F'
    ; used to share fmt_f, so format(float('inf'), 'F') was "inf"; 'G' had no
    ; case at all and fell to the %g default, so format(1e20, 'G') was
    ; "1e+20".  'E' was right by accident of already having fmt_E.
    movzx eax, byte [rbp - FS_TYPE]  ; type char
    cmp al, 'f'
    je .ffs_use_f
    cmp al, 'F'
    je .ffs_use_F
    cmp al, 'e'
    je .ffs_use_e
    cmp al, 'E'
    je .ffs_use_E
    cmp al, 'G'
    je .ffs_use_G
    ; Default: use %.*g
    lea rdx, [rel fmt_g]
    jmp .ffs_do_snprintf
.ffs_use_f:
    lea rdx, [rel fmt_f]
    jmp .ffs_do_snprintf
.ffs_use_F:
    lea rdx, [rel fmt_F]
    jmp .ffs_do_snprintf
.ffs_use_e:
    lea rdx, [rel fmt_e]
    jmp .ffs_do_snprintf
.ffs_use_G:
    lea rdx, [rel fmt_G]
    jmp .ffs_do_snprintf
.ffs_use_E:
    lea rdx, [rel fmt_E]

.ffs_do_snprintf:
    mov ecx, [rbp - FS_PREC]         ; precision
    movsd xmm0, [rbp - FS_VALUE]      ; value
    mov eax, 1                ; 1 xmm register
    call snprintf wrt ..plt

    lea rdi, [rbp - FS_BUF]
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
FH_FRAME equ 32             ; + 3 pushes = 56, not 16-aligned
DEF_FUNC float_hash, FH_FRAME
    push rbx
    push r12
    push r13
    ; As float_repr: a subclass instance is a pointer, and the tag is the only
    ; thing that distinguishes it from the bits of a subnormal.
    cmp edx, TAG_PTR
    jne .fh_have_bits
    mov rdi, [rdi + PyFloatObject.value]
.fh_have_bits:
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
fh_two28:     dq 0x41b0000000000000      ; 2.0**28
section .text

;; ============================================================================
;; float_bool(rdi = raw double bits) -> int (0 or 1) in eax
;; ============================================================================
DEF_FUNC_BARE float_bool
    ; A subclass instance arrives as a pointer; nb_bool takes a Value, so an
    ; immediate arrives boxed rather than as raw bits.
    V_TEST_PTR rdi, rax
    ja .fbool_immediate
    mov rdi, [rdi + PyFloatObject.value]
    jmp .fbool_have_bits
.fbool_immediate:
    V_TO_F64 rdi
.fbool_have_bits:
    movq xmm0, rdi
    xorpd xmm1, xmm1         ; xmm1 = 0.0
    ucomisd xmm0, xmm1
    jp .fbool_true           ; UNORDERED sets ZF too: bool(nan) is True, and
    je .is_zero              ; the je alone made it False for a float subclass
.fbool_true:
    mov eax, 1
    ret
.is_zero:
    ; -0.0 compares equal to 0.0, so it lands here too, which is right.
    xor eax, eax
    ret
END_FUNC float_bool

;; float_dealloc removed — floats are inline (TAG_FLOAT), no heap allocation

;; ============================================================================
;; float_binop_accepts(rdi = payload, esi = tag) -> eax = 1 when this operand
;; is one float arithmetic can consume: a float or an int in any of its shapes.
;;
;; The single definition of what float arithmetic accepts.  It used to be
;; written out twice inside float_compare and a third time, differently, as
;; binop_is_number in src/opcodes/arith.asm; the point of naming it is that a
;; fourth copy cannot now drift from the others.
;; ============================================================================
DEF_FUNC_BARE float_binop_accepts
    cmp esi, TAG_FLOAT
    je .fba_yes
    cmp esi, TAG_SMALLINT
    je .fba_yes
    cmp esi, TAG_PTR
    jne .fba_no
    test rdi, rdi
    jz .fba_no
    mov rax, [rdi + PyObject.ob_type]
    lea rcx, [rel int_type]
    cmp rax, rcx
    je .fba_yes
    lea rcx, [rel float_type]
    cmp rax, rcx
    je .fba_yes
    ; A bool is an int, and float_to_f64 already handles one; only the
    ; whitelist was missing it, so True < 2.5 raised and 1.0 == True was
    ; False -- which also put True and 1.0 in different dict slots.
    lea rcx, [rel bool_type]
    cmp rax, rcx
    je .fba_yes
    mov rax, [rax + PyTypeObject.tp_flags]
    test rax, TYPE_FLAG_INT_SUBCLASS
    jnz .fba_yes
    ; A float subclass keeps its double where float_to_f64 can read it.
    test rax, TYPE_FLAG_FLOAT_SUBCLASS
    jnz .fba_yes
.fba_no:
    xor eax, eax
    ret
.fba_yes:
    mov eax, 1
    ret
END_FUNC float_binop_accepts

;; ============================================================================
;; Binary arithmetic: float_add, float_sub, float_mul, float_truediv,
;;                    float_floordiv, float_mod, float_neg
;; All take (PyObject *a, PyObject *b) -> PyObject*
;; Convert both to double, perform operation, return new float.
;; ============================================================================

; Helper macro: convert both args to doubles.  Every float binop shares this
; frame, so it is named once here rather than per function.
FB_LEFT   equ 8             ; left operand, as a double
FB_RIGHT  equ 16            ; right operand, as a double
FB_RSAVE  equ 24            ; the right operand across the first conversion
FB_RTAG   equ 32            ; and its tag
FB_LSAVE  equ 40            ; the left operand across the acceptance checks
FB_LTAG   equ 48            ; and its tag
FB_FRAME  equ 48            ; + 0 pushes = 48
%macro FLOAT_BINOP_SETUP 0
    ; rdi=left, rsi=right, edx=left_tag, ecx=right_tag
    ;
    ; Both operands are classified BEFORE either is converted.  float_to_f64
    ; answers 0.0 for anything it does not recognise (.ret_zero below), so a
    ; slot that converted first read a foreign object's bytes as a double and
    ; returned a number: `a = "s"; a %= 1.5` was 0.0 rather than a TypeError.
    ; Declining with a NULL Value hands the pair back to the protocol, which
    ; then tries the other operand and finally raises.
    mov [rbp - FB_RSAVE], rsi
    mov dword [rbp - FB_RTAG], ecx
    mov [rbp - FB_LSAVE], rdi
    mov dword [rbp - FB_LTAG], edx
    mov esi, edx                       ; esi = left_tag
    call float_binop_accepts
    test eax, eax
    jz %%decline
    mov rdi, [rbp - FB_RSAVE]
    mov esi, dword [rbp - FB_RTAG]
    call float_binop_accepts
    test eax, eax
    jnz %%accepted
%%decline:
    xor eax, eax                       ; NULL Value = NotImplemented
    leave
    ret
%%accepted:
    mov rdi, [rbp - FB_LSAVE]
    mov esi, dword [rbp - FB_LTAG]
    call float_to_f64          ; rdi = left → xmm0
    movsd [rbp - FB_LEFT], xmm0
    mov rdi, [rbp - FB_RSAVE]
    mov esi, dword [rbp - FB_RTAG]    ; esi = right_tag
    call float_to_f64          ; xmm0 = right as double
    movsd [rbp - FB_RIGHT], xmm0
%endmacro

DEF_FUNC float_add, FB_FRAME
    V_UNPACK rdi, rdx           ; left  Value -> (payload, tag)
    V_UNPACK rsi, rcx           ; right Value -> (payload, tag)
    FLOAT_BINOP_SETUP
    movsd xmm0, [rbp - FB_LEFT]
    addsd xmm0, [rbp - FB_RIGHT]
    call float_from_f64
    leave
    V_PACK rax, rdx             ; return one Value
    ret
END_FUNC float_add

DEF_FUNC float_sub, FB_FRAME
    V_UNPACK rdi, rdx           ; left  Value -> (payload, tag)
    V_UNPACK rsi, rcx           ; right Value -> (payload, tag)
    FLOAT_BINOP_SETUP
    movsd xmm0, [rbp - FB_LEFT]
    subsd xmm0, [rbp - FB_RIGHT]
    call float_from_f64
    leave
    V_PACK rax, rdx             ; return one Value
    ret
END_FUNC float_sub

DEF_FUNC float_mul, FB_FRAME
    V_UNPACK rdi, rdx           ; left  Value -> (payload, tag)
    V_UNPACK rsi, rcx           ; right Value -> (payload, tag)
    FLOAT_BINOP_SETUP
    movsd xmm0, [rbp - FB_LEFT]
    mulsd xmm0, [rbp - FB_RIGHT]
    call float_from_f64
    leave
    V_PACK rax, rdx             ; return one Value
    ret
END_FUNC float_mul

DEF_FUNC float_truediv, FB_FRAME
    V_UNPACK rdi, rdx           ; left  Value -> (payload, tag)
    V_UNPACK rsi, rcx           ; right Value -> (payload, tag)
    FLOAT_BINOP_SETUP

    ; Check for division by zero.  ucomisd sets ZF for UNORDERED too, so the
    ; je alone read a NaN divisor as a zero one and 1.0 / float("nan") raised
    ; instead of answering nan.
    movsd xmm1, [rbp - FB_RIGHT]
    xorpd xmm2, xmm2
    ucomisd xmm1, xmm2
    jp .div_nonzero
    je .div_zero
.div_nonzero:

    movsd xmm0, [rbp - FB_LEFT]
    divsd xmm0, xmm1
    call float_from_f64
    leave
    V_PACK rax, rdx             ; return one Value
    ret

.div_zero:
    RAISE exc_ZeroDivisionError_type, "float division by zero"
END_FUNC float_truediv

DEF_FUNC float_floordiv, FB_FRAME
    V_UNPACK rdi, rdx           ; left  Value -> (payload, tag)
    V_UNPACK rsi, rcx           ; right Value -> (payload, tag)
    FLOAT_BINOP_SETUP

    ; Check for division by zero; jp first, as in float_truediv.
    movsd xmm1, [rbp - FB_RIGHT]
    xorpd xmm2, xmm2
    ucomisd xmm1, xmm2
    jp .floordiv_nonzero
    je .floordiv_zero
.floordiv_nonzero:

    movsd xmm0, [rbp - FB_LEFT]
    divsd xmm0, xmm1
    ; Floor: round toward negative infinity
    roundsd xmm0, xmm0, 1     ; 1 = floor
    call float_from_f64
    leave
    V_PACK rax, rdx             ; return one Value
    ret

.floordiv_zero:
    RAISE exc_ZeroDivisionError_type, "float floor division by zero"
END_FUNC float_floordiv

DEF_FUNC float_mod, FB_FRAME
    V_UNPACK rdi, rdx           ; left  Value -> (payload, tag)
    V_UNPACK rsi, rcx           ; right Value -> (payload, tag)
    FLOAT_BINOP_SETUP

    ; Check for division by zero; jp first, as in float_truediv.
    movsd xmm1, [rbp - FB_RIGHT]
    xorpd xmm2, xmm2
    ucomisd xmm1, xmm2
    jp .mod_nonzero
    je .mod_zero
.mod_nonzero:

    ; a % b = a - floor(a/b) * b
    movsd xmm0, [rbp - FB_LEFT]       ; a
    movsd xmm1, [rbp - FB_RIGHT]      ; b
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
    RAISE exc_ZeroDivisionError_type, "float modulo"
END_FUNC float_mod

;; ============================================================================
;; float_neg / float_pos / float_abs (rdi = operand Value) -> a float Value
;;
;; Only a float subclass instance reaches these.  op_unary_negative flips an
;; immediate's sign bit itself and never consults the slot for one, and until
;; float had a subclass nothing else could arrive -- which is why these used
;; to return a fat pair into a caller that reads a Value, harmlessly, because
;; they were unreachable.  CPython gives -F(x) and abs(F(x)) a plain float
;; rather than an F, which is what returning an immediate does here.
;; ============================================================================
DEF_FUNC_BARE float_neg
    V_TEST_PTR rdi, rax
    ja .fneg_immediate
    mov rdi, [rdi + PyFloatObject.value]
    btc rdi, 63
    mov rax, rdi
    V_FROM_F64 rax, rdx
    mov edx, TAG_FLOAT
    ret
.fneg_immediate:
    V_TO_F64 rdi
    btc rdi, 63
    mov rax, rdi
    V_FROM_F64 rax, rdx
    mov edx, TAG_FLOAT
    ret
END_FUNC float_neg

DEF_FUNC_BARE float_abs
    V_TEST_PTR rdi, rax
    ja .fabs_immediate
    mov rdi, [rdi + PyFloatObject.value]
    jmp .fabs_have_bits
.fabs_immediate:
    V_TO_F64 rdi
.fabs_have_bits:
    btr rdi, 63                 ; clear the sign; abs(-0.0) is 0.0
    mov rax, rdi
    V_FROM_F64 rax, rdx
    mov edx, TAG_FLOAT
    ret
END_FUNC float_abs

;; ============================================================================
;; float_pos(rdi = left, rsi = right, edx = left_tag, ecx = right_tag)
;; Unary positive: identity for floats.
;; Note: called via nb_positive slot — only left operand matters.
;; ============================================================================
DEF_FUNC_BARE float_pos
    V_TEST_PTR rdi, rax
    ja .fpos_immediate
    mov rdi, [rdi + PyFloatObject.value]
    mov rax, rdi
    V_FROM_F64 rax, rdx
    mov edx, TAG_FLOAT
    ret
.fpos_immediate:
    mov rax, rdi                ; already a float Value
    mov edx, TAG_FLOAT
    ret
END_FUNC float_pos

;; ============================================================================
;; float_pow(rdi = left, rsi = right, edx = left_tag, ecx = right_tag)
;; Compute left ** right, returning TAG_FLOAT result.
;; Both args are converted to double. Uses x87 fyl2x/f2xm1/fscale for
;; non-integer exponents, repeated squaring for integer exponents.
;; ============================================================================
DEF_FUNC float_pow, FB_FRAME
    V_UNPACK rdi, rdx           ; left  Value -> (payload, tag)
    V_UNPACK rsi, rcx           ; right Value -> (payload, tag)
    FLOAT_BINOP_SETUP
    ; [rbp - FB_LEFT] = left double, [rbp - FB_RIGHT] = right double

    movsd xmm0, [rbp - FB_LEFT]        ; base
    movsd xmm1, [rbp - FB_RIGHT]       ; exp

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
;; float_int(rdi = self Value) -> SmallInt or GMP int
;; Convert float to int by truncation.
;;
;; A Value, as every other nb_ slot takes: an immediate, or a subclass
;; instance whose double sits inline at the base's offset.  It used to take
;; raw double bits, which every caller had to know and which made it the one
;; slot a generic thunk could not call.
;; ============================================================================
DEF_FUNC float_int
    V_TEST_PTR rdi, rax
    ja .fi_immediate
    mov rdi, [rdi + PyFloatObject.value]
    jmp .fi_have_bits
.fi_immediate:
    V_TO_F64 rdi
.fi_have_bits:
    movq xmm0, rdi

    ; Check for NaN/inf.  CPython words and TYPES these differently: a NaN is
    ; a ValueError, an infinity an OverflowError.  Both were the ValueError.
    ucomisd xmm0, xmm0
    jp .not_a_number

    movsd xmm1, [rel pos_inf]
    ucomisd xmm0, xmm1
    je .is_infinite
    movsd xmm1, [rel neg_inf]
    ucomisd xmm0, xmm1
    je .is_infinite

    ; Out of int64's range, cvttsd2si answers INT64_MIN and says nothing about
    ; it -- int(1e300) and int(2.0**70) were both -9223372036854775808.  A
    ; double is exactly representable as an integer whenever it has no
    ; fractional part, so the big ones go through GMP, which converts it
    ; exactly.
    movsd xmm1, [rel fi_two63]
    ucomisd xmm0, xmm1
    jae .fi_big
    movsd xmm1, [rel fi_neg_two63]
    ucomisd xmm0, xmm1
    jb .fi_big

    ; Truncate to int64
    cvttsd2si rdi, xmm0
    call int_from_i64
    leave
    V_PACK rax, rdx             ; one Value out, as the slot's callers expect
    ret

.fi_big:
    push rbx
    sub rsp, 24
    movsd [rsp], xmm0
    ; int_new_compact, not int_from_i64: the latter answers an IMMEDIATE for a
    ; small value, and there is no mpz on an immediate to set.
    xor edi, edi
    extern int_new_compact
    call int_new_compact        ; a compact zero, promoted below
    mov rbx, rax
    INT_NEED_MPZ rbx            ; initialises the mpz and clears compact
    lea rdi, [rbx + PyIntObject.mpz]
    movsd xmm0, [rsp]
    extern __gmpz_set_d
    call __gmpz_set_d wrt ..plt
    mov rax, rbx
    mov edx, TAG_PTR
    add rsp, 24
    pop rbx
    leave
    V_PACK rax, rdx             ; a pointer is its own Value; symmetry with above
    ret

.not_a_number:
    RAISE exc_ValueError_type, "cannot convert float NaN to integer"
.is_infinite:
    extern exc_OverflowError_type
    RAISE exc_OverflowError_type, "cannot convert float infinity to integer"
END_FUNC float_int

;; ============================================================================
;; float_compare(PyObject *a, PyObject *b, int op) -> PyObject*
;; op: PY_LT=0, PY_LE=1, PY_EQ=2, PY_NE=3, PY_GT=4, PY_GE=5
;; Handles mixed int/float comparisons.
;; ============================================================================
FC_LEFT  equ 8              ; left operand, as a double
FC_RIGHT equ 16             ; right operand, as a double
FC_OP    equ 24             ; the comparison opcode (4 bytes)
FC_RTAG  equ 28             ; right operand's tag (4 bytes)
FC_LTAG  equ 32             ; left operand's tag (4 bytes; 33..36 unused)
FC_RSAVE equ 40             ; the right operand across the first conversion
FC_LSAVE equ 48             ; and the left, across the two acceptance checks
FC_FRAME equ 48             ; + 0 pushes = 48
DEF_FUNC float_compare, FC_FRAME
    V_UNPACK rdi, rcx           ; left  Value -> (payload, tag)
    V_UNPACK rsi, r8            ; right Value -> (payload, tag)
    ; rdi=left, rsi=right, edx=op, ecx=left_tag, r8d=right_tag
    ;
    ; Both operands must be numeric.  The whitelist lives in
    ; float_binop_accepts, which the arithmetic slots share; comparison used to
    ; carry its own two copies of it.
    ; Everything the code below still needs goes to the frame first: the calls
    ; clobber every caller-saved register, and ecx (the left tag) and r8d (the
    ; right tag) are both read again after this block.
    mov [rbp - FC_OP], edx
    mov [rbp - FC_LSAVE], rdi
    mov [rbp - FC_RSAVE], rsi
    mov [rbp - FC_LTAG], ecx
    mov [rbp - FC_RTAG], r8d
    mov esi, ecx                    ; esi = left_tag
    call float_binop_accepts
    test eax, eax
    jz .fc_not_impl
    mov rdi, [rbp - FC_RSAVE]
    mov esi, [rbp - FC_RTAG]
    call float_binop_accepts
    test eax, eax
    jz .fc_not_impl
    mov rdi, [rbp - FC_LSAVE]
    mov rsi, [rbp - FC_RSAVE]
    mov edx, [rbp - FC_OP]
    mov ecx, [rbp - FC_LTAG]
    mov r8d, [rbp - FC_RTAG]
.fc_right_ok:

    ; Convert both to doubles
    mov [rbp - FC_RSAVE], rsi          ; save right (8 bytes)
    mov dword [rbp - FC_RTAG], r8d    ; save right_tag (4 bytes, no overlap)
    mov esi, ecx               ; left_tag for float_to_f64
    call float_to_f64          ; left → xmm0
    movsd [rbp - FC_LEFT], xmm0
    mov rdi, [rbp - FC_RSAVE]
    mov esi, dword [rbp - FC_RTAG]    ; right_tag
    call float_to_f64          ; right → xmm0
    movsd [rbp - FC_RIGHT], xmm0

    ; Compare
    movsd xmm0, [rbp - FC_LEFT]
    ucomisd xmm0, [rbp - FC_RIGHT]

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
    mov ecx, [rbp - FC_OP]          ; op
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
    cmp dword [rbp - FC_OP], PY_NE
    je .ret_true
    jmp .ret_false

.fc_not_impl:
    ; Operand is not numeric — return NULL (NotImplemented)
    RET_NULL
    leave
    ret

.ret_true:
    RET_TRUE
    leave
    ret
.ret_false:
    RET_FALSE
    leave
    ret
END_FUNC float_compare

;; ============================================================================
;; float_getattr(rdi = self Value, rsi = name str) -> rax = Value, or NULL
;;
;; real and imag, the two numbers.py asks for.  float has no numerator or
;; denominator -- CPython raises for those -- so the chain is shorter than
;; int's.  See int_getattr for why this is a strcmp chain and not a
;; descriptor.
;; ============================================================================
FG_SELF   equ 8
FG_NAME   equ 16
FG_FRAME  equ 16            ; + 0 pushes = 16

extern ap_strcmp
DEF_FUNC float_getattr, FG_FRAME
    mov [rbp - FG_SELF], rdi
    mov [rbp - FG_NAME], rsi

    lea rdi, [rsi + PyStrObject.data]
    CSTRING rsi, "real"
    call ap_strcmp
    test eax, eax
    jz .fg_real

    mov rdi, [rbp - FG_NAME]
    lea rdi, [rdi + PyStrObject.data]
    CSTRING rsi, "imag"
    call ap_strcmp
    test eax, eax
    jz .fg_imag

    RET_NULL
    leave
    V_PACK rax, rdx
    ret

.fg_real:
    mov rdi, [rbp - FG_SELF]
    call float_get_real
    leave
    ret

.fg_imag:
    mov rdi, [rbp - FG_SELF]
    call float_get_imag
    leave
    ret
END_FUNC float_getattr

;; ============================================================================
;; float_get_real(rdi = self Value) -> rax = Value
;;
;; Behind float.real, reached from the chain above and from the getset
;; descriptor in float_type.tp_dict.  A subclass instance answers with a
;; plain float, as CPython does.
;; ============================================================================
DEF_FUNC float_get_real
    mov rax, rdi
    V_TEST_PTR rax, rcx
    ja .fgr_out                 ; already a float immediate
    mov rax, [rax + PyFloatObject.value]
    V_FROM_F64 rax, rcx
.fgr_out:
    mov edx, TAG_FLOAT
    leave
    ret
END_FUNC float_get_real

;; ============================================================================
;; float_get_imag(rdi = self Value) -> rax = Value.  Always 0.0.
;; ============================================================================
DEF_FUNC float_get_imag
    xorpd xmm0, xmm0
    movq rax, xmm0
    V_FROM_F64 rax, rcx
    mov edx, TAG_FLOAT
    leave
    ret
END_FUNC float_get_imag


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
fmt_F: db "%.*F", 0
fmt_G: db "%.*G", 0

align 8
fi_two63:     dq 0x43e0000000000000   ; 2.0**63
fi_neg_two63: dq 0xc3e0000000000000   ; -(2.0**63)
pos_inf:     dq 0x7ff0000000000000
neg_inf:     dq 0xfff0000000000000
const_one_f:  dq 0x3ff0000000000000   ; 1.0 in IEEE 754
const_half_f: dq 0x3fe0000000000000   ; 0.5
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
    dq float_abs              ; nb_absolute     +64
    dq float_bool             ; nb_bool         +72
    dq 0                      ; nb_invert       +80
    dq 0                      ; nb_lshift       +88
    dq 0                      ; nb_rshift       +96
    dq 0                      ; nb_and          +104
    dq 0                      ; nb_xor          +112
    dq 0                      ; nb_or           +120
    dq float_int              ; nb_int          +128
    dq float_get_real       ; nb_float        +136 (a float is its own float)
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
    dq float_getattr          ; tp_getattr (.real / .imag)
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
    dq TYPE_FLAG_FLOAT_SUBCLASS ; tp_flags -- the family bit type_from_parts
                                ; hands down, so a subclass is recognisable
    dq 0                      ; tp_bases
    dq 0                        ; tp_traverse
    dq 0                        ; tp_clear
    dq 0 ; tp_dictoffset
