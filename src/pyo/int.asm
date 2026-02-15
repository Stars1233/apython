; int_obj.asm - Integer type (SmallInt tagged pointers + GMP arbitrary precision)
;
; SmallInt encoding: bit 63 set = SmallInt, bits 0-62 = signed value
; Range: -2^62 to 2^62 - 1
; Encoding: bts rax, 63 (set tag bit)
; Decoding: shl rax, 1 / sar rax, 1 (sign-extend from bit 62)
;
; PyIntObject layout (GMP-backed, heap-allocated):
;   +0  ob_refcnt (8 bytes)
;   +8  ob_type   (8 bytes)
;   +16 mpz       (16 bytes: _mp_alloc:4, _mp_size:4, _mp_d:8)
;   Total: PyIntObject_size = 32

%include "macros.inc"
%include "object.inc"
%include "types.inc"

extern ap_malloc
extern ap_free
extern str_from_cstr
extern bool_true
extern bool_false
extern none_singleton
extern bool_from_int
extern type_type

; GMP functions
extern __gmpz_init
extern __gmpz_clear
extern __gmpz_set_si
extern __gmpz_set
extern __gmpz_get_si
extern __gmpz_get_str
extern __gmpz_add
extern __gmpz_sub
extern __gmpz_mul
extern __gmpz_tdiv_q
extern __gmpz_tdiv_r
extern __gmpz_neg
extern __gmpz_cmp
extern __gmpz_cmp_si
extern __gmpz_sizeinbase
extern __gmpz_set_str
extern __gmpz_and
extern __gmpz_ior
extern __gmpz_xor
extern __gmpz_com
extern __gmpz_mul_2exp
extern __gmpz_fdiv_q_2exp
extern __gmpz_pow_ui
extern __gmpz_get_d

extern raise_exception
extern exc_TypeError_type
extern exc_ValueError_type
extern exc_ZeroDivisionError_type
extern float_from_f64

;; ============================================================================
;; int_new_from_mpz - internal: alloc int obj, init mpz, copy source
;; Input:  rdi = ptr to source mpz_t
;; Output: rax = new PyIntObject*
;; ============================================================================
DEF_FUNC_LOCAL int_new_from_mpz
    push rbx
    push r12
    mov rbx, rdi
    mov edi, PyIntObject_size
    call ap_malloc
    mov r12, rax
    mov qword [r12 + PyObject.ob_refcnt], 1
    lea rax, [rel int_type]
    mov [r12 + PyObject.ob_type], rax
    lea rdi, [r12 + PyIntObject.mpz]
    call __gmpz_init wrt ..plt
    lea rdi, [r12 + PyIntObject.mpz]
    mov rsi, rbx
    call __gmpz_set wrt ..plt
    mov rax, r12
    pop r12
    pop rbx
    leave
    ret
END_FUNC int_new_from_mpz

;; ============================================================================
;; SMALLINT_DECODE - decode SmallInt tagged pointer to int64
;; Input:  register with SmallInt
;; Output: same register with decoded signed value
;; ============================================================================
%macro SMALLINT_DECODE 1
    shl %1, 1
    sar %1, 1
%endmacro

;; ============================================================================
;; int_from_i64(int64_t val) -> PyObject* (SmallInt or PyIntObject*)
;; Creates SmallInt if value fits in 62-bit signed range, else GMP int
;; ============================================================================
DEF_FUNC_BARE int_from_i64
    ; Check if value fits in SmallInt range: -2^62 .. 2^62-1
    mov rax, rdi
    sar rax, 62            ; rax = sign-extension of bits 63:62
    inc rax                ; 0 or 1 if fits, else other
    cmp rax, 2
    jae int_from_i64_gmp   ; doesn't fit, use GMP
    ; Fits: encode as SmallInt
    mov rax, rdi
    bts rax, 63            ; set tag bit
    ret
END_FUNC int_from_i64

;; ============================================================================
;; int_from_i64_gmp(int64_t val) -> PyIntObject*
;; Always creates a GMP-backed integer (no SmallInt)
;; ============================================================================
DEF_FUNC int_from_i64_gmp
    push rbx
    push r12
    mov rbx, rdi
    mov edi, PyIntObject_size
    call ap_malloc
    mov r12, rax
    mov qword [r12 + PyObject.ob_refcnt], 1
    lea rax, [rel int_type]
    mov [r12 + PyObject.ob_type], rax
    lea rdi, [r12 + PyIntObject.mpz]
    call __gmpz_init wrt ..plt
    lea rdi, [r12 + PyIntObject.mpz]
    mov rsi, rbx
    call __gmpz_set_si wrt ..plt
    mov rax, r12
    pop r12
    pop rbx
    leave
    ret
END_FUNC int_from_i64_gmp

;; ============================================================================
;; smallint_to_pyint(SmallInt val) -> PyIntObject*
;; Decode SmallInt and create GMP-backed int
;; ============================================================================
DEF_FUNC_BARE smallint_to_pyint
    SMALLINT_DECODE rdi
    jmp int_from_i64_gmp
END_FUNC smallint_to_pyint

;; ============================================================================
;; int_from_cstr(const char *str, int base) -> PyIntObject*
;; Create integer from C string. Returns NULL on parse failure.
;; ============================================================================
DEF_FUNC int_from_cstr
    push rbx
    push r12
    push r13
    mov rbx, rdi
    mov r13d, esi
    mov edi, PyIntObject_size
    call ap_malloc
    mov r12, rax
    mov qword [r12 + PyObject.ob_refcnt], 1
    lea rax, [rel int_type]
    mov [r12 + PyObject.ob_type], rax
    lea rdi, [r12 + PyIntObject.mpz]
    call __gmpz_init wrt ..plt
    lea rdi, [r12 + PyIntObject.mpz]
    mov rsi, rbx
    mov edx, r13d
    call __gmpz_set_str wrt ..plt
    test eax, eax
    jnz .parse_fail
    mov rax, r12
    pop r13
    pop r12
    pop rbx
    leave
    ret
.parse_fail:
    lea rdi, [r12 + PyIntObject.mpz]
    call __gmpz_clear wrt ..plt
    mov rdi, r12
    call ap_free
    xor eax, eax
    pop r13
    pop r12
    pop rbx
    leave
    ret
END_FUNC int_from_cstr

;; ============================================================================
;; int_to_i64(PyObject *obj) -> int64_t
;; Extract integer value as C int64. Handles SmallInt.
;; ============================================================================
DEF_FUNC_BARE int_to_i64
    test rdi, rdi
    js .smallint
    push rbp
    mov rbp, rsp
    lea rdi, [rdi + PyIntObject.mpz]
    call __gmpz_get_si wrt ..plt
    pop rbp
    ret
.smallint:
    mov rax, rdi
    SMALLINT_DECODE rax
    ret
END_FUNC int_to_i64

;; ============================================================================
;; int_repr(PyObject *self) -> PyStrObject*
;; String representation. SmallInt uses snprintf, GMP uses gmpz_get_str.
;; ============================================================================
DEF_FUNC_BARE int_repr
    test rdi, rdi
    js .smallint

    ; GMP path
    push rbp
    mov rbp, rsp
    push rbx
    push r12
    ; RSP is 16-byte aligned here (3 pushes from 8-aligned entry)
    mov rbx, rdi
    lea rdi, [rbx + PyIntObject.mpz]
    mov esi, 10
    call __gmpz_sizeinbase wrt ..plt
    lea rdi, [rax + 3]
    call ap_malloc
    mov r12, rax               ; r12 = C string buffer
    mov rdi, r12
    mov esi, 10
    lea rdx, [rbx + PyIntObject.mpz]
    call __gmpz_get_str wrt ..plt
    mov rdi, r12
    call str_from_cstr
    mov rbx, rax               ; save str result (done with original obj)
    mov rdi, r12
    call ap_free               ; free C buffer
    mov rax, rbx               ; return str object
    pop r12
    pop rbx
    pop rbp
    ret

.smallint:
    ; Direct SmallInt repr: manual int-to-string, no GMP allocation
    push rbp
    mov rbp, rsp
    sub rsp, 32                ; 24 bytes buffer + alignment
    mov rax, rdi
    SMALLINT_DECODE rax        ; rax = decoded signed value

    ; Convert int64 to decimal string in stack buffer
    ; Write digits backwards from buf[23], then reverse
    lea rdi, [rbp - 32]       ; rdi = buffer start
    xor ecx, ecx              ; ecx = 0 (negative flag)
    test rax, rax
    jns .si_positive
    neg rax
    mov ecx, 1                ; mark negative
.si_positive:
    ; rax = absolute value, ecx = negative flag
    lea rsi, [rbp - 9]        ; rsi = write position (end of buffer area)
    mov byte [rsi], 0          ; null terminator
    dec rsi

    mov r8, 10
.si_digit_loop:
    xor edx, edx
    div r8                     ; rax = quotient, rdx = remainder
    add dl, '0'
    mov [rsi], dl
    dec rsi
    test rax, rax
    jnz .si_digit_loop

    ; Add minus sign if negative
    test ecx, ecx
    jz .si_no_minus
    mov byte [rsi], '-'
    dec rsi
.si_no_minus:
    ; rsi+1 points to start of string
    inc rsi
    mov rdi, rsi
    call str_from_cstr
    leave
    ret
END_FUNC int_repr

;; ============================================================================
;; int_hash(PyObject *self) -> int64
;; SmallInt: decoded value. GMP: low bits via get_si. Never returns -1.
;; ============================================================================
DEF_FUNC_BARE int_hash
    test rdi, rdi
    js .smallint

    push rbp
    mov rbp, rsp
    push rbx
    mov rbx, rdi
    lea rdi, [rbx + PyIntObject.mpz]
    call __gmpz_get_si wrt ..plt
    cmp rax, -1
    jne .done
    mov rax, -2
.done:
    pop rbx
    pop rbp
    ret

.smallint:
    mov rax, rdi
    SMALLINT_DECODE rax
    cmp rax, -1
    jne .si_done
    mov rax, -2
.si_done:
    ret
END_FUNC int_hash

;; ============================================================================
;; int_bool(PyObject *self) -> int (0 or 1)
;; SmallInt: decoded != 0. GMP: cmp_si(0) != 0.
;; ============================================================================
DEF_FUNC_BARE int_bool
    test rdi, rdi
    js .smallint

    push rbp
    mov rbp, rsp
    lea rdi, [rdi + PyIntObject.mpz]
    xor esi, esi
    call __gmpz_cmp_si wrt ..plt
    test eax, eax
    setne al
    movzx eax, al
    pop rbp
    ret

.smallint:
    mov rax, rdi
    shl rax, 1             ; shift out tag bit
    test rax, rax
    setnz al
    movzx eax, al
    ret
END_FUNC int_bool

;; ============================================================================
;; int_add(PyObject *a, PyObject *b) -> PyObject*
;; SmallInt x SmallInt fast path with overflow check.
;; ============================================================================
DEF_FUNC_BARE int_add
    ; Check both SmallInt
    mov rax, rdi
    or rax, rsi
    jns .gmp_path           ; either is heap ptr (bit 63 clear)

    ; Both SmallInt: decode and add
    mov rax, rdi
    SMALLINT_DECODE rax
    mov rcx, rsi
    SMALLINT_DECODE rcx
    add rax, rcx
    jo .gmp_path            ; overflow, fall back to GMP

    ; Result fits: encode as SmallInt
    bts rax, 63
    ret

.gmp_path:
    push rbp
    mov rbp, rsp
    push rbx
    push r12
    push r13
    mov rbx, rdi
    mov r12, rsi

    ; Convert SmallInt args to GMP if needed
    test rbx, rbx
    jns .a_ready
    mov rdi, rbx
    call smallint_to_pyint
    mov rbx, rax
    mov r13b, 1             ; flag: a was converted
    jmp .check_b
.a_ready:
    xor r13d, r13d
.check_b:
    test r12, r12
    jns .b_ready
    mov rdi, r12
    call smallint_to_pyint
    mov r12, rax
    or r13b, 2              ; flag: b was converted
.b_ready:
    ; Allocate result
    mov edi, PyIntObject_size
    call ap_malloc
    push rax                ; save result ptr
    mov qword [rax + PyObject.ob_refcnt], 1
    lea rcx, [rel int_type]
    mov [rax + PyObject.ob_type], rcx
    lea rdi, [rax + PyIntObject.mpz]
    call __gmpz_init wrt ..plt
    mov rax, [rsp]          ; reload result ptr
    lea rdi, [rax + PyIntObject.mpz]
    lea rsi, [rbx + PyIntObject.mpz]
    lea rdx, [r12 + PyIntObject.mpz]
    call __gmpz_add wrt ..plt

    ; Free any temp GMP ints
    test r13b, 1
    jz .no_free_a
    mov rdi, rbx
    call int_dealloc
.no_free_a:
    test r13b, 2
    jz .no_free_b
    mov rdi, r12
    call int_dealloc
.no_free_b:
    pop rax
    pop r13
    pop r12
    pop rbx
    pop rbp
    ret
END_FUNC int_add

;; ============================================================================
;; int_sub(PyObject *a, PyObject *b) -> PyObject*
;; ============================================================================
DEF_FUNC_BARE int_sub
    mov rax, rdi
    or rax, rsi
    jns .gmp_path

    mov rax, rdi
    SMALLINT_DECODE rax
    mov rcx, rsi
    SMALLINT_DECODE rcx
    sub rax, rcx
    jo .gmp_path
    bts rax, 63
    ret

.gmp_path:
    push rbp
    mov rbp, rsp
    push rbx
    push r12
    push r13
    mov rbx, rdi
    mov r12, rsi
    test rbx, rbx
    jns .a_ready
    mov rdi, rbx
    call smallint_to_pyint
    mov rbx, rax
    mov r13b, 1
    jmp .check_b
.a_ready:
    xor r13d, r13d
.check_b:
    test r12, r12
    jns .b_ready
    mov rdi, r12
    call smallint_to_pyint
    mov r12, rax
    or r13b, 2
.b_ready:
    mov edi, PyIntObject_size
    call ap_malloc
    push rax
    mov qword [rax + PyObject.ob_refcnt], 1
    lea rcx, [rel int_type]
    mov [rax + PyObject.ob_type], rcx
    lea rdi, [rax + PyIntObject.mpz]
    call __gmpz_init wrt ..plt
    mov rax, [rsp]
    lea rdi, [rax + PyIntObject.mpz]
    lea rsi, [rbx + PyIntObject.mpz]
    lea rdx, [r12 + PyIntObject.mpz]
    call __gmpz_sub wrt ..plt
    test r13b, 1
    jz .no_free_a
    mov rdi, rbx
    call int_dealloc
.no_free_a:
    test r13b, 2
    jz .no_free_b
    mov rdi, r12
    call int_dealloc
.no_free_b:
    pop rax
    pop r13
    pop r12
    pop rbx
    pop rbp
    ret
END_FUNC int_sub

;; ============================================================================
;; int_mul(PyObject *a, PyObject *b) -> PyObject*
;; SmallInt x SmallInt: use imul with overflow detection
;; ============================================================================
DEF_FUNC_BARE int_mul
    mov rax, rdi
    or rax, rsi
    jns .gmp_path

    mov rax, rdi
    SMALLINT_DECODE rax
    mov rcx, rsi
    SMALLINT_DECODE rcx
    imul rax, rcx
    jo .gmp_path
    ; Check result still fits SmallInt range
    mov rcx, rax
    sar rcx, 62
    inc rcx
    cmp rcx, 2
    jae .gmp_path
    bts rax, 63
    ret

.gmp_path:
    push rbp
    mov rbp, rsp
    push rbx
    push r12
    push r13
    mov rbx, rdi
    mov r12, rsi
    test rbx, rbx
    jns .a_ready
    mov rdi, rbx
    call smallint_to_pyint
    mov rbx, rax
    mov r13b, 1
    jmp .check_b
.a_ready:
    xor r13d, r13d
.check_b:
    test r12, r12
    jns .b_ready
    mov rdi, r12
    call smallint_to_pyint
    mov r12, rax
    or r13b, 2
.b_ready:
    mov edi, PyIntObject_size
    call ap_malloc
    push rax
    mov qword [rax + PyObject.ob_refcnt], 1
    lea rcx, [rel int_type]
    mov [rax + PyObject.ob_type], rcx
    lea rdi, [rax + PyIntObject.mpz]
    call __gmpz_init wrt ..plt
    mov rax, [rsp]
    lea rdi, [rax + PyIntObject.mpz]
    lea rsi, [rbx + PyIntObject.mpz]
    lea rdx, [r12 + PyIntObject.mpz]
    call __gmpz_mul wrt ..plt
    test r13b, 1
    jz .no_free_a
    mov rdi, rbx
    call int_dealloc
.no_free_a:
    test r13b, 2
    jz .no_free_b
    mov rdi, r12
    call int_dealloc
.no_free_b:
    pop rax
    pop r13
    pop r12
    pop rbx
    pop rbp
    ret
END_FUNC int_mul

;; ============================================================================
;; int_floordiv(PyObject *a, PyObject *b) -> PyObject*
;; ============================================================================
DEF_FUNC_BARE int_floordiv
    mov rax, rdi
    or rax, rsi
    jns .gmp_path

    ; SmallInt fast path
    mov rax, rdi
    SMALLINT_DECODE rax
    mov rcx, rsi
    SMALLINT_DECODE rcx
    test rcx, rcx
    jz .gmp_path            ; div by zero -> let GMP handle/crash
    cqo
    idiv rcx
    ; Check range (idiv result always fits if inputs fit)
    bts rax, 63
    ret

.gmp_path:
    push rbp
    mov rbp, rsp
    push rbx
    push r12
    push r13
    mov rbx, rdi
    mov r12, rsi
    test rbx, rbx
    jns .a_ready
    mov rdi, rbx
    call smallint_to_pyint
    mov rbx, rax
    mov r13b, 1
    jmp .check_b
.a_ready:
    xor r13d, r13d
.check_b:
    test r12, r12
    jns .b_ready
    mov rdi, r12
    call smallint_to_pyint
    mov r12, rax
    or r13b, 2
.b_ready:
    mov edi, PyIntObject_size
    call ap_malloc
    push rax
    mov qword [rax + PyObject.ob_refcnt], 1
    lea rcx, [rel int_type]
    mov [rax + PyObject.ob_type], rcx
    lea rdi, [rax + PyIntObject.mpz]
    call __gmpz_init wrt ..plt
    mov rax, [rsp]
    lea rdi, [rax + PyIntObject.mpz]
    lea rsi, [rbx + PyIntObject.mpz]
    lea rdx, [r12 + PyIntObject.mpz]
    call __gmpz_tdiv_q wrt ..plt
    test r13b, 1
    jz .no_free_a
    mov rdi, rbx
    call int_dealloc
.no_free_a:
    test r13b, 2
    jz .no_free_b
    mov rdi, r12
    call int_dealloc
.no_free_b:
    pop rax
    pop r13
    pop r12
    pop rbx
    pop rbp
    ret
END_FUNC int_floordiv

;; ============================================================================
;; int_mod(PyObject *a, PyObject *b) -> PyObject*
;; ============================================================================
DEF_FUNC_BARE int_mod
    mov rax, rdi
    or rax, rsi
    jns .gmp_path

    mov rax, rdi
    SMALLINT_DECODE rax
    mov rcx, rsi
    SMALLINT_DECODE rcx
    test rcx, rcx
    jz .gmp_path
    cqo
    idiv rcx
    mov rax, rdx            ; remainder is in rdx
    bts rax, 63
    ret

.gmp_path:
    push rbp
    mov rbp, rsp
    push rbx
    push r12
    push r13
    mov rbx, rdi
    mov r12, rsi
    test rbx, rbx
    jns .a_ready
    mov rdi, rbx
    call smallint_to_pyint
    mov rbx, rax
    mov r13b, 1
    jmp .check_b
.a_ready:
    xor r13d, r13d
.check_b:
    test r12, r12
    jns .b_ready
    mov rdi, r12
    call smallint_to_pyint
    mov r12, rax
    or r13b, 2
.b_ready:
    mov edi, PyIntObject_size
    call ap_malloc
    push rax
    mov qword [rax + PyObject.ob_refcnt], 1
    lea rcx, [rel int_type]
    mov [rax + PyObject.ob_type], rcx
    lea rdi, [rax + PyIntObject.mpz]
    call __gmpz_init wrt ..plt
    mov rax, [rsp]
    lea rdi, [rax + PyIntObject.mpz]
    lea rsi, [rbx + PyIntObject.mpz]
    lea rdx, [r12 + PyIntObject.mpz]
    call __gmpz_tdiv_r wrt ..plt
    test r13b, 1
    jz .no_free_a
    mov rdi, rbx
    call int_dealloc
.no_free_a:
    test r13b, 2
    jz .no_free_b
    mov rdi, r12
    call int_dealloc
.no_free_b:
    pop rax
    pop r13
    pop r12
    pop rbx
    pop rbp
    ret
END_FUNC int_mod

;; ============================================================================
;; int_neg(PyObject *a) -> PyObject*
;; ============================================================================
DEF_FUNC_BARE int_neg
    test rdi, rdi
    js .smallint

    ; GMP path
    push rbp
    mov rbp, rsp
    push rbx
    push r12
    mov rbx, rdi
    mov edi, PyIntObject_size
    call ap_malloc
    mov r12, rax
    mov qword [r12 + PyObject.ob_refcnt], 1
    lea rax, [rel int_type]
    mov [r12 + PyObject.ob_type], rax
    lea rdi, [r12 + PyIntObject.mpz]
    call __gmpz_init wrt ..plt
    lea rdi, [r12 + PyIntObject.mpz]
    lea rsi, [rbx + PyIntObject.mpz]
    call __gmpz_neg wrt ..plt
    mov rax, r12
    pop r12
    pop rbx
    pop rbp
    ret

.smallint:
    mov rax, rdi
    SMALLINT_DECODE rax
    neg rax
    ; Check if result fits (only -(-2^62) = 2^62 overflows)
    mov rcx, rax
    sar rcx, 62
    inc rcx
    cmp rcx, 2
    jae .neg_overflow
    bts rax, 63
    ret
.neg_overflow:
    ; Value is 2^62, doesn't fit SmallInt. Create GMP int.
    mov rdi, rax
    jmp int_from_i64_gmp
END_FUNC int_neg

;; ============================================================================
;; int_compare(PyObject *a, PyObject *b, int op) -> PyObject*
;; op: PY_LT=0 PY_LE=1 PY_EQ=2 PY_NE=3 PY_GT=4 PY_GE=5
;; ============================================================================
DEF_FUNC int_compare
    push rbx
    push r12

    mov ebx, edx            ; save op

    ; Check if both SmallInt
    mov rax, rdi
    and rax, rsi
    js .both_smallint

    ; At least one is GMP - need full path
    push r13
    mov r12, rdi
    mov r13, rsi

    ; Convert SmallInt a if needed
    test r12, r12
    jns .a_ok
    mov rdi, r12
    call smallint_to_pyint
    mov r12, rax
    jmp .cmp_convert_b
.a_ok:
.cmp_convert_b:
    test r13, r13
    jns .b_ok
    mov rdi, r13
    call smallint_to_pyint
    mov r13, rax
.b_ok:
    lea rdi, [r12 + PyIntObject.mpz]
    lea rsi, [r13 + PyIntObject.mpz]
    call __gmpz_cmp wrt ..plt
    mov r12d, eax
    pop r13
    jmp .dispatch_op

.both_smallint:
    ; Decode and compare directly
    mov rax, rdi
    SMALLINT_DECODE rax
    mov rcx, rsi
    SMALLINT_DECODE rcx
    cmp rax, rcx
    ; Set r12d to cmp-style result: -1, 0, or 1
    mov r12d, 0
    jz .dispatch_op
    mov r12d, -1
    jl .dispatch_op
    mov r12d, 1

.dispatch_op:
    cmp ebx, PY_LT
    je .do_lt
    cmp ebx, PY_LE
    je .do_le
    cmp ebx, PY_EQ
    je .do_eq
    cmp ebx, PY_NE
    je .do_ne
    cmp ebx, PY_GT
    je .do_gt
    jmp .do_ge

.do_lt:
    test r12d, r12d
    js .ret_true
    jmp .ret_false
.do_le:
    test r12d, r12d
    jle .ret_true
    jmp .ret_false
.do_eq:
    test r12d, r12d
    jz .ret_true
    jmp .ret_false
.do_ne:
    test r12d, r12d
    jnz .ret_true
    jmp .ret_false
.do_gt:
    test r12d, r12d
    jg .ret_true
    jmp .ret_false
.do_ge:
    test r12d, r12d
    jge .ret_true
    jmp .ret_false

.ret_true:
    lea rax, [rel bool_true]
    inc qword [rax + PyObject.ob_refcnt]
    pop r12
    pop rbx
    leave
    ret
.ret_false:
    lea rax, [rel bool_false]
    inc qword [rax + PyObject.ob_refcnt]
    pop r12
    pop rbx
    leave
    ret
END_FUNC int_compare

;; ============================================================================
;; int_dealloc(PyObject *self)
;; Free GMP data + object. SmallInt guard.
;; ============================================================================
DEF_FUNC_BARE int_dealloc
    ; SmallInt should never be deallocated
    test rdi, rdi
    js .bail

    push rbp
    mov rbp, rsp
    push rbx
    mov rbx, rdi
    lea rdi, [rbx + PyIntObject.mpz]
    call __gmpz_clear wrt ..plt
    mov rdi, rbx
    call ap_free
    pop rbx
    pop rbp
.bail:
    ret
END_FUNC int_dealloc

;; ============================================================================
;; Bitwise AND: int_and(PyObject *a, PyObject *b) -> PyObject*
;; ============================================================================
DEF_FUNC_BARE int_and
    mov rax, rdi
    or rax, rsi
    jns .gmp

    ; Both SmallInt
    mov rax, rdi
    and rax, rsi           ; AND preserves tag bit, result is valid SmallInt
    ret

.gmp:
    push rbp
    mov rbp, rsp
    push rbx
    push r12
    push r13
    mov rbx, rdi
    mov r12, rsi
    test rbx, rbx
    jns .a_ok
    mov rdi, rbx
    call smallint_to_pyint
    mov rbx, rax
    mov r13b, 1
    jmp .chk_b
.a_ok:
    xor r13d, r13d
.chk_b:
    test r12, r12
    jns .b_ok
    mov rdi, r12
    call smallint_to_pyint
    mov r12, rax
    or r13b, 2
.b_ok:
    mov edi, PyIntObject_size
    call ap_malloc
    push rax
    mov qword [rax + PyObject.ob_refcnt], 1
    lea rcx, [rel int_type]
    mov [rax + PyObject.ob_type], rcx
    lea rdi, [rax + PyIntObject.mpz]
    call __gmpz_init wrt ..plt
    mov rax, [rsp]
    lea rdi, [rax + PyIntObject.mpz]
    lea rsi, [rbx + PyIntObject.mpz]
    lea rdx, [r12 + PyIntObject.mpz]
    call __gmpz_and wrt ..plt
    test r13b, 1
    jz .na
    mov rdi, rbx
    call int_dealloc
.na:
    test r13b, 2
    jz .nb
    mov rdi, r12
    call int_dealloc
.nb:
    pop rax
    pop r13
    pop r12
    pop rbx
    pop rbp
    ret
END_FUNC int_and

;; ============================================================================
;; Bitwise OR: int_or(PyObject *a, PyObject *b) -> PyObject*
;; ============================================================================
DEF_FUNC_BARE int_or
    mov rax, rdi
    or rax, rsi
    jns .gmp

    ; Both SmallInt
    mov rax, rdi
    or rax, rsi            ; OR preserves tag bit
    ret

.gmp:
    push rbp
    mov rbp, rsp
    push rbx
    push r12
    push r13
    mov rbx, rdi
    mov r12, rsi
    test rbx, rbx
    jns .a_ok
    mov rdi, rbx
    call smallint_to_pyint
    mov rbx, rax
    mov r13b, 1
    jmp .chk_b
.a_ok:
    xor r13d, r13d
.chk_b:
    test r12, r12
    jns .b_ok
    mov rdi, r12
    call smallint_to_pyint
    mov r12, rax
    or r13b, 2
.b_ok:
    mov edi, PyIntObject_size
    call ap_malloc
    push rax
    mov qword [rax + PyObject.ob_refcnt], 1
    lea rcx, [rel int_type]
    mov [rax + PyObject.ob_type], rcx
    lea rdi, [rax + PyIntObject.mpz]
    call __gmpz_init wrt ..plt
    mov rax, [rsp]
    lea rdi, [rax + PyIntObject.mpz]
    lea rsi, [rbx + PyIntObject.mpz]
    lea rdx, [r12 + PyIntObject.mpz]
    call __gmpz_ior wrt ..plt
    test r13b, 1
    jz .na
    mov rdi, rbx
    call int_dealloc
.na:
    test r13b, 2
    jz .nb
    mov rdi, r12
    call int_dealloc
.nb:
    pop rax
    pop r13
    pop r12
    pop rbx
    pop rbp
    ret
END_FUNC int_or

;; ============================================================================
;; Bitwise XOR: int_xor(PyObject *a, PyObject *b) -> PyObject*
;; ============================================================================
DEF_FUNC_BARE int_xor
    mov rax, rdi
    or rax, rsi
    jns .gmp

    ; Both SmallInt: XOR values, must re-set tag bit
    mov rax, rdi
    SMALLINT_DECODE rax
    mov rcx, rsi
    SMALLINT_DECODE rcx
    xor rax, rcx
    bts rax, 63
    ret

.gmp:
    push rbp
    mov rbp, rsp
    push rbx
    push r12
    push r13
    mov rbx, rdi
    mov r12, rsi
    test rbx, rbx
    jns .a_ok
    mov rdi, rbx
    call smallint_to_pyint
    mov rbx, rax
    mov r13b, 1
    jmp .chk_b
.a_ok:
    xor r13d, r13d
.chk_b:
    test r12, r12
    jns .b_ok
    mov rdi, r12
    call smallint_to_pyint
    mov r12, rax
    or r13b, 2
.b_ok:
    mov edi, PyIntObject_size
    call ap_malloc
    push rax
    mov qword [rax + PyObject.ob_refcnt], 1
    lea rcx, [rel int_type]
    mov [rax + PyObject.ob_type], rcx
    lea rdi, [rax + PyIntObject.mpz]
    call __gmpz_init wrt ..plt
    mov rax, [rsp]
    lea rdi, [rax + PyIntObject.mpz]
    lea rsi, [rbx + PyIntObject.mpz]
    lea rdx, [r12 + PyIntObject.mpz]
    call __gmpz_xor wrt ..plt
    test r13b, 1
    jz .na
    mov rdi, rbx
    call int_dealloc
.na:
    test r13b, 2
    jz .nb
    mov rdi, r12
    call int_dealloc
.nb:
    pop rax
    pop r13
    pop r12
    pop rbx
    pop rbp
    ret
END_FUNC int_xor

;; ============================================================================
;; Bitwise NOT: int_invert(PyObject *a, PyObject *b_unused) -> PyObject*
;; ~x = -(x+1)
;; ============================================================================
DEF_FUNC_BARE int_invert
    test rdi, rdi
    js .smallint

    ; GMP path
    push rbp
    mov rbp, rsp
    push rbx
    mov rbx, rdi
    mov edi, PyIntObject_size
    call ap_malloc
    push rax
    mov qword [rax + PyObject.ob_refcnt], 1
    lea rcx, [rel int_type]
    mov [rax + PyObject.ob_type], rcx
    lea rdi, [rax + PyIntObject.mpz]
    call __gmpz_init wrt ..plt
    mov rax, [rsp]
    lea rdi, [rax + PyIntObject.mpz]
    lea rsi, [rbx + PyIntObject.mpz]
    call __gmpz_com wrt ..plt
    pop rax
    pop rbx
    pop rbp
    ret

.smallint:
    mov rax, rdi
    SMALLINT_DECODE rax
    not rax                ; ~x = -(x+1), works for all 62-bit values
    bts rax, 63
    ret
END_FUNC int_invert

;; ============================================================================
;; Left shift: int_lshift(PyObject *a, PyObject *b) -> PyObject*
;; ============================================================================
DEF_FUNC int_lshift
    push rbx
    push r12
    push r13

    mov rbx, rdi           ; left operand
    mov r12, rsi           ; right operand (shift amount)

    ; Get shift amount as int64
    test r12, r12
    js .shift_smallint
    ; GMP right operand: get as int64
    lea rdi, [r12 + PyIntObject.mpz]
    call __gmpz_get_si wrt ..plt
    mov r13, rax
    jmp .have_shift
.shift_smallint:
    mov r13, r12
    shl r13, 1
    sar r13, 1             ; decode SmallInt

.have_shift:
    ; r13 = shift amount
    test r13, r13
    js .neg_shift

    ; Convert left to GMP if needed
    xor ecx, ecx           ; flag: converted
    test rbx, rbx
    jns .a_gmp
    mov rdi, rbx
    call smallint_to_pyint
    mov rbx, rax
    mov cl, 1
.a_gmp:
    push rcx
    mov edi, PyIntObject_size
    call ap_malloc
    push rax
    mov qword [rax + PyObject.ob_refcnt], 1
    lea rcx, [rel int_type]
    mov [rax + PyObject.ob_type], rcx
    lea rdi, [rax + PyIntObject.mpz]
    call __gmpz_init wrt ..plt
    mov rax, [rsp]
    lea rdi, [rax + PyIntObject.mpz]
    lea rsi, [rbx + PyIntObject.mpz]
    mov rdx, r13           ; shift count
    call __gmpz_mul_2exp wrt ..plt
    pop rax
    pop rcx
    test cl, cl
    jz .lsh_done
    push rax
    mov rdi, rbx
    call int_dealloc
    pop rax
.lsh_done:
    pop r13
    pop r12
    pop rbx
    leave
    ret

.neg_shift:
    lea rdi, [rel exc_ValueError_type]
    CSTRING rsi, "negative shift count"
    call raise_exception
END_FUNC int_lshift

;; ============================================================================
;; Right shift: int_rshift(PyObject *a, PyObject *b) -> PyObject*
;; ============================================================================
DEF_FUNC int_rshift
    push rbx
    push r12
    push r13

    mov rbx, rdi
    mov r12, rsi

    ; Get shift amount
    test r12, r12
    js .shift_smallint
    lea rdi, [r12 + PyIntObject.mpz]
    call __gmpz_get_si wrt ..plt
    mov r13, rax
    jmp .have_shift
.shift_smallint:
    mov r13, r12
    shl r13, 1
    sar r13, 1

.have_shift:
    test r13, r13
    js .neg_shift

    ; SmallInt fast path
    test rbx, rbx
    jns .gmp_path
    mov rax, rbx
    SMALLINT_DECODE rax
    ; Arithmetic right shift
    mov rcx, r13
    cmp rcx, 63
    jge .max_shift
    sar rax, cl
    bts rax, 63
    pop r13
    pop r12
    pop rbx
    leave
    ret
.max_shift:
    ; Shift >= 63: result is 0 or -1 depending on sign
    sar rax, 63
    bts rax, 63
    pop r13
    pop r12
    pop rbx
    leave
    ret

.gmp_path:
    mov edi, PyIntObject_size
    call ap_malloc
    push rax
    mov qword [rax + PyObject.ob_refcnt], 1
    lea rcx, [rel int_type]
    mov [rax + PyObject.ob_type], rcx
    lea rdi, [rax + PyIntObject.mpz]
    call __gmpz_init wrt ..plt
    mov rax, [rsp]
    lea rdi, [rax + PyIntObject.mpz]
    lea rsi, [rbx + PyIntObject.mpz]
    mov rdx, r13
    call __gmpz_fdiv_q_2exp wrt ..plt
    pop rax
    pop r13
    pop r12
    pop rbx
    leave
    ret

.neg_shift:
    lea rdi, [rel exc_ValueError_type]
    CSTRING rsi, "negative shift count"
    call raise_exception
END_FUNC int_rshift

;; ============================================================================
;; Power: int_power(PyObject *a, PyObject *b) -> PyObject*
;; For small positive exponents, use GMP mpz_pow_ui
;; ============================================================================
DEF_FUNC int_power
    push rbx
    push r12
    push r13

    mov rbx, rdi           ; base
    mov r12, rsi           ; exponent

    ; Get exponent as int64
    test r12, r12
    js .exp_smallint
    lea rdi, [r12 + PyIntObject.mpz]
    call __gmpz_get_si wrt ..plt
    mov r13, rax
    jmp .have_exp
.exp_smallint:
    mov r13, r12
    shl r13, 1
    sar r13, 1

.have_exp:
    ; Negative exponent: return float (int ** -n = 1/int**n)
    test r13, r13
    js .neg_exp

    ; Convert base to GMP if needed
    xor ecx, ecx
    test rbx, rbx
    jns .base_gmp
    mov rdi, rbx
    call smallint_to_pyint
    mov rbx, rax
    mov cl, 1
.base_gmp:
    push rcx
    mov edi, PyIntObject_size
    call ap_malloc
    push rax
    mov qword [rax + PyObject.ob_refcnt], 1
    lea rcx, [rel int_type]
    mov [rax + PyObject.ob_type], rcx
    lea rdi, [rax + PyIntObject.mpz]
    call __gmpz_init wrt ..plt
    mov rax, [rsp]
    lea rdi, [rax + PyIntObject.mpz]
    lea rsi, [rbx + PyIntObject.mpz]
    mov rdx, r13           ; exponent (unsigned)
    call __gmpz_pow_ui wrt ..plt
    pop rax
    pop rcx
    test cl, cl
    jz .pow_done
    push rax
    mov rdi, rbx
    call int_dealloc
    pop rax
.pow_done:
    pop r13
    pop r12
    pop rbx
    leave
    ret

.neg_exp:
    ; int ** negative → float result (1.0 / base**abs(exp))
    ; For simplicity, convert both to double and use pow
    ; Actually, just raise a TypeError for now (like many impls)
    ; Python returns float for negative int power
    ; Convert base to double
    test rbx, rbx
    js .neg_exp_smallint
    lea rdi, [rbx + PyIntObject.mpz]
    call __gmpz_get_d wrt ..plt
    jmp .neg_exp_have_base
.neg_exp_smallint:
    mov rax, rbx
    shl rax, 1
    sar rax, 1
    cvtsi2sd xmm0, rax
.neg_exp_have_base:
    ; xmm0 = base as double
    ; Compute base ** exp using repeated multiply (simple)
    ; For now: 1.0 / (base ** abs(exp))
    neg r13                ; abs(exp)
    movsd xmm1, [rel one_double]    ; xmm1 = result = 1.0
.pow_loop:
    test r13, r13
    jz .pow_loop_done
    mulsd xmm1, xmm0
    dec r13
    jmp .pow_loop
.pow_loop_done:
    ; result = 1.0 / xmm1
    movsd xmm0, [rel one_double]
    divsd xmm0, xmm1
    call float_from_f64
    pop r13
    pop r12
    pop rbx
    leave
    ret
END_FUNC int_power

;; ============================================================================
;; True divide: int_true_divide(PyObject *a, PyObject *b) -> PyObject* (float)
;; int / int always returns float in Python
;; ============================================================================
DEF_FUNC int_true_divide
    and rsp, -16           ; align for potential libc calls
    push rbx
    push r12

    mov rbx, rdi           ; left
    mov r12, rsi           ; right

    ; Convert left to double
    test rbx, rbx
    js .td_left_small
    lea rdi, [rbx + PyIntObject.mpz]
    call __gmpz_get_d wrt ..plt
    jmp .td_have_left
.td_left_small:
    mov rax, rbx
    shl rax, 1
    sar rax, 1
    cvtsi2sd xmm0, rax
.td_have_left:
    movsd [rsp-8], xmm0   ; save left double

    ; Convert right to double
    test r12, r12
    js .td_right_small
    lea rdi, [r12 + PyIntObject.mpz]
    call __gmpz_get_d wrt ..plt
    jmp .td_have_right
.td_right_small:
    mov rax, r12
    shl rax, 1
    sar rax, 1
    cvtsi2sd xmm0, rax
.td_have_right:
    ; xmm0 = right double
    ; Check division by zero
    xorpd xmm1, xmm1
    ucomisd xmm0, xmm1
    je .td_divzero

    movsd xmm1, xmm0      ; xmm1 = right
    movsd xmm0, [rsp-8]   ; xmm0 = left
    divsd xmm0, xmm1
    call float_from_f64

    pop r12
    pop rbx
    leave
    ret

.td_divzero:
    lea rdi, [rel exc_ZeroDivisionError_type]
    CSTRING rsi, "division by zero"
    call raise_exception
END_FUNC int_true_divide

;; ============================================================================
;; Data
;; ============================================================================
section .data

align 8
one_double: dq 0x3FF0000000000000  ; 1.0

int_name_str: db "int", 0

section .data

align 8
global int_number_methods
int_number_methods:
    dq int_add              ; nb_add          +0
    dq int_sub              ; nb_subtract     +8
    dq int_mul              ; nb_multiply     +16
    dq int_mod              ; nb_remainder    +24
    dq 0                    ; nb_divmod       +32
    dq int_power            ; nb_power        +40
    dq int_neg              ; nb_negative     +48
    dq 0                    ; nb_positive     +56
    dq 0                    ; nb_absolute     +64
    dq int_bool             ; nb_bool         +72
    dq int_invert           ; nb_invert       +80
    dq int_lshift           ; nb_lshift       +88
    dq int_rshift           ; nb_rshift       +96
    dq int_and              ; nb_and          +104
    dq int_xor              ; nb_xor          +112
    dq int_or               ; nb_or           +120
    dq 0                    ; nb_int          +128
    dq 0                    ; nb_float        +136
    dq int_floordiv         ; nb_floor_divide +144
    dq int_true_divide      ; nb_true_divide  +152
    dq 0                    ; nb_index        +160

align 8
global int_type
int_type:
    dq 1                    ; ob_refcnt (immortal)
    dq type_type            ; ob_type
    dq int_name_str         ; tp_name
    dq PyIntObject_size     ; tp_basicsize
    dq int_dealloc          ; tp_dealloc
    dq int_repr             ; tp_repr
    dq int_repr             ; tp_str
    dq int_hash             ; tp_hash
    dq 0                    ; tp_call
    dq 0                    ; tp_getattr
    dq 0                    ; tp_setattr
    dq int_compare          ; tp_richcompare
    dq 0                    ; tp_iter
    dq 0                    ; tp_iternext
    dq 0                    ; tp_init
    dq 0                    ; tp_new
    dq int_number_methods   ; tp_as_number
    dq 0                    ; tp_as_sequence
    dq 0                    ; tp_as_mapping
    dq 0                    ; tp_base
    dq 0                    ; tp_dict
    dq 0                    ; tp_mro
    dq TYPE_FLAG_INT_SUBCLASS ; tp_flags
    dq 0                    ; tp_bases
