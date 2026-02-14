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

section .text

extern ap_malloc
extern ap_free
extern str_from_cstr
extern bool_true
extern bool_false
extern none_singleton
extern bool_from_int
extern snprintf

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
extern __gmpz_pow_ui
extern __gmpz_neg
extern __gmpz_cmp
extern __gmpz_cmp_si
extern __gmpz_fits_slong_p
extern __gmpz_sizeinbase
extern __gmpz_set_str
extern free

;; ============================================================================
;; int_new_from_mpz - internal: alloc int obj, init mpz, copy source
;; Input:  rdi = ptr to source mpz_t
;; Output: rax = new PyIntObject*
;; ============================================================================
int_new_from_mpz:
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
    mov rsi, rbx
    call __gmpz_set wrt ..plt
    mov rax, r12
    pop r12
    pop rbx
    pop rbp
    ret

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
global int_from_i64
int_from_i64:
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

;; ============================================================================
;; int_from_i64_gmp(int64_t val) -> PyIntObject*
;; Always creates a GMP-backed integer (no SmallInt)
;; ============================================================================
global int_from_i64_gmp
int_from_i64_gmp:
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
    mov rsi, rbx
    call __gmpz_set_si wrt ..plt
    mov rax, r12
    pop r12
    pop rbx
    pop rbp
    ret

;; ============================================================================
;; smallint_to_pyint(SmallInt val) -> PyIntObject*
;; Decode SmallInt and create GMP-backed int
;; ============================================================================
global smallint_to_pyint
smallint_to_pyint:
    SMALLINT_DECODE rdi
    jmp int_from_i64_gmp

;; ============================================================================
;; int_from_cstr(const char *str, int base) -> PyIntObject*
;; Create integer from C string. Returns NULL on parse failure.
;; ============================================================================
global int_from_cstr
int_from_cstr:
    push rbp
    mov rbp, rsp
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
    pop rbp
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
    pop rbp
    ret

;; ============================================================================
;; int_to_i64(PyObject *obj) -> int64_t
;; Extract integer value as C int64. Handles SmallInt.
;; ============================================================================
global int_to_i64
int_to_i64:
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

;; ============================================================================
;; int_repr(PyObject *self) -> PyStrObject*
;; String representation. SmallInt uses snprintf, GMP uses gmpz_get_str.
;; ============================================================================
global int_repr
int_repr:
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
    ; Decode SmallInt → temp GMP int → recursive int_repr → cleanup
    push rbp
    mov rbp, rsp
    push rbx
    push r12
    ; RSP is 16-byte aligned (3 pushes from 8-aligned entry)
    mov rax, rdi
    SMALLINT_DECODE rax
    mov rdi, rax
    call int_from_i64_gmp      ; rax = temp GMP PyIntObject*
    mov rbx, rax               ; rbx = temp (callee-saved)
    mov rdi, rax
    call int_repr              ; rax = str object (heap obj, won't recurse here)
    mov r12, rax               ; r12 = str result
    mov rdi, rbx
    call int_dealloc           ; free temp GMP int
    mov rax, r12               ; return str object
    pop r12
    pop rbx
    pop rbp
    ret

;; ============================================================================
;; int_hash(PyObject *self) -> int64
;; SmallInt: decoded value. GMP: low bits via get_si. Never returns -1.
;; ============================================================================
global int_hash
int_hash:
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

;; ============================================================================
;; int_bool(PyObject *self) -> int (0 or 1)
;; SmallInt: decoded != 0. GMP: cmp_si(0) != 0.
;; ============================================================================
global int_bool
int_bool:
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

;; ============================================================================
;; int_add(PyObject *a, PyObject *b) -> PyObject*
;; SmallInt x SmallInt fast path with overflow check.
;; ============================================================================
global int_add
int_add:
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

;; ============================================================================
;; int_sub(PyObject *a, PyObject *b) -> PyObject*
;; ============================================================================
global int_sub
int_sub:
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

;; ============================================================================
;; int_mul(PyObject *a, PyObject *b) -> PyObject*
;; SmallInt x SmallInt: use imul with overflow detection
;; ============================================================================
global int_mul
int_mul:
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

;; ============================================================================
;; int_floordiv(PyObject *a, PyObject *b) -> PyObject*
;; ============================================================================
global int_floordiv
int_floordiv:
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

;; ============================================================================
;; int_mod(PyObject *a, PyObject *b) -> PyObject*
;; ============================================================================
global int_mod
int_mod:
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

;; ============================================================================
;; int_neg(PyObject *a) -> PyObject*
;; ============================================================================
global int_neg
int_neg:
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

;; ============================================================================
;; int_compare(PyObject *a, PyObject *b, int op) -> PyObject*
;; op: PY_LT=0 PY_LE=1 PY_EQ=2 PY_NE=3 PY_GT=4 PY_GE=5
;; ============================================================================
global int_compare
int_compare:
    push rbp
    mov rbp, rsp
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
    pop rbp
    ret
.ret_false:
    lea rax, [rel bool_false]
    inc qword [rax + PyObject.ob_refcnt]
    pop r12
    pop rbx
    pop rbp
    ret

;; ============================================================================
;; int_dealloc(PyObject *self)
;; Free GMP data + object. SmallInt guard.
;; ============================================================================
global int_dealloc
int_dealloc:
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

;; ============================================================================
;; Data
;; ============================================================================
section .data

int_name_str: db "int", 0

section .rodata
int_fmt_str: db "%ld", 0

section .data

align 8
global int_number_methods
int_number_methods:
    dq int_add              ; nb_add          +0
    dq int_sub              ; nb_subtract     +8
    dq int_mul              ; nb_multiply     +16
    dq int_mod              ; nb_remainder    +24
    dq 0                    ; nb_divmod       +32
    dq 0                    ; nb_power        +40
    dq int_neg              ; nb_negative     +48
    dq 0                    ; nb_positive     +56
    dq 0                    ; nb_absolute     +64
    dq int_bool             ; nb_bool         +72
    dq 0                    ; nb_invert       +80
    dq 0                    ; nb_lshift       +88
    dq 0                    ; nb_rshift       +96
    dq 0                    ; nb_and          +104
    dq 0                    ; nb_xor          +112
    dq 0                    ; nb_or           +120
    dq 0                    ; nb_int          +128
    dq 0                    ; nb_float        +136
    dq int_floordiv         ; nb_floor_divide +144
    dq 0                    ; nb_true_divide  +152
    dq 0                    ; nb_index        +160

align 8
global int_type
int_type:
    dq 1                    ; ob_refcnt (immortal)
    dq 0                    ; ob_type
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
