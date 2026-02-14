; builtins_extra.asm - Additional Python builtin functions
; Each builtin: name(PyObject **args, int64_t nargs) -> PyObject*
; args = borrowed refs; return = new ref

%include "macros.inc"
%include "object.inc"
%include "types.inc"

section .note.GNU-stack noalloc noexec nowrite progbits

; External symbols used
extern int_from_i64
extern int_to_i64
extern int_neg
extern int_add
extern int_from_cstr
extern float_from_f64
extern float_int
extern str_from_cstr
extern obj_str
extern obj_repr
extern obj_is_true
extern obj_incref
extern obj_decref
extern dict_get
extern raise_exception
extern none_singleton

extern int_type
extern float_type
extern str_type
extern bool_type
extern bool_true
extern bool_false

extern exc_TypeError_type
extern exc_ValueError_type
extern exc_AttributeError_type
extern exc_StopIteration_type

section .text

; ============================================================================
; 1. builtin_abs(args, nargs) - abs(x)
; ============================================================================
global builtin_abs
builtin_abs:
    push rbp
    mov rbp, rsp
    push rbx
    sub rsp, 8

    cmp rsi, 1
    jne .abs_error

    mov rbx, [rdi]

    test rbx, rbx
    js .abs_smallint

    mov rax, [rbx + PyObject.ob_type]
    lea rcx, [rel float_type]
    cmp rax, rcx
    je .abs_float

    lea rcx, [rel int_type]
    cmp rax, rcx
    jne .abs_type_error

    ; GMP int: check _mp_size at PyIntObject.mpz + 4
    mov eax, [rbx + PyIntObject.mpz + 4]
    test eax, eax
    jl .abs_gmp_neg

    inc qword [rbx + PyObject.ob_refcnt]
    mov rax, rbx
    add rsp, 8
    pop rbx
    pop rbp
    ret

.abs_gmp_neg:
    mov rdi, rbx
    call int_neg
    add rsp, 8
    pop rbx
    pop rbp
    ret

.abs_smallint:
    mov rax, rbx
    shl rax, 1
    sar rax, 1
    test rax, rax
    jns .abs_si_pos
    neg rax
.abs_si_pos:
    bts rax, 63
    add rsp, 8
    pop rbx
    pop rbp
    ret

.abs_float:
    movsd xmm0, [rbx + PyFloatObject.value]
    mov rax, 0x7FFFFFFFFFFFFFFF
    movq xmm1, rax
    andpd xmm0, xmm1
    call float_from_f64
    add rsp, 8
    pop rbx
    pop rbp
    ret

.abs_type_error:
    lea rdi, [rel exc_TypeError_type]
    CSTRING rsi, "bad operand type for abs()"
    call raise_exception

.abs_error:
    lea rdi, [rel exc_TypeError_type]
    CSTRING rsi, "abs() takes exactly one argument"
    call raise_exception

; ============================================================================
; 2. builtin_int_fn(args, nargs) - int(x)
; ============================================================================
global builtin_int_fn
builtin_int_fn:
    push rbp
    mov rbp, rsp
    push rbx
    sub rsp, 8

    test rsi, rsi
    jz .int_no_args

    cmp rsi, 1
    jne .int_error

    mov rbx, [rdi]

    test rbx, rbx
    js .int_return_smallint

    mov rax, [rbx + PyObject.ob_type]

    lea rcx, [rel bool_type]
    cmp rax, rcx
    je .int_from_bool

    lea rcx, [rel int_type]
    cmp rax, rcx
    je .int_from_int

    lea rcx, [rel float_type]
    cmp rax, rcx
    je .int_from_float

    lea rcx, [rel str_type]
    cmp rax, rcx
    je .int_from_str

    jmp .int_type_error

.int_no_args:
    xor eax, eax
    bts rax, 63
    add rsp, 8
    pop rbx
    pop rbp
    ret

.int_return_smallint:
    mov rax, rbx
    add rsp, 8
    pop rbx
    pop rbp
    ret

.int_from_int:
    inc qword [rbx + PyObject.ob_refcnt]
    mov rax, rbx
    add rsp, 8
    pop rbx
    pop rbp
    ret

.int_from_float:
    mov rdi, rbx
    call float_int
    add rsp, 8
    pop rbx
    pop rbp
    ret

.int_from_str:
    lea rdi, [rbx + PyStrObject.data]
    mov esi, 10
    call int_from_cstr
    test rax, rax
    jz .int_str_parse_error
    add rsp, 8
    pop rbx
    pop rbp
    ret

.int_str_parse_error:
    lea rdi, [rel exc_ValueError_type]
    CSTRING rsi, "invalid literal for int()"
    call raise_exception

.int_from_bool:
    lea rax, [rel bool_true]
    cmp rbx, rax
    je .int_bool_true
    xor eax, eax
    bts rax, 63
    add rsp, 8
    pop rbx
    pop rbp
    ret
.int_bool_true:
    mov rax, 1
    bts rax, 63
    add rsp, 8
    pop rbx
    pop rbp
    ret

.int_type_error:
    lea rdi, [rel exc_TypeError_type]
    CSTRING rsi, "int() argument must be a string or a number"
    call raise_exception

.int_error:
    lea rdi, [rel exc_TypeError_type]
    CSTRING rsi, "int() takes at most 1 argument"
    call raise_exception

; ============================================================================
; 3. builtin_str_fn(args, nargs) - str(x)
; ============================================================================
global builtin_str_fn
builtin_str_fn:
    push rbp
    mov rbp, rsp

    test rsi, rsi
    jz .str_no_args

    cmp rsi, 1
    jne .str_error

    mov rdi, [rdi]
    call obj_str
    pop rbp
    ret

.str_no_args:
    CSTRING rdi, ""
    call str_from_cstr
    pop rbp
    ret

.str_error:
    lea rdi, [rel exc_TypeError_type]
    CSTRING rsi, "str() takes at most 1 argument"
    call raise_exception

; ============================================================================
; 4. builtin_ord(args, nargs) - ord(c)
; ============================================================================
global builtin_ord
builtin_ord:
    push rbp
    mov rbp, rsp

    cmp rsi, 1
    jne .ord_nargs_error

    mov rdi, [rdi]

    test rdi, rdi
    js .ord_type_error

    mov rax, [rdi + PyObject.ob_type]
    lea rcx, [rel str_type]
    cmp rax, rcx
    jne .ord_type_error

    cmp qword [rdi + PyStrObject.ob_size], 1
    jne .ord_len_error

    movzx eax, byte [rdi + PyStrObject.data]
    bts rax, 63
    pop rbp
    ret

.ord_type_error:
    lea rdi, [rel exc_TypeError_type]
    CSTRING rsi, "ord() expected string of length 1"
    call raise_exception

.ord_len_error:
    lea rdi, [rel exc_TypeError_type]
    CSTRING rsi, "ord() expected a character"
    call raise_exception

.ord_nargs_error:
    lea rdi, [rel exc_TypeError_type]
    CSTRING rsi, "ord() takes exactly one argument"
    call raise_exception

; ============================================================================
; 5. builtin_chr(args, nargs) - chr(n)
; ============================================================================
global builtin_chr
builtin_chr:
    push rbp
    mov rbp, rsp
    sub rsp, 16

    cmp rsi, 1
    jne .chr_nargs_error

    mov rdi, [rdi]
    call int_to_i64

    cmp rax, 0
    jl .chr_range_error
    cmp rax, 0x10FFFF
    ja .chr_range_error

    ; Single byte (ASCII)
    cmp rax, 0x7F
    ja .chr_utf8_encode

    mov byte [rbp - 16], al
    mov byte [rbp - 15], 0
    lea rdi, [rbp - 16]
    call str_from_cstr
    leave
    ret

.chr_utf8_encode:
    cmp rax, 0x7FF
    ja .chr_3byte

    ; 2-byte: 110xxxxx 10xxxxxx
    mov rcx, rax
    shr rcx, 6
    or cl, 0xC0
    mov byte [rbp - 16], cl
    mov rcx, rax
    and cl, 0x3F
    or cl, 0x80
    mov byte [rbp - 15], cl
    mov byte [rbp - 14], 0
    lea rdi, [rbp - 16]
    call str_from_cstr
    leave
    ret

.chr_3byte:
    cmp rax, 0xFFFF
    ja .chr_4byte

    ; 3-byte: 1110xxxx 10xxxxxx 10xxxxxx
    mov rcx, rax
    shr rcx, 12
    or cl, 0xE0
    mov byte [rbp - 16], cl
    mov rcx, rax
    shr rcx, 6
    and cl, 0x3F
    or cl, 0x80
    mov byte [rbp - 15], cl
    mov rcx, rax
    and cl, 0x3F
    or cl, 0x80
    mov byte [rbp - 14], cl
    mov byte [rbp - 13], 0
    lea rdi, [rbp - 16]
    call str_from_cstr
    leave
    ret

.chr_4byte:
    ; 4-byte: 11110xxx 10xxxxxx 10xxxxxx 10xxxxxx
    mov rcx, rax
    shr rcx, 18
    or cl, 0xF0
    mov byte [rbp - 16], cl
    mov rcx, rax
    shr rcx, 12
    and cl, 0x3F
    or cl, 0x80
    mov byte [rbp - 15], cl
    mov rcx, rax
    shr rcx, 6
    and cl, 0x3F
    or cl, 0x80
    mov byte [rbp - 14], cl
    mov rcx, rax
    and cl, 0x3F
    or cl, 0x80
    mov byte [rbp - 13], cl
    mov byte [rbp - 12], 0
    lea rdi, [rbp - 16]
    call str_from_cstr
    leave
    ret

.chr_range_error:
    lea rdi, [rel exc_ValueError_type]
    CSTRING rsi, "chr() arg not in range(0x110000)"
    call raise_exception

.chr_nargs_error:
    lea rdi, [rel exc_TypeError_type]
    CSTRING rsi, "chr() takes exactly one argument"
    call raise_exception

; ============================================================================
; 6. builtin_hex(args, nargs) - hex(n)
; ============================================================================
global builtin_hex
builtin_hex:
    push rbp
    mov rbp, rsp
    sub rsp, 80

    cmp rsi, 1
    jne .hex_nargs_error

    mov rdi, [rdi]
    call int_to_i64

    test rax, rax
    jz .hex_zero

    test rax, rax
    jns .hex_positive

    ; Negative
    neg rax
    mov byte [rbp - 80], '-'
    mov byte [rbp - 79], '0'
    mov byte [rbp - 78], 'x'
    lea rdi, [rbp - 77]
    mov r8d, 3
    jmp .hex_digits

.hex_positive:
    mov byte [rbp - 80], '0'
    mov byte [rbp - 79], 'x'
    lea rdi, [rbp - 78]
    mov r8d, 2

.hex_digits:
    ; Write hex digits in reverse into temp area, then copy in correct order
    lea rsi, [rbp - 16]
    xor ecx, ecx

.hex_digit_loop:
    test rax, rax
    jz .hex_reverse

    mov rdx, rax
    and edx, 0xF
    cmp edx, 10
    jb .hex_dec_digit
    add edx, ('a' - 10)
    jmp .hex_store_digit
.hex_dec_digit:
    add edx, '0'
.hex_store_digit:
    mov byte [rsi], dl
    dec rsi
    inc ecx
    shr rax, 4
    jmp .hex_digit_loop

.hex_reverse:
    ; Digits at [rsi+1 .. rsi+ecx], LSB first (reversed)
    ; Copy them MSB-first into rdi
    inc rsi
    mov edx, ecx
.hex_copy_loop:
    test ecx, ecx
    jz .hex_done_copy
    mov al, byte [rsi]
    mov byte [rdi], al
    inc rsi
    inc rdi
    dec ecx
    jmp .hex_copy_loop

.hex_done_copy:
    mov byte [rdi], 0
    lea rdi, [rbp - 80]
    call str_from_cstr
    leave
    ret

.hex_zero:
    CSTRING rdi, "0x0"
    call str_from_cstr
    leave
    ret

.hex_nargs_error:
    lea rdi, [rel exc_TypeError_type]
    CSTRING rsi, "hex() takes exactly one argument"
    call raise_exception

; ============================================================================
; 7. builtin_id(args, nargs) - id(x)
; ============================================================================
global builtin_id
builtin_id:
    push rbp
    mov rbp, rsp

    cmp rsi, 1
    jne .id_error

    mov rdi, [rdi]

    test rdi, rdi
    js .id_smallint

    call int_from_i64
    pop rbp
    ret

.id_smallint:
    shl rdi, 1
    sar rdi, 1
    call int_from_i64
    pop rbp
    ret

.id_error:
    lea rdi, [rel exc_TypeError_type]
    CSTRING rsi, "id() takes exactly one argument"
    call raise_exception

; ============================================================================
; 8. builtin_hash_fn(args, nargs) - hash(x)
; ============================================================================
global builtin_hash_fn
builtin_hash_fn:
    push rbp
    mov rbp, rsp
    push rbx
    sub rsp, 8

    cmp rsi, 1
    jne .hash_nargs_error

    mov rbx, [rdi]

    test rbx, rbx
    js .hash_smallint

    mov rax, [rbx + PyObject.ob_type]
    mov rcx, [rax + PyTypeObject.tp_hash]
    test rcx, rcx
    jz .hash_type_error

    mov rdi, rbx
    call rcx
    mov rdi, rax
    call int_from_i64
    add rsp, 8
    pop rbx
    pop rbp
    ret

.hash_smallint:
    mov rax, rbx
    shl rax, 1
    sar rax, 1
    mov rdi, rax
    call int_from_i64
    add rsp, 8
    pop rbx
    pop rbp
    ret

.hash_type_error:
    lea rdi, [rel exc_TypeError_type]
    CSTRING rsi, "unhashable type"
    call raise_exception

.hash_nargs_error:
    lea rdi, [rel exc_TypeError_type]
    CSTRING rsi, "hash() takes exactly one argument"
    call raise_exception

; ============================================================================
; 9. builtin_callable(args, nargs) - callable(x)
; ============================================================================
global builtin_callable
builtin_callable:
    push rbp
    mov rbp, rsp

    cmp rsi, 1
    jne .callable_error

    mov rdi, [rdi]

    test rdi, rdi
    js .callable_false

    mov rax, [rdi + PyObject.ob_type]
    mov rcx, [rax + PyTypeObject.tp_call]
    test rcx, rcx
    jz .callable_false

    lea rax, [rel bool_true]
    inc qword [rax + PyObject.ob_refcnt]
    pop rbp
    ret

.callable_false:
    lea rax, [rel bool_false]
    inc qword [rax + PyObject.ob_refcnt]
    pop rbp
    ret

.callable_error:
    lea rdi, [rel exc_TypeError_type]
    CSTRING rsi, "callable() takes exactly one argument"
    call raise_exception

; ============================================================================
; 10. builtin_iter_fn(args, nargs) - iter(x)
; ============================================================================
global builtin_iter_fn
builtin_iter_fn:
    push rbp
    mov rbp, rsp

    cmp rsi, 1
    jne .iter_error

    mov rdi, [rdi]

    test rdi, rdi
    js .iter_type_error

    mov rax, [rdi + PyObject.ob_type]
    mov rcx, [rax + PyTypeObject.tp_iter]
    test rcx, rcx
    jz .iter_type_error

    call rcx
    pop rbp
    ret

.iter_type_error:
    lea rdi, [rel exc_TypeError_type]
    CSTRING rsi, "object is not iterable"
    call raise_exception

.iter_error:
    lea rdi, [rel exc_TypeError_type]
    CSTRING rsi, "iter() takes exactly one argument"
    call raise_exception

; ============================================================================
; 11. builtin_next_fn(args, nargs) - next(x)
; ============================================================================
global builtin_next_fn
builtin_next_fn:
    push rbp
    mov rbp, rsp

    cmp rsi, 1
    jne .next_error

    mov rdi, [rdi]

    test rdi, rdi
    js .next_type_error

    mov rax, [rdi + PyObject.ob_type]
    mov rcx, [rax + PyTypeObject.tp_iternext]
    test rcx, rcx
    jz .next_type_error

    call rcx
    test rax, rax
    jz .next_stop

    pop rbp
    ret

.next_stop:
    lea rdi, [rel exc_StopIteration_type]
    CSTRING rsi, ""
    call raise_exception

.next_type_error:
    lea rdi, [rel exc_TypeError_type]
    CSTRING rsi, "object is not an iterator"
    call raise_exception

.next_error:
    lea rdi, [rel exc_TypeError_type]
    CSTRING rsi, "next() takes exactly one argument"
    call raise_exception

; ============================================================================
; 12. builtin_any(args, nargs) - any(iterable)
; ============================================================================
global builtin_any
builtin_any:
    push rbp
    mov rbp, rsp
    push rbx
    push r12
    push r13
    sub rsp, 8

    cmp rsi, 1
    jne .any_error

    mov rdi, [rdi]

    test rdi, rdi
    js .any_type_error

    mov rax, [rdi + PyObject.ob_type]
    mov rcx, [rax + PyTypeObject.tp_iter]
    test rcx, rcx
    jz .any_type_error
    call rcx
    mov rbx, rax

    mov rax, [rbx + PyObject.ob_type]
    mov r12, [rax + PyTypeObject.tp_iternext]

.any_loop:
    mov rdi, rbx
    call r12
    test rax, rax
    jz .any_false

    mov r13, rax

    mov rdi, r13
    call obj_is_true
    test eax, eax
    jnz .any_found_true

    ; Falsy: DECREF item and continue
    mov rdi, r13
    test r13, r13
    js .any_loop
    call obj_decref
    jmp .any_loop

.any_found_true:
    mov rdi, r13
    test r13, r13
    js .any_true
    call obj_decref

.any_true:
    mov rdi, rbx
    call obj_decref
    lea rax, [rel bool_true]
    inc qword [rax + PyObject.ob_refcnt]
    add rsp, 8
    pop r13
    pop r12
    pop rbx
    pop rbp
    ret

.any_false:
    mov rdi, rbx
    call obj_decref
    lea rax, [rel bool_false]
    inc qword [rax + PyObject.ob_refcnt]
    add rsp, 8
    pop r13
    pop r12
    pop rbx
    pop rbp
    ret

.any_type_error:
    lea rdi, [rel exc_TypeError_type]
    CSTRING rsi, "argument is not iterable"
    call raise_exception

.any_error:
    lea rdi, [rel exc_TypeError_type]
    CSTRING rsi, "any() takes exactly one argument"
    call raise_exception

; ============================================================================
; 13. builtin_all(args, nargs) - all(iterable)
; ============================================================================
global builtin_all
builtin_all:
    push rbp
    mov rbp, rsp
    push rbx
    push r12
    push r13
    sub rsp, 8

    cmp rsi, 1
    jne .all_error

    mov rdi, [rdi]

    test rdi, rdi
    js .all_type_error

    mov rax, [rdi + PyObject.ob_type]
    mov rcx, [rax + PyTypeObject.tp_iter]
    test rcx, rcx
    jz .all_type_error
    call rcx
    mov rbx, rax

    mov rax, [rbx + PyObject.ob_type]
    mov r12, [rax + PyTypeObject.tp_iternext]

.all_loop:
    mov rdi, rbx
    call r12
    test rax, rax
    jz .all_true

    mov r13, rax

    mov rdi, r13
    call obj_is_true
    test eax, eax
    jz .all_found_false

    ; Truthy: DECREF item and continue
    mov rdi, r13
    test r13, r13
    js .all_loop
    call obj_decref
    jmp .all_loop

.all_found_false:
    mov rdi, r13
    test r13, r13
    js .all_false
    call obj_decref

.all_false:
    mov rdi, rbx
    call obj_decref
    lea rax, [rel bool_false]
    inc qword [rax + PyObject.ob_refcnt]
    add rsp, 8
    pop r13
    pop r12
    pop rbx
    pop rbp
    ret

.all_true:
    mov rdi, rbx
    call obj_decref
    lea rax, [rel bool_true]
    inc qword [rax + PyObject.ob_refcnt]
    add rsp, 8
    pop r13
    pop r12
    pop rbx
    pop rbp
    ret

.all_type_error:
    lea rdi, [rel exc_TypeError_type]
    CSTRING rsi, "argument is not iterable"
    call raise_exception

.all_error:
    lea rdi, [rel exc_TypeError_type]
    CSTRING rsi, "all() takes exactly one argument"
    call raise_exception

; ============================================================================
; 14. builtin_sum(args, nargs) - sum(iterable[, start])
; ============================================================================
global builtin_sum
builtin_sum:
    push rbp
    mov rbp, rsp
    push rbx
    push r12
    push r13
    push r14
    push r15
    sub rsp, 8

    mov rbx, rdi
    mov r14, rsi

    cmp r14, 1
    jb .sum_error
    cmp r14, 2
    ja .sum_error

    cmp r14, 2
    je .sum_has_start
    xor eax, eax
    bts rax, 63
    mov r13, rax
    jmp .sum_get_iter

.sum_has_start:
    mov r13, [rbx + 8]
    test r13, r13
    js .sum_get_iter
    inc qword [r13 + PyObject.ob_refcnt]

.sum_get_iter:
    mov rdi, [rbx]
    test rdi, rdi
    js .sum_type_error
    mov rax, [rdi + PyObject.ob_type]
    mov rcx, [rax + PyTypeObject.tp_iter]
    test rcx, rcx
    jz .sum_type_error
    call rcx
    mov rbx, rax

    mov rax, [rbx + PyObject.ob_type]
    mov r12, [rax + PyTypeObject.tp_iternext]

.sum_loop:
    mov rdi, rbx
    call r12
    test rax, rax
    jz .sum_done

    mov r14, rax

    mov rdi, r13
    mov rsi, r14
    call int_add
    mov r15, rax

    ; DECREF old accumulator
    mov rdi, r13
    test r13, r13
    js .sum_skip_decref_accum
    call obj_decref
.sum_skip_decref_accum:
    mov r13, r15

    ; DECREF item
    mov rdi, r14
    test r14, r14
    js .sum_skip_decref_item
    call obj_decref
.sum_skip_decref_item:

    jmp .sum_loop

.sum_done:
    mov rdi, rbx
    call obj_decref
    mov rax, r13
    add rsp, 8
    pop r15
    pop r14
    pop r13
    pop r12
    pop rbx
    pop rbp
    ret

.sum_type_error:
    lea rdi, [rel exc_TypeError_type]
    CSTRING rsi, "argument is not iterable"
    call raise_exception

.sum_error:
    lea rdi, [rel exc_TypeError_type]
    CSTRING rsi, "sum expected 1-2 arguments"
    call raise_exception

; ============================================================================
; 15. builtin_min(args, nargs) - min(a, b, ...)
; ============================================================================
global builtin_min
builtin_min:
    push rbp
    mov rbp, rsp
    push rbx
    push r12
    push r13
    push r14
    push r15
    sub rsp, 8

    cmp rsi, 1
    jb .min_error

    mov rbx, rdi
    mov r12, rsi
    mov r13, 1

    mov r14, [rbx]
    test r14, r14
    js .min_loop
    inc qword [r14 + PyObject.ob_refcnt]

.min_loop:
    cmp r13, r12
    jge .min_done

    mov r15, [rbx + r13 * 8]

    ; SmallInt fast path: both SmallInt? (AND, not OR)
    mov rax, r14
    and rax, r15
    jns .min_slow_compare

    ; Both SmallInts: decode and compare
    mov rax, r14
    shl rax, 1
    sar rax, 1
    mov rcx, r15
    shl rcx, 1
    sar rcx, 1
    cmp rcx, rax
    jge .min_no_update
    mov r14, r15
    jmp .min_no_update

.min_slow_compare:
    ; Use tp_richcompare(args[i], current_min, PY_LT)
    mov rdi, r15
    test rdi, rdi
    js .min_no_update
    mov rax, [rdi + PyObject.ob_type]
    mov rcx, [rax + PyTypeObject.tp_richcompare]
    test rcx, rcx
    jz .min_no_update

    mov rdi, r15
    mov rsi, r14
    xor edx, edx               ; PY_LT = 0
    call rcx

    ; Compare result against bool_true BEFORE DECREF
    lea rcx, [rel bool_true]
    cmp rax, rcx
    mov [rbp - 48], rax
    jne .min_slow_no_update

    ; Update min
    mov rdi, r14
    test r14, r14
    js .min_slow_update_skip_decref
    call obj_decref
.min_slow_update_skip_decref:
    mov r14, r15
    test r14, r14
    js .min_slow_update_done
    inc qword [r14 + PyObject.ob_refcnt]
.min_slow_update_done:
    mov rdi, [rbp - 48]
    test rdi, rdi
    js .min_no_update
    call obj_decref
    jmp .min_no_update

.min_slow_no_update:
    mov rdi, [rbp - 48]
    test rdi, rdi
    js .min_no_update
    call obj_decref

.min_no_update:
    inc r13
    jmp .min_loop

.min_done:
    mov rax, r14
    add rsp, 8
    pop r15
    pop r14
    pop r13
    pop r12
    pop rbx
    pop rbp
    ret

.min_error:
    lea rdi, [rel exc_TypeError_type]
    CSTRING rsi, "min expected at least 1 argument"
    call raise_exception

; ============================================================================
; 16. builtin_max(args, nargs) - max(a, b, ...)
; ============================================================================
global builtin_max
builtin_max:
    push rbp
    mov rbp, rsp
    push rbx
    push r12
    push r13
    push r14
    push r15
    sub rsp, 8

    cmp rsi, 1
    jb .max_error

    mov rbx, rdi
    mov r12, rsi
    mov r13, 1

    mov r14, [rbx]
    test r14, r14
    js .max_loop
    inc qword [r14 + PyObject.ob_refcnt]

.max_loop:
    cmp r13, r12
    jge .max_done

    mov r15, [rbx + r13 * 8]

    ; SmallInt fast path: both SmallInt? (AND, not OR)
    mov rax, r14
    and rax, r15
    jns .max_slow_compare

    ; Both SmallInts: decode and compare
    mov rax, r14
    shl rax, 1
    sar rax, 1
    mov rcx, r15
    shl rcx, 1
    sar rcx, 1
    cmp rcx, rax
    jle .max_no_update
    mov r14, r15
    jmp .max_no_update

.max_slow_compare:
    ; Use tp_richcompare(current_max, args[i], PY_LT)
    mov rdi, r14
    test rdi, rdi
    js .max_try_rhs

    mov rax, [rdi + PyObject.ob_type]
    mov rcx, [rax + PyTypeObject.tp_richcompare]
    test rcx, rcx
    jz .max_no_update

    mov rdi, r14
    mov rsi, r15
    xor edx, edx               ; PY_LT = 0
    call rcx
    jmp .max_check_result

.max_try_rhs:
    mov rdi, r15
    test rdi, rdi
    js .max_no_update
    mov rax, [rdi + PyObject.ob_type]
    mov rcx, [rax + PyTypeObject.tp_richcompare]
    test rcx, rcx
    jz .max_no_update

    mov rdi, r15
    mov rsi, r14
    mov edx, PY_GT
    call rcx

.max_check_result:
    lea rcx, [rel bool_true]
    cmp rax, rcx
    mov [rbp - 48], rax
    jne .max_slow_no_update

    ; Update max
    mov rdi, r14
    test r14, r14
    js .max_slow_update_skip_decref
    call obj_decref
.max_slow_update_skip_decref:
    mov r14, r15
    test r14, r14
    js .max_slow_update_done
    inc qword [r14 + PyObject.ob_refcnt]
.max_slow_update_done:
    mov rdi, [rbp - 48]
    test rdi, rdi
    js .max_no_update
    call obj_decref
    jmp .max_no_update

.max_slow_no_update:
    mov rdi, [rbp - 48]
    test rdi, rdi
    js .max_no_update
    call obj_decref

.max_no_update:
    inc r13
    jmp .max_loop

.max_done:
    mov rax, r14
    add rsp, 8
    pop r15
    pop r14
    pop r13
    pop r12
    pop rbx
    pop rbp
    ret

.max_error:
    lea rdi, [rel exc_TypeError_type]
    CSTRING rsi, "max expected at least 1 argument"
    call raise_exception

; ============================================================================
; 17. builtin_getattr(args, nargs) - getattr(obj, name[, default])
; ============================================================================
global builtin_getattr
builtin_getattr:
    push rbp
    mov rbp, rsp
    push rbx
    push r12
    push r13
    push r14

    mov rbx, rdi
    mov r12, rsi

    cmp r12, 2
    jb .getattr_error
    cmp r12, 3
    ja .getattr_error

    mov r13, [rbx]
    mov r14, [rbx + 8]

    test r13, r13
    js .getattr_try_type_dict

    mov rax, [r13 + PyObject.ob_type]
    mov rcx, [rax + PyTypeObject.tp_getattr]
    test rcx, rcx
    jz .getattr_try_type_dict

    mov rdi, r13
    mov rsi, r14
    call rcx
    test rax, rax
    jnz .getattr_found

    jmp .getattr_try_type_dict

.getattr_try_type_dict:
    test r13, r13
    js .getattr_smallint_type
    mov rax, [r13 + PyObject.ob_type]
    jmp .getattr_check_dict

.getattr_smallint_type:
    lea rax, [rel int_type]

.getattr_check_dict:
    mov rcx, [rax + PyTypeObject.tp_dict]
    test rcx, rcx
    jz .getattr_not_found

    mov rdi, rcx
    mov rsi, r14
    call dict_get
    test rax, rax
    jz .getattr_not_found

    inc qword [rax + PyObject.ob_refcnt]
    pop r14
    pop r13
    pop r12
    pop rbx
    pop rbp
    ret

.getattr_found:
    pop r14
    pop r13
    pop r12
    pop rbx
    pop rbp
    ret

.getattr_not_found:
    cmp r12, 3
    jne .getattr_raise

    mov rax, [rbx + 16]
    test rax, rax
    js .getattr_ret_default_si
    inc qword [rax + PyObject.ob_refcnt]
.getattr_ret_default_si:
    pop r14
    pop r13
    pop r12
    pop rbx
    pop rbp
    ret

.getattr_raise:
    lea rdi, [rel exc_AttributeError_type]
    CSTRING rsi, "object has no attribute"
    call raise_exception

.getattr_error:
    lea rdi, [rel exc_TypeError_type]
    CSTRING rsi, "getattr expected 2 or 3 arguments"
    call raise_exception

; ============================================================================
; 18. builtin_hasattr(args, nargs) - hasattr(obj, name)
; ============================================================================
global builtin_hasattr
builtin_hasattr:
    push rbp
    mov rbp, rsp
    push rbx
    push r12
    push r13
    sub rsp, 8

    cmp rsi, 2
    jne .hasattr_error

    mov r12, [rdi]
    mov r13, [rdi + 8]

    test r12, r12
    js .hasattr_try_type_dict

    mov rax, [r12 + PyObject.ob_type]
    mov rcx, [rax + PyTypeObject.tp_getattr]
    test rcx, rcx
    jz .hasattr_try_type_dict

    mov rdi, r12
    mov rsi, r13
    call rcx
    test rax, rax
    jz .hasattr_try_type_dict

    ; Found via tp_getattr - DECREF result, return True
    mov rdi, rax
    test rax, rax
    js .hasattr_found_si
    call obj_decref
.hasattr_found_si:
    lea rax, [rel bool_true]
    inc qword [rax + PyObject.ob_refcnt]
    add rsp, 8
    pop r13
    pop r12
    pop rbx
    pop rbp
    ret

.hasattr_try_type_dict:
    test r12, r12
    js .hasattr_smallint_type
    mov rax, [r12 + PyObject.ob_type]
    jmp .hasattr_check_dict

.hasattr_smallint_type:
    lea rax, [rel int_type]

.hasattr_check_dict:
    mov rcx, [rax + PyTypeObject.tp_dict]
    test rcx, rcx
    jz .hasattr_not_found

    mov rdi, rcx
    mov rsi, r13
    call dict_get
    test rax, rax
    jz .hasattr_not_found

    lea rax, [rel bool_true]
    inc qword [rax + PyObject.ob_refcnt]
    add rsp, 8
    pop r13
    pop r12
    pop rbx
    pop rbp
    ret

.hasattr_not_found:
    lea rax, [rel bool_false]
    inc qword [rax + PyObject.ob_refcnt]
    add rsp, 8
    pop r13
    pop r12
    pop rbx
    pop rbp
    ret

.hasattr_error:
    lea rdi, [rel exc_TypeError_type]
    CSTRING rsi, "hasattr() takes exactly 2 arguments"
    call raise_exception

; ============================================================================
; 19. builtin_setattr(args, nargs) - setattr(obj, name, value)
; ============================================================================
global builtin_setattr
builtin_setattr:
    push rbp
    mov rbp, rsp
    push rbx
    sub rsp, 8

    cmp rsi, 3
    jne .setattr_error

    mov rbx, rdi

    mov rdi, [rbx]
    test rdi, rdi
    js .setattr_type_error

    mov rax, [rdi + PyObject.ob_type]
    mov rcx, [rax + PyTypeObject.tp_setattr]
    test rcx, rcx
    jz .setattr_type_error

    mov rdi, [rbx]
    mov rsi, [rbx + 8]
    mov rdx, [rbx + 16]
    call rcx

    lea rax, [rel none_singleton]
    inc qword [rax + PyObject.ob_refcnt]
    add rsp, 8
    pop rbx
    pop rbp
    ret

.setattr_type_error:
    lea rdi, [rel exc_TypeError_type]
    CSTRING rsi, "object does not support attribute assignment"
    call raise_exception

.setattr_error:
    lea rdi, [rel exc_TypeError_type]
    CSTRING rsi, "setattr() takes exactly 3 arguments"
    call raise_exception
