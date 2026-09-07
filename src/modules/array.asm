; array.asm - the `array` module
;
; array.array: a mutable sequence of C scalars, one typecode for the lot.  It
; is what stands between this tree and multiprocessing, and CPython's own
; suite imports it from the test modules for struct, memoryview, io, bytes,
; socket, re, marshal, codecs and the compression family -- more than the rest
; of the missing C modules together.
;
; The storage is a plain malloc'd buffer of raw scalars, NOT Values: an array
; of 'i' holds four-byte ints, and reading one means widening it at the point
; of access.  That is the whole reason the type exists, and it is why the
; object carries neither tp_traverse nor tp_clear -- there is nothing in the
; buffer for the collector to follow, and handing it one would have it read
; machine integers as pointers.
;
; The buffer can move, so the type takes no __dict__ and no __slots__ at a
; fixed offset, the way bytearray and memoryview do not.
;
; What is here is the surface the stdlib actually uses.  fromfile and tofile
; are not: they want the file object's own read and write, and every caller in
; the suite reaches for frombytes and tobytes instead.

%include "macros.inc"
%include "object.inc"

extern dict_new
extern dict_set
extern obj_decref
extern obj_incref
extern str_from_cstr_heap
extern builtin_func_new
extern list_new
extern list_append
extern tuple_new
extern int_from_i64
extern obj_as_index
extern obj_as_slice_index
extern none_singleton
extern bool_true
extern bool_false
extern raise_exception
extern exc_TypeError_type
extern exc_ValueError_type
extern exc_IndexError_type
extern exc_OverflowError_type
extern type_type
extern str_type
extern int_type
extern float_type
extern bytes_type
extern bytes_from_data
extern ap_malloc
extern ap_free
extern ap_memcpy
extern ap_realloc
extern float_from_f64
extern float_to_f64
extern get_iterator_opt
extern obj_richcompare_bool
extern repr_append_cstr
extern rbt_append_cstr
extern str_cp_at
extern current_exception
extern hash_not_implemented
extern obj_dealloc
extern set_exception

;; ============================================================================
;; The typecode table.  Each row is (letter, itemsize, kind), where kind says
;; how a stored scalar becomes a Python object and back:
;;
;;   AK_SIGNED    a signed integer, sign-extended on the way out
;;   AK_UNSIGNED  an unsigned integer, zero-extended
;;   AK_FLOAT     4 or 8 bytes of IEEE, widened to a double
;;   AK_UNICODE   a code point, 4 bytes, which becomes a one-character str
;;
;; 'l' and 'L' are eight bytes here as they are on every LP64 platform, which
;; is what CPython reports for them too.
;; ============================================================================
AK_SIGNED   equ 0
AK_UNSIGNED equ 1
AK_FLOAT    equ 2
AK_UNICODE  equ 3

struc ArrayCode
    .letter:   resq 1
    .itemsize: resq 1
    .kind:     resq 1
endstruc

section .rodata
align 8
array_codes:
    dq 'b', 1, AK_SIGNED
    dq 'B', 1, AK_UNSIGNED
    dq 'u', 4, AK_UNICODE
    dq 'h', 2, AK_SIGNED
    dq 'H', 2, AK_UNSIGNED
    dq 'i', 4, AK_SIGNED
    dq 'I', 4, AK_UNSIGNED
    dq 'l', 8, AK_SIGNED
    dq 'L', 8, AK_UNSIGNED
    dq 'q', 8, AK_SIGNED
    dq 'Q', 8, AK_UNSIGNED
    dq 'f', 4, AK_FLOAT
    dq 'd', 8, AK_FLOAT
array_codes_end:
ARRAY_NCODES equ (array_codes_end - array_codes) / ArrayCode_size

array_name_str:      db "array.array", 0
array_typecodes_str: db "bBuhHiIlLqQfd", 0

section .text

;; ============================================================================
;; array_find_code(rdi = the typecode letter) -> rax = its ArrayCode*, or 0
;; ============================================================================
DEF_FUNC_BARE array_find_code
    lea rax, [rel array_codes]
    mov rcx, ARRAY_NCODES
.afc_loop:
    test rcx, rcx
    jz .afc_none
    cmp [rax + ArrayCode.letter], rdi
    je .afc_done
    add rax, ArrayCode_size
    dec rcx
    jmp .afc_loop
.afc_none:
    xor eax, eax
.afc_done:
    ret
END_FUNC array_find_code

;; ============================================================================
;; array_new_empty(rdi = ArrayCode*) -> rax = a new empty array, or 0
;; ============================================================================
ANE_FRAME equ 16            ; + 1 push = 24 ... see below
DEF_FUNC array_new_empty, 8
    push rbx
    mov rbx, rdi
    ; ap_malloc, not gc_alloc: the buffer holds raw scalars, so the type
    ; carries no TYPE_FLAG_HAVE_GC and there is nothing for the collector to
    ; track.  gc_alloc and gc_dealloc are a pair, and using one without the
    ; other handed free() a pointer that was never its to free.
    mov edi, PyArrayObject_size
    call ap_malloc
    test rax, rax
    jz .ane_out
    mov qword [rax + PyObject.ob_refcnt], 1
    lea rcx, [rel array_type]
    mov [rax + PyObject.ob_type], rcx
    mov qword [rax + PyArrayObject.ob_size], 0
    mov qword [rax + PyArrayObject.ob_cap], 0
    mov qword [rax + PyArrayObject.ob_data], 0
    mov rcx, [rbx + ArrayCode.letter]
    mov [rax + PyArrayObject.ob_code], rcx
    mov rcx, [rbx + ArrayCode.itemsize]
    mov [rax + PyArrayObject.ob_isize], rcx
    mov rcx, [rbx + ArrayCode.kind]
    mov [rax + PyArrayObject.ob_kind], rcx
.ane_out:
    pop rbx
    leave
    ret
END_FUNC array_new_empty

;; ============================================================================
;; array_reserve(rdi = the array, rsi = items wanted) -> eax = 1, or 0 raising
;;
;; Grows by doubling, as list does.  The buffer MOVES, which is why nothing
;; may hold a pointer into it across an append.
;; ============================================================================
AR_ARR   equ 8
AR_WANT  equ 16
AR_FRAME equ 32             ; + 0 pushes = 32, 16-aligned
DEF_FUNC array_reserve, AR_FRAME
    mov [rbp - AR_ARR], rdi
    mov [rbp - AR_WANT], rsi
    cmp rsi, [rdi + PyArrayObject.ob_cap]
    jle .arr_ok

    ; new capacity = max(want, cap * 2, 8)
    mov rax, [rdi + PyArrayObject.ob_cap]
    add rax, rax
    cmp rax, rsi
    jge .arr_have_cap
    mov rax, rsi
.arr_have_cap:
    cmp rax, 8
    jge .arr_cap_ok
    mov rax, 8
.arr_cap_ok:
    push rax
    mov rdi, [rbp - AR_ARR]
    imul rax, [rdi + PyArrayObject.ob_isize]
    mov rsi, rax
    mov rdi, [rdi + PyArrayObject.ob_data]
    call ap_realloc
    pop rcx
    test rax, rax
    jz .arr_nomem
    mov rdi, [rbp - AR_ARR]
    mov [rdi + PyArrayObject.ob_data], rax
    mov [rdi + PyArrayObject.ob_cap], rcx
.arr_ok:
    mov eax, 1
    leave
    ret
.arr_nomem:
    extern exc_MemoryError_type
    RAISE exc_MemoryError_type, "out of memory"
END_FUNC array_reserve

;; ============================================================================
;; array_item_value(rdi = the array, rsi = index) -> rax = a Value
;;
;; Widening is the whole point: a stored 'h' is two bytes and comes out as an
;; int, an 'f' is four and comes out as a float.  The kind says which, and the
;; SIZE says how far to sign- or zero-extend.
;; ============================================================================
AIV_ARG   equ 8          ; the one-element argument array chr() takes
AIV_FRAME equ 16            ; + 0 pushes = 16, 16-aligned
DEF_FUNC array_item_value, AIV_FRAME
    mov rax, [rdi + PyArrayObject.ob_data]
    mov rcx, [rdi + PyArrayObject.ob_isize]
    imul rsi, rcx
    add rax, rsi                ; rax = &item
    mov rdx, [rdi + PyArrayObject.ob_kind]

    cmp rdx, AK_FLOAT
    je .aiv_float
    cmp rdx, AK_UNICODE
    je .aiv_unicode
    cmp rdx, AK_UNSIGNED
    je .aiv_unsigned

    ; signed
    cmp rcx, 1
    je .aiv_s1
    cmp rcx, 2
    je .aiv_s2
    cmp rcx, 4
    je .aiv_s4
    mov rdi, [rax]
    jmp .aiv_int
.aiv_s1:
    movsx rdi, byte [rax]
    jmp .aiv_int
.aiv_s2:
    movsx rdi, word [rax]
    jmp .aiv_int
.aiv_s4:
    movsxd rdi, dword [rax]
    jmp .aiv_int

.aiv_unsigned:
    cmp rcx, 1
    je .aiv_u1
    cmp rcx, 2
    je .aiv_u2
    cmp rcx, 4
    je .aiv_u4
    mov rdi, [rax]
    jmp .aiv_int
.aiv_u1:
    movzx edi, byte [rax]
    jmp .aiv_int
.aiv_u2:
    movzx edi, word [rax]
    jmp .aiv_int
.aiv_u4:
    mov edi, dword [rax]
    jmp .aiv_int

.aiv_int:
    ; int_from_i64 answers a (payload, tag) PAIR, not a Value.  Handing the
    ; payload straight back stored a raw 7 where a Value was wanted, and the
    ; next dict_set read it as a pointer.
    call int_from_i64
    leave
    V_PACK rax, rdx
    ret

.aiv_float:
    cmp rcx, 4
    je .aiv_f4
    movsd xmm0, [rax]
    jmp .aiv_mkfloat
.aiv_f4:
    cvtss2sd xmm0, dword [rax]
.aiv_mkfloat:
    ; float_from_f64 takes the double in xmm0, where it already is, and
    ; answers a pair like int_from_i64.
    call float_from_f64
    leave
    V_PACK rax, rdx
    ret

.aiv_unicode:
    ; A stored code point becomes a one-character str, which is exactly what
    ; chr() builds -- and the UTF-8 encoder it needs is written once, there.
    mov edi, dword [rax]
    V_PACK_I64 rdi, rcx
    mov [rbp - AIV_ARG], rdi
    lea rdi, [rbp - AIV_ARG]
    mov esi, 1
    extern builtin_chr
    call builtin_chr
    leave
    ret
END_FUNC array_item_value

;; ============================================================================
;; array_store_item(rdi = the array, rsi = index, rdx = the Value to store)
;;   -> eax = 1, or 0 with an exception pending
;;
;; The narrowing half of array_item_value, and the half that can refuse: an
;; array of 'b' takes -128..127 and CPython raises OverflowError outside that,
;; rather than truncating.
;; ============================================================================
ASI_ARR   equ 8
ASI_IDX   equ 16
ASI_VAL   equ 24
ASI_FRAME equ 32            ; + 1 push = 40 ... padded below
DEF_FUNC array_store_item, 40
    push rbx
    mov [rbp - ASI_ARR], rdi
    mov [rbp - ASI_IDX], rsi
    mov [rbp - ASI_VAL], rdx

    mov rcx, [rdi + PyArrayObject.ob_kind]
    cmp rcx, AK_FLOAT
    je .asi_float
    cmp rcx, AK_UNICODE
    je .asi_unicode

    ; An integer typecode takes an index-like object, and refuses a float:
    ; CPython says "'float' object cannot be interpreted as an integer".
    ; obj_as_index takes the UNPACKED pair, not a Value: handing it a Value
    ; passes the biased encoding of an int immediate as the number itself,
    ; and every append then looked like an overflow.
    mov rdi, rdx
    V_UNPACK rdi, rdx
    call obj_as_index
    mov rbx, rax                ; the value, as an i64
    cmp qword [rel current_exception], 0
    jne .asi_fail

    ; Range, by size and signedness.  Out of range is OverflowError, not a
    ; silent truncation.
    mov rdi, [rbp - ASI_ARR]
    mov rcx, [rdi + PyArrayObject.ob_isize]
    mov rdx, [rdi + PyArrayObject.ob_kind]
    cmp rcx, 8
    je .asi_store                ; eight bytes takes whatever fits an i64
    cmp rdx, AK_UNSIGNED
    je .asi_range_unsigned

    ; signed: -(1 << (bits-1)) .. (1 << (bits-1)) - 1
    shl rcx, 3                  ; bits
    dec rcx
    mov rax, 1
    shl rax, cl                 ; 1 << (bits-1)
    mov rdx, rax
    neg rdx                     ; the low bound
    cmp rbx, rdx
    jl .asi_overflow
    dec rax
    cmp rbx, rax
    jg .asi_overflow
    jmp .asi_store

.asi_range_unsigned:
    test rbx, rbx
    js .asi_overflow
    shl rcx, 3                  ; bits
    mov rax, 1
    shl rax, cl
    dec rax
    cmp rbx, rax
    jg .asi_overflow

.asi_store:
    mov rdi, [rbp - ASI_ARR]
    mov rax, [rdi + PyArrayObject.ob_data]
    mov rcx, [rdi + PyArrayObject.ob_isize]
    mov rsi, [rbp - ASI_IDX]
    imul rsi, rcx
    add rax, rsi
    cmp rcx, 1
    je .asi_w1
    cmp rcx, 2
    je .asi_w2
    cmp rcx, 4
    je .asi_w4
    mov [rax], rbx
    jmp .asi_ok
.asi_w1:
    mov [rax], bl
    jmp .asi_ok
.asi_w2:
    mov [rax], bx
    jmp .asi_ok
.asi_w4:
    mov [rax], ebx
.asi_ok:
    mov eax, 1
    pop rbx
    leave
    ret

.asi_float:
    mov rdi, rdx
    V_UNPACK rdi, rsi           ; float_to_f64 takes the pair as well
    call float_to_f64           ; refuses what is not a number
    cmp qword [rel current_exception], 0
    jne .asi_fail
    mov rdi, [rbp - ASI_ARR]
    mov rax, [rdi + PyArrayObject.ob_data]
    mov rcx, [rdi + PyArrayObject.ob_isize]
    mov rsi, [rbp - ASI_IDX]
    imul rsi, rcx
    add rax, rsi
    cmp rcx, 4
    je .asi_f4
    movsd [rax], xmm0
    jmp .asi_ok
.asi_f4:
    cvtsd2ss xmm1, xmm0
    movss dword [rax], xmm1
    jmp .asi_ok

.asi_unicode:
    ; 'u' takes a one-character str and stores its code point.
    mov rdi, rdx
    V_TEST_PTR rdi, rax
    ja .asi_need_char
    mov rax, [rdi + PyObject.ob_type]
    lea rcx, [rel str_type]
    cmp rax, rcx
    jne .asi_need_char
    cmp qword [rdi + PyStrObject.ob_length], 1
    jne .asi_need_char
    xor esi, esi
    call str_cp_at
    mov rbx, rax
    mov rdi, [rbp - ASI_ARR]
    mov rax, [rdi + PyArrayObject.ob_data]
    mov rsi, [rbp - ASI_IDX]
    shl rsi, 2
    add rax, rsi
    mov [rax], ebx
    jmp .asi_ok

.asi_fail:
    xor eax, eax
    pop rbx
    leave
    ret
.asi_overflow:
    ; SET_EXC: the callers return a failure code and let their own caller
    ; unwind, so a RAISE here would skip the cleanup in between.
    SET_EXC exc_OverflowError_type, \
            "signed integer is greater than maximum"
    xor eax, eax
    pop rbx
    leave
    ret
.asi_need_char:
    SET_EXC exc_TypeError_type, \
            "array item must be a unicode character"
    xor eax, eax
    pop rbx
    leave
    ret
END_FUNC array_store_item

;; ============================================================================
;; array_dealloc(rdi = the array) -> nothing
;; ============================================================================
DEF_FUNC array_dealloc, 8
    push rbx
    mov rbx, rdi
    mov rdi, [rbx + PyArrayObject.ob_data]
    test rdi, rdi
    jz .ad_no_data
    call ap_free
.ad_no_data:
    mov rdi, rbx
    call ap_free
    pop rbx
    leave
    ret
END_FUNC array_dealloc

;; ============================================================================
;; array_length(rdi = the array) -> rax = the item count
;; ============================================================================
DEF_FUNC_BARE array_length
    mov rax, [rdi + PyArrayObject.ob_size]
    ret
END_FUNC array_length

;; ============================================================================
;; array_sq_item(rdi = the array, rsi = index) -> rax = a Value, or 0 raising
;; Negative indices count from the end, as every sequence here does.
;; ============================================================================
DEF_FUNC array_sq_item
    test rsi, rsi
    jns .asq_have
    add rsi, [rdi + PyArrayObject.ob_size]
.asq_have:
    cmp rsi, 0
    jl .asq_range
    cmp rsi, [rdi + PyArrayObject.ob_size]
    jge .asq_range
    call array_item_value
    leave
    ret
.asq_range:
    ; SET_EXC and a NULL, not RAISE.  RAISE tail-jumps into the unwinder, and
    ; the sequence iterator's whole protocol is to CATCH the IndexError this
    ; raises and read it as exhaustion -- unwinding takes the exception
    ; straight past it and out of the `for`.  The same rule a builtin
    ; __next__ follows for StopIteration.
    SET_EXC exc_IndexError_type, "array index out of range"
    xor eax, eax
    xor edx, edx
    leave
    V_PACK rax, rdx
    ret
END_FUNC array_sq_item

;; ============================================================================
;; array_append_value(rdi = the array, rsi = a Value) -> eax = 1, or 0 raising
;; ============================================================================
AAV_ARR   equ 8
AAV_VAL   equ 16
AAV_FRAME equ 32            ; + 0 pushes = 32, 16-aligned
DEF_FUNC array_append_value, AAV_FRAME
    mov [rbp - AAV_ARR], rdi
    mov [rbp - AAV_VAL], rsi
    mov rsi, [rdi + PyArrayObject.ob_size]
    inc rsi
    call array_reserve
    test eax, eax
    jz .aav_fail
    mov rdi, [rbp - AAV_ARR]
    mov rsi, [rdi + PyArrayObject.ob_size]
    mov rdx, [rbp - AAV_VAL]
    call array_store_item
    test eax, eax
    jz .aav_fail
    mov rdi, [rbp - AAV_ARR]
    inc qword [rdi + PyArrayObject.ob_size]
    mov eax, 1
    leave
    ret
.aav_fail:
    xor eax, eax
    leave
    ret
END_FUNC array_append_value

;; ============================================================================
;; array_extend_iterable(rdi = the array, rsi = a Value) -> eax = 1, or 0
;;
;; Another array of the SAME typecode is copied wholesale; anything else is
;; iterated and appended one item at a time, which is what makes the range
;; checks apply to each.
;; ============================================================================
AEI_ARR   equ 8
AEI_ITER  equ 16
AEI_FRAME equ 32            ; + 1 push = 40 ... padded below
DEF_FUNC array_extend_iterable, 40
    push rbx
    mov [rbp - AEI_ARR], rdi
    mov rbx, rsi

    ; The same typecode: a straight copy, and the only path that does not go
    ; through the per-item range check -- it cannot need one.
    V_TEST_PTR rbx, rax
    ja .aei_generic
    mov rax, [rbx + PyObject.ob_type]
    lea rcx, [rel array_type]
    cmp rax, rcx
    jne .aei_generic
    mov rcx, [rbx + PyArrayObject.ob_code]
    cmp rcx, [rdi + PyArrayObject.ob_code]
    jne .aei_generic

    mov rsi, [rdi + PyArrayObject.ob_size]
    add rsi, [rbx + PyArrayObject.ob_size]
    call array_reserve
    test eax, eax
    jz .aei_fail
    mov rdi, [rbp - AEI_ARR]
    mov rax, [rdi + PyArrayObject.ob_size]
    imul rax, [rdi + PyArrayObject.ob_isize]
    mov rcx, [rdi + PyArrayObject.ob_data]
    lea rdi, [rcx + rax]
    mov rsi, [rbx + PyArrayObject.ob_data]
    mov rdx, [rbx + PyArrayObject.ob_size]
    imul rdx, [rbx + PyArrayObject.ob_isize]
    test rdx, rdx
    jz .aei_copied
    call ap_memcpy
.aei_copied:
    mov rdi, [rbp - AEI_ARR]
    mov rax, [rbx + PyArrayObject.ob_size]
    add [rdi + PyArrayObject.ob_size], rax
    mov eax, 1
    pop rbx
    leave
    ret

.aei_generic:
    ; The real tag, not TAG_PTR: array('i', 5) hands an int IMMEDIATE here,
    ; and calling it a pointer dereferences the number 5.
    mov rdi, rbx
    V_UNPACK rdi, rsi
    call get_iterator_opt
    test rax, rax
    jz .aei_not_iterable
    mov [rbp - AEI_ITER], rax
.aei_loop:
    mov rdi, [rbp - AEI_ITER]
    mov rax, [rdi + PyObject.ob_type]
    mov rax, [rax + PyTypeObject.tp_iternext]
    test rax, rax
    jz .aei_done
    call rax
    test rax, rax
    jz .aei_done
    mov rbx, rax
    mov rdi, [rbp - AEI_ARR]
    mov rsi, rax
    call array_append_value
    push rax
    ; DECREF_V, not obj_decref: tp_iternext answers a VALUE, and an int
    ; immediate is not a pointer to dereference.
    mov rdi, rbx
    DECREF_V rdi, rcx
    pop rax
    test eax, eax
    jz .aei_iter_fail
    jmp .aei_loop
.aei_done:
    mov rdi, [rbp - AEI_ITER]
    call obj_decref
    cmp qword [rel current_exception], 0
    jne .aei_fail
    mov eax, 1
    pop rbx
    leave
    ret
.aei_iter_fail:
    mov rdi, [rbp - AEI_ITER]
    call obj_decref
.aei_fail:
    xor eax, eax
    pop rbx
    leave
    ret
.aei_not_iterable:
    pop rbx
    RAISE exc_TypeError_type, "cannot extend array from a non-iterable"
END_FUNC array_extend_iterable

;; ============================================================================
;; array_type_new(rdi = the type, rsi = args, rdx = nargs) -> a Value
;;   array(typecode[, initializer])
;;
;; The tp_new signature every builtin constructor takes: the type comes first,
;; and the arguments as written follow it.
;; ============================================================================
ATN_CODE  equ 8
ATN_ARR   equ 16
ATN_ARGS  equ 24
ATN_NARGS equ 32
ATN_FRAME equ 48            ; + 0 pushes = 48, 16-aligned
DEF_FUNC array_type_new, ATN_FRAME
    mov [rbp - ATN_ARGS], rsi
    mov [rbp - ATN_NARGS], rdx
    cmp rdx, 1
    jb .atn_arity
    cmp rdx, 2
    ja .atn_arity

    ; args[0] is the typecode.  CPython draws two different lines here: a
    ; one-character str that names no code is a ValueError about the code,
    ; and anything that is not a one-character str at all is a TypeError
    ; about the ARGUMENT, naming the type it got.
    mov rdi, [rsi]
    mov [rbp - ATN_ARR], rdi    ; the argument, for the message
    V_TEST_PTR rdi, rax
    ja .atn_not_a_char
    mov rax, [rdi + PyObject.ob_type]
    lea rcx, [rel str_type]
    cmp rax, rcx
    jne .atn_not_a_char
    cmp qword [rdi + PyStrObject.ob_length], 1
    jne .atn_not_a_char
    movzx edi, byte [rdi + PyStrObject.data]
    call array_find_code
    test rax, rax
    jz .atn_bad_code
    mov [rbp - ATN_CODE], rax

    mov rdi, rax
    call array_new_empty
    test rax, rax
    jz .atn_null
    mov [rbp - ATN_ARR], rax

    cmp qword [rbp - ATN_NARGS], 2
    jb .atn_done
    mov rdi, rax
    mov rsi, [rbp - ATN_ARGS]
    mov rsi, [rsi + 8]          ; args[1], one Value per slot
    call array_extend_iterable
    test eax, eax
    jz .atn_drop

.atn_done:
    mov rax, [rbp - ATN_ARR]
    mov edx, TAG_PTR
    leave
    V_PACK rax, rdx             ; builtins return one Value
    ret
.atn_drop:
    mov rdi, [rbp - ATN_ARR]
    call obj_decref
.atn_null:
    xor eax, eax
    xor edx, edx
    leave
    V_PACK rax, rdx
    ret
.atn_bad_code:
    RAISE exc_ValueError_type, \
          "bad typecode (must be b, B, u, h, H, i, I, l, L, q, Q, f or d)"
.atn_not_a_char:
    ; CPython's \x02 form spells NoneType as "None", which is what this
    ; message wants: "not None", not "not NoneType".
    mov rsi, [rbp - ATN_ARR]
    CSTRING rdi, \
        `array() argument 1 must be a unicode character, not \x02`
    extern raise_type_error_with_name
    jmp raise_type_error_with_name
.atn_arity:
    RAISE exc_TypeError_type, \
          "array() takes 1 or 2 arguments"
END_FUNC array_type_new

;; ============================================================================
;; array_tolist(rdi = the array) -> rax = a new list, or 0 raising
;; ============================================================================
ATL_ARR   equ 8
ATL_LIST  equ 16
ATL_I     equ 24
ATL_FRAME equ 32            ; + 0 pushes = 32, 16-aligned
DEF_FUNC array_tolist, ATL_FRAME
    mov [rbp - ATL_ARR], rdi
    xor edi, edi
    call list_new
    test rax, rax
    jz .atl_null
    mov [rbp - ATL_LIST], rax
    mov qword [rbp - ATL_I], 0
.atl_loop:
    mov rdi, [rbp - ATL_ARR]
    mov rsi, [rbp - ATL_I]
    cmp rsi, [rdi + PyArrayObject.ob_size]
    jge .atl_done
    call array_item_value
    test rax, rax
    jz .atl_drop
    push rax
    mov rdi, [rbp - ATL_LIST]
    mov rsi, rax
    call list_append
    pop rdi
    DECREF_V rdi, rcx           ; a Value; list_append took its own reference
    inc qword [rbp - ATL_I]
    jmp .atl_loop
.atl_done:
    mov rax, [rbp - ATL_LIST]
    leave
    ret
.atl_drop:
    mov rdi, [rbp - ATL_LIST]
    call obj_decref
.atl_null:
    xor eax, eax
    leave
    ret
END_FUNC array_tolist

;; ============================================================================
;; array_repr(rdi = the array) -> (rax = a str, edx = TAG_PTR), or (0, 0)
;;   array('i', [1, 2, 3]), and array('i') when it is empty
;;
;; The TAG matters: print reads it back from obj_str and skips an argument
;; whose tag is zero, so a repr that answers only in rax prints an empty line.
;;
;; Composed from the LIST's repr rather than written out item by item.  An
;; array's repr has no length bound, and list_repr already owns the growable
;; buffer and the recursion guard that needs; duplicating either here would be
;; duplicating the part that is easy to get wrong.
;; ============================================================================
ARP_ARR   equ 8
ARP_LIST  equ 16
ARP_INNER equ 24
ARP_BUF   equ 32
ARP_FRAME equ 48            ; + 0 pushes = 48, 16-aligned
DEF_FUNC array_repr, ARP_FRAME
    mov [rbp - ARP_ARR], rdi
    mov qword [rbp - ARP_LIST], 0
    mov qword [rbp - ARP_INNER], 0

    cmp qword [rdi + PyArrayObject.ob_size], 0
    jne .arp_with_items

    ; array('X')
    sub rsp, 32
    mov rdi, rsp
    lea rsi, [rel arp_open]
    call rbt_append_cstr
    mov rcx, [rbp - ARP_ARR]
    mov rcx, [rcx + PyArrayObject.ob_code]
    mov [rax], cl
    mov byte [rax + 1], 0
    lea rdi, [rax + 1]
    lea rsi, [rel arp_tail_empty]
    call rbt_append_cstr
    mov rdi, rsp
    call str_from_cstr_heap
    add rsp, 32
    mov edx, TAG_PTR
    leave
    ret

.arp_with_items:
    call array_tolist
    test rax, rax
    jz .arp_null
    mov [rbp - ARP_LIST], rax
    mov rdi, rax
    extern obj_repr
    call obj_repr
    test rax, rax
    jz .arp_drop
    mov [rbp - ARP_INNER], rax

    ; "array('X', " + inner + ")"
    mov rcx, [rax + PyStrObject.ob_size]
    lea rdi, [rcx + 32]
    call ap_malloc
    test rax, rax
    jz .arp_drop
    mov [rbp - ARP_BUF], rax
    mov rdi, rax
    lea rsi, [rel arp_open]
    call rbt_append_cstr
    mov rcx, [rbp - ARP_ARR]
    mov rcx, [rcx + PyArrayObject.ob_code]
    mov [rax], cl
    mov byte [rax + 1], 0
    lea rdi, [rax + 1]
    lea rsi, [rel arp_tail_items]
    call rbt_append_cstr
    mov rdi, rax
    mov rsi, [rbp - ARP_INNER]
    lea rsi, [rsi + PyStrObject.data]
    call rbt_append_cstr
    mov rdi, rax
    lea rsi, [rel arp_close_paren]
    call rbt_append_cstr

    mov rdi, [rbp - ARP_BUF]
    call str_from_cstr_heap
    push rax
    mov rdi, [rbp - ARP_BUF]
    call ap_free
    mov rdi, [rbp - ARP_INNER]
    call obj_decref
    mov rdi, [rbp - ARP_LIST]
    call obj_decref
    pop rax
    mov edx, TAG_PTR
    leave
    ret

.arp_drop:
    mov rdi, [rbp - ARP_INNER]
    test rdi, rdi
    jz .arp_drop_list
    call obj_decref
.arp_drop_list:
    mov rdi, [rbp - ARP_LIST]
    test rdi, rdi
    jz .arp_null
    call obj_decref
.arp_null:
    xor eax, eax
    leave
    ret
END_FUNC array_repr

section .rodata
arp_open:        db "array('", 0
arp_tail_empty:  db "')", 0
arp_tail_items:  db "', ", 0
arp_close_paren: db ")", 0
section .text

;; ============================================================================
;; array_subscript(rdi = the array, rsi = the key Value) -> rax = a Value
;; Integer indices only for now; a slice is the one thing left out.
;; ============================================================================
DEF_FUNC array_subscript, 8   ; + 1 push = 16, 16-aligned
    push rbx
    mov rbx, rdi
    mov rdi, rsi
    V_UNPACK rdi, rdx
    call obj_as_index
    cmp qword [rel current_exception], 0
    jne .asub_fail
    mov rdi, rbx
    mov rsi, rax
    pop rbx
    leave
    jmp array_sq_item
.asub_fail:
    xor eax, eax
    xor edx, edx
    pop rbx
    leave
    V_PACK rax, rdx
    ret
END_FUNC array_subscript

;; ============================================================================
;; array_ass_subscript(rdi = the array, rsi = key Value, rdx = value Value)
;;   -> eax = 0 on success, or -1 raising.  A NULL value is `del a[i]`.
;; ============================================================================
AAS_ARR   equ 8
AAS_VAL   equ 16
AAS_IDX   equ 24
AAS_FRAME equ 32            ; + 0 pushes = 32, 16-aligned
DEF_FUNC array_ass_subscript, AAS_FRAME
    mov [rbp - AAS_ARR], rdi
    mov [rbp - AAS_VAL], rdx
    mov rdi, rsi
    V_UNPACK rdi, rdx
    call obj_as_index
    cmp qword [rel current_exception], 0
    jne .aas_fail
    mov rdi, [rbp - AAS_ARR]
    test rax, rax
    jns .aas_have
    add rax, [rdi + PyArrayObject.ob_size]
.aas_have:
    cmp rax, 0
    jl .aas_range
    cmp rax, [rdi + PyArrayObject.ob_size]
    jge .aas_range
    mov [rbp - AAS_IDX], rax

    cmp qword [rbp - AAS_VAL], 0
    je .aas_delete

    mov rsi, rax
    mov rdx, [rbp - AAS_VAL]
    call array_store_item
    test eax, eax
    jz .aas_fail
    xor eax, eax
    leave
    ret

.aas_delete:
    ; Shift the tail down one item.  The buffer does not shrink: capacity is
    ; not what ob_size means.
    mov rdi, [rbp - AAS_ARR]
    mov rcx, [rdi + PyArrayObject.ob_isize]
    mov rax, [rbp - AAS_IDX]
    imul rax, rcx
    mov rsi, [rdi + PyArrayObject.ob_data]
    lea rdi, [rsi + rax]        ; dest = &item[i]
    lea rsi, [rdi + rcx]        ; src  = &item[i+1]
    mov rdx, [rbp - AAS_ARR]
    mov rdx, [rdx + PyArrayObject.ob_size]
    sub rdx, [rbp - AAS_IDX]
    dec rdx
    imul rdx, rcx               ; bytes after the hole
    test rdx, rdx
    jz .aas_shrink
    extern ap_memmove
    call ap_memmove
.aas_shrink:
    mov rdi, [rbp - AAS_ARR]
    dec qword [rdi + PyArrayObject.ob_size]
    xor eax, eax
    leave
    ret

.aas_fail:
    mov eax, -1
    leave
    ret
.aas_range:
    SET_EXC exc_IndexError_type, "array assignment index out of range"
    mov eax, -1
    leave
    ret
END_FUNC array_ass_subscript

section .data
align 8
array_seq_methods:
    dq array_length             ; +0:  sq_length
    dq 0                        ; +8:  sq_concat
    dq 0                        ; +16: sq_repeat
    ; sq_item is what reversed() looks for, and mp_subscript does not cover it.
    dq array_sq_item            ; +24: sq_item
    dq 0                        ; +32: sq_ass_item (mp_ass_subscript covers it)
    dq 0                        ; +40: sq_contains
    dq 0                        ; +48: sq_inplace_concat
    dq 0                        ; +56: sq_inplace_repeat

align 8
array_mapping_methods:
    dq array_length             ; mp_length        +0
    dq array_subscript          ; mp_subscript     +8
    dq array_ass_subscript      ; mp_ass_subscript +16

align 8
global array_type
array_type:
    dq 1                        ; ob_refcnt (immortal)
    dq type_type                ; ob_type
    dq array_name_str           ; tp_name
    dq PyArrayObject_size       ; tp_basicsize (the data is out of line)
    dq array_dealloc            ; tp_dealloc
    dq array_repr               ; tp_repr
    dq array_repr               ; tp_str
    ; Mutable, therefore unhashable -- and a 0 here is not the same thing:
    ; obj_hash falls through to the ADDRESS, so the key could never be found
    ; again.
    dq hash_not_implemented     ; tp_hash
    dq 0                        ; tp_call (set by add_builtin_type)
    dq array_getattr            ; tp_getattr (typecode, itemsize)
    dq 0                        ; tp_setattr
    dq 0                        ; tp_richcompare
    dq array_tp_iter            ; tp_iter
    dq 0                        ; tp_iternext
    dq 0                        ; tp_init
    dq 0                        ; tp_new (set by add_builtin_type)
    dq 0                        ; tp_as_number
    dq array_seq_methods        ; tp_as_sequence
    dq array_mapping_methods    ; tp_as_mapping
    dq 0                        ; tp_base
    dq 0                        ; tp_dict
    dq 0                        ; tp_mro
    ; No BASETYPE: the buffer moves, so a subclass could take neither a
    ; __dict__ nor __slots__ at a fixed offset, which is the rule bytearray
    ; and memoryview follow.
    dq TYPE_FLAG_FINAL          ; tp_flags
    dq 0                        ; tp_bases
    dq 0                        ; tp_traverse
    dq 0                        ; tp_clear
    dq 0                        ; tp_dictoffset
    dq 0                        ; tp_tailslots
section .text

;; ============================================================================
;; The methods, each (args, nargs) with args[0] the array.
;; ============================================================================

;; array_m_append(args, nargs) -> None, or 0 raising
DEF_FUNC array_m_append
    cmp rsi, 2
    jne .ama_arity
    mov rdx, [rdi + 8]
    mov rdi, [rdi]
    mov rsi, rdx
    call array_append_value
    test eax, eax
    jz .ama_fail
    LOAD_NONE rax
    INCREF rax
    mov edx, TAG_PTR
    leave
    V_PACK rax, rdx
    ret
.ama_fail:
    xor eax, eax
    xor edx, edx
    leave
    V_PACK rax, rdx
    ret
.ama_arity:
    RAISE exc_TypeError_type, "append() takes exactly one argument"
END_FUNC array_m_append

;; array_m_extend(args, nargs) -> None, or 0 raising
DEF_FUNC array_m_extend
    cmp rsi, 2
    jne .ame_arity
    mov rdx, [rdi + 8]
    mov rdi, [rdi]
    mov rsi, rdx
    call array_extend_iterable
    test eax, eax
    jz .ame_fail
    LOAD_NONE rax
    INCREF rax
    mov edx, TAG_PTR
    leave
    V_PACK rax, rdx
    ret
.ame_fail:
    xor eax, eax
    xor edx, edx
    leave
    V_PACK rax, rdx
    ret
.ame_arity:
    RAISE exc_TypeError_type, "extend() takes exactly one argument"
END_FUNC array_m_extend

;; array_m_tolist(args, nargs) -> a list of the items
DEF_FUNC array_m_tolist
    mov rdi, [rdi]
    call array_tolist
    mov edx, TAG_PTR
    test rax, rax
    jnz .amt_ok
    xor edx, edx
.amt_ok:
    leave
    V_PACK rax, rdx
    ret
END_FUNC array_m_tolist

;; array_m_fromlist(args, nargs) -> None, or 0 raising
;;
;; extend() by another name, which is what CPython's is once the list check
;; has passed.
DEF_FUNC_BARE array_m_fromlist
    jmp array_m_extend
END_FUNC array_m_fromlist

;; array_m_tobytes(args, nargs) -> the raw buffer as bytes
DEF_FUNC array_m_tobytes
    mov rdi, [rdi]
    mov rsi, [rdi + PyArrayObject.ob_size]
    imul rsi, [rdi + PyArrayObject.ob_isize]
    mov rdi, [rdi + PyArrayObject.ob_data]
    test rdi, rdi
    jnz .amb_have
    lea rdi, [rel array_empty_byte]
.amb_have:
    call bytes_from_data
    mov edx, TAG_PTR
    test rax, rax
    jnz .amb_ok
    xor edx, edx
.amb_ok:
    leave
    V_PACK rax, rdx
    ret
END_FUNC array_m_tobytes

;; array_m_frombytes(args, nargs) -> None
;;   The bytes must be a whole number of items, which is what CPython checks.
AFB_ARR   equ 8
AFB_FRAME equ 16            ; + 1 push = 24 ... padded below
DEF_FUNC array_m_frombytes, 24
    push rbx
    cmp rsi, 2
    jne .afb_arity
    mov rbx, [rdi + 8]          ; the bytes
    mov rdi, [rdi]
    mov [rbp - AFB_ARR], rdi

    V_TEST_PTR rbx, rax
    ja .afb_need_bytes
    mov rax, [rbx + PyObject.ob_type]
    lea rcx, [rel bytes_type]
    cmp rax, rcx
    jne .afb_need_bytes

    mov rax, [rbx + PyBytesObject.ob_size]
    xor edx, edx
    mov rcx, [rdi + PyArrayObject.ob_isize]
    div rcx
    test rdx, rdx
    jnz .afb_not_multiple
    mov rsi, rax                ; the item count coming in
    test rsi, rsi
    jz .afb_done
    push rsi
    add rsi, [rdi + PyArrayObject.ob_size]
    call array_reserve
    pop rsi
    test eax, eax
    jz .afb_fail

    mov rdi, [rbp - AFB_ARR]
    mov rax, [rdi + PyArrayObject.ob_size]
    imul rax, [rdi + PyArrayObject.ob_isize]
    add rax, [rdi + PyArrayObject.ob_data]
    push rsi
    mov rdi, rax
    lea rsi, [rbx + PyBytesObject.data]
    mov rdx, [rbx + PyBytesObject.ob_size]
    call ap_memcpy
    pop rsi
    mov rdi, [rbp - AFB_ARR]
    add [rdi + PyArrayObject.ob_size], rsi

.afb_done:
    LOAD_NONE rax
    INCREF rax
    mov edx, TAG_PTR
    pop rbx
    leave
    V_PACK rax, rdx
    ret
.afb_fail:
    xor eax, eax
    xor edx, edx
    pop rbx
    leave
    V_PACK rax, rdx
    ret
.afb_arity:
    pop rbx
    RAISE exc_TypeError_type, "frombytes() takes exactly one argument"
.afb_need_bytes:
    pop rbx
    RAISE exc_TypeError_type, "a bytes-like object is required"
.afb_not_multiple:
    pop rbx
    RAISE exc_ValueError_type, \
          "bytes length not a multiple of item size"
END_FUNC array_m_frombytes

;; array_m_buffer_info(args, nargs) -> (address, length)
ABI_TUP   equ 8
ABI_FRAME equ 16            ; + 1 push = 24 ... padded below
DEF_FUNC array_m_buffer_info, 24
    push rbx
    mov rbx, [rdi]
    mov edi, 2
    call tuple_new
    test rax, rax
    jz .abi_null
    mov [rbp - ABI_TUP], rax
    ; V_PACK each: a tuple slot holds a VALUE, and int_from_i64 answers a
    ; (payload, tag) pair.  Storing the payload put a raw integer where the
    ; collector expects a pointer, and tuple_traverse followed it.
    mov rdi, [rbx + PyArrayObject.ob_data]
    call int_from_i64
    V_PACK rax, rdx
    mov rcx, [rbp - ABI_TUP]
    mov rcx, [rcx + PyTupleObject.ob_item]
    mov [rcx], rax
    mov rdi, [rbx + PyArrayObject.ob_size]
    call int_from_i64
    V_PACK rax, rdx
    mov rcx, [rbp - ABI_TUP]
    mov rcx, [rcx + PyTupleObject.ob_item]
    mov [rcx + 8], rax
    mov rax, [rbp - ABI_TUP]
    mov edx, TAG_PTR
    pop rbx
    leave
    V_PACK rax, rdx
    ret
.abi_null:
    xor eax, eax
    xor edx, edx
    pop rbx
    leave
    V_PACK rax, rdx
    ret
END_FUNC array_m_buffer_info

section .rodata
array_empty_byte: db 0
section .text

;; ============================================================================
;; array_getattr(rdi = the array, rsi = the name str) -> rax = a Value
;;
;; typecode and itemsize are attributes, not methods, and the rest of the
;; surface is bound out of the type's tp_dict by the ordinary machinery -- so
;; this only has to answer the two and hand everything else back.
;; ============================================================================
AGA_ARR   equ 8
AGA_NAME  equ 16
AGA_FRAME equ 32            ; + 0 pushes = 32, 16-aligned
DEF_FUNC array_getattr, AGA_FRAME
    mov [rbp - AGA_ARR], rdi
    mov [rbp - AGA_NAME], rsi

    lea rdi, [rsi + PyStrObject.data]
    CSTRING rsi, "typecode"
    extern ap_strcmp
    call ap_strcmp
    test eax, eax
    jz .aga_typecode

    mov rsi, [rbp - AGA_NAME]
    lea rdi, [rsi + PyStrObject.data]
    CSTRING rsi, "itemsize"
    call ap_strcmp
    test eax, eax
    jz .aga_itemsize

    ; Not one of ours: the generic path, which finds the methods in tp_dict.
    mov rdi, [rbp - AGA_ARR]
    mov rsi, [rbp - AGA_NAME]
    extern obj_generic_attr
    leave
    jmp obj_generic_attr

.aga_typecode:
    ; A one-character str, which chr() knows how to build.
    mov rdi, [rbp - AGA_ARR]
    mov rdi, [rdi + PyArrayObject.ob_code]
    V_PACK_I64 rdi, rcx
    mov [rbp - AGA_NAME], rdi
    lea rdi, [rbp - AGA_NAME]
    mov esi, 1
    extern builtin_chr
    call builtin_chr
    leave
    ret

.aga_itemsize:
    mov rdi, [rbp - AGA_ARR]
    mov rdi, [rdi + PyArrayObject.ob_isize]
    call int_from_i64
    ; The tag int_from_i64 set, not TAG_PTR: overwriting it made V_PACK read
    ; a small integer as a pointer.
    leave
    V_PACK rax, rdx
    ret
END_FUNC array_getattr

;; ============================================================================
;; array_module_create() -> rax = the module object
;; ============================================================================
AMC_DICT  equ 8
AMC_KEY   equ 16
AMC_FN    equ 24
AMC_FRAME equ 32            ; + 2 pushes = 48, 16-aligned

;; AM_ADD_METHOD impl, "name" -- one entry in array's tp_dict, which rbx holds.
%macro AM_ADD_METHOD 2
    CSTRING rdi, %2
    call str_from_cstr_heap
    mov [rbp - AMC_KEY], rax
    lea rdi, [rel %1]
    CSTRING rsi, %2
    call builtin_func_new
    mov [rbp - AMC_FN], rax
    mov rdi, rbx
    mov rsi, [rbp - AMC_KEY]
    mov rdx, rax
    call dict_set
    mov rdi, [rbp - AMC_KEY]
    call obj_decref
    mov rdi, [rbp - AMC_FN]
    call obj_decref
%endmacro

;; ============================================================================
;; array_module_create() -> rax = the module object, or 0
;;
;; Builds the type's tp_dict, then the module dict around it.  import_init
;; puts what this returns straight into sys.modules, so it has to be the
;; module and not the dict.
;; ============================================================================
global array_module_create
DEF_FUNC array_module_create, AMC_FRAME
    push rbx
    push r12

    ; The type's own dict, which is where the methods are found by name.
    call dict_new
    test rax, rax
    jz .amc_out
    mov rbx, rax
    AM_ADD_METHOD array_m_append,      "append"
    AM_ADD_METHOD array_m_extend,      "extend"
    AM_ADD_METHOD array_m_tolist,      "tolist"
    AM_ADD_METHOD array_m_fromlist,    "fromlist"
    AM_ADD_METHOD array_m_tobytes,     "tobytes"
    AM_ADD_METHOD array_m_frombytes,   "frombytes"
    AM_ADD_METHOD array_m_buffer_info, "buffer_info"
    AM_ADD_METHOD array_m_getitem,     "__getitem__"
    AM_ADD_METHOD array_m_setitem,     "__setitem__"
    AM_ADD_METHOD array_m_len,         "__len__"
    lea rax, [rel array_type]
    mov [rax + PyTypeObject.tp_dict], rbx
    mov rdi, rax
    extern type_stamp_methods
    call type_stamp_methods

    ; ...and the module dict, holding the type and the typecode string.
    call dict_new
    test rax, rax
    jz .amc_out
    mov r12, rax
    mov [rbp - AMC_DICT], rax

    CSTRING rdi, "array"
    call str_from_cstr_heap
    mov [rbp - AMC_KEY], rax
    lea rcx, [rel array_type]
    mov qword [rcx + PyTypeObject.tp_new], 0
    mov rdi, r12
    mov rsi, rax
    lea rdx, [rel array_type]
    call dict_set
    mov rdi, [rbp - AMC_KEY]
    call obj_decref

    ; array.array is callable through tp_new, the way every builtin type is.
    lea rax, [rel array_type]
    lea rcx, [rel array_type_new]
    mov [rax + PyTypeObject.tp_new], rcx

    CSTRING rdi, "typecodes"
    call str_from_cstr_heap
    mov [rbp - AMC_KEY], rax
    lea rdi, [rel array_typecodes_str]
    call str_from_cstr_heap
    mov [rbp - AMC_FN], rax
    mov rdi, r12
    mov rsi, [rbp - AMC_KEY]
    mov rdx, rax
    call dict_set
    mov rdi, [rbp - AMC_KEY]
    call obj_decref
    mov rdi, [rbp - AMC_FN]
    call obj_decref

    ; And the module object around it, as every builtin module does: the
    ; table's create_fn goes straight into sys.modules, so a bare dict there
    ; makes `array.typecodes` an attribute lookup on a dict.
    CSTRING rdi, "array"
    call str_from_cstr_heap
    mov [rbp - AMC_KEY], rax
    mov rdi, rax
    mov rsi, r12
    extern module_new
    call module_new
    mov [rbp - AMC_FN], rax
    mov rdi, [rbp - AMC_KEY]
    call obj_decref             ; module_new took its own
    mov rdi, r12
    call obj_decref
    mov rax, [rbp - AMC_FN]
.amc_out:
    pop r12
    pop rbx
    leave
    ret
END_FUNC array_module_create

;; ============================================================================
;; array_tp_iter(rdi = the array) -> rax = an iterator over it, or 0
;;
;; The generic sequence iterator, which walks through sq_item.  CPython's
;; array has an iterator type of its own; this one answers the same items in
;; the same order, and it is the one already written.
;; ============================================================================
DEF_FUNC array_tp_iter
    call obj_incref             ; seq_iter_new takes ownership
    extern seq_iter_new
    call seq_iter_new
    leave
    ret
END_FUNC array_tp_iter

;; ============================================================================
;; array_m_getitem(args, nargs) -> the item at args[1], as a Value
;;
;; The sequence iterator walks through `__getitem__` by NAME, and the stdlib
;; asks by name too, so the slot is not enough on its own.
;; ============================================================================
DEF_FUNC array_m_getitem
    cmp rsi, 2
    jne .amg_arity
    mov rsi, [rdi + 8]
    mov rdi, [rdi]
    leave
    jmp array_subscript
.amg_arity:
    RAISE exc_TypeError_type, "__getitem__() takes exactly one argument"
END_FUNC array_m_getitem

;; ============================================================================
;; array_m_setitem(args, nargs) -> None, or 0 raising
;; ============================================================================
DEF_FUNC array_m_setitem
    cmp rsi, 3
    jne .ams_arity
    mov rdx, [rdi + 16]
    mov rsi, [rdi + 8]
    mov rdi, [rdi]
    call array_ass_subscript
    test eax, eax
    js .ams_fail
    LOAD_NONE rax
    INCREF rax
    mov edx, TAG_PTR
    leave
    V_PACK rax, rdx
    ret
.ams_fail:
    xor eax, eax
    xor edx, edx
    leave
    V_PACK rax, rdx
    ret
.ams_arity:
    RAISE exc_TypeError_type, "__setitem__() takes exactly two arguments"
END_FUNC array_m_setitem

;; ============================================================================
;; array_m_len(args, nargs) -> the item count, as a Value
;; ============================================================================
DEF_FUNC array_m_len
    mov rdi, [rdi]
    mov rdi, [rdi + PyArrayObject.ob_size]
    call int_from_i64
    leave
    V_PACK rax, rdx
    ret
END_FUNC array_m_len
