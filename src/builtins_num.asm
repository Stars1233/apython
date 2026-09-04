; builtins_num.asm - The numeric builtins
;
; abs, divmod, int(), bool(), float(), ord, chr, hex, bin, oct, round, pow.
; Each: name(PyObject **args, int64_t nargs) -> PyObject*, args borrowed,
; return a new reference.

%include "macros.inc"
%include "object.inc"
%include "opcodes.inc"

extern complex_to_parts
extern obj_incref
extern complex_from_doubles
extern complex_type

; External symbols used
extern int_to_i64
extern obj_as_index
extern int_base_str
extern int_is_integer
extern __gmpz_fits_slong_p
extern int_neg
extern int_from_cstr_base
extern float_from_f64
extern float_int
extern ap_malloc
extern ap_free
extern ap_memcpy
extern strlen
extern str_new
extern str_from_cstr
extern str_from_cstr_heap
extern obj_repr
extern obj_decref
extern raise_exception
extern exc_new
extern current_exception
extern eval_exception_unwind
extern int_promote_mpz

extern int_type
extern float_type
extern builtin_bool
extern builtin_float
extern str_type
extern bool_type
extern bool_true
extern bool_false

extern exc_TypeError_type
extern exc_ValueError_type
extern dunder_lookup
extern kw_names_pending
extern ap_strcmp

;; ============================================================================
;; 1. builtin_abs(args, nargs) - abs(x)
;; ============================================================================

; --- moved to a sibling file by the split ---

section .text

DEF_FUNC builtin_abs
    push rbx
    sub rsp, 8

    cmp rsi, 1
    jne .abs_error

    mov rbx, [rdi]

    V_TEST_INT_M [rdi], r11      ; args[0] an int immediate?
    jae .abs_smallint

    V_TEST_F64_M [rdi], r11      ; args[0] a float?
    jbe .abs_inline_float

    V_TEST_PTR_M [rdi], r11      ; args[0] a pointer?
    ja .abs_type_error

    mov rax, [rbx + PyObject.ob_type]
    lea rcx, [rel float_type]
    cmp rax, rcx
    je .abs_float

    lea rcx, [rel int_type]
    cmp rax, rcx
    je .abs_gmp_check

    ; Check bool_type (bool singletons: payload is 0 or 1 in mpz)
    extern bool_type
    lea rcx, [rel bool_type]
    cmp rax, rcx
    je .abs_bool

    ; Anything else goes through the numeric protocol, which now carries
    ; __abs__ for a user class.  abs(obj) used to be a flat TypeError.
    mov rcx, [rax + PyTypeObject.tp_as_number]
    test rcx, rcx
    jz .abs_type_error
    mov rcx, [rcx + PyNumberMethods.nb_absolute]
    test rcx, rcx
    jz .abs_type_error
    mov rdi, rbx                ; a pointer is its own Value
    call rcx
    V_UNPACK rax, rdx
    add rsp, 8
    pop rbx
    leave
    V_PACK rax, rdx
    ret

.abs_bool:
    ; Bool singleton: check if True (mpz=1) or False (mpz=0), both non-negative
    ; Return as SmallInt: True.abs = 1, False.abs = 0
    extern bool_true
    lea rcx, [rel bool_true]
    xor eax, eax
    cmp rbx, rcx
    sete al                    ; rax = 1 if True, 0 if False
    RET_TAG_SMALLINT
    add rsp, 8
    pop rbx
    leave
    V_PACK rax, rdx             ; builtins return one Value
    ret

.abs_gmp_check:

    ; GMP int: check _mp_size at PyIntObject.mpz + 4
    INT_NEED_MPZ rbx
    mov eax, [rbx + PyIntObject.mpz + 4]
    test eax, eax
    jl .abs_gmp_neg

    inc qword [rbx + PyObject.ob_refcnt]
    mov rax, rbx
    mov edx, TAG_PTR
    add rsp, 8
    pop rbx
    leave
    V_PACK rax, rdx             ; builtins return one Value
    ret

.abs_gmp_neg:
    mov rdi, rbx               ; a heap int pointer is its own Value
    call int_neg
    V_UNPACK rax, rdx           ; int_neg returns a Value
    add rsp, 8
    pop rbx
    leave
    V_PACK rax, rdx             ; builtins return one Value
    ret

.abs_smallint:
    mov rax, rbx
    V_TO_I64 rax
    test rax, rax
    jns .abs_si_pos
    neg rax
.abs_si_pos:
    RET_TAG_SMALLINT
    add rsp, 8
    pop rbx
    leave
    V_PACK rax, rdx             ; builtins return one Value
    ret

.abs_bool_tag:
    ; TAG_BOOL: payload is 0 or 1, already non-negative → return as SmallInt
    mov rax, rbx
    RET_TAG_SMALLINT
    add rsp, 8
    pop rbx
    leave
    V_PACK rax, rdx             ; builtins return one Value
    ret

.abs_inline_float:
    ; A float immediate: clear the sign bit inline
    V_TO_F64 rbx
    btr rbx, 63
    mov rax, rbx
    mov edx, TAG_FLOAT
    add rsp, 8
    pop rbx
    leave
    V_PACK rax, rdx             ; builtins return one Value
    ret

.abs_float:
    movsd xmm0, [rbx + PyFloatObject.value]
    mov rax, 0x7fffffffffffffff
    movq xmm1, rax
    andpd xmm0, xmm1
    call float_from_f64
    add rsp, 8
    pop rbx
    leave
    V_PACK rax, rdx             ; builtins return one Value
    ret

.abs_type_error:
    RAISE exc_TypeError_type, "bad operand type for abs()"

.abs_error:
    RAISE exc_TypeError_type, "abs() takes exactly one argument"
END_FUNC builtin_abs

;; ============================================================================
;; builtin_divmod(args, nargs) - divmod(a, b) -> (a // b, a % b)
;; ============================================================================
DEF_FUNC builtin_divmod, 8            ; 5 pushes, so rsp is 16-aligned
    push rbx
    push r12
    push r13
    push r14
    push r15

    cmp rsi, 2
    jne .divmod_error

    mov rbx, [rdi]              ; args[0] = a
    V_UNPACK rbx, r13
    mov r12, [rdi + 8]          ; args[1] = b
    V_UNPACK r12, r14

    ; nb_divmod first, where the type has one.  Building the pair out of //
    ; and % is only a stand-in for types that do not define __divmod__, and
    ; it answered ('floordiv', 'mod') for a class that did -- calling two
    ; methods the program had written for other operators and never the one
    ; it had written for this.  No builtin here fills nb_divmod, so this arm
    ; exists for classes, which reach it through the slot wrapper.
    mov rdi, rbx
    mov edx, r13d
    extern value_number_methods
    call value_number_methods
    test rax, rax
    jz .divmod_type_error
    mov rax, [rax + PyNumberMethods.nb_divmod]
    test rax, rax
    jz .divmod_no_slot
    mov rdi, rbx
    mov edx, r13d
    mov rsi, r12
    mov ecx, r14d
    V_PACK rdi, rdx
    V_PACK rsi, rcx
    call rax
    test rax, rax
    jz .divmod_no_slot          ; declined: fall back to // and %
    pop r15
    pop r14
    pop r13
    pop r12
    pop rbx
    leave
    ret

.divmod_no_slot:
    ; A zero float divisor is diagnosed here rather than left to the two
    ; slots below.  divmod(1.0, 0.0) is "float divmod()" in CPython, and
    ; reaching it through nb_floor_divide reported the floor division's own
    ; message instead.
    mov rdi, r12
    mov esi, r14d
    call divmod_is_zero
    test eax, eax
    jz .divmod_no_float_zero
    mov rdi, rbx
    mov esi, r13d
    call divmod_is_float
    test eax, eax
    jnz .divmod_float_zero
    mov rdi, r12
    mov esi, r14d
    call divmod_is_float
    test eax, eax
    jnz .divmod_float_zero
.divmod_no_float_zero:

    ; Dispatch through the left operand's numeric protocol, the way the //
    ; and % operators do.  This used to call int_floordiv unconditionally,
    ; so divmod(1.5, 1.5) handed raw f64 bits to integer code.
    mov rdi, rbx
    mov edx, r13d
    call value_number_methods
    test rax, rax
    jz .divmod_type_error
    mov rax, [rax + PyNumberMethods.nb_floor_divide]
    test rax, rax
    jz .divmod_type_error

    mov rdi, rbx
    mov edx, r13d
    mov rsi, r12
    mov ecx, r14d
    V_PACK rdi, rdx
    V_PACK rsi, rcx
    call rax
    ; A slot may decline: this branch taught int_floordiv and int_mod to
    ; answer NULL for a non-int operand, and op_binary_op to fall back on the
    ; other operand's slot.  divmod() is the other direct caller and was not
    ; taught, so the NULL went into the result tuple and `divmod(7, 2.0)`
    ; produced an object with no repr instead of (3.0, 1.0).
    test rax, rax
    jnz .divmod_have_quot
    mov rdi, r12
    mov edx, r14d
    call value_number_methods   ; the RIGHT operand's, as the reflected op
    test rax, rax
    jz .divmod_type_error
    mov rax, [rax + PyNumberMethods.nb_floor_divide]
    test rax, rax
    jz .divmod_type_error
    mov rdi, rbx
    mov edx, r13d
    mov rsi, r12
    mov ecx, r14d
    V_PACK rdi, rdx
    V_PACK rsi, rcx
    call rax
    test rax, rax
    jz .divmod_type_error
.divmod_have_quot:
    V_UNPACK rax, rdx           ; int_floordiv returns a Value
    mov r15, rax                ; r15 = quotient payload
    push rdx                   ; save quotient tag (stack slot)

    ; Same for the remainder.
    mov rdi, rbx
    mov edx, r13d
    call value_number_methods
    test rax, rax
    jz .divmod_type_error
    mov rax, [rax + PyNumberMethods.nb_remainder]
    test rax, rax
    jz .divmod_type_error

    mov rdi, rbx
    mov edx, r13d
    mov rsi, r12
    mov ecx, r14d
    V_PACK rdi, rdx
    V_PACK rsi, rcx
    call rax
    test rax, rax
    jnz .divmod_have_rem
    mov rdi, r12
    mov edx, r14d
    call value_number_methods
    test rax, rax
    jz .divmod_pop_type_error
    mov rax, [rax + PyNumberMethods.nb_remainder]
    test rax, rax
    jz .divmod_pop_type_error
    mov rdi, rbx
    mov edx, r13d
    mov rsi, r12
    mov ecx, r14d
    V_PACK rdi, rdx
    V_PACK rsi, rcx
    call rax
    test rax, rax
    jz .divmod_pop_type_error
.divmod_have_rem:
    V_UNPACK rax, rdx           ; int_mod returns a Value
    mov r12, rax                ; r12 = remainder payload
    mov r13, rdx                ; r13 = remainder tag

    ; Create 2-tuple (quotient, remainder)
    mov edi, 2
    extern tuple_new
    call tuple_new
    mov rbx, [rax + PyTupleObject.ob_item]
    pop rcx                                      ; quotient tag
    V_PACK r15, rcx
    mov [rbx], r15
    V_PACK r12, r13
    mov [rbx + 8], r12
    mov edx, TAG_PTR

    pop r15
    pop r14
    pop r13
    pop r12
    pop rbx
    leave
    V_PACK rax, rdx             ; builtins return one Value
    ret

.divmod_error:
    RAISE exc_TypeError_type, "divmod expected 2 arguments"

.divmod_pop_type_error:
    add rsp, 8                  ; the quotient tag pushed above
.divmod_type_error:
    ; Name both operands, as CPython does: with two of them, "unsupported"
    ; alone does not say which.
    mov rdi, rbx
    mov edx, r13d
    V_PACK rdi, rdx
    mov rsi, r12
    mov ecx, r14d
    V_PACK rsi, rcx
    CSTRING rdx, "unsupported operand type(s) for divmod()"
    call raise_binop_type_error
.divmod_float_zero:
    extern exc_ZeroDivisionError_type
    RAISE exc_ZeroDivisionError_type, "float divmod()"
END_FUNC builtin_divmod

;; ============================================================================
;; divmod_is_float(rdi = payload, esi = tag) -> eax = 1 for a float or a
;; float subclass instance
;; divmod_is_zero(rdi = payload, esi = tag)  -> eax = 1 for a zero int or float
;;
;; divmod(1.5, 0) is "float divmod()" in CPython even though the divisor is an
;; int, so the two questions are asked separately: is the divisor zero, and is
;; either operand a float.
;; ============================================================================
DEF_FUNC_LOCAL divmod_is_float
    cmp esi, TAG_FLOAT
    je .dif_yes
    cmp esi, TAG_PTR
    jne .dif_no
    lea rax, [rel float_type]
    cmp [rdi + PyObject.ob_type], rax
    je .dif_yes
    mov rax, [rdi + PyObject.ob_type]
    test qword [rax + PyTypeObject.tp_flags], TYPE_FLAG_FLOAT_SUBCLASS
    jz .dif_no
.dif_yes:
    mov eax, 1
    leave
    ret
.dif_no:
    xor eax, eax
    leave
    ret
END_FUNC divmod_is_float

DEF_FUNC_LOCAL divmod_is_zero
    cmp esi, TAG_SMALLINT
    je .diz_int
    cmp esi, TAG_FLOAT
    je .diz_raw
    cmp esi, TAG_PTR
    jne .diz_no
    lea rax, [rel float_type]
    cmp [rdi + PyObject.ob_type], rax
    je .diz_obj
    mov rax, [rdi + PyObject.ob_type]
    test qword [rax + PyTypeObject.tp_flags], TYPE_FLAG_FLOAT_SUBCLASS
    jnz .diz_obj
    jmp .diz_no                 ; a heap int is never zero: it would be compact
.diz_obj:
    movsd xmm0, [rdi + PyFloatObject.value]
    jmp .diz_test
.diz_raw:
    movq xmm0, rdi
.diz_test:
    xorpd xmm1, xmm1
    ucomisd xmm0, xmm1
    jp .diz_no
    jne .diz_no
    mov eax, 1
    leave
    ret
.diz_int:
    test rdi, rdi
    jnz .diz_no
    mov eax, 1
    leave
    ret
.diz_no:
    xor eax, eax
    leave
    ret
END_FUNC divmod_is_zero

; tp_call wrappers: shift (type, args, nargs) → (args, nargs)
global int_type_call
ITC_FRAME  equ 16            ; + 0 pushes = 16, 16-aligned
DEF_FUNC int_type_call, ITC_FRAME
    ; rdi=type, rsi=args, rdx=nargs
    mov rdi, rsi
    mov rsi, rdx
    ; Check for keyword args
    mov rax, [rel kw_names_pending]
    test rax, rax
    jz .itc_no_kw
    ; Have keyword args — get count
    mov rcx, [rax + PyTupleObject.ob_size]   ; n_kw
    mov r8, rsi
    sub r8, rcx                               ; n_pos = nargs - n_kw
    ; Check each keyword name
    xor r9d, r9d                              ; index
.itc_kw_loop:
    cmp r9, rcx
    jge .itc_kw_checked
    mov r10, [rax + PyTupleObject.ob_item]        ; kw names payloads
    mov r10, [r10 + r9*8]                          ; kw name str
    ; Compare to "base"
    push rdi
    push rsi
    push rcx
    push rax
    push r8
    push r9
    lea rdi, [r10 + PyStrObject.data]
    CSTRING rsi, "base"
    call ap_strcmp
    mov r11d, eax               ; save strcmp result
    pop r9
    pop r8
    pop rax
    pop rcx
    pop rsi
    pop rdi
    test r11d, r11d
    jnz .itc_kw_reject          ; not "base" → reject
    inc r9
    jmp .itc_kw_loop
.itc_kw_checked:
    ; All keywords are "base". Validate: need exactly 1 positional + 1 keyword
    cmp rcx, 1
    jne .itc_kw_reject
    cmp r8, 1
    jne .itc_kw_no_pos          ; base= without positional string → TypeError
    ; Good: int('str', base=N) — args are already [str, base], nargs=2
    ; Clear kw_names_pending (we consumed it)
    mov qword [rel kw_names_pending], 0
    leave
    jmp builtin_int_fn
.itc_kw_no_pos:
    cmp r8, 0
    jne .itc_kw_reject
    RAISE exc_TypeError_type, "int() missing string argument"
.itc_kw_reject:
    RAISE exc_TypeError_type, "'x' is an invalid keyword argument for int()"
.itc_no_kw:
    leave
    jmp builtin_int_fn
END_FUNC int_type_call

DEF_FUNC_BARE bool_type_call
    ; Check for kwargs — bool() doesn't accept keyword arguments
    mov rax, [rel kw_names_pending]
    test rax, rax
    jnz .bool_kwargs_error
    mov rdi, rsi
    mov rsi, rdx
    jmp builtin_bool
.bool_kwargs_error:
    mov qword [rel kw_names_pending], 0
    extern exc_TypeError_type
    extern raise_exception
    RAISE exc_TypeError_type, "bool() takes no keyword arguments"
END_FUNC bool_type_call

;; float_type_call(rdi = type, rsi = args, rdx = nargs) -> a fat pair
;;
;; The type argument used to be discarded, so `class F(float)` produced a
;; plain float: the subclass name was lost and its __init__ never ran.  int
;; and str were right because their family flag routes type_call elsewhere;
;; float and complex had neither the flag nor a constructor that read rdi.
FTC_TYPE  equ 8
FTC_BITS  equ 16
FTC_FRAME equ 16            ; + 0 pushes = 16
extern builtin_sub_alloc

DEF_FUNC float_type_call, FTC_FRAME
    mov [rbp - FTC_TYPE], rdi
    mov rdi, rsi
    mov rsi, rdx
    call builtin_float          ; rax = raw double bits, edx = TAG_FLOAT
    lea rcx, [rel float_type]
    cmp [rbp - FTC_TYPE], rcx
    je .ftc_out                 ; float() itself: the immediate is the answer
    test edx, edx
    jz .ftc_out                 ; it raised

    ; A subclass instance carries the double where float_to_f64 reads it,
    ; at the base's own offset.  Returning a pointer here is what lets
    ; type_call's two callers agree: .not_type_self packs the fat pair and
    ; .nnf_check_base_new unpacks it as a Value, and only a pointer means
    ; the same thing to both.
    mov [rbp - FTC_BITS], rax
    mov rdi, [rbp - FTC_TYPE]
    call builtin_sub_alloc
    mov rcx, [rbp - FTC_BITS]
    mov [rax + PyFloatObject.value], rcx
    mov edx, TAG_PTR
.ftc_out:
    leave
    ret
END_FUNC float_type_call

;; ============================================================================
;; 2. builtin_int_fn(args, nargs) - int(x) or int(x, base)
;; ============================================================================
; Frame layout:
BI_ARGS   equ 8
BI_NARGS  equ 16
BI_OBJ    equ 24       ; original string/bytes obj for error messages
BI_BASE   equ 32       ; base value for error messages
BI_ORIGIN equ 40       ; the argument's type, for the bytes-family MRO walk
BI_XLAT   equ 64       ; a Unicode-to-ASCII copy of the argument, or 0
BI_DATA   equ 72       ; the bytes actually parsed: that copy, or the original
BI_XLEN   equ 80       ; and its length, which strlen cannot recover
BI_LEN    equ 48       ; the source length: bytes and bytearray keep it in
                       ; different fields, so the shared tail cannot re-read it
BI_FRAME  equ 88            ; + 1 push = 96, 16-byte aligned

DEF_FUNC builtin_int_fn, BI_FRAME
    push rbx
    mov qword [rbp - BI_ORIGIN], 0

    test rsi, rsi
    jz .int_no_args

    cmp rsi, 1
    je .int_one_arg

    cmp rsi, 2
    je .int_two_args

    jmp .int_error

.int_one_arg:
    mov rbx, [rdi]

    V_TEST_INT_M [rdi], r11      ; args[0] an int immediate?
    jae .int_return_smallint

    V_TEST_F64_M [rdi], r11      ; args[0] a float?
    jbe .int_from_inline_float

    ; Must be TAG_PTR to dereference
    V_TEST_PTR_M [rdi], r11      ; args[0] a pointer?
    ja .int_type_error

    mov rax, [rbx + PyObject.ob_type]

    lea rcx, [rel bool_type]
    cmp rax, rcx
    je .int_from_bool

    lea rcx, [rel int_type]
    cmp rax, rcx
    je .int_from_int

    ; Check int subclass (TYPE_FLAG_INT_SUBCLASS) — e.g. class MyInt(int)
    mov rdx, [rax + PyTypeObject.tp_flags]
    test rdx, TYPE_FLAG_INT_SUBCLASS
    jnz .int_from_int_subclass

    lea rcx, [rel float_type]
    cmp rax, rcx
    je .int_from_float
    ; A float subclass keeps its double inline; .int_from_float reads it
    ; through float_int, which unwraps one.
    test rdx, TYPE_FLAG_FLOAT_SUBCLASS
    jnz .int_from_float

    lea rcx, [rel str_type]
    cmp rax, rcx
    je .int_from_str
    ; Check str subclass via flag
    mov rdx, [rax + PyTypeObject.tp_flags]
    test rdx, TYPE_FLAG_STR_SUBCLASS
    jnz .int_from_str

    extern bytes_type
    extern bytearray_type
    extern memoryview_type
    ; Check bytes, bytearray, or subclasses (walk base chain)
    mov rcx, rax
.int_check_bytes_chain:
    cmp qword [rbp - BI_ORIGIN], 0
    jne .int_chain_have_origin
    mov [rbp - BI_ORIGIN], rcx
.int_chain_have_origin:
    lea rdx, [rel bytes_type]
    cmp rcx, rdx
    je .int_from_bytes
    lea rdx, [rel bytearray_type]
    cmp rcx, rdx
    je .int_from_bytearray
    lea rdx, [rel memoryview_type]
    cmp rcx, rdx
    je .int_from_memoryview
    MRO_NEXT rcx, [rbp - BI_ORIGIN]
    test rcx, rcx
    jnz .int_check_bytes_chain

    jmp .int_try_dunder

.int_no_args:
    xor eax, eax
    RET_TAG_SMALLINT
    jmp .int_ret

.int_return_smallint:
    mov rax, rbx
    V_TO_I64 rax
    RET_TAG_SMALLINT
    jmp .int_ret

.int_from_int:
    inc qword [rbx + PyObject.ob_refcnt]
    mov rax, rbx
    mov edx, TAG_PTR
    jmp .int_ret

.int_from_bool_tag:
    ; TAG_BOOL: payload 0 (False) or 1 (True) → SmallInt
    mov rax, rbx
    RET_TAG_SMALLINT
    jmp .int_ret

.int_from_inline_float:
    ; A float immediate — delegate to float_int for the NaN/inf checks
    mov rdi, rbx
    call float_int
    V_UNPACK rax, rdx           ; .int_ret hands back the pair, not the Value
    jmp .int_ret

.int_from_float:
    ; A float subclass instance.  float_int unboxes one itself now, so the
    ; Value goes straight in; this arm used to read the double out by hand.
    mov rdi, rbx
    call float_int
    V_UNPACK rax, rdx
    jmp .int_ret

.int_from_str:
    mov [rbp - BI_OBJ], rbx           ; save original obj for error msg
    mov qword [rbp - BI_BASE], 10     ; base 10
    ; A Unicode decimal digit is a digit, and a Unicode space is a space:
    ; int("\uff11\uff12\uff13") is 123 in CPython, which runs
    ; _PyUnicode_TransformDecimalAndSpaceToASCII over the argument first.
    mov qword [rbp - BI_XLAT], 0
    mov rdi, rbx
    extern str_decimal_ascii
    call str_decimal_ascii
    test rax, rax
    jz .int_str_ascii
    mov [rbp - BI_XLAT], rax
    mov [rbp - BI_XLEN], rdx
    mov rdi, rax
    jmp .int_str_have_data
.int_str_ascii:
    mov rax, [rbx + PyStrObject.ob_size]
    mov [rbp - BI_XLEN], rax
    lea rdi, [rbx + PyStrObject.data]
.int_str_have_data:
    mov [rbp - BI_DATA], rdi
    ; Check for embedded NUL bytes -- against the length of what is actually
    ; being parsed, which is the translated copy's when there is one.
    call strlen wrt ..plt
    cmp rax, [rbp - BI_XLEN]
    jne .int_str_parse_error_x
    mov rdi, [rbp - BI_DATA]
    mov rsi, 10
    call int_from_cstr_base
    test edx, edx
    jz .int_str_parse_error_x
    push rax
    push rdx
    mov rdi, [rbp - BI_XLAT]
    test rdi, rdi
    jz .int_str_kept
    extern ap_free
    call ap_free
.int_str_kept:
    pop rdx
    pop rax
    jmp .int_ret

.int_str_parse_error_x:
    mov rdi, [rbp - BI_XLAT]
    test rdi, rdi
    jz .int_str_parse_error
    call ap_free
    jmp .int_str_parse_error

.int_from_bytes:
    ; int(bytes_obj) — need null-terminated copy for int_from_cstr_base
    mov [rbp - BI_OBJ], rbx           ; save original obj for error msg
    mov qword [rbp - BI_BASE], 10     ; base 10
    mov rcx, [rbx + PyBytesObject.ob_size]
    lea rdi, [rcx + 8]       ; size + 8-byte NUL padding
    push rcx
    call ap_malloc
    pop rcx
    push rax                  ; save buffer ptr
    ; Copy bytes data
    mov rdi, rax
    lea rsi, [rbx + PyBytesObject.data]
    mov rdx, rcx
    extern ap_memcpy
    call ap_memcpy
    ; Null-terminate with 8-byte zero-fill
    pop rdi                   ; rdi = buffer
    push rdi
    mov rcx, [rbx + PyBytesObject.ob_size]
    mov qword [rdi + rcx], 0
    ; Check for embedded NUL bytes
    call strlen wrt ..plt
    cmp rax, [rbx + PyBytesObject.ob_size]
    jne .int_bytes_nul_error  ; embedded NUL → free buf + error
    ; Parse
    mov rdi, [rsp]            ; buffer (still on stack)
    mov rsi, 10
    call int_from_cstr_base
    mov rbx, rax              ; save result payload
    push rdx                  ; save result tag
    mov rdi, [rsp + 8]       ; buffer ptr (under tag on stack)
    call ap_free
    pop rdx                   ; restore result tag
    add rsp, 8               ; pop buffer ptr
    mov rax, rbx
    test edx, edx            ; check tag (not payload — SmallInt 0 is valid)
    jz .int_str_parse_error
    jmp .int_ret

.int_bytes_nul_error:
    pop rdi                   ; free temp buffer
    call ap_free
    jmp .int_str_parse_error

.int_str_parse_error:
    jmp .int_invalid_literal_error

.int_from_bytearray:
    ; Same as int_from_bytes but using PyByteArrayObject layout (identical to PyBytesObject)
    mov [rbp - BI_OBJ], rbx
    mov qword [rbp - BI_BASE], 10
    mov rcx, [rbx + PyByteArrayObject.ob_size]
    lea rdi, [rcx + 8]
    push rcx
    call ap_malloc
    pop rcx
    push rax
    mov rdi, rax
    mov rsi, [rbx + PyByteArrayObject.ob_bytes]
    mov rdx, rcx
    call ap_memcpy
    pop rdi
    push rdi
    mov rcx, [rbx + PyByteArrayObject.ob_size]
    mov qword [rdi + rcx], 0
    ; Check for embedded NUL
    call strlen wrt ..plt
    cmp rax, [rbx + PyByteArrayObject.ob_size]
    jne .int_bytes_nul_error
    mov rdi, [rsp]
    mov rsi, 10
    call int_from_cstr_base
    mov rbx, rax              ; save result payload
    push rdx                  ; save result tag
    mov rdi, [rsp + 8]       ; buffer ptr (under tag on stack)
    call ap_free
    pop rdx                   ; restore result tag
    add rsp, 8               ; pop buffer ptr
    mov rax, rbx
    test edx, edx            ; check tag (not payload — SmallInt 0 is valid)
    jz .int_str_parse_error
    jmp .int_ret

.int_from_memoryview:
    ; int(memoryview) — copy the viewed bytes and parse.  A strided view has
    ; no contiguous run, and a number is not what one is for; CPython's own
    ; int() over a non-contiguous view raises through the buffer protocol.
    cmp qword [rbx + PyMemoryViewObject.mv_stride], 1
    jne .int_type_error
    ; int(memoryview) — copy the viewed bytes and parse
    mov [rbp - BI_OBJ], rbx
    mov qword [rbp - BI_BASE], 10
    mov rcx, [rbx + PyMemoryViewObject.mv_len]
    lea rdi, [rcx + 8]
    push rcx
    call ap_malloc
    pop rcx
    push rax
    mov rdi, rax
    mov rsi, [rbx + PyMemoryViewObject.mv_buf]
    mov rdx, rcx
    call ap_memcpy
    pop rdi
    push rdi
    mov rcx, [rbx + PyMemoryViewObject.mv_len]
    mov qword [rdi + rcx], 0
    ; Check for embedded NUL
    call strlen wrt ..plt
    cmp rax, [rbx + PyMemoryViewObject.mv_len]
    jne .int_bytes_nul_error
    mov rdi, [rsp]
    mov rsi, 10
    call int_from_cstr_base
    mov rbx, rax              ; save result payload
    push rdx                  ; save result tag
    mov rdi, [rsp + 8]       ; buffer ptr (under tag on stack)
    call ap_free
    pop rdx                   ; restore result tag
    add rsp, 8               ; pop buffer ptr
    mov rax, rbx
    test edx, edx            ; check tag (not payload — SmallInt 0 is valid)
    jz .int_str_parse_error
    jmp .int_ret

.int_from_bool:
    lea rax, [rel bool_true]
    cmp rbx, rax
    je .int_bool_true
    xor eax, eax
    RET_TAG_SMALLINT
    jmp .int_ret
.int_bool_true:
    mov rax, 1
    RET_TAG_SMALLINT
    jmp .int_ret

.int_from_int_subclass:
    ; rbx = int subclass instance (PyIntSubclassObject)
    ; Check if it has __int__ method
    mov rdi, [rbx + PyObject.ob_type]
    CSTRING rsi, "__int__"
    call dunder_lookup
    V_UNPACK rax, rdx           ; returns a Value
    test edx, edx
    jz .int_from_int_sub_extract ; no __int__, extract int_value
    ; Call __int__(self) — rax = func (borrowed ref)
    mov rcx, [rax + PyObject.ob_type]
    mov rcx, [rcx + PyTypeObject.tp_call]
    test rcx, rcx
    jz .int_from_int
    SPUSH_PTR rbx                ; args[0] = self (fat arg)
    mov rdi, rax
    mov rsi, rsp
    mov edx, 1
    call rcx
    V_UNPACK rax, rdx           ; tp_call returns a Value
    add rsp, 16
    ; Check for exception (NULL return)
    test edx, edx
    jz .int_dunder_error
    ; Verify result is int-like
    cmp edx, TAG_SMALLINT
    je .int_ret                  ; SmallInt — OK
    cmp edx, TAG_FLOAT
    je .int_dunder_returned_float
    mov rcx, [rax + PyObject.ob_type]
    lea r8, [rel int_type]
    cmp rcx, r8
    je .int_ret                  ; exact int — OK
    lea r8, [rel bool_type]
    cmp rcx, r8
    je .int_convert_bool_result  ; bool → convert to plain int
    mov r8, [rcx + PyTypeObject.tp_flags]
    test r8, TYPE_FLAG_INT_SUBCLASS
    jnz .int_ret                 ; int subclass — OK for now
    ; __int__ returned non-int
    mov rdi, rax
    call obj_decref
    RAISE exc_TypeError_type, "__int__ returned non-int (type float)"

.int_from_int_sub_extract:
    ; rbx = PyIntSubclassObject with no __int__ method
    ; Extract the int_value and return it
    mov rax, [rbx + PyIntSubclassObject.int_value]
    V_UNPACK rax, rdx
    cmp edx, TAG_SMALLINT
    je .int_ret                  ; SmallInt — no INCREF needed
    INCREF rax
    jmp .int_ret

.int_try_dunder:
    ; rbx = unknown-type object
    ; Try __int__ protocol
    mov rdi, [rbx + PyObject.ob_type]
    CSTRING rsi, "__int__"
    call dunder_lookup
    V_UNPACK rax, rdx           ; returns a Value
    test edx, edx
    jnz .int_call_dunder

    ; Try __index__ protocol
    mov rdi, [rbx + PyObject.ob_type]
    CSTRING rsi, "__index__"
    call dunder_lookup
    V_UNPACK rax, rdx           ; returns a Value
    test edx, edx
    jnz .int_call_dunder

    ; Try __trunc__ protocol
    mov rdi, [rbx + PyObject.ob_type]
    CSTRING rsi, "__trunc__"
    call dunder_lookup
    V_UNPACK rax, rdx           ; returns a Value
    test edx, edx
    jnz .int_call_dunder_trunc

    jmp .int_type_error

.int_call_dunder:
    ; rax = func (borrowed ref), rbx = self
    mov rcx, [rax + PyObject.ob_type]
    mov rcx, [rcx + PyTypeObject.tp_call]
    test rcx, rcx
    jz .int_type_error
    SPUSH_PTR rbx                ; args[0] = self (fat arg)
    mov rdi, rax
    mov rsi, rsp
    mov edx, 1
    call rcx
    V_UNPACK rax, rdx           ; tp_call returns a Value
    add rsp, 16
    ; Check for exception (NULL return)
    test edx, edx
    jz .int_dunder_error
    ; Verify result is int-like
    cmp edx, TAG_SMALLINT
    je .int_ret                  ; SmallInt — OK
    cmp edx, TAG_FLOAT
    je .int_dunder_returned_float
    mov rcx, [rax + PyObject.ob_type]
    lea r8, [rel int_type]
    cmp rcx, r8
    je .int_ret                  ; exact int — OK
    lea r8, [rel bool_type]
    cmp rcx, r8
    je .int_convert_bool_result  ; bool → convert to plain int
    mov r8, [rcx + PyTypeObject.tp_flags]
    test r8, TYPE_FLAG_INT_SUBCLASS
    jnz .int_ret                 ; int subclass — OK
    ; Not int-like
    mov rdi, rax
    call obj_decref
    RAISE exc_TypeError_type, "__int__ returned non-int"

.int_call_dunder_trunc:
    ; rax = __trunc__ func, rbx = self
    ; Call __trunc__(self); result must be int-like or have __index__
    ; CPython 3.12: tries __index__ on result, but NOT __int__
    mov rcx, [rax + PyObject.ob_type]
    mov rcx, [rcx + PyTypeObject.tp_call]
    test rcx, rcx
    jz .int_type_error
    SPUSH_PTR rbx                ; args[0] = self (fat arg)
    mov rdi, rax
    mov rsi, rsp
    mov edx, 1
    call rcx
    V_UNPACK rax, rdx           ; tp_call returns a Value
    add rsp, 16
    ; rax = result of __trunc__()
    ; Check for exception (NULL return)
    test edx, edx
    jz .int_dunder_error
    ; If it's already an int, return it
    cmp edx, TAG_SMALLINT
    je .int_ret                  ; SmallInt — OK
    cmp edx, TAG_PTR
    jne .int_trunc_nonint_error  ; non-pointer (Float/None/Bool) — not int
    mov rcx, [rax + PyObject.ob_type]
    lea r8, [rel int_type]
    cmp rcx, r8
    je .int_ret
    lea r8, [rel bool_type]
    cmp rcx, r8
    je .int_convert_bool_result
    mov r8, [rcx + PyTypeObject.tp_flags]
    test r8, TYPE_FLAG_INT_SUBCLASS
    jnz .int_ret
    ; __trunc__ returned non-int — try __index__ only (CPython behavior)
    mov rbx, rax                 ; save __trunc__ result
    mov rdi, [rax + PyObject.ob_type]
    CSTRING rsi, "__index__"
    call dunder_lookup
    V_UNPACK rax, rdx           ; returns a Value
    test edx, edx
    jnz .int_call_trunc_index
    ; No __index__ — raise TypeError with type name
    ; Get type name from __trunc__ result
    mov rax, [rbx + PyObject.ob_type]
    mov rax, [rax + PyTypeObject.tp_name]  ; C string
    push rax                               ; save type name
    mov rdi, rbx
    call obj_decref
    pop rsi                                ; type name
    jmp .int_trunc_type_error_with_name

.int_call_trunc_index:
    ; rax = __index__ func, rbx = __trunc__ result
    mov rcx, [rax + PyObject.ob_type]
    mov rcx, [rcx + PyTypeObject.tp_call]
    test rcx, rcx
    jz .int_trunc_no_index
    SPUSH_PTR rbx                ; args[0] = __trunc__ result (fat arg)
    mov rdi, rax
    mov rsi, rsp
    mov edx, 1
    call rcx
    V_UNPACK rax, rdx           ; tp_call returns a Value
    add rsp, 16
    ; rax = __index__ result, rbx = __trunc__ result (still needs DECREF)
    ; Save __index__ result and DECREF __trunc__ result first
    push rax
    push rdx
    mov rdi, rbx
    call obj_decref              ; DECREF __trunc__ result
    pop rdx
    pop rax
    ; Now check __index__ result
    test edx, edx
    jz .int_dunder_error
    ; Verify it's an int
    cmp edx, TAG_SMALLINT
    je .int_ret                  ; SmallInt — OK
    cmp edx, TAG_PTR
    jne .int_index_nonint_error  ; non-pointer (Float/None/Bool) — not int
    mov rcx, [rax + PyObject.ob_type]
    lea r8, [rel int_type]
    cmp rcx, r8
    je .int_ret
    lea r8, [rel bool_type]
    cmp rcx, r8
    je .int_convert_bool_result
    mov r8, [rcx + PyTypeObject.tp_flags]
    test r8, TYPE_FLAG_INT_SUBCLASS
    jnz .int_ret
    ; __index__ returned non-int (heap object)
    mov rdi, rax
    call obj_decref
.int_index_nonint_error:
    RAISE exc_TypeError_type, "__index__ returned non-int"

.int_trunc_no_index:
    mov rax, [rbx + PyObject.ob_type]
    mov rax, [rax + PyTypeObject.tp_name]
    push rax                               ; save type name
    mov rdi, rbx
    call obj_decref
    pop rsi                                ; type name
    jmp .int_trunc_type_error_with_name

.int_trunc_type_error_with_name:
    ; rsi = type name (C string ptr)
    ; Build: "__trunc__ returned non-Integral (type <name>)"
    ; Use str_from_cstr + str_concat approach
    push rsi                               ; save type name
    CSTRING rdi, "__trunc__ returned non-Integral (type "
    call str_from_cstr_heap
    push rax                               ; save prefix str

    ; Create type name str
    mov rdi, [rsp + 8]                     ; type name C string
    call str_from_cstr_heap
    push rax                               ; save name str

    ; Create suffix str
    CSTRING rdi, ")"
    call str_from_cstr_heap
    push rax                               ; save suffix str

    ; Concat: prefix + name
    extern str_concat
    mov rdi, [rsp + 16]                    ; prefix str
    mov rsi, [rsp + 8]                     ; name str
    mov ecx, TAG_PTR                       ; right_tag (heap str)
    call str_concat
    push rax                               ; save partial

    ; Concat: partial + suffix
    mov rdi, rax                           ; partial
    mov rsi, [rsp + 8]                     ; suffix str
    mov ecx, TAG_PTR                       ; right_tag (heap str)
    call str_concat
    mov rbx, rax                           ; rbx = full message str

    ; DECREF intermediate strings (5 items on stack: partial, suffix, name, prefix, type_name_cstr)
    pop rdi                                ; partial
    call obj_decref
    pop rdi                                ; suffix
    call obj_decref
    pop rdi                                ; name
    call obj_decref
    pop rdi                                ; prefix
    call obj_decref
    add rsp, 8                             ; pop type name C string

    ; Raise TypeError with the message
    lea rdi, [rel exc_TypeError_type]
    mov rsi, rbx
    mov edx, TAG_PTR
    call exc_new
    push rax                               ; save exc
    mov rdi, rbx
    call obj_decref                        ; DECREF msg str
    pop rax                                ; exc obj

    ; Store exception and jump to unwind
    mov [rel current_exception], rax
    jmp eval_exception_unwind

.int_trunc_nonint_error:
    ; __trunc__ returned non-pointer non-int (TAG_FLOAT, TAG_NONE, etc)
    RAISE exc_TypeError_type, "__trunc__ returned non-Integral"

.int_dunder_returned_float:
    ; __int__/__trunc__ returned TAG_FLOAT — TypeError (non-int return)
    RAISE exc_TypeError_type, "__int__ returned non-int (type float)"

.int_convert_bool_result:
    ; rax = bool_true or bool_false, convert to SmallInt
    lea rcx, [rel bool_true]
    cmp rax, rcx
    je .int_bool_result_true
    xor eax, eax
    RET_TAG_SMALLINT
    jmp .int_ret
.int_bool_result_true:
    mov rax, 1
    RET_TAG_SMALLINT
    jmp .int_ret

.int_dunder_error:
    ; Dunder method raised an exception — propagate it (return NULL)
    xor eax, eax
    jmp .int_ret

.int_type_error:
    ; CPython's wording names the type, and this one ended on "not " with
    ; nothing after it -- the one word a reader needs.
    mov rsi, rbx                ; the argument, as a Value
    CSTRING rdi, `int() argument must be a string, a bytes-like object or a real number, not '\x01'`
    extern raise_type_error_with_name
    jmp raise_type_error_with_name

.int_error:
    RAISE exc_TypeError_type, "int() takes at most 2 arguments"

; ------- int(x, base) -------
.int_two_args:
    mov [rbp - BI_ARGS], rdi       ; save args pointer
    ; Get base from args[1]
    mov rax, [rdi + 8]            ; args[1]
    V_UNPACK rax, rdx
    cmp edx, TAG_SMALLINT
    je .int_base_smallint
    ; Reject the non-pointer immediates (float, and NULL)
    cmp edx, TAG_PTR
    jne .int_base_type_error
    ; base is a heap object — check if it's an int or has __index__
    ; args already saved in [rbp - BI_ARGS]
    mov rcx, [rax + PyObject.ob_type]
    lea rdx, [rel int_type]
    cmp rcx, rdx
    je .int_base_heap_int
    lea rdx, [rel bool_type]
    cmp rcx, rdx
    je .int_base_heap_int
    mov rdx, [rcx + PyTypeObject.tp_flags]
    test rdx, TYPE_FLAG_INT_SUBCLASS
    jnz .int_base_heap_int
    ; Try __index__ protocol on base
    SPUSH_PTR rax                 ; save base obj as fat arg
    mov rdi, rcx                  ; type
    CSTRING rsi, "__index__"
    call dunder_lookup
    V_UNPACK rax, rdx           ; returns a Value
    test edx, edx
    jz .int_base_no_index
    ; Call __index__(base_obj)
    mov rcx, [rax + PyObject.ob_type]
    mov rcx, [rcx + PyTypeObject.tp_call]
    test rcx, rcx
    jz .int_base_no_index
    mov rdi, rax
    lea rsi, [rsp]               ; args[0] = base_obj (fat arg on stack)
    mov edx, 1
    call rcx
    V_UNPACK rax, rdx           ; tp_call returns a Value
    add rsp, 16                  ; pop fat arg
    ; rax = __index__ result, should be int
    test edx, edx
    jz .int_dunder_error         ; __index__ raised exception
    cmp edx, TAG_SMALLINT
    je .int_base_si_from_index
    ; heap int — check if it fits in i64 first
    push rax
    INT_NEED_MPZ rax
    lea rdi, [rax + PyIntObject.mpz]
    call __gmpz_fits_slong_p wrt ..plt
    test eax, eax
    pop rdi                      ; rdi = __index__ result
    jz .int_base_range_error     ; doesn't fit → definitely out of 2-36 range
    mov edx, TAG_PTR             ; heap int
    call int_to_i64
    jmp .int_have_base
.int_base_si_from_index:
    jmp .int_have_base
.int_base_no_index:
    add rsp, 16                  ; pop fat arg
    jmp .int_base_type_error
.int_base_heap_int:
    ; rax = heap int object (GMP). Check if it fits in i64.
    push rax
    INT_NEED_MPZ rax
    lea rdi, [rax + PyIntObject.mpz]
    call __gmpz_fits_slong_p wrt ..plt
    test eax, eax
    pop rdi                      ; rdi = heap int obj
    jz .int_base_range_error     ; doesn't fit → out of 2-36 range
    mov edx, TAG_PTR             ; heap int
    call int_to_i64
    jmp .int_have_base
.int_base_smallint:
.int_have_base:
    ; rax = base value
    mov [rbp - BI_NARGS], rax      ; save base
    ; Validate base: must be 0 or 2..36
    test rax, rax
    jz .int_base_ok
    cmp rax, 2
    jl .int_base_range_error
    cmp rax, 36
    jg .int_base_range_error
.int_base_ok:
    ; Save base for error reporting
    mov rax, [rbp - BI_NARGS]
    mov [rbp - BI_BASE], rax
    ; Get x from args[0] — must be string or bytes
    mov rdi, [rbp - BI_ARGS]
    mov rbx, [rdi]                 ; args[0] payload
    mov [rbp - BI_OBJ], rbx       ; save original obj for error msg
    V_TEST_PTR_M [rdi], r11      ; args[0] a pointer?
    ja .int_base_type_error_str
    mov rax, [rbx + PyObject.ob_type]
    lea rcx, [rel str_type]
    cmp rax, rcx
    je .int_base_from_str
    mov rdx, [rax + PyTypeObject.tp_flags]
    test rdx, TYPE_FLAG_STR_SUBCLASS
    jnz .int_base_from_str
    ; Check bytes, bytearray, or subclasses (walk base chain)
    mov rcx, rax
.int_base_check_bytes_chain:
    cmp qword [rbp - BI_ORIGIN], 0
    jne .int_base_chain_have_origin
    mov [rbp - BI_ORIGIN], rcx
.int_base_chain_have_origin:
    lea rdx, [rel bytes_type]
    cmp rcx, rdx
    je .int_base_from_bytes
    lea rdx, [rel bytearray_type]
    cmp rcx, rdx
    je .int_base_from_bytearray
    MRO_NEXT rcx, [rbp - BI_ORIGIN]
    test rcx, rcx
    jnz .int_base_check_bytes_chain
    jmp .int_base_type_error_str

.int_base_from_str:
    ; Check for embedded NUL bytes
    lea rdi, [rbx + PyStrObject.data]
    call strlen wrt ..plt
    cmp rax, [rbx + PyStrObject.ob_size]
    jne .int_base_parse_error      ; embedded NUL → reject
    ; Parse string with given base
    lea rdi, [rbx + PyStrObject.data]
    mov rsi, [rbp - BI_NARGS]      ; base
    call int_from_cstr_base
    test edx, edx            ; check tag (not payload — SmallInt 0 is valid)
    jz .int_base_parse_error
    jmp .int_ret

.int_base_from_bytearray:
    ; A bytearray keeps its data OUT OF LINE, so it cannot be read through
    ; the bytes offsets -- which is what this did while the two layouts
    ; happened to match.  rsi and rcx are set here, then the shared body
    ; below copies from them.
    mov rcx, [rbx + PyByteArrayObject.ob_size]
    mov rsi, [rbx + PyByteArrayObject.ob_bytes]
    test rsi, rsi
    jnz .int_base_bytes_have
    lea rsi, [rel int_base_empty]
    jmp .int_base_bytes_have

.int_base_from_bytes:
    ; Parse bytes with given base — make null-terminated copy
    mov rcx, [rbx + PyBytesObject.ob_size]
    lea rsi, [rbx + PyBytesObject.data]

.int_base_bytes_have:
    mov [rbp - BI_LEN], rcx
    push rsi
    lea rdi, [rcx + 8]
    push rcx
    call ap_malloc
    pop rcx
    pop rsi
    push rax
    mov rdi, rax
    mov rdx, rcx
    call ap_memcpy
    pop rdi
    push rdi
    mov rcx, [rbp - BI_LEN]
    mov qword [rdi + rcx], 0
    ; Check for embedded NUL
    call strlen wrt ..plt
    cmp rax, [rbp - BI_LEN]
    jne .int_base_bytes_nul_error
    mov rdi, [rsp]                 ; buffer
    mov rsi, [rbp - BI_NARGS]      ; base
    call int_from_cstr_base
    mov rbx, rax                   ; save result payload
    push rdx                       ; save result tag
    mov rdi, [rsp + 8]            ; buffer ptr (under tag on stack)
    call ap_free
    pop rdx                        ; restore result tag
    add rsp, 8                    ; pop buffer ptr
    mov rax, rbx
    test edx, edx                 ; check tag (not payload — SmallInt 0 is valid)
    jz .int_base_parse_error
    jmp .int_ret

.int_base_bytes_nul_error:
    pop rdi                   ; free temp buffer
    call ap_free
    jmp .int_base_parse_error

.int_base_type_error:
    RAISE exc_TypeError_type, "int() second arg must be an integer"

.int_base_type_error_str:
    RAISE exc_TypeError_type, "int() can't convert non-string with explicit base"

.int_base_range_error:
    RAISE exc_ValueError_type, "int() base must be >= 2 and <= 36, or 0"

.int_base_parse_error:
    ; Restore rbx from BI_OBJ (may have been clobbered in bytes path)
    mov rbx, [rbp - BI_OBJ]
    jmp .int_invalid_literal_error

.int_invalid_literal_error:
    ; Build "invalid literal for int() with base N: <repr>"
    ; [rbp - BI_OBJ] = original obj, [rbp - BI_BASE] = base
    ;
    ; Strategy: build "...base N: " as C string in stack buffer, then
    ; create ONE PyStr, concat with repr, minimal DECREF.
    ;
    ; Stack layout (sub rsp, 72, aligned to 16):
    ;   [rsp+0..47]  = C string buffer (48 bytes)
    ;   [rsp+48]     = saved prefix_str
    ;   [rsp+56]     = saved repr_str
    ;   [rsp+64]     = saved full_msg / exc
    sub rsp, 72                         ; rsp ≡ 0 (mod 16) — aligned

    ; --- Build "invalid literal for int() with base N: " as C string ---
    mov rdi, rsp
    CSTRING rsi, "invalid literal for int() with base "
    mov edx, 36
    call ap_memcpy
    ; rdi = rsp + 36 (past prefix, ap_memcpy advances rdi via rep movsb)

    ; Append base as decimal (0-36)
    mov rax, [rbp - BI_BASE]
    cmp rax, 10
    jb .ile_one_digit
    ; Two digits
    xor edx, edx
    mov ecx, 10
    div ecx
    add al, '0'
    mov [rdi], al
    inc rdi
    add dl, '0'
    mov [rdi], dl
    inc rdi
    jmp .ile_base_done
.ile_one_digit:
    add al, '0'
    mov [rdi], al
    inc rdi
.ile_base_done:
    mov byte [rdi], ':'
    mov byte [rdi+1], ' '
    mov byte [rdi+2], 0

    ; Create PyStr from buffer (heap — passed to str_concat, DECREFed)
    mov rdi, rsp
    call str_from_cstr_heap
    mov [rsp + 48], rax

    ; Get repr of original object (always a heap ptr)
    mov rdi, [rbp - BI_OBJ]
    call obj_repr
    test rax, rax
    jnz .ile_have_repr
    CSTRING rdi, "???"
    call str_from_cstr_heap
    jmp .ile_repr_ready
.ile_have_repr:
    ; rax = repr string (heap ptr)
.ile_repr_ready:
    mov [rsp + 56], rax

    ; Concat prefix_str + repr_str → full message
    mov rdi, [rsp + 48]
    mov rsi, [rsp + 56]
    mov ecx, TAG_PTR            ; right_tag (heap str)
    call str_concat
    mov [rsp + 64], rax

    ; DECREF prefix_str and repr_str
    mov rdi, [rsp + 48]
    call obj_decref
    mov rdi, [rsp + 56]
    call obj_decref

    ; Create ValueError
    lea rdi, [rel exc_ValueError_type]
    mov rsi, [rsp + 64]
    mov edx, TAG_PTR
    call exc_new
    mov rbx, rax                        ; rbx = exc (callee-saved)

    ; DECREF full message
    mov rdi, [rsp + 64]
    call obj_decref

    ; DECREF previous exception if any
    mov rax, [rel current_exception]
    test rax, rax
    jz .int_ile_no_prev
    mov rdi, rax
    call obj_decref
.int_ile_no_prev:
    mov [rel current_exception], rbx
    add rsp, 72
    jmp eval_exception_unwind

.int_ret:
    ; Common epilogue: rax = payload, edx = tag (set by callee)
    ; rbx was pushed after sub rsp, BI_FRAME, so it's at rbp - BI_FRAME - 8
    lea rsp, [rbp - BI_FRAME - 8]
    pop rbx
    leave
    ret

END_FUNC builtin_int_fn

;; ============================================================================
;; 4. builtin_ord(args, nargs) - ord(c)
;; ============================================================================
DEF_FUNC builtin_ord

    cmp rsi, 1
    jne .ord_nargs_error

    V_TEST_PTR_M [rdi], r11      ; args[0] a pointer?
    ja .ord_type_error

    mov rdi, [rdi]                 ; args[0] payload

    mov rax, [rdi + PyObject.ob_type]
    lea rcx, [rel str_type]
    cmp rax, rcx
    jne .ord_type_error

    ; A string is stored as UTF-8, so one character can be up to four bytes.
    ; Requiring ob_size == 1 made ord(chr(233)) a TypeError.
    mov rcx, [rdi + PyStrObject.ob_size]
    test rcx, rcx
    jz .ord_len_error
    movzx eax, byte [rdi + PyStrObject.data]
    test al, 0x80
    jz .ord_ascii
    ; Multi-byte: decode and check it is the whole string.
    mov r8d, eax
    and r8d, 0xf8
    cmp r8d, 0xf0
    je .ord_four
    mov r8d, eax
    and r8d, 0xf0
    cmp r8d, 0xe0
    je .ord_three
    mov r8d, eax
    and r8d, 0xe0
    cmp r8d, 0xc0
    jne .ord_len_error
    cmp rcx, 2
    jne .ord_len_error
    and eax, 0x1f
    shl eax, 6
    movzx edx, byte [rdi + PyStrObject.data + 1]
    and edx, 0x3f
    or eax, edx
    jmp .ord_done
.ord_three:
    cmp rcx, 3
    jne .ord_len_error
    and eax, 0x0f
    shl eax, 12
    movzx edx, byte [rdi + PyStrObject.data + 1]
    and edx, 0x3f
    shl edx, 6
    or eax, edx
    movzx edx, byte [rdi + PyStrObject.data + 2]
    and edx, 0x3f
    or eax, edx
    jmp .ord_done
.ord_four:
    cmp rcx, 4
    jne .ord_len_error
    and eax, 0x07
    shl eax, 18
    movzx edx, byte [rdi + PyStrObject.data + 1]
    and edx, 0x3f
    shl edx, 12
    or eax, edx
    movzx edx, byte [rdi + PyStrObject.data + 2]
    and edx, 0x3f
    shl edx, 6
    or eax, edx
    movzx edx, byte [rdi + PyStrObject.data + 3]
    and edx, 0x3f
    or eax, edx
    jmp .ord_done
.ord_ascii:
    cmp rcx, 1
    jne .ord_len_error
.ord_done:
    RET_TAG_SMALLINT
    leave
    V_PACK rax, rdx             ; builtins return one Value
    ret

.ord_type_error:
    RAISE exc_TypeError_type, "ord() expected string of length 1"

.ord_len_error:
    RAISE exc_TypeError_type, "ord() expected a character"

.ord_nargs_error:
    RAISE exc_TypeError_type, "ord() takes exactly one argument"
END_FUNC builtin_ord

;; ============================================================================
;; 5. builtin_chr(args, nargs) - chr(n)
;; ============================================================================
BC_BUF    equ 16            ; up to four UTF-8 bytes, then a NUL
DEF_FUNC builtin_chr, 16

    cmp rsi, 1
    jne .chr_nargs_error

    mov rdi, [rdi]            ; args[0]

    V_UNPACK rdi, rdx
    call int_to_i64

    cmp rax, 0
    jl .chr_range_error
    cmp rax, 0x10ffff
    ja .chr_range_error

    ; Single byte (ASCII)
    cmp rax, 0x7f
    ja .chr_utf8_encode

    ; str_new, not str_from_cstr: chr(0) is a one-character string holding a
    ; NUL, and measuring it with strlen made it empty.
    mov byte [rbp - BC_BUF], al
    mov byte [rbp - BC_BUF + 1], 0
    lea rdi, [rbp - BC_BUF]
    mov esi, 1
    call str_new
    leave
    V_PACK rax, rdx             ; builtins return one Value
    ret

.chr_utf8_encode:
    cmp rax, 0x7ff
    ja .chr_3byte

    ; 2-byte: 110xxxxx 10xxxxxx
    mov rcx, rax
    shr rcx, 6
    or cl, 0xc0
    mov byte [rbp - BC_BUF], cl
    mov rcx, rax
    and cl, 0x3f
    or cl, 0x80
    mov byte [rbp - BC_BUF + 1], cl
    mov byte [rbp - BC_BUF + 2], 0
    lea rdi, [rbp - BC_BUF]
    call str_from_cstr
    leave
    V_PACK rax, rdx             ; builtins return one Value
    ret

.chr_3byte:
    cmp rax, 0xffff
    ja .chr_4byte

    ; 3-byte: 1110xxxx 10xxxxxx 10xxxxxx
    mov rcx, rax
    shr rcx, 12
    or cl, 0xe0
    mov byte [rbp - BC_BUF], cl
    mov rcx, rax
    shr rcx, 6
    and cl, 0x3f
    or cl, 0x80
    mov byte [rbp - BC_BUF + 1], cl
    mov rcx, rax
    and cl, 0x3f
    or cl, 0x80
    mov byte [rbp - BC_BUF + 2], cl
    mov byte [rbp - BC_BUF + 3], 0
    lea rdi, [rbp - BC_BUF]
    call str_from_cstr
    leave
    V_PACK rax, rdx             ; builtins return one Value
    ret

.chr_4byte:
    ; 4-byte: 11110xxx 10xxxxxx 10xxxxxx 10xxxxxx
    mov rcx, rax
    shr rcx, 18
    or cl, 0xf0
    mov byte [rbp - BC_BUF], cl
    mov rcx, rax
    shr rcx, 12
    and cl, 0x3f
    or cl, 0x80
    mov byte [rbp - BC_BUF + 1], cl
    mov rcx, rax
    shr rcx, 6
    and cl, 0x3f
    or cl, 0x80
    mov byte [rbp - BC_BUF + 2], cl
    mov rcx, rax
    and cl, 0x3f
    or cl, 0x80
    mov byte [rbp - BC_BUF + 3], cl
    mov byte [rbp - BC_BUF + 4], 0
    lea rdi, [rbp - BC_BUF]
    call str_from_cstr
    leave
    V_PACK rax, rdx             ; builtins return one Value
    ret

.chr_range_error:
    RAISE exc_ValueError_type, "chr() arg not in range(0x110000)"

.chr_nargs_error:
    RAISE exc_TypeError_type, "chr() takes exactly one argument"
END_FUNC builtin_chr

;; ============================================================================
;; 6. builtin_hex(args, nargs) - hex(n)
;; ============================================================================
HEXB_VAL   equ 8
HEXB_STR   equ 16
HEXB_OUT   equ 24
HEXB_FRAME equ 32           ; + 2 pushes = 48
DEF_FUNC builtin_hex, HEXB_FRAME
    push rbx
    push r12
    cmp rsi, 1
    jne .hex_nargs_error

    ; obj_as_index truncates a value too wide for int64, so builtin_hex(2**70)
    ; came out as "0x0".  It is still what validates the argument and
    ; honours __index__; int_base_str renders any width through GMP.
    mov rdi, [rdi]
    mov [rbp - HEXB_VAL], rdi
    V_UNPACK rdi, rdx
    call int_is_integer
    test eax, eax
    jnz .hex_have_value
    mov rdi, [rbp - HEXB_VAL]
    call obj_as_index
    mov rdx, TAG_SMALLINT
    V_PACK rax, rdx
    mov [rbp - HEXB_VAL], rax
.hex_have_value:
    mov rdi, [rbp - HEXB_VAL]
    mov esi, 16
    xor edx, edx
    call int_base_str
    mov [rbp - HEXB_STR], rax

    mov rbx, rax
    cmp byte [rbx], '-'
    jne .hex_positive
    inc rbx
.hex_positive:
    xor ecx, ecx
.hex_len:
    cmp byte [rbx + rcx], 0
    je .hex_have_len
    inc rcx
    jmp .hex_len
.hex_have_len:
    mov r12, rcx
    lea rdi, [rcx + 8]
    call ap_malloc
    mov [rbp - HEXB_OUT], rax
    xor edx, edx
    mov r8, [rbp - HEXB_STR]
    cmp byte [r8], '-'
    jne .hex_no_sign
    mov byte [rax], '-'
    mov edx, 1
.hex_no_sign:
    mov byte [rax + rdx], '0'
    mov byte [rax + rdx + 1], 'x'
    add rdx, 2
    xor r9d, r9d
.hex_copy:
    cmp r9, r12
    jge .hex_copied
    mov r10b, [rbx + r9]
    mov [rax + rdx], r10b
    inc rdx
    inc r9
    jmp .hex_copy
.hex_copied:
    mov byte [rax + rdx], 0

    mov rdi, [rbp - HEXB_STR]
    call ap_free
    mov rdi, [rbp - HEXB_OUT]
    call str_from_cstr
    mov rbx, rax
    mov r12, rdx
    mov rdi, [rbp - HEXB_OUT]
    call ap_free
    mov rax, rbx
    mov rdx, r12
    pop r12
    pop rbx
    leave
    V_PACK rax, rdx             ; builtins return one Value
    ret

.hex_nargs_error:
    RAISE exc_TypeError_type, "hex() takes exactly one argument"
END_FUNC builtin_hex

; builtin_eval_fn used to live here: a stub that parsed a single integer
; literal and raised ValueError for anything else.  It is now a real
; evaluator in compiler/evalexec.asm, backed by the source compiler.

;; ============================================================================
;; builtin_round_fn(args, nargs) - round(number[, ndigits])
;;
;; Every arm of this used to round with cvtsd2si, which answers the integer
;; INDEFINITE value -- 0x8000000000000000 -- for anything outside int64 and
;; reports nothing about it.  round(1e300), round(inf) and round(nan) were
;; all -9223372036854775808.  float_int had already learned this lesson for
;; int(); round() had not, and the two-argument arm had three more of its
;; own: 10**ndigits was an int64 that wrapped at ndigits >= 19 and looped 400
;; times for round(x, 400), x * 10**n overflowed to infinity for a large x,
;; and the cvtsd2si/cvtsi2sd round trip lost the sign of -0.0.
;;
;; The float work is float_round_ndigits (src/pyo/float.asm), which rounds
;; the decimal representation the way CPython's double_round does rather than
;; the scaled binary value.  The int work is GMP's, so round(10**30, -5) is
;; an answer rather than "type cannot be rounded".
;; ============================================================================

extern float_round_ndigits
extern int_fits_i64
extern int_shrink
extern __gmpz_init
extern __gmpz_set_si
extern __gmpz_ui_pow_ui
extern __gmpz_tdiv_qr
extern __gmpz_mul
extern __gmpz_add
extern __gmpz_sub
extern __gmpz_cmp
extern __gmpz_cmpabs
extern __gmpz_mul_2exp
extern __gmpz_cmp_si
extern __gmpz_set
extern __gmpz_powm
extern __gmpz_invert
extern __gmpz_neg
extern __gmpz_clear
extern __gmpz_tdiv_q_ui

global builtin_round_fn
RND_X      equ 8              ; x payload
RND_XTAG   equ 16             ; x tag
RND_ND     equ 24             ; ndigits as int64
RND_SAVE   equ 32             ; scratch across a call
RND_FRAME  equ 56            ; + 1 push = 64, 16-aligned
DEF_FUNC builtin_round_fn, RND_FRAME
    push rbx

    cmp rsi, 1
    je .rnd_one_arg
    cmp rsi, 2
    je .rnd_two_arg
    jmp .rnd_error

.rnd_one_arg:
    ; round(x) -> int.  int_unwrap flattens bool, compact heap ints and int
    ; subclasses to (value, TAG_SMALLINT); a genuinely large int stays a
    ; pointer and is returned as itself.
    extern int_unwrap
    mov rdi, [rdi]              ; args[0]
    V_UNPACK rdi, rdx
    call int_unwrap
    mov [rbp - RND_X], rdi
    mov [rbp - RND_XTAG], rdx

    cmp edx, TAG_SMALLINT
    je .rnd_int_identity

    mov rdi, [rbp - RND_X]
    mov esi, [rbp - RND_XTAG]
    call round_self_double      ; -> xmm0, eax = 1 when it was a float
    test eax, eax
    jz .rnd_one_not_float

    ; Round to nearest, ties to even -- what cvtsd2si was providing -- and
    ; hand the result to float_int, which knows about NaN, the infinities and
    ; the values that need GMP.
    roundsd xmm0, xmm0, 0
    movq rdi, xmm0
    V_FROM_F64 rdi, rax
    extern float_int
    call float_int
    pop rbx
    leave
    ret

.rnd_one_not_float:
    ; A heap int is its own answer.  It used to go through int_to_i64, which
    ; truncates: round(10**30) came back as garbage.
    mov rdi, [rbp - RND_X]
    cmp qword [rbp - RND_XTAG], TAG_PTR
    jne .rnd_one_dunder
    lea rax, [rel int_type]
    cmp [rdi + PyObject.ob_type], rax
    jne .rnd_one_dunder
    INCREF rdi
    mov rax, rdi
    mov edx, TAG_PTR
    pop rbx
    leave
    V_PACK rax, rdx
    ret

.rnd_int_identity:
    mov rax, [rbp - RND_X]
    mov edx, TAG_SMALLINT
    pop rbx
    leave
    V_PACK rax, rdx
    ret

.rnd_one_dunder:
    ; Anything else is asked for __round__, which is how round() works on a
    ; Decimal, a Fraction or any class that defines it.
    mov rdi, [rbp - RND_X]
    mov edx, [rbp - RND_XTAG]
    V_PACK rdi, rdx
    CSTRING rsi, "__round__"
    extern dunder_call_1
    call dunder_call_1
    test edx, edx
    jz .rnd_type_error          ; no __round__, or it raised
    pop rbx
    leave
    V_PACK rax, rdx
    ret

.rnd_two_arg:
    extern int_unwrap
    mov rbx, rdi                ; args array
    mov rdi, [rbx]
    V_UNPACK rdi, rdx
    call int_unwrap
    mov [rbp - RND_X], rdi
    mov [rbp - RND_XTAG], rdx

    mov rdi, [rbx + 8]
    V_UNPACK rdi, rdx           ; args[1]
    call int_unwrap
    mov rbx, rdi
    mov r8d, edx

    ; ndigits has to be an int, and one that fits: CPython clamps a huge one
    ; to the same "past every bound" answer the small ones reach.
    cmp r8d, TAG_SMALLINT
    je .rnd_nd_small
    cmp r8d, TAG_PTR
    jne .rnd_type_error
    lea rax, [rel int_type]
    cmp [rbx + PyObject.ob_type], rax
    jne .rnd_type_error
    mov rdi, rbx
    mov edx, TAG_PTR
    call int_fits_i64
    test eax, eax
    jz .rnd_nd_huge
    mov rdi, rbx
    mov edx, TAG_PTR
    call int_to_i64
    mov [rbp - RND_ND], rax
    jmp .rnd_have_nd
.rnd_nd_huge:
    ; Larger than an int64 either way.  Its sign is all that is left of it.
    INT_NEED_MPZ rbx
    lea rdi, [rbx + PyIntObject.mpz]
    xor esi, esi
    call __gmpz_cmp_si wrt ..plt
    test eax, eax
    js .rnd_nd_min
    mov qword [rbp - RND_ND], 1000
    jmp .rnd_have_nd
.rnd_nd_min:
    mov qword [rbp - RND_ND], -1000
    jmp .rnd_have_nd
.rnd_nd_small:
    mov [rbp - RND_ND], rbx

.rnd_have_nd:
    cmp qword [rbp - RND_XTAG], TAG_SMALLINT
    je .rnd_two_int

    mov rdi, [rbp - RND_X]
    mov esi, [rbp - RND_XTAG]
    call round_self_double
    test eax, eax
    jz .rnd_two_not_float

    ; float_round_ndigits takes the count in edi, saturated to a range where
    ; it is the sign that decides.
    mov rax, [rbp - RND_ND]
    cmp rax, 2000
    jle .rnd_nd_lo
    mov eax, 2000
.rnd_nd_lo:
    cmp rax, -2000
    jge .rnd_nd_ok
    mov eax, -2000
.rnd_nd_ok:
    mov edi, eax
    call float_round_ndigits
    movq rax, xmm0
    V_FROM_F64 rax, rdx
    pop rbx
    leave
    ret

.rnd_two_not_float:
    mov rdi, [rbp - RND_X]
    cmp qword [rbp - RND_XTAG], TAG_PTR
    jne .rnd_two_dunder
    lea rax, [rel int_type]
    cmp [rdi + PyObject.ob_type], rax
    je .rnd_two_bigint
.rnd_two_dunder:
    ; round(x, n) on anything else is __round__ with two arguments.  It used
    ; to be a flat TypeError, so round(Decimal(1), 2) could not work.
    mov rdi, [rbp - RND_X]
    cmp qword [rbp - RND_XTAG], TAG_PTR
    jne .rnd_type_error         ; an immediate has no __round__ of its own
    mov rsi, [rbp - RND_ND]     ; dunder_call_2 takes the (payload, tag) pair
    CSTRING rdx, "__round__"
    mov ecx, TAG_SMALLINT
    extern dunder_call_2
    call dunder_call_2
    V_UNPACK rax, rdx
    test edx, edx
    jz .rnd_type_error
    pop rbx
    leave
    V_PACK rax, rdx
    ret

.rnd_two_int:
    ; round(int, n).  A non-negative n leaves it alone; a negative one rounds
    ; to the nearest multiple of 10**|n|, ties to even.  It used to do that
    ; through a double, which is wrong the moment the int does not fit one.
    mov rax, [rbp - RND_ND]
    test rax, rax
    jns .rnd_int_identity
    mov rdi, [rbp - RND_X]
    extern smallint_to_pyint
    call smallint_to_pyint
    mov [rbp - RND_SAVE], rax
    mov rdi, rax
    mov rsi, [rbp - RND_ND]
    call int_round_to_power10
    mov rbx, rax
    mov rdi, [rbp - RND_SAVE]
    extern int_dealloc
    call int_dealloc
    mov rax, rbx
    mov edx, TAG_PTR
    pop rbx
    leave
    V_PACK rax, rdx
    ret

.rnd_two_bigint:
    mov rdi, [rbp - RND_X]
    mov rax, [rbp - RND_ND]
    test rax, rax
    jns .rnd_bigint_identity
    mov rsi, rax
    call int_round_to_power10
    mov edx, TAG_PTR
    pop rbx
    leave
    V_PACK rax, rdx
    ret
.rnd_bigint_identity:
    INCREF rdi
    mov rax, rdi
    mov edx, TAG_PTR
    pop rbx
    leave
    V_PACK rax, rdx
    ret

.rnd_error:
    RAISE exc_TypeError_type, "round() takes 1 or 2 arguments"

.rnd_type_error:
    RAISE exc_TypeError_type, "type cannot be rounded"
END_FUNC builtin_round_fn

;; ============================================================================
;; round_self_double(rdi = payload, esi = tag)
;;   -> xmm0 = the double, eax = 1 when it is one; eax = 0 otherwise
;;
;; A float arrives three ways -- as an immediate, as a PyFloatObject, and as a
;; subclass instance with the double inline at the base's offset -- and both
;; arms of round() have to know all three.
;; ============================================================================
DEF_FUNC_LOCAL round_self_double
    mov rax, rdi
    mov edx, esi
    cmp edx, TAG_FLOAT
    je .rsd_raw
    cmp edx, TAG_PTR
    jne .rsd_no
    lea rcx, [rel float_type]
    cmp [rax + PyObject.ob_type], rcx
    je .rsd_obj
    mov rcx, [rax + PyObject.ob_type]
    test qword [rcx + PyTypeObject.tp_flags], TYPE_FLAG_FLOAT_SUBCLASS
    jz .rsd_no
.rsd_obj:
    movsd xmm0, [rax + PyFloatObject.value]
    mov eax, 1
    leave
    ret
.rsd_raw:
    movq xmm0, rax
    mov eax, 1
    leave
    ret
.rsd_no:
    xor eax, eax
    leave
    ret
END_FUNC round_self_double

;; ============================================================================
;; int_round_to_power10(rdi = a GMP-backed PyIntObject*, rsi = ndigits < 0)
;;   -> rax = a new int, rounded to the nearest multiple of 10**|ndigits|
;;
;; Ties to even, as Python's round() is throughout.  In GMP because the whole
;; point is the ints a double cannot hold: round(10**30, -5) used to be
;; "type cannot be rounded", and round(1234, -2) went through a double.
;; ============================================================================
IRP_SELF  equ 8
IRP_ND    equ 16
IRP_POW   equ 32              ; mpz_t, 16 bytes
IRP_Q     equ 48
IRP_R     equ 64
IRP_T     equ 80
IRP_FRAME equ 96              ; + 1 push + 8 pad = 112, 16-aligned
DEF_FUNC_LOCAL int_round_to_power10, IRP_FRAME
    push rbx
    sub rsp, 8
    mov [rbp - IRP_SELF], rdi
    neg rsi
    mov [rbp - IRP_ND], rsi     ; |ndigits|

    ; 10**|ndigits|.  An exponent past what any reachable int has digits for
    ; still works: the quotient is simply 0 or +-1.
    lea rdi, [rbp - IRP_POW]
    call __gmpz_init wrt ..plt
    lea rdi, [rbp - IRP_Q]
    call __gmpz_init wrt ..plt
    lea rdi, [rbp - IRP_R]
    call __gmpz_init wrt ..plt
    lea rdi, [rbp - IRP_T]
    call __gmpz_init wrt ..plt

    mov rax, [rbp - IRP_ND]
    cmp rax, 100000
    jle .irp_pow_ok
    mov qword [rbp - IRP_ND], 100000    ; far past any int we can hold
.irp_pow_ok:
    lea rdi, [rbp - IRP_POW]
    mov esi, 10
    mov rdx, [rbp - IRP_ND]
    call __gmpz_ui_pow_ui wrt ..plt

    ; q, r = trunc-divide.  GMP's remainder carries the dividend's sign,
    ; which is what makes the half comparison below sign-agnostic.
    mov rdi, [rbp - IRP_SELF]
    INT_NEED_MPZ rdi
    lea rdi, [rbp - IRP_Q]
    lea rsi, [rbp - IRP_R]
    mov rdx, [rbp - IRP_SELF]
    add rdx, PyIntObject.mpz
    lea rcx, [rbp - IRP_POW]
    call __gmpz_tdiv_qr wrt ..plt

    ; 2*|r| against 10**|n|
    lea rdi, [rbp - IRP_T]
    lea rsi, [rbp - IRP_R]
    mov edx, 1
    call __gmpz_mul_2exp wrt ..plt
    lea rdi, [rbp - IRP_T]
    lea rsi, [rbp - IRP_POW]
    call __gmpz_cmpabs wrt ..plt
    test eax, eax
    js .irp_done                ; below half: the quotient stands
    jz .irp_tie

    ; Above half: step the quotient away from zero.
.irp_step:
    lea rdi, [rbp - IRP_R]
    xor esi, esi
    call __gmpz_cmp_si wrt ..plt
    test eax, eax
    js .irp_step_down
    lea rdi, [rbp - IRP_T]
    mov esi, 1
    call __gmpz_set_si wrt ..plt
    jmp .irp_step_add
.irp_step_down:
    lea rdi, [rbp - IRP_T]
    mov rsi, -1                 ; a 64-bit signed long: `mov esi, -1` would
                                ; zero-extend to 4294967295
    call __gmpz_set_si wrt ..plt
.irp_step_add:
    lea rdi, [rbp - IRP_Q]
    lea rsi, [rbp - IRP_Q]
    lea rdx, [rbp - IRP_T]
    call __gmpz_add wrt ..plt
    jmp .irp_done

.irp_tie:
    ; Exactly half: to the even quotient.  q is even when q - 2*(q/2) is 0.
    lea rdi, [rbp - IRP_T]
    lea rsi, [rbp - IRP_Q]
    mov edx, 2
    call __gmpz_tdiv_q_ui wrt ..plt
    test eax, eax               ; the remainder of q / 2
    jz .irp_done                ; q already even
    jmp .irp_step

.irp_done:
    ; result = q * 10**|n|
    lea rdi, [rbp - IRP_Q]
    lea rsi, [rbp - IRP_Q]
    lea rdx, [rbp - IRP_POW]
    call __gmpz_mul wrt ..plt

    mov edi, PyIntObject_size
    extern ap_malloc
    call ap_malloc
    mov rbx, rax
    mov qword [rbx + PyObject.ob_refcnt], 1
    lea rcx, [rel int_type]
    mov [rbx + PyObject.ob_type], rcx
    mov qword [rbx + PyIntObject.compact], 0
    INT_NEED_MPZ rbx
    lea rdi, [rbx + PyIntObject.mpz]
    call __gmpz_init wrt ..plt
    lea rdi, [rbx + PyIntObject.mpz]
    lea rsi, [rbp - IRP_Q]
    extern __gmpz_set
    call __gmpz_set wrt ..plt

    lea rdi, [rbp - IRP_POW]
    call __gmpz_clear wrt ..plt
    lea rdi, [rbp - IRP_Q]
    call __gmpz_clear wrt ..plt
    lea rdi, [rbp - IRP_R]
    call __gmpz_clear wrt ..plt
    lea rdi, [rbp - IRP_T]
    call __gmpz_clear wrt ..plt

    mov rdi, rbx
    call int_shrink
    add rsp, 8
    pop rbx
    leave
    ret
END_FUNC int_round_to_power10

;; ============================================================================
;; builtin_pow_fn(args, nargs) - pow(base, exp[, mod])
;; 2 args: base ** exp
;; 3 args: pow(base, exp, mod) — modular exponentiation
;; ============================================================================
global builtin_pow_fn
POW_BASE equ 8
POW_BTAG equ 16
POW_EXP  equ 24
POW_ETAG equ 32
POW_MOD  equ 40
POW_MTAG equ 48
POW_MB   equ 80             ; four mpz_t, 16 bytes each
POW_MEXP equ 96
POW_MMOD equ 112
POW_MRES equ 128
POW_FRAME equ 136           ; + 3 pushes = 8 + 136 + 24 = 168, 16-aligned
DEF_FUNC builtin_pow_fn, POW_FRAME
    push rbx
    push r12
    push r13

    cmp rsi, 2
    je .pow_two
    cmp rsi, 3
    je .pow_three
    jmp .pow_error

.pow_two:
    ; pow(base, exp) — extract operands and delegate to int_power/float path
    mov rax, [rdi]          ; args[0] = base
    V_UNPACK rax, rcx
    mov rbx, [rdi + 8]      ; args[1] = exp
    V_UNPACK rbx, r8

    ; Both integers?  Delegate to int_power, which handles SmallInt, heap
    ; ints and int subclasses (and GMP overflow) itself.
    extern int_is_integer
    mov [rbp - POW_BTAG], rcx
    mov [rbp - POW_ETAG], r8
    mov r12, rax                ; base payload
    mov r13, rbx                ; exp payload
    mov rdi, rax
    mov edx, ecx
    call int_is_integer
    test eax, eax
    jz .pow_reload_float
    mov rdi, r13
    mov edx, [rbp - POW_ETAG]
    call int_is_integer
    test eax, eax
    jz .pow_reload_float
    mov rax, r12
    mov rbx, r13
    mov ecx, [rbp - POW_BTAG]
    mov r8d, [rbp - POW_ETAG]
    jmp .pow_two_int
.pow_reload_float:
    ; Not two integers.  Everything else goes through obj_binary_op, the same
    ; protocol `base ** exp` uses, so pow() answers whatever the operator
    ; answers -- for float subclasses, for complex, and for any class with
    ; __pow__.  The hand-rolled float path that used to be here tested
    ; `ob_type == float_type` exactly and knew nothing of complex, so
    ; pow(F(2.0), 2) raised where F(2.0) ** 2 worked, and pow(1+2j, 2) raised
    ; where (1+2j) ** 2 worked.
    mov rdi, r12
    mov esi, [rbp - POW_BTAG]
    V_PACK rdi, rsi
    mov rsi, r13
    mov edx, [rbp - POW_ETAG]
    V_PACK rsi, rdx
    mov edx, NB_POWER
    extern obj_binary_op
    call obj_binary_op
    test rax, rax
    jz .pow_propagate
    V_UNPACK rax, rdx
    pop r13
    pop r12
    pop rbx
    leave
    V_PACK rax, rdx             ; builtins return one Value
    ret

.pow_propagate:
    xor eax, eax                ; a NULL Value, with the exception pending
    xor edx, edx
    pop r13
    pop r12
    pop rbx
    leave
    ret
.pow_two_int:

    ; int ** int — call int_power(base, exp, base_tag, exp_tag)
    extern int_power
    mov rdi, rax            ; base payload
    mov rsi, rbx            ; exp payload
    mov edx, ecx            ; base tag (TAG_SMALLINT)
    mov ecx, r8d            ; exp tag (TAG_SMALLINT)
    V_PACK rdi, rdx
    V_PACK rsi, rcx
    call int_power
    V_UNPACK rax, rdx       ; int_power returns a Value
    ; rax = result payload, edx = result tag
    pop r13
    pop r12
    pop rbx
    leave
    V_PACK rax, rdx             ; builtins return one Value
    ret

; The hand-rolled float path that used to live here is gone with it: one
; implementation of `**` rather than two that disagreed about what a float is.

.pow_three:
    ; pow(base, exp, mod) -- modular exponentiation, in GMP.
    ;
    ; It used to be an int64 square-and-multiply, so every operand had to be
    ; an immediate: pow(2, 10**20, 7) and pow(10**30, 3, 10**7) were both
    ; "pow() arguments must be numeric".  It also rejected a negative
    ; exponent outright, where CPython since 3.8 answers the modular
    ; INVERSE, and it tested the exponent's sign before the modulus, so
    ; pow(2, -1, 0) named the wrong argument.
    extern int_unwrap
    mov r13, rdi                ; args array
    mov rdi, [r13]
    V_UNPACK rdi, rdx
    call int_unwrap
    mov [rbp - POW_BASE], rdi
    mov [rbp - POW_BTAG], rdx
    mov rdi, [r13 + 8]
    V_UNPACK rdi, rdx
    call int_unwrap
    mov [rbp - POW_EXP], rdi
    mov [rbp - POW_ETAG], rdx
    mov rdi, [r13 + 16]
    V_UNPACK rdi, rdx
    call int_unwrap
    mov [rbp - POW_MOD], rdi
    mov [rbp - POW_MTAG], rdx

    ; A base that is not an int is asked for its own three-argument __pow__,
    ; which is how pow(x, y, z) works on a class that defines one.  Nothing
    ; consulted it, so every such call was "pow() arguments must be numeric".
    mov rdi, [rbp - POW_BASE]
    mov esi, [rbp - POW_BTAG]
    call pow_is_int_operand
    test eax, eax
    jz .pow_three_dunder

    ; With an int base, all three have to be ints, and CPython words that
    ; refusal specifically.
    mov rdi, [rbp - POW_EXP]
    mov esi, [rbp - POW_ETAG]
    call pow_is_int_operand
    test eax, eax
    jz .pow_not_all_ints
    mov rdi, [rbp - POW_MOD]
    mov esi, [rbp - POW_MTAG]
    call pow_is_int_operand
    test eax, eax
    jz .pow_not_all_ints

    ; Into GMP.  The modulus is checked FIRST: pow(2, -1, 0) is about the
    ; third argument, not the second.
    lea rdi, [rbp - POW_MB]
    call __gmpz_init wrt ..plt
    lea rdi, [rbp - POW_MEXP]
    call __gmpz_init wrt ..plt
    lea rdi, [rbp - POW_MMOD]
    call __gmpz_init wrt ..plt
    lea rdi, [rbp - POW_MRES]
    call __gmpz_init wrt ..plt

    lea rdi, [rbp - POW_MB]
    mov rsi, [rbp - POW_BASE]
    mov edx, [rbp - POW_BTAG]
    call pow_load_mpz
    lea rdi, [rbp - POW_MEXP]
    mov rsi, [rbp - POW_EXP]
    mov edx, [rbp - POW_ETAG]
    call pow_load_mpz
    lea rdi, [rbp - POW_MMOD]
    mov rsi, [rbp - POW_MOD]
    mov edx, [rbp - POW_MTAG]
    call pow_load_mpz

    lea rdi, [rbp - POW_MMOD]
    xor esi, esi
    call __gmpz_cmp_si wrt ..plt
    test eax, eax
    jz .pow_zero_mod

    lea rdi, [rbp - POW_MEXP]
    xor esi, esi
    call __gmpz_cmp_si wrt ..plt
    test eax, eax
    js .pow_mod_inverse

    lea rdi, [rbp - POW_MRES]
    lea rsi, [rbp - POW_MB]
    lea rdx, [rbp - POW_MEXP]
    lea rcx, [rbp - POW_MMOD]
    call __gmpz_powm wrt ..plt
    jmp .pow_mod_sign

.pow_mod_inverse:
    ; A negative exponent: invert the base, then raise the inverse to |exp|.
    lea rdi, [rbp - POW_MRES]
    lea rsi, [rbp - POW_MB]
    lea rdx, [rbp - POW_MMOD]
    call __gmpz_invert wrt ..plt
    test eax, eax
    jz .pow_not_invertible
    lea rdi, [rbp - POW_MEXP]
    lea rsi, [rbp - POW_MEXP]
    call __gmpz_neg wrt ..plt
    lea rdi, [rbp - POW_MRES]
    lea rsi, [rbp - POW_MRES]
    lea rdx, [rbp - POW_MEXP]
    lea rcx, [rbp - POW_MMOD]
    call __gmpz_powm wrt ..plt

.pow_mod_sign:
    ; GMP's powm answers in [0, |mod|); Python's result carries the sign of
    ; the modulus, so a negative modulus needs the representative shifted
    ; down by |mod| unless the result is already zero.
    lea rdi, [rbp - POW_MMOD]
    xor esi, esi
    call __gmpz_cmp_si wrt ..plt
    test eax, eax
    jns .pow_mod_build
    lea rdi, [rbp - POW_MRES]
    xor esi, esi
    call __gmpz_cmp_si wrt ..plt
    test eax, eax
    jz .pow_mod_build
    lea rdi, [rbp - POW_MRES]
    lea rsi, [rbp - POW_MRES]
    lea rdx, [rbp - POW_MMOD]
    call __gmpz_add wrt ..plt

.pow_mod_build:
    mov edi, PyIntObject_size
    call ap_malloc
    mov rbx, rax
    mov qword [rbx + PyObject.ob_refcnt], 1
    lea rcx, [rel int_type]
    mov [rbx + PyObject.ob_type], rcx
    mov qword [rbx + PyIntObject.compact], 0
    INT_NEED_MPZ rbx
    lea rdi, [rbx + PyIntObject.mpz]
    call __gmpz_init wrt ..plt
    lea rdi, [rbx + PyIntObject.mpz]
    lea rsi, [rbp - POW_MRES]
    call __gmpz_set wrt ..plt
    call pow_clear_mpz
    mov rdi, rbx
    call int_shrink
    mov edx, TAG_PTR
    pop r13
    pop r12
    pop rbx
    leave
    V_PACK rax, rdx             ; builtins return one Value
    ret

.pow_three_dunder:
    ; __pow__(self, exp, mod).  No nb_ slot can carry a third operand, so the
    ; name is looked up and called directly.
    mov rdi, [rbp - POW_BASE]
    cmp dword [rbp - POW_BTAG], TAG_PTR
    jne .pow_not_all_ints
    mov rdi, [rdi + PyObject.ob_type]
    CSTRING rsi, "__pow__"
    extern dunder_lookup
    call dunder_lookup
    V_UNPACK rax, rdx
    test edx, edx
    jz .pow_not_all_ints
    test edx, TAG_RC_BIT
    jz .pow_not_all_ints
    mov r12, rax                ; the function

    mov rax, [r12 + PyObject.ob_type]
    mov rax, [rax + PyTypeObject.tp_call]
    test rax, rax
    jz .pow_not_all_ints

    sub rsp, 32                 ; 3 Values, padded to keep rsp 16-aligned
    mov rdi, [rbp - POW_BASE]
    mov esi, [rbp - POW_BTAG]
    V_PACK rdi, rsi
    mov [rsp], rdi
    mov rdi, [rbp - POW_EXP]
    mov esi, [rbp - POW_ETAG]
    V_PACK rdi, rsi
    mov [rsp + 8], rdi
    mov rdi, [rbp - POW_MOD]
    mov esi, [rbp - POW_MTAG]
    V_PACK rdi, rsi
    mov [rsp + 16], rdi
    mov rdi, r12
    mov rsi, rsp
    mov edx, 3
    call rax
    add rsp, 32
    V_UNPACK rax, rdx
    test edx, edx
    jz .pow_propagate
    ; NotImplemented from a by-name call means the type declined the pair.
    extern notimpl_singleton
    lea rcx, [rel notimpl_singleton]
    cmp rax, rcx
    je .pow_not_all_ints
    pop r13
    pop r12
    pop rbx
    leave
    V_PACK rax, rdx             ; builtins return one Value
    ret

.pow_not_all_ints:
    RAISE exc_TypeError_type, "pow() 3rd argument not allowed unless all arguments are integers"

.pow_not_invertible:
    call pow_clear_mpz
    RAISE exc_ValueError_type, "base is not invertible for the given modulus"

.pow_zero_mod:
    call pow_clear_mpz
    RAISE exc_ValueError_type, "pow() 3rd argument cannot be 0"

.pow_error:
    RAISE exc_TypeError_type, "pow() takes 2 or 3 arguments"

.pow_type_error:
    RAISE exc_TypeError_type, "pow() arguments must be numeric"
END_FUNC builtin_pow_fn

;; ============================================================================
;; pow_is_int_operand(rdi = payload, esi = tag) -> eax = 1 when it is an int
;;
;; int_unwrap has already flattened bool, a compact heap int and an int
;; subclass to TAG_SMALLINT; what is left as a pointer is either a GMP-backed
;; int or something that is not an int at all, so the type has to be read.
;; ============================================================================
DEF_FUNC_LOCAL pow_is_int_operand
    cmp esi, TAG_SMALLINT
    je .poi_yes
    cmp esi, TAG_PTR
    jne .poi_no
    lea rax, [rel int_type]
    cmp [rdi + PyObject.ob_type], rax
    jne .poi_no
.poi_yes:
    mov eax, 1
    leave
    ret
.poi_no:
    xor eax, eax
    leave
    ret
END_FUNC pow_is_int_operand

;; ============================================================================
;; pow_load_mpz(rdi = an initialised mpz_t, rsi = payload, edx = tag)
;; Sets the mpz from an int in either representation.
;; ============================================================================
DEF_FUNC_LOCAL pow_load_mpz
    cmp edx, TAG_SMALLINT
    je .plm_small
    mov rax, rsi
    lea rcx, [rel int_type]
    cmp [rax + PyObject.ob_type], rcx
    jne .plm_bad
    push rdi
    sub rsp, 8
    INT_NEED_MPZ rax
    add rsp, 8
    pop rdi
    mov rax, rsi
    add rax, PyIntObject.mpz
    mov rsi, rax
    call __gmpz_set wrt ..plt
    leave
    ret
.plm_small:
    call __gmpz_set_si wrt ..plt
    leave
    ret
.plm_bad:
    RAISE exc_TypeError_type, "pow() arguments must be numeric"
END_FUNC pow_load_mpz

;; ============================================================================
;; pow_clear_mpz() -- releases the four mpz_t in builtin_pow_fn's frame.
;;
;; Reads its CALLER's frame through the saved rbp, because DEF_FUNC gives it
;; one of its own.  Four calls in a row otherwise, at every exit and at both
;; raises.
;; ============================================================================
DEF_FUNC_LOCAL pow_clear_mpz
    push rbx
    sub rsp, 8
    mov rbx, [rbp]              ; the caller's rbp
    lea rdi, [rbx - POW_MB]
    call __gmpz_clear wrt ..plt
    lea rdi, [rbx - POW_MEXP]
    call __gmpz_clear wrt ..plt
    lea rdi, [rbx - POW_MMOD]
    call __gmpz_clear wrt ..plt
    lea rdi, [rbx - POW_MRES]
    call __gmpz_clear wrt ..plt
    add rsp, 8
    pop rbx
    leave
    ret
END_FUNC pow_clear_mpz

;; ============================================================================
;; builtin_bin(args, nargs) - bin(x)
;; Returns binary string representation: '0b...' or '-0b...'
;; ============================================================================
global builtin_bin
BINB_VAL   equ 8
BINB_STR   equ 16
BINB_OUT   equ 24
BINB_FRAME equ 32           ; + 2 pushes = 48
DEF_FUNC builtin_bin, BINB_FRAME
    push rbx
    push r12
    cmp rsi, 1
    jne .bin_nargs_error

    ; obj_as_index truncates a value too wide for int64, so builtin_bin(2**70)
    ; came out as "0b0".  It is still what validates the argument and
    ; honours __index__; int_base_str renders any width through GMP.
    mov rdi, [rdi]
    mov [rbp - BINB_VAL], rdi
    V_UNPACK rdi, rdx
    call int_is_integer
    test eax, eax
    jnz .bin_have_value
    mov rdi, [rbp - BINB_VAL]
    call obj_as_index
    mov rdx, TAG_SMALLINT
    V_PACK rax, rdx
    mov [rbp - BINB_VAL], rax
.bin_have_value:
    mov rdi, [rbp - BINB_VAL]
    mov esi, 2
    xor edx, edx
    call int_base_str
    mov [rbp - BINB_STR], rax

    mov rbx, rax
    cmp byte [rbx], '-'
    jne .bin_positive
    inc rbx
.bin_positive:
    xor ecx, ecx
.bin_len:
    cmp byte [rbx + rcx], 0
    je .bin_have_len
    inc rcx
    jmp .bin_len
.bin_have_len:
    mov r12, rcx
    lea rdi, [rcx + 8]
    call ap_malloc
    mov [rbp - BINB_OUT], rax
    xor edx, edx
    mov r8, [rbp - BINB_STR]
    cmp byte [r8], '-'
    jne .bin_no_sign
    mov byte [rax], '-'
    mov edx, 1
.bin_no_sign:
    mov byte [rax + rdx], '0'
    mov byte [rax + rdx + 1], 'b'
    add rdx, 2
    xor r9d, r9d
.bin_copy:
    cmp r9, r12
    jge .bin_copied
    mov r10b, [rbx + r9]
    mov [rax + rdx], r10b
    inc rdx
    inc r9
    jmp .bin_copy
.bin_copied:
    mov byte [rax + rdx], 0

    mov rdi, [rbp - BINB_STR]
    call ap_free
    mov rdi, [rbp - BINB_OUT]
    call str_from_cstr
    mov rbx, rax
    mov r12, rdx
    mov rdi, [rbp - BINB_OUT]
    call ap_free
    mov rax, rbx
    mov rdx, r12
    pop r12
    pop rbx
    leave
    V_PACK rax, rdx             ; builtins return one Value
    ret

.bin_nargs_error:
    RAISE exc_TypeError_type, "bin() takes exactly one argument"
END_FUNC builtin_bin

;; ============================================================================
;; builtin_oct(args, nargs) - oct(x)
;; Returns octal string representation: '0o...' or '-0o...'
;; ============================================================================
global builtin_oct
OCTB_VAL   equ 8
OCTB_STR   equ 16
OCTB_OUT   equ 24
OCTB_FRAME equ 32           ; + 2 pushes = 48
DEF_FUNC builtin_oct, OCTB_FRAME
    push rbx
    push r12
    cmp rsi, 1
    jne .oct_nargs_error

    ; obj_as_index truncates a value too wide for int64, so builtin_oct(2**70)
    ; came out as "0o0".  It is still what validates the argument and
    ; honours __index__; int_base_str renders any width through GMP.
    mov rdi, [rdi]
    mov [rbp - OCTB_VAL], rdi
    V_UNPACK rdi, rdx
    call int_is_integer
    test eax, eax
    jnz .oct_have_value
    mov rdi, [rbp - OCTB_VAL]
    call obj_as_index
    mov rdx, TAG_SMALLINT
    V_PACK rax, rdx
    mov [rbp - OCTB_VAL], rax
.oct_have_value:
    mov rdi, [rbp - OCTB_VAL]
    mov esi, 8
    xor edx, edx
    call int_base_str
    mov [rbp - OCTB_STR], rax

    mov rbx, rax
    cmp byte [rbx], '-'
    jne .oct_positive
    inc rbx
.oct_positive:
    xor ecx, ecx
.oct_len:
    cmp byte [rbx + rcx], 0
    je .oct_have_len
    inc rcx
    jmp .oct_len
.oct_have_len:
    mov r12, rcx
    lea rdi, [rcx + 8]
    call ap_malloc
    mov [rbp - OCTB_OUT], rax
    xor edx, edx
    mov r8, [rbp - OCTB_STR]
    cmp byte [r8], '-'
    jne .oct_no_sign
    mov byte [rax], '-'
    mov edx, 1
.oct_no_sign:
    mov byte [rax + rdx], '0'
    mov byte [rax + rdx + 1], 'o'
    add rdx, 2
    xor r9d, r9d
.oct_copy:
    cmp r9, r12
    jge .oct_copied
    mov r10b, [rbx + r9]
    mov [rax + rdx], r10b
    inc rdx
    inc r9
    jmp .oct_copy
.oct_copied:
    mov byte [rax + rdx], 0

    mov rdi, [rbp - OCTB_STR]
    call ap_free
    mov rdi, [rbp - OCTB_OUT]
    call str_from_cstr
    mov rbx, rax
    mov r12, rdx
    mov rdi, [rbp - OCTB_OUT]
    call ap_free
    mov rax, rbx
    mov rdx, r12
    pop r12
    pop rbx
    leave
    V_PACK rax, rdx             ; builtins return one Value
    ret

.oct_nargs_error:
    RAISE exc_TypeError_type, "oct() takes exactly one argument"
END_FUNC builtin_oct

; const_one is read by round() and pow(); it lived in a .rodata block shared
; with format()'s name string, which is now in builtins_obj.asm.
section .rodata
int_base_empty: db 0

align 8
const_one: dq 0x3ff0000000000000   ; 1.0 in IEEE 754

section .text

;; ============================================================================
;; complex_type_call(rdi = type, rsi = args, rdx = nargs) -> Value
;; The tp_new thunk.  Keywords are not accepted yet -- CPython takes real= and
;; imag= -- so a pending kw_names is rejected rather than ignored, and cleared
;; on the way out the way int_type_call and bool_type_call do.
;; ============================================================================
CTC_TYPE  equ 8
CTC_VAL   equ 16
CTC_FRAME equ 16            ; + 0 pushes = 16

DEF_FUNC complex_type_call, CTC_FRAME
    cmp qword [rel kw_names_pending], 0
    jne .ctc_kwargs
    mov [rbp - CTC_TYPE], rdi
    mov rdi, rsi
    mov rsi, rdx
    call builtin_complex        ; rax = a complex pointer, edx = TAG_PTR
    lea rcx, [rel complex_type]
    cmp [rbp - CTC_TYPE], rcx
    je .ctc_out                 ; complex() itself
    test edx, edx
    jz .ctc_out                 ; it raised

    ; Re-home the two doubles into an instance of the subclass, then drop the
    ; exact complex builtin_complex built.  Copying is cheaper than teaching
    ; builtin_complex a type argument it would have to thread through six
    ; return paths.
    mov [rbp - CTC_VAL], rax
    mov rdi, [rbp - CTC_TYPE]
    call builtin_sub_alloc
    mov rcx, [rbp - CTC_VAL]
    movsd xmm0, [rcx + PyComplexObject.cval_real]
    movsd xmm1, [rcx + PyComplexObject.cval_imag]
    movsd [rax + PyComplexObject.cval_real], xmm0
    movsd [rax + PyComplexObject.cval_imag], xmm1
    mov [rbp - CTC_TYPE], rax
    mov rdi, [rbp - CTC_VAL]
    call obj_decref
    mov rax, [rbp - CTC_TYPE]
    mov edx, TAG_PTR
.ctc_out:
    leave
    ret
.ctc_kwargs:
    mov qword [rel kw_names_pending], 0
    RAISE exc_TypeError_type, "complex() takes no keyword arguments"
END_FUNC complex_type_call

;; ============================================================================
;; builtin_complex(rdi = args, rsi = nargs) -> Value
;;
;;   complex()            0j
;;   complex(z)           z itself when it is already an exact complex
;;   complex(x)           (float(x)+0j) for an int, bool or float
;;   complex(a, b)        real = ar - bi, imag = ai + br
;;
;; That last formula is the whole of the two-argument case, and it is not
;; (a, b): complex(1j, 1) is 2j and complex(1, 1j) is 0j.
;; ============================================================================
BCX_A     equ 16              ; first argument's parts
BCX_B     equ 32              ; second argument's parts
BCX_ARGS  equ 40
BCX_NARGS equ 48
BCX_RA    equ 56              ; bcx_coerce's verdict for each argument: the
BCX_RB    equ 64              ; COERCED value's shape decides, not the given one
BCX_FRAME equ 64              ; + 0 pushes = 64
;; ============================================================================
;; bcx_coerce(rdi = Value, rsi = &double[2], edx = may call __complex__)
;;   -> eax = 0 not a number, 1 a real number, 2 a complex one
;;
;; complex_to_parts, then the conversion protocols CPython's constructor
;; consults and its arithmetic does not: __complex__, then __float__, then
;; __index__.  They belong here rather than in complex_to_parts because
;; `1j + obj` must go on returning NotImplemented for an object with only a
;; __float__; only complex() itself coerces.
;;
;; __complex__ is offered for the first argument alone -- CPython calls
;; try_complex_special_method on r and never on i, so complex(1, C()) is a
;; TypeError even though complex(C(), 1) is not.
;;
;; Telling 1 from 2 is what the caller needs to finish the two-argument case:
;; it is the coerced value's shape that decides, not the argument's, so
;; complex(C(), 1) where C.__complex__ gives 3+4j is (3+5j).
;;
;; A __complex__ returning something other than a complex raises here rather
;; than answering 0: that is a different error, and CPython reports it as one.
;; ============================================================================
BCC_OUT   equ 8
BCC_SELF  equ 16
BCC_EXC   equ 24
BCC_DUN   equ 32            ; whether __complex__ may be tried
BCC_FRAME equ 32            ; + 0 pushes = 32

extern dunder_call_1
extern obj_dealloc
extern complex_type
extern raise_type_error_with_name
extern raise_binop_type_error
extern eval_exception_unwind

DEF_FUNC_LOCAL bcx_coerce, BCC_FRAME
    mov [rbp - BCC_OUT], rsi
    mov [rbp - BCC_SELF], rdi
    mov [rbp - BCC_DUN], rdx
    ; Is it complex-valued?  A subclass counts, as PyComplex_Check does.
    xor r8d, r8d
    V_TEST_PTR rdi, rax
    ja .bcc_classified
    mov rax, [rdi + PyObject.ob_type]
    lea rcx, [rel complex_type]
    cmp rax, rcx
    je .bcc_is_complex
    test qword [rax + PyTypeObject.tp_flags], TYPE_FLAG_COMPLEX_SUBCLASS
    jz .bcc_classified
.bcc_is_complex:
    mov r8d, 1
.bcc_classified:
    mov [rbp - BCC_EXC], r8     ; parked; DUNDER_EXC_SAVE comes later
    call complex_to_parts
    test eax, eax
    jz .bcc_protocols
    mov rax, [rbp - BCC_EXC]
    add eax, 1                  ; 1 for a real number, 2 for a complex one
    jmp .bcc_done

.bcc_protocols:
    ; Only a heaptype can carry these; a builtin that had one would already
    ; have been recognised above.
    mov rdi, [rbp - BCC_SELF]
    V_TEST_PTR rdi, rax
    ja .bcc_no
    mov rax, [rdi + PyObject.ob_type]
    test qword [rax + PyTypeObject.tp_flags], TYPE_FLAG_HEAPTYPE
    jz .bcc_no

    DUNDER_EXC_SAVE [rbp - BCC_EXC]
    cmp qword [rbp - BCC_DUN], 0
    je .bcc_try_float

    ; --- __complex__ ---
    CSTRING rsi, "__complex__"
    call dunder_call_1
    test edx, edx
    jz .bcc_try_float           ; absent, or it raised
    ; The result must be a complex.  Take its parts and release it: an exact
    ; complex or a subclass both answer, as CPython accepts both.
    mov [rbp - BCC_SELF], rax
    V_TEST_PTR rax, rcx
    ja .bcc_bad_complex
    mov rcx, [rax + PyObject.ob_type]
    lea rdx, [rel complex_type]
    cmp rcx, rdx
    je .bcc_complex_ok
    test qword [rcx + PyTypeObject.tp_flags], TYPE_FLAG_COMPLEX_SUBCLASS
    jz .bcc_bad_complex
.bcc_complex_ok:
    mov rdi, rax
    mov rsi, [rbp - BCC_OUT]
    call complex_to_parts
    mov rdi, [rbp - BCC_SELF]
    DECREF_V rdi, rdx
    mov eax, 2                  ; complex-valued, whatever the argument was
    jmp .bcc_done

.bcc_bad_complex:
    mov rsi, [rbp - BCC_SELF]
    CSTRING rdi, `__complex__ returned non-complex (type \x01)`
    call raise_type_error_with_name

.bcc_try_float:
    DUNDER_RAISED [rbp - BCC_EXC], .bcc_raised
    ; --- __float__, then __index__: both through their slots, which is where
    ; slots.asm puts a heaptype's, and both give a real part with a +0.0
    ; imaginary one.
    mov rdi, [rbp - BCC_SELF]
    mov rax, [rdi + PyObject.ob_type]
    mov rcx, [rax + PyTypeObject.tp_as_number]
    test rcx, rcx
    jz .bcc_no
    mov rax, [rcx + PyNumberMethods.nb_float]
    test rax, rax
    jnz .bcc_call_conv
    mov rax, [rcx + PyNumberMethods.nb_index]
    test rax, rax
    jz .bcc_no
.bcc_call_conv:
    call rax                    ; rdi is still self; returns a Value
    test rax, rax
    jz .bcc_raised
    mov [rbp - BCC_SELF], rax
    mov rdi, rax
    mov rsi, [rbp - BCC_OUT]
    call complex_to_parts       ; a float or an int: 1 on success, 0 otherwise
    mov [rbp - BCC_EXC], rax    ; the verdict, across the release
    mov rdi, [rbp - BCC_SELF]
    DECREF_V rdi, rdx
    mov rax, [rbp - BCC_EXC]
    jmp .bcc_done

.bcc_raised:
    ; A protocol method raised.  Report that, not "not a number".
    leave
    jmp eval_exception_unwind

.bcc_no:
    xor eax, eax
.bcc_done:
    leave
    ret
END_FUNC bcx_coerce

DEF_FUNC builtin_complex, BCX_FRAME
    mov [rbp - BCX_ARGS], rdi
    mov [rbp - BCX_NARGS], rsi
    cmp rsi, 0
    je .bcx_zero
    cmp rsi, 2
    ja .bcx_argcount

    ; --- a string argument, which is a parse rather than a conversion ---
    mov rdi, [rbp - BCX_ARGS]
    mov rdi, [rdi]
    V_TEST_PTR rdi, rax
    ja .bcx_first_not_str
    mov rax, [rdi + PyObject.ob_type]
    lea rcx, [rel str_type]
    cmp rax, rcx
    je .bcx_from_string
    test qword [rax + PyTypeObject.tp_flags], TYPE_FLAG_STR_SUBCLASS
    jnz .bcx_from_string
.bcx_first_not_str:
    ; The second argument may not be one either, and that is a distinct
    ; message from "must be a number".
    cmp qword [rbp - BCX_NARGS], 2
    jne .bcx_first
    mov rdi, [rbp - BCX_ARGS]
    mov rdi, [rdi + 8]
    V_TEST_PTR rdi, rax
    ja .bcx_first
    mov rax, [rdi + PyObject.ob_type]
    lea rcx, [rel str_type]
    cmp rax, rcx
    je .bcx_second_is_str
    test qword [rax + PyTypeObject.tp_flags], TYPE_FLAG_STR_SUBCLASS
    jnz .bcx_second_is_str

.bcx_first:
    mov rdi, [rbp - BCX_ARGS]
    mov rdi, [rdi]
    lea rsi, [rbp - BCX_A]
    mov edx, 1                  ; __complex__ is offered to this one only
    call bcx_coerce
    mov [rbp - BCX_RA], rax
    test eax, eax
    jz .bcx_bad_type

    cmp qword [rbp - BCX_NARGS], 1
    jne .bcx_two

    ; complex(z) hands back an exact complex unchanged, as CPython does.
    mov rdi, [rbp - BCX_ARGS]
    mov rdi, [rdi]
    V_TEST_PTR rdi, rax
    ja .bcx_one_build
    mov rax, [rdi + PyObject.ob_type]
    lea rcx, [rel complex_type]
    cmp rax, rcx
    jne .bcx_one_build
    push rdi
    call obj_incref
    pop rax
    mov edx, TAG_PTR
    leave
    ret
.bcx_one_build:
    movsd xmm0, [rbp - BCX_A]
    movsd xmm1, [rbp - BCX_A + 8]
    call complex_from_doubles
    leave
    ret

.bcx_two:
    mov rdi, [rbp - BCX_ARGS]
    mov rdi, [rdi + 8]
    lea rsi, [rbp - BCX_B]
    xor edx, edx                ; ...and never to the second
    call bcx_coerce
    mov [rbp - BCX_RB], rax
    test eax, eax
    jz .bcx_bad_second

    ; CPython subtracts the second argument's imaginary part only when that
    ; argument really is a complex, and *assigns* the imaginary part rather
    ; than adding to it unless the FIRST argument was one.  The difference is
    ; invisible except on a signed zero, where it is the whole answer:
    ; complex(0, -0.0) is -0j, and 0.0 + -0.0 is +0.0.
    movsd xmm0, [rbp - BCX_A]           ; real = ar
    cmp qword [rbp - BCX_RB], 2
    jne .bcx_two_imag
    movsd xmm1, [rbp - BCX_B + 8]
    subsd xmm0, xmm1                    ; real -= bi
.bcx_two_imag:
    movsd [rbp - BCX_A], xmm0
    movsd xmm1, [rbp - BCX_B]           ; br
    cmp qword [rbp - BCX_RA], 2
    jne .bcx_two_build                  ; imag = br
    addsd xmm1, [rbp - BCX_A + 8]       ; imag = ai + br
.bcx_two_build:
    movsd xmm0, [rbp - BCX_A]
    call complex_from_doubles
    leave
    ret

.bcx_zero:
    xorpd xmm0, xmm0
    xorpd xmm1, xmm1
    call complex_from_doubles
    leave
    ret

.bcx_from_string:
    ; complex("1+2j").  Reached only when there is one argument -- a string
    ; with a second is its own error, below.
    cmp qword [rbp - BCX_NARGS], 2
    je .bcx_str_and_second
    mov rdi, [rbp - BCX_ARGS]
    mov rdi, [rdi]
    lea rsi, [rbp - BCX_A]
    extern complex_parse_string
    call complex_parse_string    ; raises rather than returning on a bad parse
    jmp .bcx_one_build

.bcx_bad_type:
    mov rdi, [rbp - BCX_ARGS]
    mov rsi, [rdi]
    CSTRING rdi, `complex() first argument must be a string or a number, not '\x01'`
    call raise_type_error_with_name

.bcx_bad_second:
    mov rdi, [rbp - BCX_ARGS]
    mov rsi, [rdi + 8]
    CSTRING rdi, `complex() second argument must be a number, not '\x01'`
    call raise_type_error_with_name

.bcx_str_and_second:
    RAISE exc_TypeError_type, "complex() can't take second arg if first is a string"

.bcx_second_is_str:
    RAISE exc_TypeError_type, "complex() second arg can't be a string"

.bcx_argcount:
    RAISE exc_TypeError_type, "complex() takes at most 2 arguments"
END_FUNC builtin_complex


