; builtins_num.asm - The numeric builtins
;
; abs, divmod, int(), bool(), float(), ord, chr, hex, bin, oct, round, pow.
; Each: name(PyObject **args, int64_t nargs) -> PyObject*, args borrowed,
; return a new reference.

%include "macros.inc"
%include "object.inc"

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
DEF_FUNC builtin_divmod
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

    ; Dispatch through the left operand's numeric protocol, the way the //
    ; and % operators do.  This used to call int_floordiv unconditionally,
    ; so divmod(1.5, 1.5) handed raw f64 bits to integer code.
    mov rdi, rbx
    mov edx, r13d
    extern value_number_methods
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

.divmod_type_error:
    RAISE exc_TypeError_type, "unsupported operand type(s) for divmod()"
END_FUNC builtin_divmod

; tp_call wrappers: shift (type, args, nargs) → (args, nargs)
global int_type_call
ITC_FRAME  equ 8            ; + 0 pushes = 8, not 16-aligned
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
BI_FRAME  equ 48            ; + 1 push = 56, not 16-aligned

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
    V_TO_F64 rdi
    call float_int
    jmp .int_ret

.int_from_float:
    ; A float subclass instance: the double is inline, at the base's offset.
    ; Exact float never arrives here -- a float is an immediate, so no pointer
    ; ever has float_type -- which is why this arm read the pointer as bits
    ; unnoticed until float had a subclass.
    mov rdi, [rbx + PyFloatObject.value]
    call float_int
    jmp .int_ret

.int_from_str:
    mov [rbp - BI_OBJ], rbx           ; save original obj for error msg
    mov qword [rbp - BI_BASE], 10     ; base 10
    ; Check for embedded NUL bytes
    lea rdi, [rbx + PyStrObject.data]
    call strlen wrt ..plt
    cmp rax, [rbx + PyStrObject.ob_size]
    jne .int_str_parse_error           ; embedded NUL → reject
    lea rdi, [rbx + PyStrObject.data]
    mov rsi, 10
    call int_from_cstr_base
    test edx, edx
    jz .int_str_parse_error
    jmp .int_ret

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
    lea rsi, [rbx + PyByteArrayObject.data]
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
    RAISE exc_TypeError_type, "int() argument must be a string or a number, not"

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
    je .int_base_from_bytes            ; same layout as bytes
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

.int_base_from_bytes:
    ; Parse bytes with given base — make null-terminated copy
    mov rcx, [rbx + PyBytesObject.ob_size]
    lea rdi, [rcx + 8]
    push rcx
    call ap_malloc
    pop rcx
    push rax
    mov rdi, rax
    lea rsi, [rbx + PyBytesObject.data]
    mov rdx, rcx
    call ap_memcpy
    pop rdi
    push rdi
    mov rcx, [rbx + PyBytesObject.ob_size]
    mov qword [rdi + rcx], 0
    ; Check for embedded NUL
    call strlen wrt ..plt
    cmp rax, [rbx + PyBytesObject.ob_size]
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
;; 1 arg: round to nearest int (banker's rounding)
;; 2 args: round to ndigits decimal places
;; ============================================================================

global builtin_round_fn
RND_NDIGITS equ 16      ; historical: referenced as [rbp - RND_NDIGITS]
RND_XPAY    equ 24
RND_XTAG    equ 32
RND_FRAME   equ 48          ; + 1 push = 56, not 16-aligned
DEF_FUNC builtin_round_fn, RND_FRAME
    push rbx

    cmp rsi, 1
    je .rnd_one_arg
    cmp rsi, 2
    je .rnd_two_arg
    jmp .rnd_error

.rnd_one_arg:
    ; round(x) — return int.  Normalize first: int_unwrap flattens bool,
    ; compact heap ints and int subclasses to (value, TAG_SMALLINT).
    extern int_unwrap
    mov rdi, [rdi]            ; args[0]
    V_UNPACK rdi, rdx
    call int_unwrap
    mov rax, rdi            ; payload
    mov ecx, edx            ; tag

    cmp ecx, TAG_SMALLINT
    je .rnd_int_ret          ; int → return as-is

    ; Extract double from TAG_FLOAT or TAG_PTR (PyFloatObject)
    cmp ecx, TAG_FLOAT
    je .rnd_one_raw_float
    cmp ecx, TAG_PTR
    jne .rnd_type_error
    ; Check if it's a PyFloatObject
    lea rcx, [rel float_type]
    cmp [rax + PyObject.ob_type], rcx
    jne .rnd_one_check_int_obj
    movsd xmm0, [rax + PyFloatObject.value]
    jmp .rnd_one_do_round
.rnd_one_check_int_obj:
    ; Check if it's a PyIntObject (heap int)
    lea rcx, [rel int_type]
    cmp [rax + PyObject.ob_type], rcx
    jne .rnd_type_error
    ; It's a heap int — convert to i64 and return as SmallInt.
    ; int_to_i64 dispatches on edx, so the tag must be supplied.
    mov rdi, rax
    mov edx, TAG_PTR
    call int_to_i64
    RET_TAG_SMALLINT
    pop rbx
    leave
    V_PACK rax, rdx             ; builtins return one Value
    ret
.rnd_one_raw_float:
    movq xmm0, rax

.rnd_one_do_round:
    ; Float: banker's rounding (x86 default rounding mode = round-to-nearest-even)
    cvtsd2si rax, xmm0     ; round-to-nearest-even
    RET_TAG_SMALLINT
    pop rbx
    leave
    V_PACK rax, rdx             ; builtins return one Value
    ret

.rnd_int_ret:
    RET_TAG_SMALLINT
    pop rbx
    leave
    V_PACK rax, rdx             ; builtins return one Value
    ret

.rnd_two_arg:
    ; round(x, ndigits) — normalize both operands (see .rnd_one_arg)
    extern int_unwrap
    mov r9, rdi                 ; args array
    mov rdi, [r9]            ; args[0]
    V_UNPACK rdi, rdx
    call int_unwrap
    mov [rbp - RND_XPAY], rdi
    mov [rbp - RND_XTAG], rdx
    mov rdi, [r9 + 8]
    V_UNPACK rdi, rdx       ; args[1]
    call int_unwrap
    mov rbx, rdi                ; ndigits payload
    mov r8d, edx                ; ndigits tag
    mov rax, [rbp - RND_XPAY]   ; x payload
    mov ecx, [rbp - RND_XTAG]   ; x tag

    ; ndigits must be int
    cmp r8d, TAG_SMALLINT
    jne .rnd_type_error

    ; Check x type — extract double
    cmp ecx, TAG_SMALLINT
    je .rnd_two_int
    cmp ecx, TAG_FLOAT
    je .rnd_two_raw_float
    cmp ecx, TAG_PTR
    jne .rnd_type_error
    ; Check if PyFloatObject
    lea rcx, [rel float_type]
    cmp [rax + PyObject.ob_type], rcx
    jne .rnd_type_error
    movsd xmm0, [rax + PyFloatObject.value]
    jmp .rnd_two_got_float
.rnd_two_raw_float:
    movq xmm0, rax          ; xmm0 = x (double)
.rnd_two_got_float:

    ; round(float, ndigits): multiply by 10^ndigits, round, divide
    mov [rbp - RND_NDIGITS], rbx  ; save ndigits

    ; Compute 10^ndigits (ndigits in rbx as int64)
    mov rax, 1               ; multiplier = 1
    test rbx, rbx
    jz .rnd_two_no_scale
    js .rnd_two_neg_scale
    mov rcx, rbx
.rnd_pow10_loop:
    imul rax, 10
    dec rcx
    jnz .rnd_pow10_loop

.rnd_two_no_scale:
    ; xmm0 = x, rax = 10^ndigits
    cvtsi2sd xmm1, rax      ; xmm1 = 10^ndigits
    mulsd xmm0, xmm1        ; x * 10^n
    cvtsd2si rax, xmm0      ; banker's round
    cvtsi2sd xmm0, rax      ; back to double
    divsd xmm0, xmm1        ; / 10^n
    movq rax, xmm0
    mov edx, TAG_FLOAT
    pop rbx
    leave
    V_PACK rax, rdx             ; builtins return one Value
    ret

.rnd_two_neg_scale:
    ; Negative ndigits for float: e.g., round(1234.5, -2) = 1200.0
    neg rbx
    mov rax, 1
    mov rcx, rbx
.rnd_pow10n_loop:
    imul rax, 10
    dec rcx
    jnz .rnd_pow10n_loop

    cvtsi2sd xmm1, rax      ; xmm1 = 10^|ndigits|
    divsd xmm0, xmm1        ; x / 10^n
    cvtsd2si rax, xmm0      ; banker's round
    cvtsi2sd xmm0, rax
    mulsd xmm0, xmm1        ; * 10^n
    movq rax, xmm0
    mov edx, TAG_FLOAT
    pop rbx
    leave
    V_PACK rax, rdx             ; builtins return one Value
    ret

.rnd_two_int:
    ; round(int, ndigits) — ndigits >= 0: return int as-is
    ; ndigits < 0: round to nearest 10^|ndigits|
    test rbx, rbx
    jns .rnd_int_ret         ; ndigits >= 0, int stays the same

    ; Negative ndigits: round(1234, -2) = 1200
    neg rbx
    ; Compute 10^|ndigits|
    mov rcx, 1
.rnd_int_pow10:
    imul rcx, 10
    dec rbx
    jnz .rnd_int_pow10

    ; rax = x, rcx = divisor
    ; rounded = (x + divisor/2) / divisor * divisor (away from zero simple)
    ; Python uses banker's: convert to float, round, convert back
    cvtsi2sd xmm0, rax
    cvtsi2sd xmm1, rcx
    divsd xmm0, xmm1
    cvtsd2si rax, xmm0      ; banker's round
    imul rax, rcx
    RET_TAG_SMALLINT
    pop rbx
    leave
    V_PACK rax, rdx             ; builtins return one Value
    ret

.rnd_error:
    RAISE exc_TypeError_type, "round() takes 1 or 2 arguments"

.rnd_type_error:
    RAISE exc_TypeError_type, "type cannot be rounded"
END_FUNC builtin_round_fn

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
POW_FRAME equ 48            ; + 3 pushes = 72, not 16-aligned
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
    mov rax, r12
    mov rbx, r13
    mov ecx, [rbp - POW_BTAG]
    mov r8d, [rbp - POW_ETAG]
    jmp .pow_two_float
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

.pow_two_float:
    ; At least one is float: convert both to double
    cmp ecx, TAG_SMALLINT
    jne .pow_f_base_float
    cvtsi2sd xmm0, rax
    jmp .pow_f_got_base
.pow_f_base_float:
    cmp ecx, TAG_FLOAT
    je .pow_f_base_raw
    ; TAG_PTR: extract from PyFloatObject
    cmp ecx, TAG_PTR
    jne .pow_type_error
    lea rcx, [rel float_type]
    cmp [rax + PyObject.ob_type], rcx
    jne .pow_type_error
    movsd xmm0, [rax + PyFloatObject.value]
    jmp .pow_f_got_base
.pow_f_base_raw:
    movq xmm0, rax
.pow_f_got_base:
    cmp r8d, TAG_SMALLINT
    jne .pow_f_exp_float
    cvtsi2sd xmm1, rbx
    jmp .pow_f_got_exp
.pow_f_exp_float:
    cmp r8d, TAG_FLOAT
    je .pow_f_exp_raw
    ; TAG_PTR: extract from PyFloatObject
    cmp r8d, TAG_PTR
    jne .pow_type_error
    lea rcx, [rel float_type]
    cmp [rbx + PyObject.ob_type], rcx
    jne .pow_type_error
    movsd xmm1, [rbx + PyFloatObject.value]
    jmp .pow_f_got_exp
.pow_f_exp_raw:
    movq xmm1, rbx
.pow_f_got_exp:
    ; xmm0 = base, xmm1 = exp
    ; Use repeated squaring for integer exponents, or fall back to exp*ln
    ; Simple: convert to C pow() equivalent using exp/ln
    ; x^y = exp2(y * log2(x)) — but we don't have those instructions easily
    ; Use a simpler approach: if exp is a small integer, use repeated mult
    cvtsd2si rcx, xmm1
    cvtsi2sd xmm2, rcx
    ucomisd xmm1, xmm2
    jne .pow_f_general       ; exp is not an integer
    jp .pow_f_general        ; NaN

    ; Integer exponent: repeated squaring
    mov r13, rcx
    test r13, r13
    js .pow_f_neg

    movq xmm2, [rel const_one] ; result = 1.0
.pow_f_sq:
    test r13, r13
    jz .pow_f_sq_done
    test r13, 1
    jz .pow_f_sq_even
    mulsd xmm2, xmm0
.pow_f_sq_even:
    mulsd xmm0, xmm0
    shr r13, 1
    jmp .pow_f_sq
.pow_f_sq_done:
    movq rax, xmm2
    mov edx, TAG_FLOAT
    pop r13
    pop r12
    pop rbx
    leave
    V_PACK rax, rdx             ; builtins return one Value
    ret

.pow_f_neg:
    neg r13
    movq xmm2, [rel const_one]
.pow_f_neg_sq:
    test r13, r13
    jz .pow_f_neg_done
    test r13, 1
    jz .pow_f_neg_even
    mulsd xmm2, xmm0
.pow_f_neg_even:
    mulsd xmm0, xmm0
    shr r13, 1
    jmp .pow_f_neg_sq
.pow_f_neg_done:
    movq xmm0, [rel const_one]
    divsd xmm0, xmm2
    movq rax, xmm0
    mov edx, TAG_FLOAT
    pop r13
    pop r12
    pop rbx
    leave
    V_PACK rax, rdx             ; builtins return one Value
    ret

.pow_f_general:
    ; Non-integer float exponent: x^y = 2^(y * log2(x))
    ; xmm0 = base, xmm1 = exp
    ; fyl2x computes st(1) * log2(st(0)), so load exp first, then base
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
    movq rax, xmm0
    mov edx, TAG_FLOAT
    pop r13
    pop r12
    pop rbx
    leave
    V_PACK rax, rdx             ; builtins return one Value
    ret

.pow_three:
    ; pow(base, exp, mod) — modular exponentiation.
    ; Normalize the operands first: int_unwrap flattens bool, compact heap
    ; ints and int subclasses to (value, TAG_SMALLINT).  Genuinely huge
    ; GMP-backed ints stay TAG_PTR and are rejected below, as before.
    extern int_unwrap
    mov r13, rdi                ; args array
    mov rdi, [r13]              ; args[0]
    V_UNPACK rdi, rdx
    call int_unwrap
    mov [rbp - POW_BASE], rdi
    mov [rbp - POW_BTAG], rdx
    mov rdi, [r13 + 8]
    V_UNPACK rdi, rdx       ; args[1]
    call int_unwrap
    mov [rbp - POW_EXP], rdi
    mov [rbp - POW_ETAG], rdx
    mov rdi, [r13 + 16]
    V_UNPACK rdi, rdx       ; args[2]
    call int_unwrap
    mov r12, rdi                ; mod
    mov r9d, edx                ; mod tag
    mov rax, [rbp - POW_BASE]   ; base
    mov ecx, [rbp - POW_BTAG]   ; base tag
    mov rbx, [rbp - POW_EXP]    ; exp
    mov r8d, [rbp - POW_ETAG]   ; exp tag

    ; All must now be plain int64
    cmp ecx, TAG_SMALLINT
    jne .pow_type_error
    cmp r8d, TAG_SMALLINT
    jne .pow_type_error
    cmp r9d, TAG_SMALLINT
    jne .pow_type_error

    ; exp must be >= 0
    test rbx, rbx
    js .pow_neg_mod_exp
    ; mod must be != 0
    test r12, r12
    jz .pow_zero_mod

    ; Modular exponentiation: result = base^exp mod mod
    mov r13, rbx            ; exp
    ; rax = base, r12 = mod
    ; Reduce base mod first
    cqo
    idiv r12                ; rax=quot, rdx=rem
    mov rax, rdx            ; base = base % mod
    ; Adjust remainder to match Python semantics (sign of mod)
    test rax, rax
    jz .pow_mod_pos
    mov rdx, rax
    xor rdx, r12
    jns .pow_mod_pos         ; same sign → OK
    add rax, r12             ; different signs → adjust
.pow_mod_pos:
    mov rcx, 1              ; result = 1
.pow_mod_loop:
    test r13, r13
    jz .pow_mod_done
    test r13, 1
    jz .pow_mod_even
    imul rcx, rax           ; result *= base
    ; result %= mod
    push rax
    mov rax, rcx
    cqo
    idiv r12
    mov rcx, rdx
    test rcx, rcx
    jz .pow_mod_pos2
    mov rdx, rcx
    xor rdx, r12
    jns .pow_mod_pos2
    add rcx, r12
.pow_mod_pos2:
    pop rax
.pow_mod_even:
    imul rax, rax           ; base *= base
    ; base %= mod
    push rcx
    cqo
    idiv r12
    mov rax, rdx
    test rax, rax
    jz .pow_mod_pos3
    mov rdx, rax
    xor rdx, r12
    jns .pow_mod_pos3
    add rax, r12
.pow_mod_pos3:
    pop rcx
    shr r13, 1
    jmp .pow_mod_loop
.pow_mod_done:
    ; Apply final result % mod (needed for exp=0 case: pow(x,0,mod) = 1 % mod)
    mov rax, rcx
    cqo
    idiv r12
    mov rax, rdx
    test rax, rax
    jz .pow_mod_final
    mov rdx, rax
    xor rdx, r12
    jns .pow_mod_final
    add rax, r12
.pow_mod_final:
    RET_TAG_SMALLINT
    pop r13
    pop r12
    pop rbx
    leave
    V_PACK rax, rdx             ; builtins return one Value
    ret

.pow_neg_mod_exp:
    RAISE exc_ValueError_type, "pow() 2nd argument cannot be negative when 3rd argument specified"

.pow_zero_mod:
    RAISE exc_ValueError_type, "pow() 3rd argument cannot be 0"

.pow_error:
    RAISE exc_TypeError_type, "pow() takes 2 or 3 arguments"

.pow_type_error:
    RAISE exc_TypeError_type, "pow() arguments must be numeric"
END_FUNC builtin_pow_fn

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
BCX_FRAME equ 48              ; + 0 pushes = 48
DEF_FUNC builtin_complex, BCX_FRAME
    mov [rbp - BCX_ARGS], rdi
    mov [rbp - BCX_NARGS], rsi
    cmp rsi, 0
    je .bcx_zero
    cmp rsi, 2
    ja .bcx_argcount

    mov rdi, [rbp - BCX_ARGS]
    mov rdi, [rdi]
    lea rsi, [rbp - BCX_A]
    call complex_to_parts
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
    call complex_to_parts
    test eax, eax
    jz .bcx_bad_type

    ; CPython subtracts the second argument's imaginary part only when that
    ; argument really is a complex, and *assigns* the imaginary part rather
    ; than adding to it unless the FIRST argument was one.  The difference is
    ; invisible except on a signed zero, where it is the whole answer:
    ; complex(0, -0.0) is -0j, and 0.0 + -0.0 is +0.0.
    movsd xmm0, [rbp - BCX_A]           ; real = ar
    mov rdi, [rbp - BCX_ARGS]
    mov rdi, [rdi + 8]
    call bcx_is_complex
    test eax, eax
    jz .bcx_two_imag
    movsd xmm1, [rbp - BCX_B + 8]
    subsd xmm0, xmm1                    ; real -= bi
.bcx_two_imag:
    movsd [rbp - BCX_A], xmm0           ; park the real part across the call
    mov rdi, [rbp - BCX_ARGS]
    mov rdi, [rdi]
    call bcx_is_complex
    movsd xmm1, [rbp - BCX_B]           ; br
    test eax, eax
    jz .bcx_two_build                   ; imag = br
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

.bcx_bad_type:
    RAISE exc_TypeError_type, "complex() argument must be a number"
.bcx_argcount:
    RAISE exc_TypeError_type, "complex() takes at most 2 arguments"
END_FUNC builtin_complex

;; ============================================================================
;; bcx_is_complex(rdi = Value) -> eax = 1 when it is an exact complex.
;; ============================================================================
DEF_FUNC_BARE bcx_is_complex
    V_TEST_PTR rdi, rax
    ja .bic_no
    mov rax, [rdi + PyObject.ob_type]
    lea rcx, [rel complex_type]
    cmp rax, rcx
    jne .bic_no
    mov eax, 1
    ret
.bic_no:
    xor eax, eax
    ret
END_FUNC bcx_is_complex

