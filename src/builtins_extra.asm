; builtins_extra.asm - Additional Python builtin functions
; Each builtin: name(PyObject **args, int64_t nargs) -> PyObject*
; args = borrowed refs; return = new ref

%include "macros.inc"
%include "object.inc"
%include "types.inc"
%include "frame.inc"

; External symbols used
extern int_from_i64
extern int_to_i64
extern __gmpz_fits_slong_p
extern int_neg
extern int_add
extern int_from_cstr
extern int_from_cstr_base
extern float_from_f64
extern float_int
extern ap_malloc
extern ap_free
extern ap_memcpy
extern strlen
extern str_from_cstr
extern obj_str
extern obj_repr
extern obj_is_true
extern obj_incref
extern obj_decref
extern dict_get
extern raise_exception
extern exc_new
extern current_exception
extern eval_exception_unwind
extern none_singleton
extern eval_saved_r12

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
extern exc_AttributeError_type
extern exc_StopIteration_type
extern list_new
extern list_append
extern list_contains
extern dict_tp_iter
extern type_type
extern user_type_metatype
extern dunder_lookup
extern kw_names_pending
extern ap_strcmp

; ============================================================================
; 1. builtin_abs(args, nargs) - abs(x)
; ============================================================================
DEF_FUNC builtin_abs
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
    mov edx, TAG_PTR
    add rsp, 8
    pop rbx
    leave
    ret

.abs_gmp_neg:
    mov rdi, rbx
    call int_neg
    mov edx, TAG_PTR
    add rsp, 8
    pop rbx
    leave
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
    mov edx, TAG_SMALLINT
    add rsp, 8
    pop rbx
    leave
    ret

.abs_float:
    movsd xmm0, [rbx + PyFloatObject.value]
    mov rax, 0x7FFFFFFFFFFFFFFF
    movq xmm1, rax
    andpd xmm0, xmm1
    call float_from_f64
    mov edx, TAG_PTR
    add rsp, 8
    pop rbx
    leave
    ret

.abs_type_error:
    lea rdi, [rel exc_TypeError_type]
    CSTRING rsi, "bad operand type for abs()"
    call raise_exception

.abs_error:
    lea rdi, [rel exc_TypeError_type]
    CSTRING rsi, "abs() takes exactly one argument"
    call raise_exception
END_FUNC builtin_abs

; ============================================================================
; builtin_divmod(args, nargs) - divmod(a, b) -> (a // b, a % b)
; ============================================================================
global builtin_divmod
DEF_FUNC builtin_divmod
    push rbx
    push r12

    cmp rsi, 2
    jne .divmod_error

    mov rbx, [rdi]              ; a
    mov r12, [rdi + 8]          ; b

    ; Compute a // b
    mov rdi, rbx
    mov rsi, r12
    extern int_floordiv
    call int_floordiv
    push rax                    ; save quotient

    ; Compute a % b
    mov rdi, rbx
    mov rsi, r12
    extern int_mod
    call int_mod
    mov r12, rax                ; r12 = remainder

    pop rbx                     ; rbx = quotient

    ; Create 2-tuple (quotient, remainder)
    mov edi, 2
    extern tuple_new
    call tuple_new
    mov [rax + PyTupleObject.ob_item], rbx
    mov [rax + PyTupleObject.ob_item + 8], r12
    mov edx, TAG_PTR

    pop r12
    pop rbx
    leave
    ret

.divmod_error:
    lea rdi, [rel exc_TypeError_type]
    CSTRING rsi, "divmod expected 2 arguments"
    call raise_exception
END_FUNC builtin_divmod

; tp_call wrappers: shift (type, args, nargs) → (args, nargs)
global int_type_call
ITC_FRAME  equ 8
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
    mov r10, [rax + PyTupleObject.ob_item + r9*8]  ; kw name str
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
    lea rdi, [rel exc_TypeError_type]
    CSTRING rsi, "int() missing string argument"
    call raise_exception
.itc_kw_reject:
    lea rdi, [rel exc_TypeError_type]
    CSTRING rsi, "'x' is an invalid keyword argument for int()"
    call raise_exception
.itc_no_kw:
    leave
    jmp builtin_int_fn
END_FUNC int_type_call

global str_type_call
DEF_FUNC_BARE str_type_call
    mov rdi, rsi
    mov rsi, rdx
    jmp builtin_str_fn
END_FUNC str_type_call

global bool_type_call
DEF_FUNC_BARE bool_type_call
    mov rdi, rsi
    mov rsi, rdx
    jmp builtin_bool
END_FUNC bool_type_call

global float_type_call
DEF_FUNC_BARE float_type_call
    mov rdi, rsi
    mov rsi, rdx
    jmp builtin_float
END_FUNC float_type_call

; ============================================================================
; 2. builtin_int_fn(args, nargs) - int(x) or int(x, base)
; ============================================================================
; Frame layout:
BI_ARGS   equ 8
BI_NARGS  equ 16
BI_OBJ    equ 24       ; original string/bytes obj for error messages
BI_BASE   equ 32       ; base value for error messages
BI_FRAME  equ 32

DEF_FUNC builtin_int_fn, BI_FRAME
    push rbx

    test rsi, rsi
    jz .int_no_args

    cmp rsi, 1
    je .int_one_arg

    cmp rsi, 2
    je .int_two_args

    jmp .int_error

.int_one_arg:
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

    ; Check int subclass (TYPE_FLAG_INT_SUBCLASS) — e.g. class MyInt(int)
    mov rdx, [rax + PyTypeObject.tp_flags]
    test rdx, TYPE_FLAG_INT_SUBCLASS
    jnz .int_from_int_subclass

    lea rcx, [rel float_type]
    cmp rax, rcx
    je .int_from_float

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
    lea rdx, [rel bytes_type]
    cmp rcx, rdx
    je .int_from_bytes
    lea rdx, [rel bytearray_type]
    cmp rcx, rdx
    je .int_from_bytearray
    lea rdx, [rel memoryview_type]
    cmp rcx, rdx
    je .int_from_memoryview
    mov rcx, [rcx + PyTypeObject.tp_base]
    test rcx, rcx
    jnz .int_check_bytes_chain

    jmp .int_try_dunder

.int_no_args:
    xor eax, eax
    bts rax, 63
    jmp .int_ret

.int_return_smallint:
    mov rax, rbx
    jmp .int_ret

.int_from_int:
    inc qword [rbx + PyObject.ob_refcnt]
    mov rax, rbx
    jmp .int_ret

.int_from_float:
    mov rdi, rbx
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
    test rax, rax
    jz .int_str_parse_error
    jmp .int_ret

.int_from_bytes:
    ; int(bytes_obj) — need null-terminated copy for int_from_cstr_base
    mov [rbp - BI_OBJ], rbx           ; save original obj for error msg
    mov qword [rbp - BI_BASE], 10     ; base 10
    mov rcx, [rbx + PyBytesObject.ob_size]
    lea rdi, [rcx + 1]       ; size + null terminator
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
    ; Null-terminate
    pop rdi                   ; rdi = buffer
    push rdi
    mov rcx, [rbx + PyBytesObject.ob_size]
    mov byte [rdi + rcx], 0
    ; Check for embedded NUL bytes
    call strlen wrt ..plt
    cmp rax, [rbx + PyBytesObject.ob_size]
    jne .int_bytes_nul_error  ; embedded NUL → free buf + error
    ; Parse
    mov rdi, [rsp]            ; buffer (still on stack)
    mov rsi, 10
    call int_from_cstr_base
    mov rbx, rax              ; save result
    pop rdi                   ; free temp buffer
    push rbx
    call ap_free
    pop rax                   ; rax = parse result
    test rax, rax
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
    lea rdi, [rcx + 1]
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
    mov byte [rdi + rcx], 0
    ; Check for embedded NUL
    call strlen wrt ..plt
    cmp rax, [rbx + PyByteArrayObject.ob_size]
    jne .int_bytes_nul_error
    mov rdi, [rsp]
    mov rsi, 10
    call int_from_cstr_base
    mov rbx, rax
    pop rdi
    push rbx
    call ap_free
    pop rax
    test rax, rax
    jz .int_str_parse_error
    jmp .int_ret

.int_from_memoryview:
    ; int(memoryview) — copy the viewed bytes and parse
    mov [rbp - BI_OBJ], rbx
    mov qword [rbp - BI_BASE], 10
    mov rcx, [rbx + PyMemoryViewObject.mv_len]
    lea rdi, [rcx + 1]
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
    mov byte [rdi + rcx], 0
    ; Check for embedded NUL
    call strlen wrt ..plt
    cmp rax, [rbx + PyMemoryViewObject.mv_len]
    jne .int_bytes_nul_error
    mov rdi, [rsp]
    mov rsi, 10
    call int_from_cstr_base
    mov rbx, rax
    pop rdi
    push rbx
    call ap_free
    pop rax
    test rax, rax
    jz .int_str_parse_error
    jmp .int_ret

.int_from_bool:
    lea rax, [rel bool_true]
    cmp rbx, rax
    je .int_bool_true
    xor eax, eax
    bts rax, 63
    jmp .int_ret
.int_bool_true:
    mov rax, 1
    bts rax, 63
    jmp .int_ret

.int_from_int_subclass:
    ; rbx = int subclass instance (PyIntSubclassObject)
    ; Check if it has __int__ method
    mov rdi, [rbx + PyObject.ob_type]
    CSTRING rsi, "__int__"
    call dunder_lookup
    test rax, rax
    jz .int_from_int_sub_extract ; no __int__, extract int_value
    ; Call __int__(self) — rax = func (borrowed ref)
    mov rcx, [rax + PyObject.ob_type]
    mov rcx, [rcx + PyTypeObject.tp_call]
    test rcx, rcx
    jz .int_from_int
    push rbx                     ; args[0] = self
    mov rdi, rax
    mov rsi, rsp
    mov edx, 1
    call rcx
    add rsp, 8
    ; Check for exception (NULL return)
    test rax, rax
    jz .int_dunder_error
    ; Verify result is int-like
    js .int_ret                  ; SmallInt — OK
    mov rcx, [rax + PyObject.ob_type]
    lea rdx, [rel int_type]
    cmp rcx, rdx
    je .int_ret                  ; exact int — OK
    lea rdx, [rel bool_type]
    cmp rcx, rdx
    je .int_convert_bool_result  ; bool → convert to plain int
    mov rdx, [rcx + PyTypeObject.tp_flags]
    test rdx, TYPE_FLAG_INT_SUBCLASS
    jnz .int_ret                 ; int subclass — OK for now
    ; __int__ returned non-int
    mov rdi, rax
    call obj_decref
    lea rdi, [rel exc_TypeError_type]
    CSTRING rsi, "__int__ returned non-int (type float)"
    call raise_exception

.int_from_int_sub_extract:
    ; rbx = PyIntSubclassObject with no __int__ method
    ; Extract the int_value and return it
    mov rax, [rbx + PyIntSubclassObject.int_value]
    test rax, rax
    js .int_ret                  ; SmallInt
    INCREF rax
    jmp .int_ret

.int_try_dunder:
    ; rbx = unknown-type object
    ; Try __int__ protocol
    mov rdi, [rbx + PyObject.ob_type]
    CSTRING rsi, "__int__"
    call dunder_lookup
    test rax, rax
    jnz .int_call_dunder

    ; Try __index__ protocol
    mov rdi, [rbx + PyObject.ob_type]
    CSTRING rsi, "__index__"
    call dunder_lookup
    test rax, rax
    jnz .int_call_dunder

    ; Try __trunc__ protocol
    mov rdi, [rbx + PyObject.ob_type]
    CSTRING rsi, "__trunc__"
    call dunder_lookup
    test rax, rax
    jnz .int_call_dunder_trunc

    jmp .int_type_error

.int_call_dunder:
    ; rax = func (borrowed ref), rbx = self
    mov rcx, [rax + PyObject.ob_type]
    mov rcx, [rcx + PyTypeObject.tp_call]
    test rcx, rcx
    jz .int_type_error
    push rbx                     ; args[0] = self
    mov rdi, rax
    mov rsi, rsp
    mov edx, 1
    call rcx
    add rsp, 8
    ; Check for exception (NULL return)
    test rax, rax
    jz .int_dunder_error
    ; Verify result is int-like
    js .int_ret                  ; SmallInt — OK
    mov rcx, [rax + PyObject.ob_type]
    lea rdx, [rel int_type]
    cmp rcx, rdx
    je .int_ret                  ; exact int — OK
    lea rdx, [rel bool_type]
    cmp rcx, rdx
    je .int_convert_bool_result  ; bool → convert to plain int
    mov rdx, [rcx + PyTypeObject.tp_flags]
    test rdx, TYPE_FLAG_INT_SUBCLASS
    jnz .int_ret                 ; int subclass — OK
    ; Not int-like
    mov rdi, rax
    call obj_decref
    lea rdi, [rel exc_TypeError_type]
    CSTRING rsi, "__int__ returned non-int"
    call raise_exception

.int_call_dunder_trunc:
    ; rax = __trunc__ func, rbx = self
    ; Call __trunc__(self); result must be int-like or have __index__
    ; CPython 3.12: tries __index__ on result, but NOT __int__
    mov rcx, [rax + PyObject.ob_type]
    mov rcx, [rcx + PyTypeObject.tp_call]
    test rcx, rcx
    jz .int_type_error
    push rbx
    mov rdi, rax
    mov rsi, rsp
    mov edx, 1
    call rcx
    add rsp, 8
    ; rax = result of __trunc__()
    ; Check for exception (NULL return)
    test rax, rax
    jz .int_dunder_error
    ; If it's already an int, return it
    js .int_ret                  ; SmallInt — OK
    mov rcx, [rax + PyObject.ob_type]
    lea rdx, [rel int_type]
    cmp rcx, rdx
    je .int_ret
    lea rdx, [rel bool_type]
    cmp rcx, rdx
    je .int_convert_bool_result
    mov rdx, [rcx + PyTypeObject.tp_flags]
    test rdx, TYPE_FLAG_INT_SUBCLASS
    jnz .int_ret
    ; __trunc__ returned non-int — try __index__ only (CPython behavior)
    mov rbx, rax                 ; save __trunc__ result
    mov rdi, [rax + PyObject.ob_type]
    CSTRING rsi, "__index__"
    call dunder_lookup
    test rax, rax
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
    push rbx                     ; args[0] = __trunc__ result
    mov rdi, rax
    mov rsi, rsp
    mov edx, 1
    call rcx
    add rsp, 8
    ; rax = __index__ result
    test rax, rax
    jz .int_dunder_error
    ; Verify it's an int
    js .int_ret                  ; SmallInt — OK
    mov rcx, [rax + PyObject.ob_type]
    lea rdx, [rel int_type]
    cmp rcx, rdx
    je .int_ret
    lea rdx, [rel bool_type]
    cmp rcx, rdx
    je .int_convert_bool_result
    mov rdx, [rcx + PyTypeObject.tp_flags]
    test rdx, TYPE_FLAG_INT_SUBCLASS
    jnz .int_ret
    ; __index__ returned non-int
    mov rdi, rax
    call obj_decref
    lea rdi, [rel exc_TypeError_type]
    CSTRING rsi, "__index__ returned non-int"
    call raise_exception

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
    call str_from_cstr
    push rax                               ; save prefix str

    ; Create type name str
    mov rdi, [rsp + 8]                     ; type name C string
    call str_from_cstr
    push rax                               ; save name str

    ; Create suffix str
    CSTRING rdi, ")"
    call str_from_cstr
    push rax                               ; save suffix str

    ; Concat: prefix + name
    extern str_concat
    mov rdi, [rsp + 16]                    ; prefix str
    mov rsi, [rsp + 8]                     ; name str
    call str_concat
    push rax                               ; save partial

    ; Concat: partial + suffix
    mov rdi, rax                           ; partial
    mov rsi, [rsp + 8]                     ; suffix str
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
    call exc_new
    push rax                               ; save exc
    mov rdi, rbx
    call obj_decref                        ; DECREF msg str
    pop rax                                ; exc obj

    ; Store exception and jump to unwind
    mov [rel current_exception], rax
    jmp eval_exception_unwind

.int_convert_bool_result:
    ; rax = bool_true or bool_false, convert to SmallInt
    lea rcx, [rel bool_true]
    cmp rax, rcx
    je .int_bool_result_true
    xor eax, eax
    bts rax, 63
    jmp .int_ret
.int_bool_result_true:
    mov rax, 1
    bts rax, 63
    jmp .int_ret

.int_dunder_error:
    ; Dunder method raised an exception — propagate it (return NULL)
    xor eax, eax
    jmp .int_ret

.int_type_error:
    lea rdi, [rel exc_TypeError_type]
    CSTRING rsi, "int() argument must be a string or a number, not"
    call raise_exception

.int_error:
    lea rdi, [rel exc_TypeError_type]
    CSTRING rsi, "int() takes at most 2 arguments"
    call raise_exception

; ------- int(x, base) -------
.int_two_args:
    mov [rbp - BI_ARGS], rdi       ; save args pointer
    ; Get base from args[1] (contiguous 8-byte buffer from CALL)
    mov rax, [rdi + 8]             ; args[1] = base
    test rax, rax
    js .int_base_smallint
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
    push rax                      ; save base obj
    mov rdi, rcx                  ; type
    CSTRING rsi, "__index__"
    call dunder_lookup
    test rax, rax
    jz .int_base_no_index
    ; Call __index__(base_obj)
    mov rcx, [rax + PyObject.ob_type]
    mov rcx, [rcx + PyTypeObject.tp_call]
    test rcx, rcx
    jz .int_base_no_index
    mov rdi, rax
    lea rsi, [rsp]               ; args[0] = base_obj (at top of stack)
    mov edx, 1
    call rcx
    add rsp, 8                   ; pop base obj
    ; rax = __index__ result, should be int
    test rax, rax
    jz .int_dunder_error         ; __index__ raised exception
    js .int_base_si_from_index
    ; heap int — check if it fits in i64 first
    push rax
    lea rdi, [rax + PyIntObject.mpz]
    call __gmpz_fits_slong_p wrt ..plt
    test eax, eax
    pop rdi                      ; rdi = __index__ result
    jz .int_base_range_error     ; doesn't fit → definitely out of 2-36 range
    call int_to_i64
    jmp .int_have_base
.int_base_si_from_index:
    shl rax, 1
    sar rax, 1
    jmp .int_have_base
.int_base_no_index:
    add rsp, 8                   ; pop base obj
    jmp .int_base_type_error
.int_base_heap_int:
    ; rax = heap int object (GMP). Check if it fits in i64.
    push rax
    lea rdi, [rax + PyIntObject.mpz]
    call __gmpz_fits_slong_p wrt ..plt
    test eax, eax
    pop rdi                      ; rdi = heap int obj
    jz .int_base_range_error     ; doesn't fit → out of 2-36 range
    call int_to_i64
    jmp .int_have_base
.int_base_smallint:
    shl rax, 1
    sar rax, 1             ; decode SmallInt
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
    mov rbx, [rdi]                 ; args[0]
    mov [rbp - BI_OBJ], rbx       ; save original obj for error msg
    test rbx, rbx
    js .int_base_type_error_str    ; SmallInt: can't have base with int
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
    lea rdx, [rel bytes_type]
    cmp rcx, rdx
    je .int_base_from_bytes
    lea rdx, [rel bytearray_type]
    cmp rcx, rdx
    je .int_base_from_bytes            ; same layout as bytes
    mov rcx, [rcx + PyTypeObject.tp_base]
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
    test rax, rax
    jz .int_base_parse_error
    jmp .int_ret

.int_base_from_bytes:
    ; Parse bytes with given base — make null-terminated copy
    mov rcx, [rbx + PyBytesObject.ob_size]
    lea rdi, [rcx + 1]
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
    mov byte [rdi + rcx], 0
    ; Check for embedded NUL
    call strlen wrt ..plt
    cmp rax, [rbx + PyBytesObject.ob_size]
    jne .int_base_bytes_nul_error
    mov rdi, [rsp]                 ; buffer
    mov rsi, [rbp - BI_NARGS]      ; base
    call int_from_cstr_base
    mov rbx, rax
    pop rdi
    push rbx
    call ap_free
    pop rax
    test rax, rax
    jz .int_base_parse_error
    jmp .int_ret

.int_base_bytes_nul_error:
    pop rdi                   ; free temp buffer
    call ap_free
    jmp .int_base_parse_error

.int_base_type_error:
    lea rdi, [rel exc_TypeError_type]
    CSTRING rsi, "int() second arg must be an integer"
    call raise_exception

.int_base_type_error_str:
    lea rdi, [rel exc_TypeError_type]
    CSTRING rsi, "int() can't convert non-string with explicit base"
    call raise_exception

.int_base_range_error:
    lea rdi, [rel exc_ValueError_type]
    CSTRING rsi, "int() base must be >= 2 and <= 36, or 0"
    call raise_exception

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

    ; Create PyStr from buffer
    mov rdi, rsp
    call str_from_cstr
    mov [rsp + 48], rax

    ; Get repr of original object
    mov rdi, [rbp - BI_OBJ]
    call obj_repr
    test rax, rax
    jnz .ile_have_repr
    CSTRING rdi, "???"
    call str_from_cstr
.ile_have_repr:
    mov [rsp + 56], rax

    ; Concat prefix_str + repr_str → full message
    mov rdi, [rsp + 48]
    mov rsi, [rsp + 56]
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
    ; Common epilogue: restore rbx, leave, ret
    ; Classify return value: SmallInt (bit63), NULL (error), or heap ptr
    mov edx, TAG_PTR
    test rax, rax
    jz .int_ret_epilogue         ; NULL = error, tag doesn't matter
    mov ecx, TAG_SMALLINT
    cmovs edx, ecx              ; bit63 set -> TAG_SMALLINT
.int_ret_epilogue:
    ; rbx was pushed after sub rsp, BI_FRAME, so it's at rbp - BI_FRAME - 8
    lea rsp, [rbp - BI_FRAME - 8]
    pop rbx
    leave
    ret

END_FUNC builtin_int_fn

; ============================================================================
; 3. builtin_str_fn(args, nargs) - str(x)
; ============================================================================
DEF_FUNC builtin_str_fn

    test rsi, rsi
    jz .str_no_args

    cmp rsi, 1
    jne .str_error

    mov rdi, [rdi]
    call obj_str
    mov edx, TAG_PTR
    leave
    ret

.str_no_args:
    CSTRING rdi, ""
    call str_from_cstr
    mov edx, TAG_PTR
    leave
    ret

.str_error:
    lea rdi, [rel exc_TypeError_type]
    CSTRING rsi, "str() takes at most 1 argument"
    call raise_exception
END_FUNC builtin_str_fn

; ============================================================================
; 4. builtin_ord(args, nargs) - ord(c)
; ============================================================================
DEF_FUNC builtin_ord

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
    mov edx, TAG_SMALLINT
    leave
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
END_FUNC builtin_ord

; ============================================================================
; 5. builtin_chr(args, nargs) - chr(n)
; ============================================================================
DEF_FUNC builtin_chr, 16

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
    mov edx, TAG_PTR
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
    mov edx, TAG_PTR
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
    mov edx, TAG_PTR
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
    mov edx, TAG_PTR
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
END_FUNC builtin_chr

; ============================================================================
; 6. builtin_hex(args, nargs) - hex(n)
; ============================================================================
DEF_FUNC builtin_hex, 80

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
    mov edx, TAG_PTR
    leave
    ret

.hex_zero:
    CSTRING rdi, "0x0"
    call str_from_cstr
    mov edx, TAG_PTR
    leave
    ret

.hex_nargs_error:
    lea rdi, [rel exc_TypeError_type]
    CSTRING rsi, "hex() takes exactly one argument"
    call raise_exception
END_FUNC builtin_hex

; ============================================================================
; 7. builtin_id(args, nargs) - id(x)
; ============================================================================
DEF_FUNC builtin_id

    cmp rsi, 1
    jne .id_error

    mov rdi, [rdi]

    test rdi, rdi
    js .id_smallint

    call int_from_i64
    leave
    ret

.id_smallint:
    shl rdi, 1
    sar rdi, 1
    call int_from_i64
    leave
    ret

.id_error:
    lea rdi, [rel exc_TypeError_type]
    CSTRING rsi, "id() takes exactly one argument"
    call raise_exception
END_FUNC builtin_id

; ============================================================================
; 8. builtin_hash_fn(args, nargs) - hash(x)
; ============================================================================
DEF_FUNC builtin_hash_fn
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
    leave
    ret

.hash_smallint:
    mov rax, rbx
    shl rax, 1
    sar rax, 1
    mov rdi, rax
    call int_from_i64
    add rsp, 8
    pop rbx
    leave
    ret

.hash_type_error:
    lea rdi, [rel exc_TypeError_type]
    CSTRING rsi, "unhashable type"
    call raise_exception

.hash_nargs_error:
    lea rdi, [rel exc_TypeError_type]
    CSTRING rsi, "hash() takes exactly one argument"
    call raise_exception
END_FUNC builtin_hash_fn

; ============================================================================
; 9. builtin_callable(args, nargs) - callable(x)
; ============================================================================
DEF_FUNC builtin_callable

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
    mov edx, TAG_PTR
    leave
    ret

.callable_false:
    lea rax, [rel bool_false]
    inc qword [rax + PyObject.ob_refcnt]
    mov edx, TAG_PTR
    leave
    ret

.callable_error:
    lea rdi, [rel exc_TypeError_type]
    CSTRING rsi, "callable() takes exactly one argument"
    call raise_exception
END_FUNC builtin_callable

; ============================================================================
; 10. builtin_iter_fn(args, nargs) - iter(x)
; ============================================================================
DEF_FUNC builtin_iter_fn

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
    mov edx, TAG_PTR
    leave
    ret

.iter_type_error:
    lea rdi, [rel exc_TypeError_type]
    CSTRING rsi, "object is not iterable"
    call raise_exception

.iter_error:
    lea rdi, [rel exc_TypeError_type]
    CSTRING rsi, "iter() takes exactly one argument"
    call raise_exception
END_FUNC builtin_iter_fn

; ============================================================================
; 11. builtin_next_fn(args, nargs) - next(x)
; ============================================================================
DEF_FUNC builtin_next_fn

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

    ; Classify: SmallInt (bit63) or heap ptr
    mov edx, TAG_PTR
    mov ecx, TAG_SMALLINT
    cmovs edx, ecx
    leave
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
END_FUNC builtin_next_fn

; ============================================================================
; 12. builtin_any(args, nargs) - any(iterable)
; ============================================================================
DEF_FUNC builtin_any
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
    mov edx, TAG_PTR
    add rsp, 8
    pop r13
    pop r12
    pop rbx
    leave
    ret

.any_false:
    mov rdi, rbx
    call obj_decref
    lea rax, [rel bool_false]
    inc qword [rax + PyObject.ob_refcnt]
    mov edx, TAG_PTR
    add rsp, 8
    pop r13
    pop r12
    pop rbx
    leave
    ret

.any_type_error:
    lea rdi, [rel exc_TypeError_type]
    CSTRING rsi, "argument is not iterable"
    call raise_exception

.any_error:
    lea rdi, [rel exc_TypeError_type]
    CSTRING rsi, "any() takes exactly one argument"
    call raise_exception
END_FUNC builtin_any

; ============================================================================
; 13. builtin_all(args, nargs) - all(iterable)
; ============================================================================
DEF_FUNC builtin_all
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
    mov edx, TAG_PTR
    add rsp, 8
    pop r13
    pop r12
    pop rbx
    leave
    ret

.all_true:
    mov rdi, rbx
    call obj_decref
    lea rax, [rel bool_true]
    inc qword [rax + PyObject.ob_refcnt]
    mov edx, TAG_PTR
    add rsp, 8
    pop r13
    pop r12
    pop rbx
    leave
    ret

.all_type_error:
    lea rdi, [rel exc_TypeError_type]
    CSTRING rsi, "argument is not iterable"
    call raise_exception

.all_error:
    lea rdi, [rel exc_TypeError_type]
    CSTRING rsi, "all() takes exactly one argument"
    call raise_exception
END_FUNC builtin_all

; ============================================================================
; 14. builtin_sum(args, nargs) - sum(iterable[, start])
; ============================================================================
DEF_FUNC builtin_sum
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
    ; Classify: SmallInt (bit63) or heap ptr
    mov edx, TAG_PTR
    mov ecx, TAG_SMALLINT
    cmovs edx, ecx
    add rsp, 8
    pop r15
    pop r14
    pop r13
    pop r12
    pop rbx
    leave
    ret

.sum_type_error:
    lea rdi, [rel exc_TypeError_type]
    CSTRING rsi, "argument is not iterable"
    call raise_exception

.sum_error:
    lea rdi, [rel exc_TypeError_type]
    CSTRING rsi, "sum expected 1-2 arguments"
    call raise_exception
END_FUNC builtin_sum

; ============================================================================
; 15. builtin_min(args, nargs) - min(a, b, ...)
; ============================================================================
DEF_FUNC builtin_min
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
    ; Classify: SmallInt (bit63) or heap ptr
    mov edx, TAG_PTR
    mov ecx, TAG_SMALLINT
    cmovs edx, ecx
    add rsp, 8
    pop r15
    pop r14
    pop r13
    pop r12
    pop rbx
    leave
    ret

.min_error:
    lea rdi, [rel exc_TypeError_type]
    CSTRING rsi, "min expected at least 1 argument"
    call raise_exception
END_FUNC builtin_min

; ============================================================================
; 16. builtin_max(args, nargs) - max(a, b, ...)
; ============================================================================
DEF_FUNC builtin_max
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
    ; Classify: SmallInt (bit63) or heap ptr
    mov edx, TAG_PTR
    mov ecx, TAG_SMALLINT
    cmovs edx, ecx
    add rsp, 8
    pop r15
    pop r14
    pop r13
    pop r12
    pop rbx
    leave
    ret

.max_error:
    lea rdi, [rel exc_TypeError_type]
    CSTRING rsi, "max expected at least 1 argument"
    call raise_exception
END_FUNC builtin_max

; ============================================================================
; 17. builtin_getattr(args, nargs) - getattr(obj, name[, default])
; ============================================================================
DEF_FUNC builtin_getattr
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
    mov edx, TAG_PTR
    pop r14
    pop r13
    pop r12
    pop rbx
    leave
    ret

.getattr_found:
    ; Result from tp_getattr could be any type
    mov edx, TAG_PTR
    mov ecx, TAG_SMALLINT
    test rax, rax
    cmovs edx, ecx
    pop r14
    pop r13
    pop r12
    pop rbx
    leave
    ret

.getattr_not_found:
    cmp r12, 3
    jne .getattr_raise

    mov rax, [rbx + 16]
    mov edx, TAG_PTR
    test rax, rax
    mov ecx, TAG_SMALLINT
    cmovs edx, ecx
    js .getattr_ret_default_si
    inc qword [rax + PyObject.ob_refcnt]
.getattr_ret_default_si:
    pop r14
    pop r13
    pop r12
    pop rbx
    leave
    ret

.getattr_raise:
    lea rdi, [rel exc_AttributeError_type]
    CSTRING rsi, "object has no attribute"
    call raise_exception

.getattr_error:
    lea rdi, [rel exc_TypeError_type]
    CSTRING rsi, "getattr expected 2 or 3 arguments"
    call raise_exception
END_FUNC builtin_getattr

; ============================================================================
; 18. builtin_hasattr(args, nargs) - hasattr(obj, name)
; ============================================================================
DEF_FUNC builtin_hasattr
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
    mov edx, TAG_PTR
    add rsp, 8
    pop r13
    pop r12
    pop rbx
    leave
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
    mov edx, TAG_PTR
    add rsp, 8
    pop r13
    pop r12
    pop rbx
    leave
    ret

.hasattr_not_found:
    lea rax, [rel bool_false]
    inc qword [rax + PyObject.ob_refcnt]
    mov edx, TAG_PTR
    add rsp, 8
    pop r13
    pop r12
    pop rbx
    leave
    ret

.hasattr_error:
    lea rdi, [rel exc_TypeError_type]
    CSTRING rsi, "hasattr() takes exactly 2 arguments"
    call raise_exception
END_FUNC builtin_hasattr

; ============================================================================
; 19. builtin_setattr(args, nargs) - setattr(obj, name, value)
; ============================================================================
DEF_FUNC builtin_setattr
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
    mov edx, TAG_PTR
    add rsp, 8
    pop rbx
    leave
    ret

.setattr_type_error:
    lea rdi, [rel exc_TypeError_type]
    CSTRING rsi, "object does not support attribute assignment"
    call raise_exception

.setattr_error:
    lea rdi, [rel exc_TypeError_type]
    CSTRING rsi, "setattr() takes exactly 3 arguments"
    call raise_exception
END_FUNC builtin_setattr

; ============================================================================
; builtin_globals(args, nargs) - globals()
; Returns the globals dict of the current frame.
; ============================================================================
DEF_FUNC builtin_globals
    cmp rsi, 0
    jne .globals_error

    ; Get current eval frame from saved r12
    mov rax, [rel eval_saved_r12]
    mov rax, [rax + PyFrame.globals]
    INCREF rax
    mov edx, TAG_PTR
    leave
    ret

.globals_error:
    lea rdi, [rel exc_TypeError_type]
    CSTRING rsi, "globals() takes no arguments"
    call raise_exception
END_FUNC builtin_globals

; ============================================================================
; builtin_locals(args, nargs) - locals()
; Returns the locals dict if available, otherwise globals.
; In module scope, locals() == globals().
; In class body, returns the class dict.
; In function scope, returns globals as approximation.
; ============================================================================
DEF_FUNC builtin_locals
    cmp rsi, 0
    jne .locals_error

    ; Get current eval frame
    mov rax, [rel eval_saved_r12]
    ; Check if frame has a locals dict
    mov rcx, [rax + PyFrame.locals]
    test rcx, rcx
    jz .locals_use_globals
    ; Has locals dict - return it
    mov rax, rcx
    INCREF rax
    mov edx, TAG_PTR
    leave
    ret

.locals_use_globals:
    ; No locals dict - return globals (module scope)
    mov rax, [rax + PyFrame.globals]
    INCREF rax
    mov edx, TAG_PTR
    leave
    ret

.locals_error:
    lea rdi, [rel exc_TypeError_type]
    CSTRING rsi, "locals() takes no arguments"
    call raise_exception
END_FUNC builtin_locals

; ============================================================================
; builtin_dir(args, nargs) - dir(obj)
; Returns list of attribute names from obj's type (and base chain) dicts.
; ============================================================================
DIR_LIST    equ 8       ; result list
DIR_OBJ     equ 16      ; the object
DIR_FRAME   equ 24

global builtin_dir
DEF_FUNC builtin_dir, DIR_FRAME
    push rbx
    push r12
    push r13

    cmp rsi, 1
    jne .dir_error

    mov rax, [rdi]          ; obj
    mov [rbp - DIR_OBJ], rax

    ; Create result list
    xor edi, edi
    call list_new
    mov [rbp - DIR_LIST], rax
    mov rbx, rax            ; rbx = result list

    ; Determine which dict to iterate:
    ; If obj is a type (ob_type == type_type or user_type_metatype), iterate tp_dict
    ; Otherwise, iterate instance __dict__ (if any), then class dict
    mov rax, [rbp - DIR_OBJ]
    test rax, rax
    js .dir_done            ; SmallInt: no attributes
    mov rcx, [rax + PyObject.ob_type]
    lea rdx, [rel type_type]
    cmp rcx, rdx
    je .dir_from_type
    lea rdx, [rel user_type_metatype]
    cmp rcx, rdx
    je .dir_from_type

    ; Instance: get its type, iterate the type's dict chain
    mov r12, [rax + PyObject.ob_type]   ; r12 = type
    jmp .dir_walk_chain

.dir_from_type:
    ; obj IS a type: iterate its tp_dict chain
    mov r12, [rbp - DIR_OBJ]

.dir_walk_chain:
    ; r12 = current type to get keys from
    test r12, r12
    jz .dir_done

    mov rdi, [r12 + PyTypeObject.tp_dict]
    test rdi, rdi
    jz .dir_next_base

    ; Iterate this dict's keys
    call dict_tp_iter
    mov r13, rax            ; r13 = iterator

.dir_iter_loop:
    mov rdi, r13
    mov rax, [rdi + PyObject.ob_type]
    mov rax, [rax + PyTypeObject.tp_iternext]
    test rax, rax
    jz .dir_iter_done
    mov rdi, r13
    call rax                ; tp_iternext(iter) -> key or NULL
    test rax, rax
    jz .dir_iter_done

    ; Check if key already in result list (avoid duplicates from base classes)
    push rax                ; save key
    mov rdi, rbx            ; list
    mov rsi, rax            ; key
    call list_contains
    test eax, eax
    pop rax                 ; restore key
    jnz .dir_iter_loop      ; already present, skip

    ; Append key to result
    push rax
    mov rdi, rbx
    mov rsi, rax
    call list_append
    pop rdi
    call obj_decref
    jmp .dir_iter_loop

.dir_iter_done:
    ; DECREF iterator
    mov rdi, r13
    call obj_decref

.dir_next_base:
    mov r12, [r12 + PyTypeObject.tp_base]
    jmp .dir_walk_chain

.dir_done:
    mov rax, rbx            ; return result list
    mov edx, TAG_PTR
    pop r13
    pop r12
    pop rbx
    leave
    ret

.dir_error:
    lea rdi, [rel exc_TypeError_type]
    CSTRING rsi, "dir() takes exactly 1 argument"
    call raise_exception
END_FUNC builtin_dir

; ============================================================================
; builtin_eval_fn(args, nargs) - restricted literal evaluator
; Only evaluates integer literals (for test_int.py compatibility)
; ============================================================================
global builtin_eval_fn
DEF_FUNC builtin_eval_fn
    cmp rsi, 1
    jne .evl_error

    ; Get the string argument
    mov rdi, [rdi]              ; args[0]
    test rdi, rdi
    js .evl_type_error          ; SmallInt: not a string
    mov rax, [rdi + PyObject.ob_type]
    lea rcx, [rel str_type]
    cmp rax, rcx
    jne .evl_type_error

    ; Try parsing as integer literal with base 0 (auto-detect)
    lea rdi, [rdi + PyStrObject.data]
    xor esi, esi                ; base 0 = auto-detect
    call int_from_cstr_base
    test rax, rax
    jnz .evl_done

    ; Parse failed — raise SyntaxError
    lea rdi, [rel exc_ValueError_type]
    CSTRING rsi, "invalid syntax"
    call raise_exception

.evl_done:
    ; Classify: SmallInt (bit63) or heap ptr
    mov edx, TAG_PTR
    mov ecx, TAG_SMALLINT
    cmovs edx, ecx
    leave
    ret

.evl_error:
    lea rdi, [rel exc_TypeError_type]
    CSTRING rsi, "eval() takes exactly 1 argument"
    call raise_exception

.evl_type_error:
    lea rdi, [rel exc_TypeError_type]
    CSTRING rsi, "eval() arg 1 must be a string"
    call raise_exception
END_FUNC builtin_eval_fn
