; bool_obj.asm - Bool type, True/False singletons

%include "macros.inc"
%include "object.inc"
%include "types.inc"

extern str_from_cstr
extern __gmpz_init
extern __gmpz_set_si

; bool_repr(PyObject *self) -> PyObject*
; Returns "True" or "False" string
DEF_FUNC bool_repr
    ; Check if this is True or False
    lea rax, [rel bool_true]
    cmp rdi, rax
    je .is_true
    lea rdi, [rel bool_false_str]
    call str_from_cstr
    leave
    ret
.is_true:
    lea rdi, [rel bool_true_str]
    call str_from_cstr
    leave
    ret
END_FUNC bool_repr

; bool_hash(PyObject *self) -> int64
; True -> 1, False -> 0
DEF_FUNC_BARE bool_hash
    lea rax, [rel bool_true]
    cmp rdi, rax
    je .hash_true
    xor eax, eax           ; False -> hash 0
    ret
.hash_true:
    mov eax, 1              ; True -> hash 1
    ret
END_FUNC bool_hash

; bool_bool(PyObject *self) -> int
; True -> 1, False -> 0
DEF_FUNC_BARE bool_bool
    lea rax, [rel bool_true]
    cmp rdi, rax
    je .ret_true
    xor eax, eax
    ret
.ret_true:
    mov eax, 1
    ret
END_FUNC bool_bool

; bool_from_int(int value) -> PyObject*
; Returns True if value != 0, else False
DEF_FUNC_BARE bool_from_int
    test edi, edi
    jnz .true
    lea rax, [rel bool_false]
    ret
.true:
    lea rax, [rel bool_true]
    ret
END_FUNC bool_from_int

section .data

bool_true_str:  db "True", 0
bool_false_str: db "False", 0
bool_name_str:  db "bool", 0

; Bool number methods (only nb_bool is set)
align 8
bool_number_methods:
    dq 0                    ; nb_add (TODO: inherit from int)
    dq 0                    ; nb_subtract
    dq 0                    ; nb_multiply
    dq 0                    ; nb_remainder
    dq 0                    ; nb_divmod
    dq 0                    ; nb_power
    dq 0                    ; nb_negative
    dq 0                    ; nb_positive
    dq 0                    ; nb_absolute
    dq bool_bool            ; nb_bool
    dq 0                    ; nb_invert
    dq 0                    ; nb_lshift
    dq 0                    ; nb_rshift
    dq 0                    ; nb_and
    dq 0                    ; nb_xor
    dq 0                    ; nb_or
    dq 0                    ; nb_int
    dq 0                    ; nb_float
    dq 0                    ; nb_floor_divide
    dq 0                    ; nb_true_divide
    dq 0                    ; nb_index

; Bool type object
align 8
global bool_type
bool_type:
    dq 1                    ; ob_refcnt
    dq 0                    ; ob_type (simplified: no metatype)
    dq bool_name_str        ; tp_name
    dq PyIntObject_size     ; tp_basicsize (bool is subtype of int)
    dq 0                    ; tp_dealloc
    dq bool_repr            ; tp_repr
    dq bool_repr            ; tp_str
    dq bool_hash            ; tp_hash
    dq 0                    ; tp_call
    dq 0                    ; tp_getattr
    dq 0                    ; tp_setattr
    dq 0                    ; tp_richcompare
    dq 0                    ; tp_iter
    dq 0                    ; tp_iternext
    dq 0                    ; tp_init
    dq 0                    ; tp_new
    dq bool_number_methods  ; tp_as_number
    dq 0                    ; tp_as_sequence
    dq 0                    ; tp_as_mapping
    dq 0                    ; tp_base (should point to int_type, set later)
    dq 0                    ; tp_dict
    dq 0                    ; tp_mro
    dq 0                    ; tp_flags
    dq 0                    ; tp_bases

; True singleton - has embedded mpz_t value of 1
align 8
global bool_true
bool_true:
    dq 0x7FFFFFFFFFFFFFFF   ; ob_refcnt (immortal)
    dq bool_type            ; ob_type
    ; mpz_t inline: _mp_alloc(4 bytes), _mp_size(4 bytes), _mp_d(8 bytes)
    dd 0                    ; _mp_alloc (set by gmpz_init)
    dd 0                    ; _mp_size  (set by gmpz_set_si)
    dq 0                    ; _mp_d     (set by gmpz_init)

; False singleton - has embedded mpz_t value of 0
align 8
global bool_false
bool_false:
    dq 0x7FFFFFFFFFFFFFFF   ; ob_refcnt (immortal)
    dq bool_type            ; ob_type
    dd 0                    ; _mp_alloc
    dd 0                    ; _mp_size
    dq 0                    ; _mp_d

; bool_init() - Initialize True/False singletons' mpz values
; Must be called once at startup
section .text
DEF_FUNC bool_init

    ; Init True's mpz to 1
    lea rdi, [rel bool_true + PyIntObject.mpz]
    call __gmpz_init wrt ..plt
    lea rdi, [rel bool_true + PyIntObject.mpz]
    mov rsi, 1
    call __gmpz_set_si wrt ..plt

    ; Init False's mpz to 0
    lea rdi, [rel bool_false + PyIntObject.mpz]
    call __gmpz_init wrt ..plt
    lea rdi, [rel bool_false + PyIntObject.mpz]
    xor esi, esi
    call __gmpz_set_si wrt ..plt

    leave
    ret
END_FUNC bool_init
