; bool_obj.asm - Bool type, True/False singletons

%include "macros.inc"
%include "object.inc"
%include "types.inc"

extern str_from_cstr
extern __gmpz_init
extern __gmpz_set_si
extern int_add
extern int_sub
extern int_mul
extern int_mod
extern int_power
extern int_neg
extern int_invert
extern int_lshift
extern int_rshift
extern int_floordiv
extern int_true_divide
extern int_and
extern int_or
extern int_xor
extern int_type
extern int_compare
extern type_type
extern none_singleton

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

;; ============================================================================
;; Bool-specific bitwise: return bool when both args bool, else delegate to int
;; Calling convention: rdi=left_payload, edx=left_tag, rsi=right_payload, ecx=right_tag
;; ============================================================================

; bool_and(rdi=left, rsi=right, edx=left_tag, ecx=right_tag)
DEF_FUNC_BARE bool_and
    cmp edx, TAG_BOOL
    jne .delegate
    cmp ecx, TAG_BOOL
    jne .delegate
    ; Both TAG_BOOL: payload is 0 or 1
    and rdi, rsi
    mov rax, rdi
    mov edx, TAG_BOOL
    ret
.delegate:
    jmp int_and
END_FUNC bool_and

; bool_or(rdi=left, rsi=right, edx=left_tag, ecx=right_tag)
DEF_FUNC_BARE bool_or
    cmp edx, TAG_BOOL
    jne .delegate
    cmp ecx, TAG_BOOL
    jne .delegate
    ; Both TAG_BOOL
    or rdi, rsi
    mov rax, rdi
    mov edx, TAG_BOOL
    ret
.delegate:
    jmp int_or
END_FUNC bool_or

; bool_xor(rdi=left, rsi=right, edx=left_tag, ecx=right_tag)
DEF_FUNC_BARE bool_xor
    cmp edx, TAG_BOOL
    jne .delegate
    cmp ecx, TAG_BOOL
    jne .delegate
    ; Both TAG_BOOL
    xor rdi, rsi
    mov rax, rdi
    mov edx, TAG_BOOL
    ret
.delegate:
    jmp int_xor
END_FUNC bool_xor

;; ============================================================================
;; Bool unary: +True -> 1 (int), abs(True) -> 1 (int)
;; For TAG_BOOL, payload is already 0 or 1, just change tag to SmallInt
;; Calling convention for unary: rdi=payload, edx=tag (but ignored since we
;; know we're called from bool's number methods, so tag is TAG_BOOL)
;; ============================================================================

; bool_positive: +False -> 0, +True -> 1 (as SmallInt)
DEF_FUNC_BARE bool_positive
    mov rax, rdi           ; payload (0 or 1)
    RET_TAG_SMALLINT
    ret
END_FUNC bool_positive

; bool_absolute: abs(False) -> 0, abs(True) -> 1 (as SmallInt)
DEF_FUNC_BARE bool_absolute
    mov rax, rdi           ; payload (0 or 1), already non-negative
    RET_TAG_SMALLINT
    ret
END_FUNC bool_absolute

;; ============================================================================
;; bool_getattr(self, name_str) -> (rax=payload, edx=tag)
;; Handles .real and .imag for bool; returns SmallInt
;; rdi = self (either TAG_BOOL payload 0/1, or TAG_PTR bool singleton ptr)
;; rsi = name string
;; ============================================================================
DEF_FUNC bool_getattr
    push rbx
    push r12
    mov rbx, rdi           ; self (payload or ptr)
    mov r12, rsi           ; name string

    ; Compare name against "real"
    lea rdi, [r12 + PyStrObject.data]
    CSTRING rsi, "real"
    extern ap_strcmp
    call ap_strcmp
    test eax, eax
    jz .real

    ; Compare name against "imag"
    lea rdi, [r12 + PyStrObject.data]
    CSTRING rsi, "imag"
    call ap_strcmp
    test eax, eax
    jz .imag

    ; Not real/imag — return NULL (attr not found)
    RET_NULL
    pop r12
    pop rbx
    leave
    ret

.real:
    ; True.real -> 1, False.real -> 0 (as SmallInt)
    ; rbx is either 0/1 (TAG_BOOL) or ptr to bool_true/bool_false (TAG_PTR)
    ; Detect: if rbx <= 1, it's TAG_BOOL payload; else it's a pointer
    cmp rbx, 1
    jbe .real_tag_bool
    ; TAG_PTR: compare with bool_true singleton
    lea rax, [rel bool_true]
    cmp rbx, rax
    je .real_one
    xor eax, eax              ; False.real = 0
    RET_TAG_SMALLINT
    pop r12
    pop rbx
    leave
    ret
.real_one:
    mov eax, 1
    RET_TAG_SMALLINT
    pop r12
    pop rbx
    leave
    ret
.real_tag_bool:
    mov rax, rbx               ; 0 or 1
    RET_TAG_SMALLINT
    pop r12
    pop rbx
    leave
    ret

.imag:
    ; True.imag -> 0, False.imag -> 0 (as SmallInt)
    xor eax, eax
    RET_TAG_SMALLINT
    pop r12
    pop rbx
    leave
    ret
END_FUNC bool_getattr

section .data

bool_true_str:  db "True", 0
bool_false_str: db "False", 0
bool_name_str:  db "bool", 0

; Bool number methods
align 8
bool_number_methods:
    dq int_add              ; nb_add (inherited from int)
    dq int_sub              ; nb_subtract
    dq int_mul              ; nb_multiply
    dq int_mod              ; nb_remainder
    dq 0                    ; nb_divmod
    dq int_power            ; nb_power
    dq int_neg              ; nb_negative
    dq bool_positive        ; nb_positive
    dq bool_absolute        ; nb_absolute
    dq bool_bool            ; nb_bool
    dq int_invert           ; nb_invert
    dq int_lshift           ; nb_lshift
    dq int_rshift           ; nb_rshift
    dq bool_and             ; nb_and
    dq bool_xor             ; nb_xor
    dq bool_or              ; nb_or
    dq 0                    ; nb_int
    dq 0                    ; nb_float
    dq int_floordiv         ; nb_floor_divide
    dq int_true_divide      ; nb_true_divide
    dq 0                    ; nb_index

; Bool type object
align 8
global bool_type
bool_type:
    dq 1                    ; ob_refcnt
    dq type_type            ; ob_type
    dq bool_name_str        ; tp_name
    dq PyIntObject_size     ; tp_basicsize (bool is subtype of int)
    dq 0                    ; tp_dealloc
    dq bool_repr            ; tp_repr
    dq bool_repr            ; tp_str
    dq bool_hash            ; tp_hash
    dq 0                    ; tp_call
    dq bool_getattr         ; tp_getattr
    dq 0                    ; tp_setattr
    dq int_compare          ; tp_richcompare (inherit from int)
    dq 0                    ; tp_iter
    dq 0                    ; tp_iternext
    dq 0                    ; tp_init
    dq 0                    ; tp_new
    dq bool_number_methods  ; tp_as_number
    dq 0                    ; tp_as_sequence
    dq 0                    ; tp_as_mapping
    dq 0                    ; tp_base (set to int_type in bool_init)
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

; bool_init() - Initialize True/False singletons' mpz values and set tp_base
; Must be called once at startup
section .text
DEF_FUNC bool_init

    ; Set bool_type.tp_base = int_type
    lea rax, [rel int_type]
    mov [rel bool_type + PyTypeObject.tp_base], rax

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
