; dunder.asm - Dunder method dispatch helpers for user-defined classes
;
; Provides lookup and call helpers for __eq__, __add__, __iter__, etc.
; Used as fallback when tp_richcompare / tp_as_number / tp_iter etc. are NULL
; on heaptype (user-defined class) objects.

%include "macros.inc"
%include "object.inc"
%include "types.inc"

extern str_from_cstr
extern dict_get
extern obj_decref
extern obj_incref

; ---------------------------------------------------------------------------
; dunder_lookup(PyTypeObject *type, const char *name) -> PyObject*
;
; Walk type->tp_base chain, looking up name in each type's tp_dict.
; rdi = type object, rsi = C string name
; Returns: borrowed reference to function, or NULL if not found.
; ---------------------------------------------------------------------------
DEF_FUNC dunder_lookup
    push rbx
    push r12
    push r13
    push r14                ; alignment

    mov rbx, rdi            ; rbx = type (walks chain)
    mov r12, rsi            ; r12 = name C string

    ; Create PyStrObject from C string for dict lookup
    mov rdi, r12
    call str_from_cstr
    mov r13, rax            ; r13 = name string object

.walk:
    test rbx, rbx
    jz .not_found

    ; Check tp_dict
    mov rdi, [rbx + PyTypeObject.tp_dict]
    test rdi, rdi
    jz .try_base

    mov rsi, r13
    call dict_get           ; dict_get(tp_dict, name_str) -> borrowed ref
    test rax, rax
    jnz .found

.try_base:
    mov rbx, [rbx + PyTypeObject.tp_base]
    jmp .walk

.found:
    push rax                ; save result
    mov rdi, r13
    call obj_decref         ; DECREF name string
    pop rax

    pop r14
    pop r13
    pop r12
    pop rbx
    leave
    ret

.not_found:
    mov rdi, r13
    call obj_decref         ; DECREF name string
    xor eax, eax            ; return NULL

    pop r14
    pop r13
    pop r12
    pop rbx
    leave
    ret
END_FUNC dunder_lookup

; ---------------------------------------------------------------------------
; dunder_call_1(PyObject *self, const char *name) -> PyObject*
;
; Look up dunder on self's type, call with self as only arg.
; rdi = self, rsi = dunder name C string
; Returns: result object, or NULL if dunder not found.
; ---------------------------------------------------------------------------
DEF_FUNC dunder_call_1
    push rbx
    push r12
    push r13
    push r14                ; alignment

    mov rbx, rdi            ; rbx = self

    ; Lookup dunder on self's type
    mov rdi, [rbx + PyObject.ob_type]
    ; rsi = name already set
    call dunder_lookup
    test rax, rax
    jz .not_found

    ; Call: tp_call(dunder_func, &[self], 1)
    mov r12, rax            ; r12 = dunder func
    mov rax, [r12 + PyObject.ob_type]
    mov rax, [rax + PyTypeObject.tp_call]
    test rax, rax
    jz .not_found

    push rbx                ; args[0] = self
    mov rdi, r12            ; callable
    mov rsi, rsp            ; args ptr
    mov edx, 1              ; nargs
    call rax
    add rsp, 8              ; pop args
    ; rax = result

    pop r14
    pop r13
    pop r12
    pop rbx
    leave
    ret

.not_found:
    xor eax, eax
    pop r14
    pop r13
    pop r12
    pop rbx
    leave
    ret
END_FUNC dunder_call_1

; ---------------------------------------------------------------------------
; dunder_call_2(PyObject *self, PyObject *other, const char *name) -> PyObject*
;
; Look up dunder on self's type, call with (self, other).
; rdi = self, rsi = other, rdx = dunder name C string
; Returns: result object, or NULL if dunder not found.
; ---------------------------------------------------------------------------
DEF_FUNC dunder_call_2
    push rbx
    push r12
    push r13
    push r14                ; alignment

    mov rbx, rdi            ; rbx = self
    mov r12, rsi            ; r12 = other

    ; Lookup dunder on self's type
    mov rdi, [rbx + PyObject.ob_type]
    mov rsi, rdx            ; name
    call dunder_lookup
    test rax, rax
    jz .not_found

    ; Call: tp_call(dunder_func, &[self, other], 2)
    mov r13, rax            ; r13 = dunder func
    mov rax, [r13 + PyObject.ob_type]
    mov rax, [rax + PyTypeObject.tp_call]
    test rax, rax
    jz .not_found

    push r12                ; args[1] = other
    push rbx                ; args[0] = self
    mov rdi, r13            ; callable
    mov rsi, rsp            ; args ptr
    mov edx, 2              ; nargs
    call rax
    add rsp, 16             ; pop args
    ; rax = result

    pop r14
    pop r13
    pop r12
    pop rbx
    leave
    ret

.not_found:
    xor eax, eax
    pop r14
    pop r13
    pop r12
    pop rbx
    leave
    ret
END_FUNC dunder_call_2

section .data
; Pre-defined dunder name strings (C strings for convenience)
global dunder_eq
global dunder_ne
global dunder_lt
global dunder_le
global dunder_gt
global dunder_ge
global dunder_add
global dunder_radd
global dunder_sub
global dunder_rsub
global dunder_mul
global dunder_rmul
global dunder_truediv
global dunder_rtruediv
global dunder_floordiv
global dunder_rfloordiv
global dunder_mod
global dunder_rmod
global dunder_pow
global dunder_rpow
global dunder_and
global dunder_or
global dunder_xor
global dunder_lshift
global dunder_rshift
global dunder_neg
global dunder_iter
global dunder_next
global dunder_getitem
global dunder_setitem
global dunder_delitem
global dunder_contains
global dunder_len
global dunder_bool
global dunder_call
global dunder_hash
global dunder_iadd
global dunder_isub
global dunder_imul
global dunder_repr
global dunder_str
global dunder_matmul

dunder_eq:       db "__eq__", 0
dunder_ne:       db "__ne__", 0
dunder_lt:       db "__lt__", 0
dunder_le:       db "__le__", 0
dunder_gt:       db "__gt__", 0
dunder_ge:       db "__ge__", 0
dunder_add:      db "__add__", 0
dunder_radd:     db "__radd__", 0
dunder_sub:      db "__sub__", 0
dunder_rsub:     db "__rsub__", 0
dunder_mul:      db "__mul__", 0
dunder_rmul:     db "__rmul__", 0
dunder_truediv:  db "__truediv__", 0
dunder_rtruediv: db "__rtruediv__", 0
dunder_floordiv: db "__floordiv__", 0
dunder_rfloordiv: db "__rfloordiv__", 0
dunder_mod:      db "__mod__", 0
dunder_rmod:     db "__rmod__", 0
dunder_pow:      db "__pow__", 0
dunder_rpow:     db "__rpow__", 0
dunder_and:      db "__and__", 0
dunder_or:       db "__or__", 0
dunder_xor:      db "__xor__", 0
dunder_lshift:   db "__lshift__", 0
dunder_rshift:   db "__rshift__", 0
dunder_neg:      db "__neg__", 0
dunder_iter:     db "__iter__", 0
dunder_next:     db "__next__", 0
dunder_getitem:  db "__getitem__", 0
dunder_setitem:  db "__setitem__", 0
dunder_delitem:  db "__delitem__", 0
dunder_contains: db "__contains__", 0
dunder_len:      db "__len__", 0
dunder_bool:     db "__bool__", 0
dunder_call:     db "__call__", 0
dunder_hash:     db "__hash__", 0
dunder_iadd:     db "__iadd__", 0
dunder_isub:     db "__isub__", 0
dunder_imul:     db "__imul__", 0
dunder_repr:     db "__repr__", 0
dunder_str:      db "__str__", 0
dunder_matmul:   db "__matmul__", 0

; Compare op -> dunder name lookup table
global cmp_dunder_table
align 8
cmp_dunder_table:
    dq dunder_lt            ; 0 = PY_LT
    dq dunder_le            ; 1 = PY_LE
    dq dunder_eq            ; 2 = PY_EQ
    dq dunder_ne            ; 3 = PY_NE
    dq dunder_gt            ; 4 = PY_GT
    dq dunder_ge            ; 5 = PY_GE

; Binary op -> dunder name lookup table (indexed by NB_* code)
; Covers NB_ADD(0) through NB_XOR(12)
global binop_dunder_table
align 8
binop_dunder_table:
    dq dunder_add           ; 0  = NB_ADD
    dq dunder_and           ; 1  = NB_AND
    dq dunder_floordiv      ; 2  = NB_FLOOR_DIVIDE
    dq dunder_lshift        ; 3  = NB_LSHIFT
    dq dunder_matmul        ; 4  = NB_MATRIX_MULTIPLY
    dq dunder_mul           ; 5  = NB_MULTIPLY
    dq dunder_mod           ; 6  = NB_REMAINDER
    dq dunder_or            ; 7  = NB_OR
    dq dunder_pow           ; 8  = NB_POWER
    dq dunder_rshift        ; 9  = NB_RSHIFT
    dq dunder_sub           ; 10 = NB_SUBTRACT
    dq dunder_truediv       ; 11 = NB_TRUE_DIVIDE
    dq dunder_xor           ; 12 = NB_XOR

; Reflected binary op -> dunder name lookup table
global binop_rdunder_table
align 8
binop_rdunder_table:
    dq dunder_radd          ; 0  = NB_ADD -> __radd__
    dq 0                    ; 1  = NB_AND (no __rand__)
    dq dunder_rfloordiv     ; 2  = NB_FLOOR_DIVIDE -> __rfloordiv__
    dq 0                    ; 3  = NB_LSHIFT (no __rlshift__)
    dq 0                    ; 4  = NB_MATRIX_MULTIPLY (no __rmatmul__)
    dq dunder_rmul          ; 5  = NB_MULTIPLY -> __rmul__
    dq dunder_rmod          ; 6  = NB_REMAINDER -> __rmod__
    dq 0                    ; 7  = NB_OR (no __ror__)
    dq dunder_rpow          ; 8  = NB_POWER -> __rpow__
    dq 0                    ; 9  = NB_RSHIFT (no __rrshift__)
    dq dunder_rsub          ; 10 = NB_SUBTRACT -> __rsub__
    dq dunder_rtruediv      ; 11 = NB_TRUE_DIVIDE -> __rtruediv__
    dq 0                    ; 12 = NB_XOR (no __rxor__)
