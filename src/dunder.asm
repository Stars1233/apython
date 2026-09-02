; dunder.asm - Dunder method dispatch helpers for user-defined classes
;
; Provides lookup and call helpers for __eq__, __add__, __iter__, etc.
; Used as fallback when tp_richcompare / tp_as_number / tp_iter etc. are NULL
; on heaptype (user-defined class) objects.

%include "macros.inc"
%include "object.inc"

extern kw_names_pending

; A dunder is invoked on behalf of an operation, not as the call the user
; wrote, so it must not inherit that call's keyword names.  Without this,
; sorted(MyIterable(), reverse=True) reached __iter__ with reverse=True still
; pending and raised "unexpected keyword argument".  The leak only became
; reachable once heaptypes had real slots to dispatch through.
%macro DUNDER_KW_SAVE 1
    mov %1, [rel kw_names_pending]
    mov qword [rel kw_names_pending], 0
%endmacro

%macro DUNDER_KW_RESTORE 1
    mov [rel kw_names_pending], %1
%endmacro

extern none_singleton
extern str_from_cstr_heap
extern dict_get
extern obj_decref
extern obj_incref
extern exc_TypeError_type
extern raise_exception

;; ============================================================================
;; dunder_name_obj(rdi = const char *literal) -> rax = PyStrObject*, borrowed
;;
;; dunder_lookup used to build a fresh PyStrObject from its C string on every
;; call and free it again: ap_strlen, ap_malloc, ap_memcpy, a code-point scan to
;; set ob_length, a full string hash (the object being new, ob_hash was always
;; cold), and ap_free.  That sat behind 27 direct call sites and 131 DUNDER_*
;; macro uses -- every __add__, __iter__, __next__, __len__, __getitem__ and
;; __enter__ fallback in the interpreter.
;;
;; The names are compile-time literals in .rodata, so the pointer is stable per
;; call site and comparing pointers is enough to recognise one.  A miss interns
;; the string once and keeps it forever; the interned object then caches its own
;; ob_hash, so the dict probes stop rehashing too.  Two call sites that spell
;; the same name in different literals simply get two entries, and dict lookup
;; matches them by value regardless.
;;
;; The table never evicts.  Distinct dunder literals number a few dozen against
;; 256 slots, so the probe terminates.
;; ============================================================================
DUNDER_CACHE_SLOTS equ 256

DEF_FUNC dunder_name_obj
    push rbx
    push r12                    ; 2 pushes + frame 0 = 16
    mov rbx, rdi
    mov rax, rdi
    shr rax, 4                  ; literals are not 16-byte aligned; spread them
    and rax, DUNDER_CACHE_SLOTS - 1
    mov r12, rax

.probe:
    lea rcx, [rel dunder_cache_keys]
    mov rdx, [rcx + r12*8]
    test rdx, rdx
    jz .miss
    cmp rdx, rbx
    je .hit
    inc r12
    and r12, DUNDER_CACHE_SLOTS - 1
    jmp .probe

.hit:
    lea rcx, [rel dunder_cache_vals]
    mov rax, [rcx + r12*8]
    pop r12
    pop rbx
    leave
    ret

.miss:
    mov rdi, rbx
    call str_from_cstr_heap     ; kept for the life of the process
    lea rcx, [rel dunder_cache_keys]
    mov [rcx + r12*8], rbx
    lea rcx, [rel dunder_cache_vals]
    mov [rcx + r12*8], rax
    pop r12
    pop rbx
    leave
    ret
END_FUNC dunder_name_obj

section .bss
align 8
dunder_cache_keys: resq DUNDER_CACHE_SLOTS
dunder_cache_vals: resq DUNDER_CACHE_SLOTS

section .text

;; ============================================================================
;; dunder_lookup(PyTypeObject *type, const char *name) -> rax = Value
;;
;; Walk type->tp_base chain, looking up name in each type's tp_dict.
;; rdi = type object, rsi = C string name
;; Returns: borrowed reference to function, or NULL if not found.
;; ============================================================================
DLO_OWNER equ 8
DLO_FRAME equ 16            ; + 4 pushes = 48, 16-aligned
global dunder_lookup_owner
DEF_FUNC_BARE dunder_lookup
    xor edx, edx                ; no owner wanted
    jmp dunder_lookup_owner
END_FUNC dunder_lookup

;; ============================================================================
;; dunder_lookup_owner(PyTypeObject *type, const char *name,
;;                     PyTypeObject **owner_out) -> rax = Value
;;
;; The same walk, reporting WHERE it stopped.  The caller that needs this is
;; type_install_slots: a dunder a builtin base supplies is not a definition
;; the subclass made, and installing a generic wrapper over it is wrong --
;; type_from_parts has already handed the subclass the base's real slot by
;; pointer.  Which type's tp_dict answered is the only way to tell the two
;; apart, and the value alone cannot say.
;;
;; rdx may be 0, which is what dunder_lookup passes.
;; ============================================================================
DEF_FUNC dunder_lookup_owner, DLO_FRAME
    push rbx
    push r12
    push r13
    push r14                ; holds the origin type

    mov [rbp - DLO_OWNER], rdx
    mov rbx, rdi            ; rbx = type (walks the MRO)
    mov r14, rdi            ; r14 = origin of the walk
    mov r12, rsi            ; r12 = name C string

    ; The interned name for this literal; borrowed, so no DECREF on the way out.
    mov rdi, r12
    call dunder_name_obj
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
    V_UNPACK rax, rdx           ; dict_get returns a Value
    test edx, edx
    jnz .found

.try_base:
    MRO_NEXT rbx, r14
    jmp .walk

.found:
    mov rcx, [rbp - DLO_OWNER]
    test rcx, rcx
    jz .found_no_owner
    mov [rcx], rbx          ; the MRO entry whose tp_dict answered
.found_no_owner:
    pop r14
    pop r13
    pop r12
    pop rbx
    leave
    V_PACK rax, rdx             ; return one Value
    ret

.not_found:
    RET_NULL                ; rax=0, edx=TAG_NULL(0)

    pop r14
    pop r13
    pop r12
    pop rbx
    leave
    V_PACK rax, rdx             ; return one Value
    ret
END_FUNC dunder_lookup_owner

;; ============================================================================
;; dunder_call_1(PyObject *self, const char *name) -> (rax=payload, rdx=tag)
;;
;; Look up dunder on self's type, call with self as only arg.
;; rdi = self (heap ptr), rsi = dunder name C string
;; Returns: result fat value (rax=payload, rdx=tag), or (0, TAG_NULL) if not found.
;; ============================================================================
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
    V_UNPACK rax, rdx           ; returns a Value
    test edx, edx
    jz .not_found
    ; Guard: a dunder explicitly set to None, or any non-pointer, is not callable
    test edx, TAG_RC_BIT
    jz .not_found
    IS_NONE rax, r9
    je .not_found

    ; Call: tp_call(dunder_func, &[self], 1)
    mov r12, rax            ; r12 = dunder func
    mov rax, [r12 + PyObject.ob_type]
    mov rax, [rax + PyTypeObject.tp_call]
    test rax, rax
    jz .not_found

    sub rsp, 16             ; one Value; 16 keeps rsp aligned
    mov [rsp], rbx          ; args[0] = self
    mov rdi, r12            ; callable
    mov rsi, rsp            ; args ptr
    mov edx, 1              ; nargs
    push r15
    push r15                ; pushed twice: rsp must stay 16-byte aligned
    DUNDER_KW_SAVE r15      ; at the call, and the args pointer was taken
    call rax                ; before these, so it is unaffected
    V_UNPACK rax, rdx           ; tp_call returns a Value
    DUNDER_KW_RESTORE r15
    pop r15
    pop r15
    add rsp, 16             ; pop args
    ; rax = result payload, rdx = result tag

    pop r14
    pop r13
    pop r12
    pop rbx
    leave
    V_PACK rax, rdx             ; return one Value
    ret

.not_found:
    RET_NULL
    pop r14
    pop r13
    pop r12
    pop rbx
    leave
    V_PACK rax, rdx             ; return one Value
    ret
END_FUNC dunder_call_1

;; ============================================================================
;; dunder_call_2(PyObject *self, PyObject *other, const char *name, int other_tag)
;;   -> rax = the result Value, or a NULL one when there is no such dunder
;;
;; Look up dunder on self's type, call with (self, other).
;; rdi = self (heap ptr), rsi = other payload, rdx = dunder name, ecx = other_tag
;;
;; One Value out, packed -- not the (payload, tag) pair this said for a long
;; time after it stopped being true.  str.translate believed the comment and
;; packed the answer a second time, which is a no-op for a pointer and shifts
;; an int by V_INT_BIAS: a mapping that answered an ordinal reported
;; "character mapping must be in range(0x110000)".
;; ============================================================================
DEF_FUNC dunder_call_2
    push rbx
    push r12
    push r13
    push r14

    mov rbx, rdi            ; rbx = self
    mov r12, rsi            ; r12 = other payload
    mov r14, rcx            ; r14 = other tag

    ; Lookup dunder on self's type
    mov rdi, [rbx + PyObject.ob_type]
    mov rsi, rdx            ; name
    call dunder_lookup
    V_UNPACK rax, rdx           ; returns a Value
    test edx, edx
    jz .not_found
    ; Guard: a dunder explicitly set to None, or any non-pointer, is not callable
    test edx, TAG_RC_BIT
    jz .not_found
    IS_NONE rax, r9
    je .not_found

    ; Call: tp_call(dunder_func, &[self, other], 2)
    mov r13, rax            ; r13 = dunder func
    mov rax, [r13 + PyObject.ob_type]
    mov rax, [rax + PyTypeObject.tp_call]
    test rax, rax
    jz .not_found

    sub rsp, 16             ; 2 Values
    mov [rsp], rbx          ; args[0] = self
    V_PACK r12, r14         ; args[1] = other
    mov [rsp+8], r12
    mov rdi, r13            ; callable
    mov rsi, rsp            ; args ptr
    mov edx, 2              ; nargs
    push r15
    push r15                ; pushed twice: rsp must stay 16-byte aligned
    DUNDER_KW_SAVE r15      ; at the call, and the args pointer was taken
    call rax                ; before these, so it is unaffected
    V_UNPACK rax, rdx           ; tp_call returns a Value
    DUNDER_KW_RESTORE r15
    pop r15
    pop r15
    add rsp, 16             ; pop args
    ; rax = result payload, rdx = result tag

    pop r14
    pop r13
    pop r12
    pop rbx
    leave
    V_PACK rax, rdx             ; return one Value
    ret

.not_found:
    RET_NULL
    pop r14
    pop r13
    pop r12
    pop rbx
    leave
    V_PACK rax, rdx             ; return one Value
    ret
END_FUNC dunder_call_2

;; ============================================================================
;; dunder_call_3(PyObject *self, PyObject *arg1, PyObject *arg2, const char *name,
;;               int arg2_tag)
;;   -> (rax=payload, rdx=tag)
;;
;; Look up dunder on self's type, call with (self, arg1, arg2).
;; rdi = self (heap), rsi = arg1 (heap), rdx = arg2, rcx = dunder name,
;; r8d = arg2 tag (use TAG_PTR if arg2 is always a heap ptr).
;; Returns: result fat value (rax=payload, rdx=tag), or (0, TAG_NULL) if not found.
;; ============================================================================
DEF_FUNC dunder_call_3
    push rbx
    push r12
    push r13
    push r14
    push r15

    mov rbx, rdi            ; rbx = self
    mov r12, rsi            ; r12 = arg1
    mov r13, rdx            ; r13 = arg2
    mov r15d, r8d           ; r15d = arg2 tag

    ; Lookup dunder on self's type
    mov rdi, [rbx + PyObject.ob_type]
    mov rsi, rcx            ; name
    call dunder_lookup
    V_UNPACK rax, rdx           ; returns a Value
    test edx, edx
    jz .not_found
    ; Guard: a dunder explicitly set to None, or any non-pointer, is not callable
    test edx, TAG_RC_BIT
    jz .not_found
    IS_NONE rax, r9
    je .not_found

    ; Call: tp_call(dunder_func, &[self, arg1, arg2], 3)
    mov r14, rax            ; r14 = dunder func
    mov rax, [r14 + PyObject.ob_type]
    mov rax, [rax + PyTypeObject.tp_call]
    test rax, rax
    jz .not_found

    sub rsp, 32             ; 3 Values, rounded up to keep rsp aligned
    mov [rsp], rbx          ; args[0] = self
    mov [rsp+8], r12        ; args[1] = arg1
    V_PACK r13, r15         ; args[2] = arg2
    mov [rsp+16], r13
    mov rdi, r14            ; callable
    mov rsi, rsp            ; args ptr
    mov edx, 3              ; nargs
    push r15
    push r15                ; pushed twice: rsp must stay 16-byte aligned
    DUNDER_KW_SAVE r15      ; at the call, and the args pointer was taken
    call rax                ; before these, so it is unaffected
    V_UNPACK rax, rdx           ; tp_call returns a Value
    DUNDER_KW_RESTORE r15
    pop r15
    pop r15
    add rsp, 32             ; pop args
    ; rax = result payload, rdx = result tag

    pop r15
    pop r14
    pop r13
    pop r12
    pop rbx
    leave
    V_PACK rax, rdx             ; return one Value
    ret

.not_found:
    RET_NULL
    pop r15
    pop r14
    pop r13
    pop r12
    pop rbx
    leave
    V_PACK rax, rdx             ; return one Value
    ret
END_FUNC dunder_call_3

;; ============================================================================
;; obj_call_n(Value callable, Value *args, uint64_t nargs) -> Value, or 0
;;
;; Call anything callable: a function, a builtin, a type, or an instance of a
;; class that defines __call__.  Going through tp_call alone misses the last of
;; those -- a user class carries __call__ in its dict, and only op_call knew to
;; look there -- so a property whose getter was an operator.itemgetter reported
;; "unreadable attribute" rather than calling it.
;;
;; Returns 0 with a TypeError pending when the object is not callable.  nargs
;; is bounded because the self-prepended copy lives in this frame; every
;; descriptor use passes one or two.
;; ============================================================================
OCN_MAX equ 8

OCN_FN    equ 8
OCN_BUF   equ 32 + (OCN_MAX + 1) * 8
OCN_FRAME equ ((OCN_BUF + 15) / 16) * 16 + 8    ; + 3 pushes = 16-aligned
DEF_FUNC obj_call_n, OCN_FRAME
    push rbx
    push r12
    push r13
    mov rbx, rdi
    mov r12, rsi
    mov r13, rdx

    V_TEST_PTR rbx, rax
    ja .not_callable
    test rbx, rbx
    jz .not_callable

    mov rax, [rbx + PyObject.ob_type]
    mov rcx, [rax + PyTypeObject.tp_call]
    test rcx, rcx
    jz .try_dunder

    mov rdi, rbx
    mov rsi, r12
    mov rdx, r13
    call rcx
    jmp .ret

.try_dunder:
    ; A heaptype instance whose class defines __call__: dispatch to that with
    ; the object itself as the first argument.
    test dword [rax + PyTypeObject.tp_flags], TYPE_FLAG_HEAPTYPE
    jz .not_callable
    mov rdi, rax
    lea rsi, [rel dunder_call]
    call dunder_lookup
    V_UNPACK rax, rdx
    test edx, edx
    jz .not_callable
    test edx, TAG_RC_BIT
    jz .not_callable
    mov [rbp - OCN_FN], rax

    cmp r13, OCN_MAX
    ja .not_callable

    ; args' = [self, args...]
    lea rdi, [rbp - OCN_BUF]
    mov [rdi], rbx
    xor ecx, ecx
.copy:
    cmp rcx, r13
    jae .copied
    mov rax, [r12 + rcx*8]
    mov [rdi + rcx*8 + 8], rax
    inc rcx
    jmp .copy
.copied:
    mov rax, [rbp - OCN_FN]
    mov rcx, [rax + PyObject.ob_type]
    mov rcx, [rcx + PyTypeObject.tp_call]
    test rcx, rcx
    jz .not_callable
    mov rdi, rax
    lea rsi, [rbp - OCN_BUF]
    lea rdx, [r13 + 1]
    call rcx
    jmp .ret

.not_callable:
    RAISE exc_TypeError_type, "object is not callable"
.ret:
    pop r13
    pop r12
    pop rbx
    leave
    ret
END_FUNC obj_call_n

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
global dunder_iter
global dunder_aiter
global dunder_anext
global dunder_next
global dunder_getitem
global dunder_setitem
global dunder_contains
global dunder_len
global dunder_bool
global dunder_call
global obj_call_n
global dunder_iadd
global dunder_isub
global dunder_imul
global dunder_iand
global dunder_ifloordiv
global dunder_ilshift
global dunder_imatmul
global dunder_imod
global dunder_ior
global dunder_ipow
global dunder_irshift
global dunder_itruediv
global dunder_ixor
global dunder_rmatmul
global dunder_rand
global dunder_ror
global dunder_rxor
global dunder_rlshift
global dunder_rrshift
global dunder_repr
global dunder_str
global dunder_matmul
global dunder_get
global dunder_set
global dunder_del

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
dunder_iter:     db "__iter__", 0
dunder_next:     db "__next__", 0
dunder_aiter:    db "__aiter__", 0
dunder_anext:    db "__anext__", 0
dunder_getitem:  db "__getitem__", 0
dunder_setitem:  db "__setitem__", 0
dunder_contains: db "__contains__", 0
dunder_len:      db "__len__", 0
dunder_bool:     db "__bool__", 0
dunder_call:     db "__call__", 0
dunder_iadd:     db "__iadd__", 0
dunder_isub:     db "__isub__", 0
dunder_imul:     db "__imul__", 0
dunder_iand:     db "__iand__", 0
dunder_ifloordiv: db "__ifloordiv__", 0
dunder_ilshift:  db "__ilshift__", 0
dunder_imatmul:  db "__imatmul__", 0
dunder_imod:     db "__imod__", 0
dunder_ior:      db "__ior__", 0
dunder_ipow:     db "__ipow__", 0
dunder_irshift:  db "__irshift__", 0
dunder_itruediv: db "__itruediv__", 0
dunder_ixor:     db "__ixor__", 0
dunder_rmatmul:  db "__rmatmul__", 0
dunder_rand:     db "__rand__", 0
dunder_ror:      db "__ror__", 0
dunder_rxor:     db "__rxor__", 0
dunder_rlshift:  db "__rlshift__", 0
dunder_rrshift:  db "__rrshift__", 0
dunder_repr:     db "__repr__", 0
dunder_str:      db "__str__", 0
dunder_matmul:   db "__matmul__", 0
dunder_get:      db "__get__", 0
dunder_set:      db "__set__", 0
dunder_del:      db "__del__", 0

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
    dq dunder_rand          ; 1  = NB_AND -> __rand__
    dq dunder_rfloordiv     ; 2  = NB_FLOOR_DIVIDE -> __rfloordiv__
    dq dunder_rlshift       ; 3  = NB_LSHIFT -> __rlshift__
    dq dunder_rmatmul       ; 4  = NB_MATRIX_MULTIPLY -> __rmatmul__
    dq dunder_rmul          ; 5  = NB_MULTIPLY -> __rmul__
    dq dunder_rmod          ; 6  = NB_REMAINDER -> __rmod__
    dq dunder_ror           ; 7  = NB_OR -> __ror__
    dq dunder_rpow          ; 8  = NB_POWER -> __rpow__
    dq dunder_rrshift       ; 9  = NB_RSHIFT -> __rrshift__
    dq dunder_rsub          ; 10 = NB_SUBTRACT -> __rsub__
    dq dunder_rtruediv      ; 11 = NB_TRUE_DIVIDE -> __rtruediv__
    dq dunder_rxor          ; 12 = NB_XOR -> __rxor__

; Inplace binary op -> dunder name lookup table
; Indexed by (NB_INPLACE_* - 13), same 0-12 order as binop_dunder_table
global binop_inplace_dunder_table
align 8
binop_inplace_dunder_table:
    dq dunder_iadd           ; 0  = NB_ADD -> __iadd__
    dq dunder_iand           ; 1  = NB_AND -> __iand__
    dq dunder_ifloordiv      ; 2  = NB_FLOOR_DIVIDE -> __ifloordiv__
    dq dunder_ilshift        ; 3  = NB_LSHIFT -> __ilshift__
    dq dunder_imatmul        ; 4  = NB_MATRIX_MULTIPLY -> __imatmul__
    dq dunder_imul           ; 5  = NB_MULTIPLY -> __imul__
    dq dunder_imod           ; 6  = NB_REMAINDER -> __imod__
    dq dunder_ior            ; 7  = NB_OR -> __ior__
    dq dunder_ipow           ; 8  = NB_POWER -> __ipow__
    dq dunder_irshift        ; 9  = NB_RSHIFT -> __irshift__
    dq dunder_isub           ; 10 = NB_SUBTRACT -> __isub__
    dq dunder_itruediv       ; 11 = NB_TRUE_DIVIDE -> __itruediv__
    dq dunder_ixor           ; 12 = NB_XOR -> __ixor__
