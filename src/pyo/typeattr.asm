; typeattr.asm - the descriptors `type.__dict__` answers a class's own
; attributes through, and `type.mro()`.
;
; `C.__mro__` has worked for a long time: type_getattr special-cases the name
; before it walks anything.  What did not exist was the entry in
; `type.__dict__` that CPython answers it THROUGH, and the stdlib reaches for
; that entry directly rather than for the attribute --
;
;     _static_getmro = type.__dict__['__mro__'].__get__
;
; is inspect.py's way of reading an MRO without triggering a __getattr__, and
; it is a KeyError without one.  inspect is imported by dataclasses, pdb,
; pydoc, doctest, unittest and asyncio, so the one missing key stopped a
; seventh of the standard library.
;
; Each descriptor is a getset whose getter is a two-instruction thunk: load
; the interned name for its slot, and hand it to type_getattr, which already
; knows how to answer it.  That keeps one implementation of what `__mro__`
; means rather than two that can drift, and it is why a static type with no
; tp_mro answers here too -- the synthesis lives there.

%include "macros.inc"
%include "object.inc"
%include "value.inc"

extern type_getattr
extern type_type
extern getset_descr_new
extern str_from_cstr_heap
extern dict_set
extern obj_decref
extern list_new
extern list_append
extern exc_TypeError_type
extern raise_exception

;; The attributes exposed, in slot order.  Every one of them is a name
;; type_getattr answers from the type itself rather than from its dict.
TYA_COUNT equ 10

section .bss
;; The interned name for each slot, built once by type_dict_add_attrs.  A
;; thunk has no argument but self, so this is how it knows which it is.
tya_names: resq TYA_COUNT

section .rodata
tya_mro:           db "__mro__", 0
tya_bases:         db "__bases__", 0
tya_base:          db "__base__", 0
tya_name:          db "__name__", 0
tya_qualname:      db "__qualname__", 0
tya_module:        db "__module__", 0
tya_dict:          db "__dict__", 0
tya_basicsize:     db "__basicsize__", 0
tya_dictoffset:    db "__dictoffset__", 0
tya_weakrefoffset: db "__weakrefoffset__", 0

;; Parallel to tya_names: the C string for each slot, so the registration is
;; one loop rather than ten copies of it.
align 8
tya_cstrs:
    dq tya_mro, tya_bases, tya_base, tya_name, tya_qualname
    dq tya_module, tya_dict, tya_basicsize, tya_dictoffset, tya_weakrefoffset

section .text

;; ============================================================================
;; tya_fetch(rdi = self Value, rsi = name str) -> rax = Value, or 0 raising
;;
;; The body every thunk tail-jumps into.  `self` reaches a getset getter
;; unchecked as far as its own type goes -- getset_check_receiver only proves
;; it is an instance of the owner -- and the owner here is `type`, whose
;; instances are classes.  A class IS a type object, so the test is that its
;; own type is a metatype.
;; ============================================================================
TYF_FRAME equ 16            ; + 0 pushes = 16
DEF_FUNC_LOCAL tya_fetch, TYF_FRAME
    V_TEST_PTR rdi, rax
    ja .tyf_not_a_type
    mov rax, [rdi + PyObject.ob_type]
    test rax, rax
    jz .tyf_not_a_type
    test dword [rax + PyTypeObject.tp_flags], TYPE_FLAG_METATYPE
    jz .tyf_not_a_type
    leave
    jmp type_getattr
.tyf_not_a_type:
    RAISE exc_TypeError_type, "descriptor requires a type object"
END_FUNC tya_fetch

;; TYA_GET name, slot -- one getter, which is the whole body of a descriptor.
%macro TYA_GET 2
DEF_FUNC_BARE %1
    mov rsi, [rel tya_names + 8 * %2]
    jmp tya_fetch
END_FUNC %1
%endmacro

TYA_GET tya_get_mro,           0
TYA_GET tya_get_bases,         1
TYA_GET tya_get_base,          2
TYA_GET tya_get_name,          3
TYA_GET tya_get_qualname,      4
TYA_GET tya_get_module,        5
TYA_GET tya_get_dict,          6
TYA_GET tya_get_basicsize,     7
TYA_GET tya_get_dictoffset,    8
TYA_GET tya_get_weakrefoffset, 9

section .rodata
align 8
tya_getters:
    dq tya_get_mro, tya_get_bases, tya_get_base, tya_get_name
    dq tya_get_qualname, tya_get_module, tya_get_dict, tya_get_basicsize
    dq tya_get_dictoffset, tya_get_weakrefoffset
section .text

;; ============================================================================
;; type_dict_add_attrs(rdi = type's tp_dict) -> void
;;
;; Called while type_type's dict is being built.  Interns each name, keeps it
;; in tya_names for the thunk that will need it, and stores a read-only getset
;; under it.  gs_owner is type_type, which is what makes the repr read
;; "<attribute '__mro__' of 'type' objects>".
;; ============================================================================
TDA_DICT  equ 8
TDA_I     equ 16
TDA_NAME  equ 24
TDA_FRAME equ 32            ; + 2 pushes = 48
DEF_FUNC type_dict_add_attrs, TDA_FRAME
    push rbx
    push r12
    mov [rbp - TDA_DICT], rdi
    xor ebx, ebx
.tda_loop:
    cmp rbx, TYA_COUNT
    jge .tda_done
    lea rax, [rel tya_cstrs]
    mov rdi, [rax + rbx*8]
    call str_from_cstr_heap
    test rax, rax
    jz .tda_done
    mov [rbp - TDA_NAME], rax
    ; The slot keeps the reference the dict key would otherwise be the only
    ; owner of: a thunk reads it for the life of the process.
    lea rcx, [rel tya_names]
    mov [rcx + rbx*8], rax
    lea rcx, [rel tya_getters]
    mov rdi, [rcx + rbx*8]
    xor esi, esi                ; read-only, as CPython's are
    mov rdx, rax
    call getset_descr_new
    test rax, rax
    jz .tda_done
    mov r12, rax
    lea rcx, [rel type_type]
    mov [r12 + PyGetSetDescrObject.gs_owner], rcx
    mov rdi, [rbp - TDA_DICT]
    mov rsi, [rbp - TDA_NAME]
    mov rdx, r12
    call dict_set
    mov rdi, r12
    call obj_decref
    inc rbx
    jmp .tda_loop
.tda_done:
    pop r12
    pop rbx
    leave
    ret
END_FUNC type_dict_add_attrs

;; ============================================================================
;; type_method_mro(rdi = args Value*, rsi = nargs) -> rax = Value
;;
;; `type.mro(C)`, and `C.mro()` through the same entry -- a builtin in
;; type's dict binds to the class the way any other method binds to its
;; receiver, so both arrive here with the class as args[0].
;;
;; CPython's recomputes the linearization; this reads the one the class
;; already carries, which is the same list for every class that has one and
;; is the only answer available for a static type that has none.
;; ============================================================================
TMM_LIST  equ 8
TMM_TUPLE equ 16
TMM_FRAME equ 32            ; + 2 pushes = 48
DEF_FUNC type_method_mro, TMM_FRAME
    push rbx
    push r12
    cmp rsi, 1
    jne .tmm_arity
    mov rdi, [rdi]
    V_TEST_PTR rdi, rax
    ja .tmm_not_a_type
    mov rax, [rdi + PyObject.ob_type]
    test rax, rax
    jz .tmm_not_a_type
    test dword [rax + PyTypeObject.tp_flags], TYPE_FLAG_METATYPE
    jz .tmm_not_a_type

    lea rax, [rel tya_names]
    mov rsi, [rax]              ; slot 0 is "__mro__"
    test rsi, rsi
    jz .tmm_not_a_type
    call type_getattr
    test rax, rax
    jz .tmm_failed
    mov [rbp - TMM_TUPLE], rax

    mov rdi, [rax + PyTupleObject.ob_size]
    call list_new
    test rax, rax
    jz .tmm_drop_tuple
    mov [rbp - TMM_LIST], rax

    mov r12, [rbp - TMM_TUPLE]
    xor ebx, ebx
.tmm_copy:
    cmp rbx, [r12 + PyTupleObject.ob_size]
    jge .tmm_copied
    mov rdi, [rbp - TMM_LIST]
    mov rax, [r12 + PyTupleObject.ob_item]
    mov rsi, [rax + rbx*8]
    call list_append
    inc rbx
    jmp .tmm_copy
.tmm_copied:
    mov rdi, r12
    call obj_decref
    mov rax, [rbp - TMM_LIST]
    pop r12
    pop rbx
    leave
    ret

.tmm_drop_tuple:
    mov rdi, [rbp - TMM_TUPLE]
    call obj_decref
.tmm_failed:
    xor eax, eax
    pop r12
    pop rbx
    leave
    ret
.tmm_arity:
    RAISE exc_TypeError_type, "mro() takes no arguments"
.tmm_not_a_type:
    RAISE exc_TypeError_type, "mro() requires a type"
END_FUNC type_method_mro
