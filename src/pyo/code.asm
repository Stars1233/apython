; code_obj.asm - Code object type

%include "macros.inc"
%include "object.inc"
%include "types.inc"

extern ap_free
extern ap_strcmp
extern obj_decref
extern obj_dealloc
extern obj_incref
extern str_from_cstr
extern type_type

; code_dealloc(PyObject *self)
; Free code object and decref contained objects
DEF_FUNC code_dealloc
    push rbx
    push r12
    push r13
    mov rbx, rdi

    ; co_consts is a fat tuple — just DECREF it (tuple_dealloc handles elements)
    mov rdi, [rbx + PyCodeObject.co_consts]
    test rdi, rdi
    jz .skip_consts
    call obj_decref
.skip_consts:

    ; DECREF co_names
    mov rdi, [rbx + PyCodeObject.co_names]
    test rdi, rdi
    jz .skip_names
    call obj_decref
.skip_names:

    ; DECREF co_localsplusnames
    mov rdi, [rbx + PyCodeObject.co_localsplusnames]
    test rdi, rdi
    jz .skip_locals
    call obj_decref
.skip_locals:

    ; DECREF co_localspluskinds
    mov rdi, [rbx + PyCodeObject.co_localspluskinds]
    test rdi, rdi
    jz .skip_kinds
    call obj_decref
.skip_kinds:

    ; DECREF co_filename
    mov rdi, [rbx + PyCodeObject.co_filename]
    test rdi, rdi
    jz .skip_filename
    call obj_decref
.skip_filename:

    ; DECREF co_name
    mov rdi, [rbx + PyCodeObject.co_name]
    test rdi, rdi
    jz .skip_name
    call obj_decref
.skip_name:

    ; DECREF co_qualname
    mov rdi, [rbx + PyCodeObject.co_qualname]
    test rdi, rdi
    jz .skip_qualname
    call obj_decref
.skip_qualname:

    ; DECREF co_exceptiontable
    mov rdi, [rbx + PyCodeObject.co_exceptiontable]
    test rdi, rdi
    jz .skip_exc
    call obj_decref
.skip_exc:

    ; Free the code object itself
    mov rdi, rbx
    call ap_free

    pop r13
    pop r12
    pop rbx
    leave
    ret
END_FUNC code_dealloc

; code_repr(PyObject *self) -> PyStrObject*
DEF_FUNC_BARE code_repr
    lea rdi, [rel code_repr_str]
    jmp str_from_cstr
END_FUNC code_repr

; code_getattr(PyCodeObject *self, PyObject *name) -> (rax, edx) or NULL
; rdi = code object, rsi = name string
DEF_FUNC code_getattr
    push rbx
    push r12

    mov rbx, rdi            ; rbx = code
    mov r12, rsi            ; r12 = name

    ; Check for co_kwonlyargcount
    lea rdi, [rel co_attr_kwonlyargcount]
    lea rsi, [r12 + PyStrObject.data]
    call ap_strcmp
    test eax, eax
    jz .return_kwonlyargcount

    ; Check for co_argcount
    lea rdi, [rel co_attr_argcount]
    lea rsi, [r12 + PyStrObject.data]
    call ap_strcmp
    test eax, eax
    jz .return_argcount

    ; Check for co_varnames (return co_localsplusnames)
    lea rdi, [rel co_attr_varnames]
    lea rsi, [r12 + PyStrObject.data]
    call ap_strcmp
    test eax, eax
    jz .return_varnames

    ; Not found
    RET_NULL
    pop r12
    pop rbx
    leave
    ret

.return_kwonlyargcount:
    movsxd rax, dword [rbx + PyCodeObject.co_kwonlyargcount]
    mov edx, TAG_SMALLINT
    pop r12
    pop rbx
    leave
    ret

.return_argcount:
    movsxd rax, dword [rbx + PyCodeObject.co_argcount]
    mov edx, TAG_SMALLINT
    pop r12
    pop rbx
    leave
    ret

.return_varnames:
    mov rax, [rbx + PyCodeObject.co_localsplusnames]
    test rax, rax
    jz .return_none
    INCREF rax
    mov edx, TAG_PTR
    pop r12
    pop rbx
    leave
    ret

.return_none:
    xor eax, eax
    mov edx, TAG_NONE
    pop r12
    pop rbx
    leave
    ret
END_FUNC code_getattr

section .data

co_attr_kwonlyargcount: db "co_kwonlyargcount", 0
co_attr_argcount:       db "co_argcount", 0
co_attr_varnames:       db "co_varnames", 0
code_repr_str: db "<code object>", 0
code_type_name: db "code", 0

; code type object
align 8
global code_type
code_type:
    dq 1                ; ob_refcnt
    dq type_type        ; ob_type
    dq code_type_name   ; tp_name
    dq PyCodeObject_size ; tp_basicsize
    dq code_dealloc     ; tp_dealloc
    dq code_repr        ; tp_repr
    dq code_repr        ; tp_str
    dq 0                ; tp_hash
    dq 0                ; tp_call
    dq code_getattr     ; tp_getattr
    dq 0                ; tp_setattr
    dq 0                ; tp_richcompare
    dq 0                ; tp_iter
    dq 0                ; tp_iternext
    dq 0                ; tp_init
    dq 0                ; tp_new
    dq 0                ; tp_as_number
    dq 0                ; tp_as_sequence
    dq 0                ; tp_as_mapping
    dq 0                ; tp_base
    dq 0                ; tp_dict
    dq 0                ; tp_mro
    dq 0                ; tp_flags
    dq 0                ; tp_bases
