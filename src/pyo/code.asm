; code_obj.asm - Code object type

%include "macros.inc"
%include "object.inc"
%include "types.inc"

extern ap_free
extern obj_decref
extern obj_dealloc
extern str_from_cstr
extern type_type

; code_dealloc(PyObject *self)
; Free code object and decref contained objects
DEF_FUNC code_dealloc
    push rbx
    push r12
    push r13
    mov rbx, rdi

    ; Free co_consts fat array: iterate elements, DECREF_VAL each, ap_free
    mov rax, [rbx + PyCodeObject.co_consts]
    test rax, rax
    jz .skip_consts
    mov r12, rax                            ; r12 = fat array base (for ap_free)
    mov r13, [rax]                          ; r13 = count
    add rax, 8                              ; rax = data start
.consts_decref_loop:
    test r13, r13
    jz .consts_decref_done
    push rax                                ; save data pointer
    mov rdi, [rax]                          ; payload
    mov rsi, [rax + 8]                      ; tag
    DECREF_VAL rdi, rsi
    pop rax
    add rax, 16
    dec r13
    jmp .consts_decref_loop
.consts_decref_done:
    mov rdi, r12
    call ap_free
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

section .data

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
    dq 0                ; tp_getattr
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
