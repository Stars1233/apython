; module.asm - Module type for the import system
; PyModuleObject: name + dict

%include "macros.inc"
%include "object.inc"
%include "types.inc"

extern ap_malloc
extern ap_free
extern obj_decref
extern obj_dealloc
extern obj_incref
extern str_from_cstr
extern str_type
extern dict_new
extern dict_get
extern dict_set
extern type_type
extern none_singleton
extern ap_strcmp

; ============================================================================
; module_new(PyObject *name_str, PyObject *dict) -> PyModuleObject*
; Create a new module with given name and dict
; If dict is NULL, creates a new empty dict
; ============================================================================
DEF_FUNC module_new
    push rbx
    push r12

    mov rbx, rdi                ; name_str
    mov r12, rsi                ; dict (or NULL)

    ; Create dict if NULL
    test r12, r12
    jnz .have_dict
    call dict_new
    mov r12, rax
    jmp .alloc
.have_dict:
    ; INCREF the dict (module holds a reference)
    mov rdi, r12
    call obj_incref

.alloc:
    ; Allocate PyModuleObject
    mov edi, PyModuleObject_size
    call ap_malloc

    ; Fill header
    mov qword [rax + PyObject.ob_refcnt], 1
    lea rcx, [rel module_type]
    mov [rax + PyObject.ob_type], rcx

    ; INCREF name
    push rax
    mov rdi, rbx
    call obj_incref
    pop rax

    ; Fill module fields
    mov [rax + PyModuleObject.mod_name], rbx
    mov [rax + PyModuleObject.mod_dict], r12

    pop r12
    pop rbx
    leave
    ret
END_FUNC module_new

; ============================================================================
; module_dealloc(PyObject *self)
; ============================================================================
DEF_FUNC_LOCAL module_dealloc
    push rbx
    mov rbx, rdi

    ; XDECREF name
    mov rdi, [rbx + PyModuleObject.mod_name]
    test rdi, rdi
    jz .no_name
    call obj_decref
.no_name:
    ; XDECREF dict
    mov rdi, [rbx + PyModuleObject.mod_dict]
    test rdi, rdi
    jz .no_dict
    call obj_decref
.no_dict:
    mov rdi, rbx
    call ap_free

    pop rbx
    leave
    ret
END_FUNC module_dealloc

; ============================================================================
; module_getattr(PyObject *self, PyObject *name_str) -> PyObject*
; Look up attribute in module's dict
; ============================================================================
DEF_FUNC module_getattr
    push rbx
    push r12
    mov rbx, rdi                ; self
    mov r12, rsi                ; save name_str

    ; Check for __dict__ special attribute
    lea rdi, [r12 + PyStrObject.data]
    lea rsi, [rel ma_dunder_dict]
    call ap_strcmp
    test eax, eax
    jnz .normal_lookup

    ; Return the module's dict directly (always a heap object)
    mov rax, [rbx + PyModuleObject.mod_dict]
    test rax, rax
    jz .not_found
    inc qword [rax + PyObject.ob_refcnt]
    mov edx, TAG_PTR
    pop r12
    pop rbx
    leave
    ret

.normal_lookup:
    ; dict_get(mod_dict, name_str)
    mov rdi, [rbx + PyModuleObject.mod_dict]
    mov rsi, r12
    mov edx, TAG_PTR
    call dict_get

    ; INCREF if found (dict_get returns borrowed ref)
    test edx, edx
    jz .not_found
    INCREF_VAL rax, rdx         ; tag-aware: skip for SmallInt
    pop r12
    pop rbx
    leave
    ret

.not_found:
    RET_NULL
    pop r12
    pop rbx
    leave
    ret
END_FUNC module_getattr

; ============================================================================
; module_setattr(PyObject *self, PyObject *name_str, PyObject *value) -> int
; Set attribute in module's dict
; ============================================================================
DEF_FUNC module_setattr
    ; dict_set(mod_dict, name, value, value_tag, key_tag)
    mov rax, rdi                ; self
    mov rdi, [rax + PyModuleObject.mod_dict]
    ; rsi = name_str, rdx = value (already in place)
    ; ecx = value_tag (from caller, already in place)
    mov r8d, TAG_PTR            ; key_tag (name is always heap string)
    call dict_set
    xor eax, eax               ; return 0 (success)
    leave
    ret
END_FUNC module_setattr

; ============================================================================
; module_repr(PyObject *self) -> PyObject*
; Returns "<module 'name'>"
; Simplified: just returns the name
; ============================================================================
DEF_FUNC module_repr
    mov rax, [rdi + PyModuleObject.mod_name]
    test rax, rax
    jz .fallback
    inc qword [rax + PyObject.ob_refcnt]
    leave
    ret
.fallback:
    lea rdi, [rel module_repr_str]
    call str_from_cstr
    leave
    ret
END_FUNC module_repr

; ============================================================================
; Data
; ============================================================================
section .rodata
module_repr_str: db "<module>", 0
module_type_name: db "module", 0
ma_dunder_dict: db "__dict__", 0

section .data
align 8
global module_type
module_type:
    dq 1                        ; ob_refcnt (immortal)
    dq type_type                ; ob_type
    dq module_type_name         ; tp_name
    dq PyModuleObject_size      ; tp_basicsize
    dq module_dealloc           ; tp_dealloc
    dq module_repr              ; tp_repr
    dq module_repr              ; tp_str
    dq 0                        ; tp_hash
    dq 0                        ; tp_call
    dq module_getattr           ; tp_getattr
    dq module_setattr           ; tp_setattr
    dq 0                        ; tp_richcompare
    dq 0                        ; tp_iter
    dq 0                        ; tp_iternext
    dq 0                        ; tp_init
    dq 0                        ; tp_new
    dq 0                        ; tp_as_number
    dq 0                        ; tp_as_sequence
    dq 0                        ; tp_as_mapping
    dq 0                        ; tp_base
    dq 0                        ; tp_dict
    dq 0                        ; tp_mro
    dq 0                        ; tp_flags
    dq 0                        ; tp_bases
