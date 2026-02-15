; cell.asm - PyCellObject implementation for closures
;
; PyCellObject layout:
;   +0  ob_refcnt (8 bytes)
;   +8  ob_type   (8 bytes)
;   +16 ob_ref    (8 bytes: ptr to contained object or NULL)
;   Total: 24 bytes

%include "macros.inc"
%include "object.inc"
%include "types.inc"

extern ap_malloc
extern ap_free
extern obj_incref
extern obj_dealloc
extern str_from_cstr
extern type_type

;; ============================================================================
;; cell_new(PyObject *obj) -> PyCellObject*
;; Create a new cell containing obj (may be NULL for empty cell).
;; If obj is non-NULL, INCREFs it.
;; ============================================================================
DEF_FUNC cell_new
    push rbx
    mov rbx, rdi               ; save obj

    mov edi, PyCellObject_size
    call ap_malloc
    ; rax = new cell

    mov qword [rax + PyObject.ob_refcnt], 1
    lea rcx, [rel cell_type]
    mov [rax + PyObject.ob_type], rcx
    mov [rax + PyCellObject.ob_ref], rbx

    ; INCREF obj if non-NULL
    test rbx, rbx
    jz .done
    push rax
    mov rdi, rbx
    call obj_incref
    pop rax

.done:
    pop rbx
    leave
    ret
END_FUNC cell_new

;; ============================================================================
;; cell_get(PyCellObject *cell) -> PyObject*
;; Returns the contained object (may be NULL). Does NOT INCREF.
;; ============================================================================
DEF_FUNC_BARE cell_get
    mov rax, [rdi + PyCellObject.ob_ref]
    ret
END_FUNC cell_get

;; ============================================================================
;; cell_set(PyCellObject *cell, PyObject *obj)
;; Sets the contained object, DECREFs old, INCREFs new.
;; ============================================================================
DEF_FUNC cell_set
    push rbx
    push r12

    mov rbx, rdi               ; cell
    mov r12, rsi               ; new obj

    ; INCREF new obj if non-NULL
    test r12, r12
    jz .skip_incref
    mov rdi, r12
    call obj_incref
.skip_incref:

    ; DECREF old obj if non-NULL
    mov rdi, [rbx + PyCellObject.ob_ref]
    test rdi, rdi
    jz .skip_decref
    DECREF_REG rdi
.skip_decref:

    ; Store new obj
    mov [rbx + PyCellObject.ob_ref], r12

    pop r12
    pop rbx
    leave
    ret
END_FUNC cell_set

;; ============================================================================
;; cell_dealloc(PyCellObject *self)
;; ============================================================================
DEF_FUNC cell_dealloc
    push rbx
    mov rbx, rdi

    ; DECREF contained object if non-NULL
    mov rdi, [rbx + PyCellObject.ob_ref]
    test rdi, rdi
    jz .free
    DECREF_REG rdi

.free:
    mov rdi, rbx
    call ap_free

    pop rbx
    leave
    ret
END_FUNC cell_dealloc

;; ============================================================================
;; cell_repr(PyCellObject *self) -> PyStrObject*
;; ============================================================================
DEF_FUNC_BARE cell_repr
    lea rdi, [rel cell_repr_str]
    jmp str_from_cstr
END_FUNC cell_repr

;; ============================================================================
;; Data
;; ============================================================================
section .data

cell_name_str: db "cell", 0
cell_repr_str: db "<cell>", 0

align 8
global cell_type
cell_type:
    dq 1                      ; ob_refcnt (immortal)
    dq type_type              ; ob_type
    dq cell_name_str          ; tp_name
    dq PyCellObject_size      ; tp_basicsize
    dq cell_dealloc           ; tp_dealloc
    dq cell_repr              ; tp_repr
    dq cell_repr              ; tp_str
    dq 0                      ; tp_hash
    dq 0                      ; tp_call
    dq 0                      ; tp_getattr
    dq 0                      ; tp_setattr
    dq 0                      ; tp_richcompare
    dq 0                      ; tp_iter
    dq 0                      ; tp_iternext
    dq 0                      ; tp_init
    dq 0                      ; tp_new
    dq 0                      ; tp_as_number
    dq 0                      ; tp_as_sequence
    dq 0                      ; tp_as_mapping
    dq 0                      ; tp_base
    dq 0                      ; tp_dict
    dq 0                      ; tp_mro
    dq 0                      ; tp_flags
    dq 0                      ; tp_bases
