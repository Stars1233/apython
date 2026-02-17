; cell.asm - PyCellObject implementation for closures
;
; PyCellObject layout:
;   +0  ob_refcnt   (8 bytes)
;   +8  ob_type     (8 bytes)
;   +16 ob_ref      (8 bytes: contained value payload or 0 if empty)
;   +24 ob_ref_tag  (8 bytes: contained value tag, TAG_NULL if empty)
;   Total: 32 bytes

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
    push r12
    mov rbx, rdi               ; save payload
    mov r12, rsi               ; save tag

    mov edi, PyCellObject_size
    call ap_malloc
    ; rax = new cell

    mov qword [rax + PyObject.ob_refcnt], 1
    lea rcx, [rel cell_type]
    mov [rax + PyObject.ob_type], rcx
    mov [rax + PyCellObject.ob_ref], rbx
    mov [rax + PyCellObject.ob_ref_tag], r12

    ; INCREF value if refcounted (tag-aware)
    push rax
    INCREF_VAL rbx, r12
    pop rax

.done:
    pop r12
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
    mov rdx, [rdi + PyCellObject.ob_ref_tag]
    ret
END_FUNC cell_get

;; ============================================================================
;; cell_set(PyCellObject *cell, PyObject *obj)
;; Sets the contained object, DECREFs old, INCREFs new.
;; ============================================================================
DEF_FUNC cell_set
    push rbx
    push r12
    push r13

    mov rbx, rdi               ; cell
    mov r12, rsi               ; new payload
    mov r13, rdx               ; new tag

    ; INCREF new value (tag-aware)
    INCREF_VAL r12, r13

    ; DECREF old value (tag-aware)
    mov rdi, [rbx + PyCellObject.ob_ref]
    mov rsi, [rbx + PyCellObject.ob_ref_tag]
    DECREF_VAL rdi, rsi

    ; Store new value + tag
    mov [rbx + PyCellObject.ob_ref], r12
    mov [rbx + PyCellObject.ob_ref_tag], r13

    pop r13
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

    ; DECREF contained value (tag-aware)
    mov rdi, [rbx + PyCellObject.ob_ref]
    mov rsi, [rbx + PyCellObject.ob_ref_tag]
    DECREF_VAL rdi, rsi

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
