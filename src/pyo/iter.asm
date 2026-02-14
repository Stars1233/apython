; iter_obj.asm - Iterator types and range object
; Phase 9: list_iter, tuple_iter, range_iter, range_obj

%include "macros.inc"
%include "object.inc"
%include "types.inc"

section .note.GNU-stack noalloc noexec nowrite progbits

section .text

extern ap_malloc
extern ap_free
extern obj_decref
extern str_from_cstr
extern int_from_i64
extern fatal_error
extern list_type
extern tuple_type

;; ============================================================================
;; list_iter_new(PyListObject *list) -> PyListIterObject*
;; Create a new list iterator
;; ============================================================================
global list_iter_new
list_iter_new:
    push rbp
    mov rbp, rsp
    push rbx

    mov rbx, rdi               ; save list

    mov edi, PyListIterObject_size
    call ap_malloc

    mov qword [rax + PyObject.ob_refcnt], 1
    lea rcx, [rel list_iter_type]
    mov [rax + PyObject.ob_type], rcx
    mov [rax + PyListIterObject.it_seq], rbx
    mov qword [rax + PyListIterObject.it_index], 0

    ; INCREF the list
    INCREF rbx

    pop rbx
    pop rbp
    ret

;; ============================================================================
;; list_iter_next(PyListIterObject *self) -> PyObject* or NULL
;; Return next item or NULL if exhausted
;; ============================================================================
global list_iter_next
list_iter_next:
    mov rax, [rdi + PyListIterObject.it_seq]      ; list
    mov rcx, [rdi + PyListIterObject.it_index]     ; index

    ; Check bounds
    cmp rcx, [rax + PyListObject.ob_size]
    jge .exhausted

    ; Get item
    mov rdx, [rax + PyListObject.ob_item]
    mov rax, [rdx + rcx*8]
    INCREF rax

    ; Advance index
    inc qword [rdi + PyListIterObject.it_index]
    ret

.exhausted:
    xor eax, eax
    ret

;; ============================================================================
;; list_iter_dealloc(PyObject *self)
;; ============================================================================
list_iter_dealloc:
    push rbp
    mov rbp, rsp
    push rbx
    mov rbx, rdi

    ; DECREF the list
    mov rdi, [rbx + PyListIterObject.it_seq]
    call obj_decref

    ; Free self
    mov rdi, rbx
    call ap_free

    pop rbx
    pop rbp
    ret

;; ============================================================================
;; list_iter_self(PyObject *self) -> PyObject*
;; tp_iter for iterators: return self with INCREF
;; ============================================================================
iter_self:
    inc qword [rdi + PyObject.ob_refcnt]
    mov rax, rdi
    ret

;; ============================================================================
;; list_tp_iter(PyListObject *list) -> PyListIterObject*
;; tp_iter for list type: create a new list iterator
;; This is called when GET_ITER is used on a list.
;; ============================================================================
global list_tp_iter
list_tp_iter:
    jmp list_iter_new

;; ============================================================================
;; tuple_iter_new(PyTupleObject *tuple) -> PyTupleIterObject*
;; Create a new tuple iterator
;; ============================================================================
global tuple_iter_new
tuple_iter_new:
    push rbp
    mov rbp, rsp
    push rbx

    mov rbx, rdi

    mov edi, PyTupleIterObject_size
    call ap_malloc

    mov qword [rax + PyObject.ob_refcnt], 1
    lea rcx, [rel tuple_iter_type]
    mov [rax + PyObject.ob_type], rcx
    mov [rax + PyTupleIterObject.it_seq], rbx
    mov qword [rax + PyTupleIterObject.it_index], 0

    INCREF rbx

    pop rbx
    pop rbp
    ret

;; ============================================================================
;; tuple_iter_next(PyTupleIterObject *self) -> PyObject* or NULL
;; ============================================================================
global tuple_iter_next
tuple_iter_next:
    mov rax, [rdi + PyTupleIterObject.it_seq]
    mov rcx, [rdi + PyTupleIterObject.it_index]

    cmp rcx, [rax + PyTupleObject.ob_size]
    jge .exhausted

    ; Get item (inline array)
    mov rax, [rax + PyTupleObject.ob_item + rcx*8]
    INCREF rax

    inc qword [rdi + PyTupleIterObject.it_index]
    ret

.exhausted:
    xor eax, eax
    ret

;; ============================================================================
;; tuple_iter_dealloc(PyObject *self)
;; ============================================================================
tuple_iter_dealloc:
    push rbp
    mov rbp, rsp
    push rbx
    mov rbx, rdi

    mov rdi, [rbx + PyTupleIterObject.it_seq]
    call obj_decref

    mov rdi, rbx
    call ap_free

    pop rbx
    pop rbp
    ret

;; ============================================================================
;; tuple_tp_iter(PyTupleObject *tuple) -> PyTupleIterObject*
;; tp_iter for tuple type
;; ============================================================================
global tuple_tp_iter
tuple_tp_iter:
    jmp tuple_iter_new

;; ============================================================================
;; range_new(int64_t start, int64_t stop, int64_t step) -> PyRangeIterObject*
;; Create a range iterator directly (simplified: range IS its own iterator)
;; ============================================================================
global range_new
range_new:
    push rbp
    mov rbp, rsp
    push rbx

    mov rbx, rdi               ; start
    push rsi                   ; stop
    push rdx                   ; step

    mov edi, PyRangeIterObject_size
    call ap_malloc

    mov qword [rax + PyObject.ob_refcnt], 1
    lea rcx, [rel range_iter_type]
    mov [rax + PyObject.ob_type], rcx

    ; Store as SmallInt-tagged values
    mov rcx, rbx
    bts rcx, 63
    mov [rax + PyRangeIterObject.it_current], rcx

    pop rcx                    ; step
    bts rcx, 63
    mov [rax + PyRangeIterObject.it_step], rcx

    pop rcx                    ; stop
    bts rcx, 63
    mov [rax + PyRangeIterObject.it_stop], rcx

    pop rbx
    pop rbp
    ret

;; ============================================================================
;; range_iter_next(PyRangeIterObject *self) -> PyObject* or NULL
;; Returns SmallInt for current value, advances by step
;; ============================================================================
global range_iter_next
range_iter_next:
    ; Decode current, stop, step
    mov rax, [rdi + PyRangeIterObject.it_current]
    shl rax, 1
    sar rax, 1                 ; current (decoded)

    mov rcx, [rdi + PyRangeIterObject.it_stop]
    shl rcx, 1
    sar rcx, 1                 ; stop (decoded)

    mov rdx, [rdi + PyRangeIterObject.it_step]
    shl rdx, 1
    sar rdx, 1                 ; step (decoded)

    ; Check if exhausted
    test rdx, rdx
    js .negative_step

    ; Positive step: current >= stop means exhausted
    cmp rax, rcx
    jge .exhausted
    jmp .has_value

.negative_step:
    ; Negative step: current <= stop means exhausted
    cmp rax, rcx
    jle .exhausted

.has_value:
    ; Return current as SmallInt
    mov r8, rax
    bts r8, 63                 ; SmallInt encode

    ; Advance: current += step
    add rax, rdx
    bts rax, 63               ; SmallInt encode new current
    mov [rdi + PyRangeIterObject.it_current], rax

    mov rax, r8
    ret

.exhausted:
    xor eax, eax
    ret

;; ============================================================================
;; range_iter_dealloc(PyObject *self)
;; ============================================================================
range_iter_dealloc:
    jmp ap_free                ; no references to DECREF, just free

;; ============================================================================
;; range_tp_iter(PyObject *self) -> PyObject*
;; Range iterator IS its own iterator
;; ============================================================================
range_tp_iter:
    inc qword [rdi + PyObject.ob_refcnt]
    mov rax, rdi
    ret

;; ============================================================================
;; init_iter_types
;; Patches list_type.tp_iter and tuple_type.tp_iter at startup
;; Called from main.asm or builtins_init
;; ============================================================================
global init_iter_types
init_iter_types:
    ; Set list_type.tp_iter = list_tp_iter
    lea rax, [rel list_tp_iter]
    lea rcx, [rel list_type]
    mov [rcx + PyTypeObject.tp_iter], rax

    ; Set tuple_type.tp_iter = tuple_tp_iter
    lea rax, [rel tuple_tp_iter]
    lea rcx, [rel tuple_type]
    mov [rcx + PyTypeObject.tp_iter], rax

    ret

;; ============================================================================
;; Data section
;; ============================================================================
section .data

list_iter_name: db "list_iterator", 0
tuple_iter_name: db "tuple_iterator", 0
range_iter_name: db "range", 0

; List iterator type
align 8
global list_iter_type
list_iter_type:
    dq 1                    ; ob_refcnt
    dq 0                    ; ob_type
    dq list_iter_name       ; tp_name
    dq PyListIterObject_size ; tp_basicsize
    dq list_iter_dealloc    ; tp_dealloc
    dq 0                    ; tp_repr
    dq 0                    ; tp_str
    dq 0                    ; tp_hash
    dq 0                    ; tp_call
    dq 0                    ; tp_getattr
    dq 0                    ; tp_setattr
    dq 0                    ; tp_richcompare
    dq iter_self            ; tp_iter (return self)
    dq list_iter_next       ; tp_iternext
    dq 0                    ; tp_init
    dq 0                    ; tp_new
    dq 0                    ; tp_as_number
    dq 0                    ; tp_as_sequence
    dq 0                    ; tp_as_mapping
    dq 0                    ; tp_base
    dq 0                    ; tp_dict
    dq 0                    ; tp_mro
    dq 0                    ; tp_flags
    dq 0                    ; tp_bases

; Tuple iterator type
align 8
global tuple_iter_type
tuple_iter_type:
    dq 1
    dq 0
    dq tuple_iter_name
    dq PyTupleIterObject_size
    dq tuple_iter_dealloc
    dq 0                    ; tp_repr
    dq 0                    ; tp_str
    dq 0                    ; tp_hash
    dq 0                    ; tp_call
    dq 0                    ; tp_getattr
    dq 0                    ; tp_setattr
    dq 0                    ; tp_richcompare
    dq iter_self            ; tp_iter
    dq tuple_iter_next      ; tp_iternext
    dq 0                    ; tp_init
    dq 0                    ; tp_new
    dq 0                    ; tp_as_number
    dq 0                    ; tp_as_sequence
    dq 0                    ; tp_as_mapping
    dq 0                    ; tp_base
    dq 0                    ; tp_dict
    dq 0                    ; tp_mro
    dq 0                    ; tp_flags
    dq 0                    ; tp_bases

; Range iterator type
align 8
global range_iter_type
range_iter_type:
    dq 1
    dq 0
    dq range_iter_name
    dq PyRangeIterObject_size
    dq range_iter_dealloc
    dq 0                    ; tp_repr
    dq 0                    ; tp_str
    dq 0                    ; tp_hash
    dq 0                    ; tp_call
    dq 0                    ; tp_getattr
    dq 0                    ; tp_setattr
    dq 0                    ; tp_richcompare
    dq range_tp_iter        ; tp_iter (return self)
    dq range_iter_next      ; tp_iternext
    dq 0                    ; tp_init
    dq 0                    ; tp_new
    dq 0                    ; tp_as_number
    dq 0                    ; tp_as_sequence
    dq 0                    ; tp_as_mapping
    dq 0                    ; tp_base
    dq 0                    ; tp_dict
    dq 0                    ; tp_mro
    dq 0                    ; tp_flags
    dq 0                    ; tp_bases
