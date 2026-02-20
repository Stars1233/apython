; iter_obj.asm - Iterator types and range object
; Phase 9: list_iter, tuple_iter, range_iter, range_obj

%include "macros.inc"
%include "object.inc"
%include "types.inc"

extern ap_malloc
extern ap_free
extern obj_decref
extern str_from_cstr
extern int_from_i64
extern fatal_error
extern list_type
extern tuple_type
extern type_type

;; ============================================================================
;; list_iter_new(PyListObject *list) -> PyListIterObject*
;; Create a new list iterator
;; ============================================================================
DEF_FUNC list_iter_new
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
    leave
    ret
END_FUNC list_iter_new

;; ============================================================================
;; list_iter_next(PyListIterObject *self) -> PyObject* or NULL
;; Return next item or NULL if exhausted
;; ============================================================================
DEF_FUNC_BARE list_iter_next
    mov rax, [rdi + PyListIterObject.it_seq]      ; list
    mov rcx, [rdi + PyListIterObject.it_index]     ; index

    ; Check bounds
    cmp rcx, [rax + PyListObject.ob_size]
    jge .exhausted

    ; Get fat item (16-byte stride)
    mov rdx, [rax + PyListObject.ob_item]
    shl rcx, 4                    ; index * 16
    mov rax, [rdx + rcx]          ; payload
    mov rdx, [rdx + rcx + 8]     ; tag
    INCREF_VAL rax, rdx

    ; Advance index
    inc qword [rdi + PyListIterObject.it_index]
    ret

.exhausted:
    RET_NULL
    ret
END_FUNC list_iter_next

;; ============================================================================
;; list_iter_dealloc(PyObject *self)
;; ============================================================================
DEF_FUNC_LOCAL list_iter_dealloc
    push rbx
    mov rbx, rdi

    ; DECREF the list
    mov rdi, [rbx + PyListIterObject.it_seq]
    call obj_decref

    ; Free self
    mov rdi, rbx
    call ap_free

    pop rbx
    leave
    ret
END_FUNC list_iter_dealloc

;; ============================================================================
;; list_iter_self(PyObject *self) -> PyObject*
;; tp_iter for iterators: return self with INCREF
;; ============================================================================
global iter_self
iter_self:
    inc qword [rdi + PyObject.ob_refcnt]
    mov rax, rdi
    ret
END_FUNC iter_self

;; ============================================================================
;; list_tp_iter(PyListObject *list) -> PyListIterObject*
;; tp_iter for list type: create a new list iterator
;; This is called when GET_ITER is used on a list.
;; ============================================================================
DEF_FUNC_BARE list_tp_iter
    jmp list_iter_new
END_FUNC list_tp_iter

;; ============================================================================
;; tuple_iter_new(PyTupleObject *tuple) -> PyTupleIterObject*
;; Create a new tuple iterator
;; ============================================================================
DEF_FUNC tuple_iter_new
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
    leave
    ret
END_FUNC tuple_iter_new

;; ============================================================================
;; tuple_iter_next(PyTupleIterObject *self) -> (rax=payload, rdx=tag) or NULL
;; Fat tuple: 16-byte slots
;; ============================================================================
DEF_FUNC_BARE tuple_iter_next
    mov rax, [rdi + PyTupleIterObject.it_seq]
    mov rcx, [rdi + PyTupleIterObject.it_index]

    cmp rcx, [rax + PyTupleObject.ob_size]
    jge .exhausted

    ; Get fat item (16-byte slot)
    shl rcx, 4                  ; index * 16
    mov rdx, [rax + PyTupleObject.ob_item + rcx + 8]   ; tag
    mov rax, [rax + PyTupleObject.ob_item + rcx]        ; payload
    INCREF_VAL rax, rdx

    inc qword [rdi + PyTupleIterObject.it_index]
    ret

.exhausted:
    RET_NULL
    ret
END_FUNC tuple_iter_next

;; ============================================================================
;; tuple_iter_dealloc(PyObject *self)
;; ============================================================================
DEF_FUNC_LOCAL tuple_iter_dealloc
    push rbx
    mov rbx, rdi

    mov rdi, [rbx + PyTupleIterObject.it_seq]
    call obj_decref

    mov rdi, rbx
    call ap_free

    pop rbx
    leave
    ret
END_FUNC tuple_iter_dealloc

;; ============================================================================
;; tuple_tp_iter(PyTupleObject *tuple) -> PyTupleIterObject*
;; tp_iter for tuple type
;; ============================================================================
DEF_FUNC_BARE tuple_tp_iter
    jmp tuple_iter_new
END_FUNC tuple_tp_iter

;; ============================================================================
;; range_new(int64_t start, int64_t stop, int64_t step) -> PyRangeObject*
;; Create a range SEQUENCE object (reusable; tp_iter creates fresh iterators)
;; ============================================================================
DEF_FUNC range_new
    push rbx

    mov rbx, rdi               ; start
    push rsi                   ; stop
    push rdx                   ; step

    mov edi, PyRangeObject_size
    call ap_malloc

    mov qword [rax + PyObject.ob_refcnt], 1
    lea rcx, [rel range_obj_type]
    mov [rax + PyObject.ob_type], rcx

    mov [rax + PyRangeObject.start], rbx

    pop rcx                    ; step
    mov [rax + PyRangeObject.step], rcx

    pop rcx                    ; stop
    mov [rax + PyRangeObject.stop], rcx

    pop rbx
    leave
    ret
END_FUNC range_new

;; ============================================================================
;; range_iter_next(PyRangeIterObject *self) -> PyObject* or NULL
;; Returns SmallInt for current value, advances by step
;; ============================================================================
DEF_FUNC_BARE range_iter_next
    ; Decode current, stop, step
    mov rax, [rdi + PyRangeIterObject.it_current]

    mov rcx, [rdi + PyRangeIterObject.it_stop]

    mov rdx, [rdi + PyRangeIterObject.it_step]

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

    ; Advance: current += step
    add rax, rdx
    mov [rdi + PyRangeIterObject.it_current], rax

    mov rax, r8
    RET_TAG_SMALLINT
    ret

.exhausted:
    RET_NULL
    ret
END_FUNC range_iter_next

;; ============================================================================
;; range_iter_dealloc(PyObject *self)
;; ============================================================================
range_iter_dealloc:
    jmp ap_free                ; no references to DECREF, just free
END_FUNC range_iter_dealloc

;; ============================================================================
;; range_iter_self(PyObject *self) -> PyObject*
;; Range iterator returns itself
;; ============================================================================
range_iter_self:
    inc qword [rdi + PyObject.ob_refcnt]
    mov rax, rdi
    ret
END_FUNC range_iter_self

;; ============================================================================
;; range_obj_tp_iter(PyRangeObject *self) -> PyRangeIterObject*
;; Creates a NEW range iterator from the range sequence object.
;; ============================================================================
DEF_FUNC range_obj_tp_iter
    push rbx
    mov rbx, rdi               ; save range object

    mov edi, PyRangeIterObject_size
    call ap_malloc

    mov qword [rax + PyObject.ob_refcnt], 1
    lea rcx, [rel range_iter_type]
    mov [rax + PyObject.ob_type], rcx

    ; Copy start → current, stop, step from range object
    mov rcx, [rbx + PyRangeObject.start]
    mov [rax + PyRangeIterObject.it_current], rcx
    mov rcx, [rbx + PyRangeObject.stop]
    mov [rax + PyRangeIterObject.it_stop], rcx
    mov rcx, [rbx + PyRangeObject.step]
    mov [rax + PyRangeIterObject.it_step], rcx

    pop rbx
    leave
    ret
END_FUNC range_obj_tp_iter

;; ============================================================================
;; range_obj_dealloc(PyObject *self)
;; ============================================================================
range_obj_dealloc:
    jmp ap_free                ; no references to DECREF, just free
END_FUNC range_obj_dealloc

;; ============================================================================
;; range_obj_sq_length(PyRangeObject *self) -> int64_t
;; Returns max(0, ceil((stop - start) / step))
;; ============================================================================
DEF_FUNC_BARE range_obj_sq_length
    mov rax, [rdi + PyRangeObject.stop]
    mov rcx, [rdi + PyRangeObject.start]
    mov rdx, [rdi + PyRangeObject.step]

    test rdx, rdx
    js .neg_step

    ; Positive step: len = max(0, (stop - start - 1) / step + 1) if stop > start
    sub rax, rcx               ; rax = stop - start
    jle .zero
    dec rax                    ; rax = stop - start - 1
    xor edx, edx              ; clear for div (but we need signed)
    mov rcx, [rdi + PyRangeObject.step]
    cqo                        ; sign-extend rax into rdx:rax
    idiv rcx                   ; rax = quotient
    inc rax                    ; +1
    ret

.neg_step:
    ; Negative step: len = max(0, (start - stop - 1) / (-step) + 1) if start > stop
    xchg rax, rcx             ; rax = start, rcx = stop
    sub rax, rcx              ; rax = start - stop
    jle .zero
    dec rax                   ; rax = start - stop - 1
    mov rcx, [rdi + PyRangeObject.step]
    neg rcx                   ; -step
    cqo
    idiv rcx
    inc rax
    ret

.zero:
    xor eax, eax
    ret
END_FUNC range_obj_sq_length

;; ============================================================================
;; range_obj_sq_item(PyRangeObject *self, int64_t index) -> fat(rax, edx)
;; Returns start + index * step as SmallInt. Raises IndexError if out of range.
;; ============================================================================
extern exc_IndexError_type
DEF_FUNC range_obj_sq_item
    push rbx
    push r12
    mov rbx, rdi               ; self
    mov r12, rsi               ; index

    ; Get length
    call range_obj_sq_length
    ; rax = length

    ; Handle negative index
    test r12, r12
    jns .pos_idx
    add r12, rax               ; index += length
.pos_idx:
    ; Bounds check
    cmp r12, 0
    jl .index_error
    cmp r12, rax
    jge .index_error

    ; Compute: start + index * step
    mov rax, [rbx + PyRangeObject.step]
    imul rax, r12              ; index * step
    add rax, [rbx + PyRangeObject.start]  ; + start
    RET_TAG_SMALLINT

    pop r12
    pop rbx
    leave
    ret

.index_error:
    lea rdi, [rel exc_IndexError_type]
    CSTRING rsi, "range object index out of range"
    extern raise_exception
    call raise_exception
END_FUNC range_obj_sq_item

;; ============================================================================
;; range_obj_reversed: __reversed__ for range objects
;; Returns a new range_iterator that iterates in reverse.
;; ============================================================================
DEF_FUNC range_obj_reversed
    push rbx
    mov rbx, rdi               ; self = range object

    ; Get length
    call range_obj_sq_length
    ; rax = length
    test rax, rax
    jz .rev_empty

    ; Last element = start + (length-1) * step
    dec rax
    imul rax, [rbx + PyRangeObject.step]
    add rax, [rbx + PyRangeObject.start]
    push rax                   ; save last_element

    ; Compute new stop = start - step (one before the first element)
    mov rcx, [rbx + PyRangeObject.start]
    mov rdx, [rbx + PyRangeObject.step]
    sub rcx, rdx
    push rcx                   ; save new_stop

    ; New step = -step
    neg rdx
    push rdx                   ; save new_step

    ; Allocate range iterator
    mov edi, PyRangeIterObject_size
    call ap_malloc

    mov qword [rax + PyObject.ob_refcnt], 1
    lea rcx, [rel range_iter_type]
    mov [rax + PyObject.ob_type], rcx

    pop rcx                    ; new_step
    mov [rax + PyRangeIterObject.it_step], rcx
    pop rcx                    ; new_stop
    mov [rax + PyRangeIterObject.it_stop], rcx
    pop rcx                    ; last_element = current
    mov [rax + PyRangeIterObject.it_current], rcx

    mov edx, TAG_PTR
    pop rbx
    leave
    ret

.rev_empty:
    ; Empty range: create iterator that's immediately exhausted
    mov edi, PyRangeIterObject_size
    call ap_malloc
    mov qword [rax + PyObject.ob_refcnt], 1
    lea rcx, [rel range_iter_type]
    mov [rax + PyObject.ob_type], rcx
    mov qword [rax + PyRangeIterObject.it_current], 0
    mov qword [rax + PyRangeIterObject.it_stop], 0
    mov qword [rax + PyRangeIterObject.it_step], 1
    mov edx, TAG_PTR
    pop rbx
    leave
    ret
END_FUNC range_obj_reversed

;; ============================================================================
;; init_iter_types
;; Patches list_type.tp_iter and tuple_type.tp_iter at startup
;; Called from main.asm or builtins_init
;; ============================================================================
DEF_FUNC_BARE init_iter_types
    ; Set list_type.tp_iter = list_tp_iter
    lea rax, [rel list_tp_iter]
    lea rcx, [rel list_type]
    mov [rcx + PyTypeObject.tp_iter], rax

    ; Set tuple_type.tp_iter = tuple_tp_iter
    lea rax, [rel tuple_tp_iter]
    lea rcx, [rel tuple_type]
    mov [rcx + PyTypeObject.tp_iter], rax

    ret
END_FUNC init_iter_types

;; ============================================================================
;; Data section
;; ============================================================================
section .data

list_iter_name: db "list_iterator", 0
tuple_iter_name: db "tuple_iterator", 0
range_iter_name: db "range_iterator", 0
range_obj_name: db "range", 0

; List iterator type
align 8
global list_iter_type
list_iter_type:
    dq 1                    ; ob_refcnt
    dq type_type            ; ob_type
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
    dq type_type
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
    dq type_type
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
    dq range_iter_self      ; tp_iter (return self)
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

; Range object type (reusable sequence, creates fresh iterators)
align 8
global range_obj_type
range_obj_type:
    dq 1                        ; ob_refcnt (immortal)
    dq type_type                ; ob_type
    dq range_obj_name           ; tp_name
    dq PyRangeObject_size       ; tp_basicsize
    dq range_obj_dealloc        ; tp_dealloc
    dq 0                        ; tp_repr
    dq 0                        ; tp_str
    dq 0                        ; tp_hash
    dq 0                        ; tp_call
    dq 0                        ; tp_getattr
    dq 0                        ; tp_setattr
    dq 0                        ; tp_richcompare
    dq range_obj_tp_iter        ; tp_iter (creates new iterator)
    dq 0                        ; tp_iternext (NOT an iterator)
    dq 0                        ; tp_init
    dq 0                        ; tp_new
    dq 0                        ; tp_as_number
    dq range_obj_seq_methods    ; tp_as_sequence
    dq 0                        ; tp_as_mapping
    dq 0                        ; tp_base
    dq 0                        ; tp_dict
    dq 0                        ; tp_mro
    dq 0                        ; tp_flags
    dq 0                        ; tp_bases

; Range object sequence methods
align 8
range_obj_seq_methods:
    dq range_obj_sq_length      ; sq_length
    dq 0                        ; sq_concat
    dq 0                        ; sq_repeat
    dq range_obj_sq_item        ; sq_item
    dq 0                        ; sq_ass_item
    dq 0                        ; sq_contains
    dq 0                        ; sq_inplace_concat
    dq 0                        ; sq_inplace_repeat
