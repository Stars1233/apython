; pyo/iter.asm - Iterator types and range object
; Phase 9: list_iter, tuple_iter, range_iter, range_obj

%include "macros.inc"
%include "object.inc"

extern ap_malloc
extern gc_alloc
extern ap_free
extern obj_decref
extern fatal_error
extern list_type
extern tuple_type
extern type_type
extern bool_true
extern bool_false
extern int_type
extern bool_type
extern exc_ValueError_type
extern exc_TypeError_type
extern raise_exception

;; ============================================================================
;; list_iter_new(PyListObject *list) -> PyListIterObject*
;; Create a new list iterator
;; ============================================================================
;; ============================================================================
;; The shared traverse and clear for the simple iterators and views.
;;
;; Every one of them -- list, tuple, str, bytes, set and dict iterators, the
;; three dict views, and the sequence-protocol iterator -- keeps exactly one
;; owned pointer, at the same offset: the thing it is walking.  None of them
;; was GC-tracked, so `a = []; a.append(iter(a))` was a cycle the collector
;; could not see, and neither could `d["k"] = d.keys()`.
;;
;; The offset is named for the list iterator because that is the struct these
;; all agree with; it is 16 in every one of them.
;; ============================================================================
global iter_traverse_one
DEF_FUNC iter_traverse_one
    mov rdi, [rdi + PyListIterObject.it_seq]
    VISIT_PTR rdi
    leave
    ret
END_FUNC iter_traverse_one

global iter_clear_one
DEF_FUNC iter_clear_one
    push rbx
    mov rbx, rdi
    mov rdi, [rbx + PyListIterObject.it_seq]
    test rdi, rdi
    jz .ico_done
    mov qword [rbx + PyListIterObject.it_seq], 0
    call obj_decref
.ico_done:
    pop rbx
    leave
    ret
END_FUNC iter_clear_one

DEF_FUNC list_iter_new
    push rbx

    mov rbx, rdi               ; save list

    ; gc_alloc, not ap_malloc: an iterator holds the thing it walks, and a
    ; container holding its own iterator is a cycle only the collector can
    ; break.  gc_alloc fills ob_refcnt and ob_type itself.
    mov edi, PyListIterObject_size
    lea rsi, [rel list_iter_type]
    call gc_alloc

    mov [rax + PyListIterObject.it_seq], rbx
    mov qword [rax + PyListIterObject.it_index], 0

    ; INCREF the list
    INCREF rbx

    push rax
    mov rdi, rax
    extern gc_track
    call gc_track
    pop rax

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
    test rax, rax
    jz .exhausted                                  ; already exhausted
    mov rcx, [rdi + PyListIterObject.it_index]     ; index

    ; Check bounds
    cmp rcx, [rax + PyListObject.ob_size]
    jge .exhausted_mark

    ; Get item
    mov rdx, [rax + PyListObject.ob_item]
    mov rax, [rdx + rcx * 8]     ; item Value
    INCREF_V rax, rdx

    ; Advance index
    inc qword [rdi + PyListIterObject.it_index]
    ret

.exhausted_mark:
    ; Mark as permanently exhausted by clearing it_seq
    ; DECREF the list
    push rdi
    mov rdi, [rdi + PyListIterObject.it_seq]
    call obj_decref
    pop rdi
    mov qword [rdi + PyListIterObject.it_seq], 0
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

    ; DECREF the list (if not already exhausted)
    mov rdi, [rbx + PyListIterObject.it_seq]
    test rdi, rdi
    jz .lid_no_decref
    call obj_decref
.lid_no_decref:

    ; Free self
    mov rdi, rbx
    extern gc_dealloc
    call gc_dealloc

    pop rbx
    leave
    ret
END_FUNC list_iter_dealloc

;; ============================================================================
;; list_iter_self(PyObject *self) -> PyObject*
;; tp_iter for iterators: return self with INCREF
;; ============================================================================
DEF_FUNC_BARE iter_self
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
    lea rsi, [rel tuple_iter_type]
    call gc_alloc

    mov [rax + PyTupleIterObject.it_seq], rbx
    mov qword [rax + PyTupleIterObject.it_index], 0

    INCREF rbx

    push rax
    mov rdi, rax
    call gc_track
    pop rax

    pop rbx
    leave
    ret
END_FUNC tuple_iter_new

;; ============================================================================
;; tuple_iter_next(PyTupleIterObject *self) -> rax = item Value, 0 when exhausted
;; ============================================================================
DEF_FUNC_BARE tuple_iter_next
    mov rax, [rdi + PyTupleIterObject.it_seq]
    mov rcx, [rdi + PyTupleIterObject.it_index]

    cmp rcx, [rax + PyTupleObject.ob_size]
    jge .exhausted

    ; Get item
    mov rax, [rax + PyTupleObject.ob_item]
    mov rax, [rax + rcx * 8]       ; item Value
    INCREF_V rax, rdx

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
    call gc_dealloc

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
    V_PACK_I64 rax, rdx         ; range values can exceed the immediate range
    ret

.exhausted:
    RET_NULL
    ret
END_FUNC range_iter_next

;; ============================================================================
;; range_iter_dealloc(PyObject *self)
;; ============================================================================
DEF_FUNC_BARE range_iter_dealloc
    jmp ap_free                ; no references to DECREF, just free
END_FUNC range_iter_dealloc

;; ============================================================================
;; range_iter_self(PyObject *self) -> PyObject*
;; Range iterator returns itself
;; ============================================================================
DEF_FUNC_BARE range_iter_self
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
DEF_FUNC_BARE range_obj_dealloc
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
    V_PACK rax, rdx             ; return one Value
    ret

.index_error:
    extern raise_exception
    RAISE exc_IndexError_type, "range object index out of range"
END_FUNC range_obj_sq_item

;; ============================================================================
;; range_obj_mp_subscript(PyRangeObject *self, Value key, int keytag) -> Value
;;
;; r[i] and r[a:b:c].  Only integer indexing existed, so a slice reached
;; sq_item and came back as "range object index out of range" --
;; `range(len(x))[::-1]`, which re/_compiler.py uses to walk a subpattern
;; backwards, could not run.  Slicing a range gives a range, computed the way
;; CPython's compute_slice does: the slice's resolved indices are positions in
;; this range, so each maps back through start + i*step.
;; ============================================================================
extern slice_type
extern slice_indices
extern obj_as_index
RMS_SELF  equ 8
RMS_START equ 16
RMS_STEP  equ 24
RMS_FRAME equ 40          ; + 1 push = 56
DEF_FUNC range_obj_mp_subscript, RMS_FRAME
    push rbx
    mov rbx, rdi
    ; Classify from the Value, not from the tag in edx: BINARY_SUBSCR builds
    ; the Value with V_PACK, which clobbers the register the tag was in.
    mov rax, rsi
    V_TEST_PTR rax, rcx
    ja .rms_int                     ; an immediate can only be an index
    test rax, rax
    jz .rms_int
    mov rcx, [rax + PyObject.ob_type]
    lea rdx, [rel slice_type]
    cmp rcx, rdx
    je .rms_slice

.rms_int:
    ; obj_as_index covers int, bool, an int subclass and __index__.
    mov rdi, rsi
    V_UNPACK rdi, rdx
    call obj_as_index
    mov rdi, rbx
    mov rsi, rax
    call range_obj_sq_item          ; already a Value
    pop rbx
    leave
    ret

.rms_slice:
    mov [rbp - RMS_SELF], rbx
    mov [rbp - RMS_START], rsi      ; the slice, across the length call
    mov rdi, rbx
    call range_obj_sq_length
    mov rdi, [rbp - RMS_START]      ; the slice
    mov rsi, rax                    ; the range's length
    call slice_indices              ; rax = start, rdx = stop, rcx = step

    ; substart = r.start + start * r.step,  substop = r.start + stop * r.step,
    ; substep  = r.step * step.
    mov r8, [rbx + PyRangeObject.step]
    imul rax, r8
    add rax, [rbx + PyRangeObject.start]
    mov [rbp - RMS_START], rax
    imul rdx, r8
    add rdx, [rbx + PyRangeObject.start]
    imul rcx, r8
    mov [rbp - RMS_STEP], rcx

    mov rdi, [rbp - RMS_START]
    mov rsi, rdx
    mov rdx, [rbp - RMS_STEP]
    call range_new
    mov edx, TAG_PTR
    pop rbx
    leave
    V_PACK rax, rdx
    ret
END_FUNC range_obj_mp_subscript

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
;; range's own protocol: ==, hash, index, count, and start/stop/step.
;;
;; range had none of it.  `range(3) == range(3)` was False, because with no
;; tp_richcompare the comparison fell back to identity; `{range(3),
;; range(0, 3, 1)}` held two elements where CPython holds one; and
;; `r.start`, `r.index(x)` and `r.count(x)` were all AttributeError.  A range
;; is a value, and the stdlib treats it as one.
;; ============================================================================

;; range_obj_richcompare(rdi = self, rsi = other, edx = op) -> Value
;;
;; CPython's rule, which is NOT field-by-field: two ranges are equal when
;; they generate the same sequence.  Empty ranges are all equal to each
;; other -- range(0) == range(5, 3) -- a one-element range ignores its step,
;; and only from two elements up does the step matter.  So range(0, 3, 1)
;; == range(3) even though the objects differ.
DEF_FUNC range_obj_richcompare
    push rbx
    push r12
    push r13
    mov rbx, rdi
    mov r12, rsi
    mov r13d, edx

    cmp r13d, PY_EQ
    je .rrc_eq
    cmp r13d, PY_NE
    je .rrc_eq
    jmp .rrc_notimpl            ; ranges are unordered, as CPython has them

.rrc_eq:
    V_TEST_PTR r12, rax
    ja .rrc_notimpl
    test r12, r12
    jz .rrc_notimpl
    mov rax, [r12 + PyObject.ob_type]
    lea rcx, [rel range_obj_type]
    cmp rax, rcx
    jne .rrc_notimpl

    mov rdi, rbx
    call range_obj_sq_length
    mov rdi, rax                ; our length
    push rdi
    mov rdi, r12
    call range_obj_sq_length
    pop rdi
    cmp rdi, rax
    jne .rrc_false
    test rdi, rdi
    jz .rrc_true                ; both empty: equal whatever the fields say

    mov rax, [rbx + PyRangeObject.start]
    cmp rax, [r12 + PyRangeObject.start]
    jne .rrc_false
    cmp rdi, 1
    je .rrc_true                ; one element: the step cannot be observed

    mov rax, [rbx + PyRangeObject.step]
    cmp rax, [r12 + PyRangeObject.step]
    jne .rrc_false

.rrc_true:
    cmp r13d, PY_NE
    je .rrc_ret_false
.rrc_ret_true:
    RET_TRUE
    jmp .rrc_out
.rrc_false:
    cmp r13d, PY_NE
    je .rrc_ret_true
.rrc_ret_false:
    RET_FALSE
.rrc_out:
    pop r13
    pop r12
    pop rbx
    leave
    ret
.rrc_notimpl:
    ; tp_richcompare declines with NULL and nothing pending -- not with the
    ; NotImplemented object.  Returning the object made `range(1) < range(2)`
    ; answer NotImplemented instead of raising, because the caller took it
    ; for the result.
    RET_NULL
    pop r13
    pop r12
    pop rbx
    leave
    ret
END_FUNC range_obj_richcompare

;; range_obj_hash(rdi = self) -> rax = the hash
;;
;; Equal ranges must hash alike, so this hashes exactly what the comparison
;; looks at: the length, then the start once there is an element, then the
;; step once there are two.  CPython hashes the tuple (len, start, step) with
;; None standing in for the fields that do not count; the mixing here is the
;; same shape without building the tuple.
ROH_FRAME equ 32            ; + 1 push = 40, not 16-aligned
DEF_FUNC range_obj_hash, ROH_FRAME
    push rbx
    mov rbx, rdi
    call range_obj_sq_length
    mov rcx, rax                ; the length

    mov rax, 0x345678
    imul rax, 1000003
    xor rax, rcx
    test rcx, rcx
    jz .roh_done

    mov rdx, [rbx + PyRangeObject.start]
    imul rax, 1000003
    xor rax, rdx
    cmp rcx, 1
    je .roh_done

    mov rdx, [rbx + PyRangeObject.step]
    imul rax, 1000003
    xor rax, rdx
.roh_done:
    ; -1 is the error signal everywhere a hash is consumed.
    cmp rax, -1
    jne .roh_out
    mov rax, -2
.roh_out:
    pop rbx
    leave
    ret
END_FUNC range_obj_hash

;; range_index_of(rdi = self, rsi = the value as an i64, rdx = out flag)
;;   -> rax = the index, or -1 when the value is not in the range
DEF_FUNC_BARE range_index_of
    mov rcx, [rdi + PyRangeObject.start]
    sub rsi, rcx                ; value - start
    mov rcx, [rdi + PyRangeObject.step]
    mov rax, rsi
    cqo
    idiv rcx                    ; rax = quotient, rdx = remainder
    test rdx, rdx
    jnz .rio_no                 ; not on the step grid
    test rax, rax
    js .rio_no                  ; before the start
    push rax
    call range_obj_sq_length
    pop rcx
    cmp rcx, rax
    jge .rio_no                 ; past the end
    mov rax, rcx
    ret
.rio_no:
    mov rax, -1
    ret
END_FUNC range_index_of

;; range_arg_i64(rdi = a Value, rsi = out) -> eax = 1 and [rsi] = the number,
;; or eax = 0 when the value is not an integer at all.
DEF_FUNC range_arg_i64
    push rbx
    mov rbx, rsi
    V_IS_INT rdi, rax
    jb .rai_not_immediate
    V_TO_I64 rdi
    mov [rbx], rdi
    mov eax, 1
    pop rbx
    leave
    ret
.rai_not_immediate:
    V_TEST_PTR rdi, rax
    ja .rai_no
    test rdi, rdi
    jz .rai_no
    mov rax, [rdi + PyObject.ob_type]
    lea rcx, [rel int_type]
    cmp rax, rcx
    je .rai_heap_int
    lea rcx, [rel bool_type]
    cmp rax, rcx
    je .rai_heap_int
    test qword [rax + PyTypeObject.tp_flags], TYPE_FLAG_INT_SUBCLASS
    jz .rai_no
.rai_heap_int:
    extern int_to_i64
    call int_to_i64
    mov [rbx], rax
    mov eax, 1
    pop rbx
    leave
    ret
.rai_no:
    xor eax, eax
    pop rbx
    leave
    ret
END_FUNC range_arg_i64

;; range_method_index(args, nargs) -> the index of the value
RMI_VAL   equ 8
RMI_FRAME equ 16            ; + 1 push = 24, not 16-aligned
DEF_FUNC range_method_index, RMI_FRAME
    push rbx
    cmp rsi, 2
    jne .rmi_arity
    mov rbx, [rdi]              ; self
    mov rdi, [rdi + 8]
    lea rsi, [rbp - RMI_VAL]
    call range_arg_i64
    test eax, eax
    jz .rmi_not_integer         ; a non-integer could never be a member
    mov rdi, rbx
    mov rsi, [rbp - RMI_VAL]
    call range_index_of
    cmp rax, -1
    je .rmi_missing
    mov edx, TAG_SMALLINT
    pop rbx
    leave
    V_PACK rax, rdx             ; builtins return one Value
    ret
.rmi_missing:
    ; CPython has two wordings here and they differ by what was asked for:
    ; an integer that is simply not a member names itself, and anything that
    ; could never be a member at all gets the generic sequence message.
    pop rbx
    mov rsi, [rbp - RMI_VAL]
    call range_raise_not_in
.rmi_not_integer:
    pop rbx
    RAISE exc_ValueError_type, "sequence.index(x): x not in sequence"
.rmi_arity:
    pop rbx
    RAISE exc_TypeError_type, "index() takes exactly one argument"
END_FUNC range_method_index

;; range_raise_not_in(rsi = the value) -- does not return
;; "6 is not in range", CPython's wording for an integer that is not a member.
RRN_N     equ 8
RRN_BUF   equ 176
RRN_FRAME equ 176           ; + 0 pushes = 176, 16-aligned
DEF_FUNC_LOCAL range_raise_not_in, RRN_FRAME
    mov [rbp - RRN_N], rsi
    lea rdi, [rbp - RRN_BUF]
    CSTRING rsi, ""
    extern rbt_append_cstr
    call rbt_append_cstr
    mov rdi, rax
    mov rsi, [rbp - RRN_N]
    extern msg_append_i64
    call msg_append_i64
    mov rdi, rax
    CSTRING rsi, " is not in range"
    call rbt_append_cstr
    lea rdi, [rel exc_ValueError_type]
    lea rsi, [rbp - RRN_BUF]
    call raise_exception
END_FUNC range_raise_not_in

;; range_method_count(args, nargs) -> 1 when the value is in the range, else 0
RMC_VAL   equ 8
RMC_FRAME equ 16            ; + 1 push = 24, not 16-aligned
DEF_FUNC range_method_count, RMC_FRAME
    push rbx
    cmp rsi, 2
    jne .rmc_arity
    mov rbx, [rdi]
    mov rdi, [rdi + 8]
    lea rsi, [rbp - RMC_VAL]
    call range_arg_i64
    test eax, eax
    jz .rmc_zero
    mov rdi, rbx
    mov rsi, [rbp - RMC_VAL]
    call range_index_of
    cmp rax, -1
    je .rmc_zero
    mov eax, 1
    jmp .rmc_out
.rmc_zero:
    xor eax, eax
.rmc_out:
    mov edx, TAG_SMALLINT
    pop rbx
    leave
    V_PACK rax, rdx
    ret
.rmc_arity:
    pop rbx
    RAISE exc_TypeError_type, "count() takes exactly one argument"
END_FUNC range_method_count

;; The three fields, as getset descriptors.  A range is immutable, so there
;; is no setter.
%macro DEF_RANGE_GETTER 1
DEF_FUNC range_get_%1
    mov rax, [rdi + PyRangeObject.%1]
    mov edx, TAG_SMALLINT
    leave
    V_PACK rax, rdx
    ret
END_FUNC range_get_%1
%endmacro
DEF_RANGE_GETTER start
DEF_RANGE_GETTER stop
DEF_RANGE_GETTER step

;; The by-name protocol.  range answered `hasattr(range, "index")` and
;; `hasattr(range, "__getitem__")` with False, because range_obj_type had no
;; tp_dict at all -- every one of these lived only in a slot.  The stdlib asks
;; by name: collections.abc.Sequence.register(range) leans on __getitem__ and
;; __len__ being there, and reversed(range(n)) goes through __reversed__.
DEF_FUNC range_dunder_getitem
    cmp rsi, 2
    jne .rdg_bad
    mov rax, [rdi]
    mov rsi, [rdi + 8]
    mov rdi, rax
    call range_obj_mp_subscript
    leave
    ret
.rdg_bad:
    RAISE exc_TypeError_type, "__getitem__() takes exactly one argument"
END_FUNC range_dunder_getitem

DEF_FUNC range_dunder_reversed
    test rsi, rsi
    jz .rdr_bad
    mov rdi, [rdi]
    call range_obj_reversed
    mov edx, TAG_PTR
    leave
    V_PACK rax, rdx             ; builtins return one Value
    ret
.rdr_bad:
    RAISE exc_TypeError_type, "__reversed__() takes no arguments"
END_FUNC range_dunder_reversed

;; ============================================================================
;; range_obj_repr(PyRangeObject *self) -> PyStrObject*
;; Returns "range(start, stop)" or "range(start, stop, step)" if step != 1
;; ============================================================================
extern str_from_cstr_heap
ROR_BUF equ 8
ROR_POS equ 16
ROR_FRAME equ 16            ; + 4 pushes = 48
DEF_FUNC range_obj_repr, ROR_FRAME
    push rbx
    push r12
    push r13
    push r14

    mov rbx, rdi               ; self (range object)
    mov r12, [rbx + PyRangeObject.start]
    mov r13, [rbx + PyRangeObject.stop]
    mov r14, [rbx + PyRangeObject.step]

    ; Allocate buffer (128 bytes is plenty for "range(i64, i64, i64)")
    mov edi, 128
    call ap_malloc
    mov [rbp - ROR_BUF], rax
    mov qword [rbp - ROR_POS], 0

    ; Write "range("
    mov rdi, rax
    mov byte [rdi], 'r'
    mov byte [rdi+1], 'a'
    mov byte [rdi+2], 'n'
    mov byte [rdi+3], 'g'
    mov byte [rdi+4], 'e'
    mov byte [rdi+5], '('
    mov qword [rbp - ROR_POS], 6

    ; Format start
    mov rdi, r12               ; start value (i64)
    call .ror_format_i64

    ; Write ", "
    mov rdi, [rbp - ROR_BUF]
    mov rcx, [rbp - ROR_POS]
    mov byte [rdi + rcx], ','
    mov byte [rdi + rcx + 1], ' '
    add qword [rbp - ROR_POS], 2

    ; Format stop
    mov rdi, r13               ; stop value (i64)
    call .ror_format_i64

    ; If step != 1, add ", step"
    cmp r14, 1
    je .ror_close
    mov rdi, [rbp - ROR_BUF]
    mov rcx, [rbp - ROR_POS]
    mov byte [rdi + rcx], ','
    mov byte [rdi + rcx + 1], ' '
    add qword [rbp - ROR_POS], 2
    mov rdi, r14               ; step value (i64)
    call .ror_format_i64

.ror_close:
    ; Write ")"
    mov rdi, [rbp - ROR_BUF]
    mov rcx, [rbp - ROR_POS]
    mov byte [rdi + rcx], ')'
    mov byte [rdi + rcx + 1], 0  ; NUL terminate
    inc qword [rbp - ROR_POS]

    ; Create string from buffer
    mov rdi, [rbp - ROR_BUF]
    call str_from_cstr_heap
    push rax                   ; save string

    ; Free buffer
    mov rdi, [rbp - ROR_BUF]
    call ap_free

    pop rax                    ; return string
    pop r14
    pop r13
    pop r12
    pop rbx
    leave
    ret

; Helper: format i64 in rdi to buffer, appending at current position
.ror_format_i64:
    push rbx
    push r12
    mov rbx, rdi               ; value
    mov r12, [rbp - ROR_BUF]
    mov rcx, [rbp - ROR_POS]

    ; Handle negative
    test rbx, rbx
    jns .ror_fi_pos
    mov byte [r12 + rcx], '-'
    inc rcx
    mov [rbp - ROR_POS], rcx
    neg rbx

.ror_fi_pos:
    ; Convert to decimal digits (reversed)
    sub rsp, 24                ; temp digit buffer on stack
    mov rdi, rsp
    xor r8d, r8d               ; digit count
    mov rax, rbx
.ror_fi_loop:
    xor edx, edx
    mov rcx, 10
    div rcx                    ; rax = quotient, rdx = remainder
    add dl, '0'
    mov [rdi + r8], dl
    inc r8
    test rax, rax
    jnz .ror_fi_loop

    ; Copy reversed digits to buffer
    mov rcx, [rbp - ROR_POS]
    mov rdi, [rbp - ROR_BUF]
.ror_fi_copy:
    dec r8
    movzx eax, byte [rsp + r8]
    mov [rdi + rcx], al
    inc rcx
    test r8, r8
    jnz .ror_fi_copy

    add rsp, 24
    mov [rbp - ROR_POS], rcx
    pop r12
    pop rbx
    ret

END_FUNC range_obj_repr

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
    dq TYPE_FLAG_HAVE_GC                    ; tp_flags
    dq 0                    ; tp_bases
    dq iter_traverse_one                        ; tp_traverse
    dq iter_clear_one                        ; tp_clear
    dq 0 ; tp_dictoffset

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
    dq TYPE_FLAG_HAVE_GC                    ; tp_flags
    dq 0                    ; tp_bases
    dq iter_traverse_one                        ; tp_traverse
    dq iter_clear_one                        ; tp_clear
    dq 0 ; tp_dictoffset

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
    dq 0                        ; tp_traverse
    dq 0                        ; tp_clear
    dq 0 ; tp_dictoffset

; Range object type (reusable sequence, creates fresh iterators)
align 8
global range_obj_type
range_obj_type:
    dq 1                        ; ob_refcnt (immortal)
    dq type_type                ; ob_type
    dq range_obj_name           ; tp_name
    dq PyRangeObject_size       ; tp_basicsize
    dq range_obj_dealloc        ; tp_dealloc
    dq range_obj_repr           ; tp_repr
    dq range_obj_repr           ; tp_str
    dq range_obj_hash           ; tp_hash
    dq 0                        ; tp_call
    dq 0                        ; tp_getattr
    dq 0                        ; tp_setattr
    dq range_obj_richcompare    ; tp_richcompare
    dq range_obj_tp_iter        ; tp_iter (creates new iterator)
    dq 0                        ; tp_iternext (NOT an iterator)
    dq 0                        ; tp_init
    dq 0                        ; tp_new
    dq 0                        ; tp_as_number
    dq range_obj_seq_methods    ; tp_as_sequence
    dq range_obj_map_methods    ; tp_as_mapping
    dq 0                        ; tp_base
    dq 0                        ; tp_dict
    dq 0                        ; tp_mro
    dq TYPE_FLAG_FINAL          ; tp_flags -- CPython gives this type no
                                ; Py_TPFLAGS_BASETYPE
    dq 0                        ; tp_bases
    dq 0                        ; tp_traverse
    dq 0                        ; tp_clear
    dq 0 ; tp_dictoffset

; Range object sequence methods
align 8
range_obj_map_methods:
    dq range_obj_sq_length      ; mp_length
    dq range_obj_mp_subscript   ; mp_subscript
    dq 0                        ; mp_ass_subscript

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
