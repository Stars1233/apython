; list_obj.asm - List type implementation
; Phase 9: dynamic array with amortized O(1) append

%include "macros.inc"
%include "object.inc"
%include "types.inc"

extern ap_malloc
extern ap_free
extern ap_realloc
extern obj_decref
extern obj_dealloc
extern str_from_cstr
extern str_new
extern obj_repr
extern fatal_error
extern raise_exception
extern exc_IndexError_type
extern int_to_i64
extern bool_true
extern bool_false
extern obj_incref
extern slice_type
extern slice_indices
extern type_type

;; ============================================================================
;; list_new(int64_t capacity) -> PyListObject*
;; Allocate a new empty list with given initial capacity
;; ============================================================================
DEF_FUNC list_new
    push rbx
    push r12

    mov r12, rdi               ; r12 = capacity
    test r12, r12
    jnz .has_cap
    mov r12, 4                 ; minimum capacity
.has_cap:

    ; Allocate PyListObject header
    mov edi, PyListObject_size
    call ap_malloc
    mov rbx, rax               ; rbx = list

    ; Fill header
    mov qword [rbx + PyObject.ob_refcnt], 1
    lea rax, [rel list_type]
    mov [rbx + PyObject.ob_type], rax
    mov qword [rbx + PyListObject.ob_size], 0
    mov [rbx + PyListObject.allocated], r12

    ; Allocate items array: capacity * 16 (fat values)
    mov rdi, r12
    shl rdi, 4
    call ap_malloc
    mov [rbx + PyListObject.ob_item], rax

    mov rax, rbx
    pop r12
    pop rbx
    leave
    ret
END_FUNC list_new

;; ============================================================================
;; list_append(PyListObject *list, PyObject *item, int item_tag)
;; Append item, grow if needed. INCREF item. rdx = item_tag.
;; ============================================================================
DEF_FUNC list_append
    push rbx
    push r12
    push r13

    mov rbx, rdi               ; list
    mov r12, rsi               ; item payload
    mov r13, rdx               ; item tag

    ; Check if need to grow
    mov rax, [rbx + PyListObject.ob_size]
    cmp rax, [rbx + PyListObject.allocated]
    jl .no_grow

    ; Double capacity
    mov rdi, [rbx + PyListObject.allocated]
    shl rdi, 1                 ; new_cap = old * 2
    mov [rbx + PyListObject.allocated], rdi

    ; Realloc items array
    mov rdi, [rbx + PyListObject.ob_item]
    mov rsi, [rbx + PyListObject.allocated]
    shl rsi, 4                 ; new_cap * 16
    call ap_realloc
    mov [rbx + PyListObject.ob_item], rax

.no_grow:
    ; Append item (16-byte fat slot)
    mov rax, [rbx + PyListObject.ob_size]
    mov rcx, [rbx + PyListObject.ob_item]
    shl rax, 4                 ; index * 16
    mov [rcx + rax], r12       ; payload
    ; Store item tag from caller
    mov [rcx + rax + 8], r13

    ; INCREF item (tag-aware)
    INCREF_VAL r12, r13

    ; Increment size
    inc qword [rbx + PyListObject.ob_size]

    pop r13
    pop r12
    pop rbx
    leave
    ret
END_FUNC list_append

;; ============================================================================
;; list_getitem(PyListObject *list, int64_t index) -> PyObject*
;; sq_item: return item at index with bounds check and negative index support
;; ============================================================================
DEF_FUNC list_getitem

    ; Handle negative index
    test rsi, rsi
    jns .positive
    add rsi, [rdi + PyListObject.ob_size]
.positive:

    ; Bounds check
    cmp rsi, [rdi + PyListObject.ob_size]
    jge .index_error
    cmp rsi, 0
    jl .index_error

    ; Return item with INCREF (16-byte stride)
    mov rax, [rdi + PyListObject.ob_item]
    shl rsi, 4                ; index * 16
    mov rdx, [rax + rsi + 8]  ; tag
    mov rax, [rax + rsi]      ; payload
    INCREF_VAL rax, rdx

    leave
    ret

.index_error:
    lea rdi, [rel exc_IndexError_type]
    CSTRING rsi, "list index out of range"
    call raise_exception
END_FUNC list_getitem

;; ============================================================================
;; list_setitem(PyListObject *list, int64_t index, PyObject *value, int value_tag)
;; sq_ass_item: set item at index, DECREF old, INCREF new. rcx = value_tag.
;; ============================================================================
DEF_FUNC list_setitem
    push rbx
    push r12
    push r13

    mov rbx, rdi               ; list
    mov r12, rdx               ; new value payload
    mov r13, rcx               ; new value tag

    ; Handle negative index
    test rsi, rsi
    jns .positive
    add rsi, [rbx + PyListObject.ob_size]
.positive:

    ; Bounds check
    cmp rsi, [rbx + PyListObject.ob_size]
    jge .index_error
    cmp rsi, 0
    jl .index_error

    ; DECREF old value (16-byte stride)
    mov rax, [rbx + PyListObject.ob_item]
    shl rsi, 4                ; offset = index * 16
    mov rdi, [rax + rsi]      ; old value payload
    mov rcx, [rax + rsi + 8]  ; old value tag
    push rax
    push rsi                  ; save shifted offset
    DECREF_VAL rdi, rcx
    pop rsi                   ; shifted offset
    pop rax

    ; Store new value and INCREF (16-byte fat slot)
    mov [rax + rsi], r12      ; payload
    ; Store value tag from caller
    mov [rax + rsi + 8], r13
    INCREF_VAL r12, r13

    pop r13
    pop r12
    pop rbx
    leave
    ret

.index_error:
    lea rdi, [rel exc_IndexError_type]
    CSTRING rsi, "list assignment index out of range"
    call raise_exception
END_FUNC list_setitem

;; ============================================================================
;; list_subscript(PyListObject *list, PyObject *key) -> PyObject*
;; mp_subscript: index with int or slice key (for BINARY_SUBSCR)
;; ============================================================================
DEF_FUNC list_subscript
    push rbx

    mov rbx, rdi               ; save list

    ; Check if key is a SmallInt (rdx = key tag from caller)
    cmp edx, TAG_SMALLINT
    je .ls_smallint
    ; Check if key is a slice
    mov rax, [rsi + PyObject.ob_type]
    lea rcx, [rel slice_type]
    cmp rax, rcx
    je .ls_slice

    ; Heap int -> convert to i64
    mov rdi, rsi
    call int_to_i64
    mov rsi, rax
    jmp .ls_do_getitem

.ls_smallint:
    ; SmallInt: payload IS the int64 index
    mov rsi, rsi               ; nop — rsi already = payload

.ls_do_getitem:

    ; Call list_getitem
    mov rdi, rbx
    call list_getitem

    pop rbx
    leave
    ret

.ls_slice:
    ; Call list_getslice(list, slice)
    mov rdi, rbx
    ; rsi = slice (already set)
    call list_getslice
    pop rbx
    leave
    ret
END_FUNC list_subscript

;; ============================================================================
;; list_ass_subscript(PyListObject *list, PyObject *key, PyObject *value,
;;                    int key_tag, int value_tag)
;; mp_ass_subscript: set with int or slice key
;; rdi=list, rsi=key, rdx=value, ecx=key_tag, r8d=value_tag
;; ============================================================================
LAS_VTAG  equ 8
LAS_FRAME equ 8
DEF_FUNC list_ass_subscript, LAS_FRAME
    push rbx
    push r12

    mov rbx, rdi               ; list
    mov r12, rdx               ; value
    mov [rbp - LAS_VTAG], r8   ; save value tag

    ; Check if key is a SmallInt (ecx = key tag from caller)
    cmp ecx, TAG_SMALLINT
    je .las_int                ; SmallInt -> int path
    mov rax, [rsi + PyObject.ob_type]
    lea rcx, [rel slice_type]
    cmp rax, rcx
    je .las_slice

.las_int:
    ; Convert key to i64
    mov rdi, rsi
    mov edx, ecx              ; key tag for int_to_i64
    call int_to_i64
    mov rsi, rax

    ; Call list_setitem
    mov rdi, rbx
    mov rdx, r12
    mov ecx, [rbp - LAS_VTAG]  ; value tag from caller
    call list_setitem

    pop r12
    pop rbx
    leave
    ret

.las_slice:
    ; Slice assignment: a[start:stop] = value
    ; rbx = list, rsi = slice key, r12 = value (new items)
    push r13
    push r14
    push r15
    sub rsp, 8             ; align

    ; Get slice indices relative to list length
    mov rdi, rsi           ; slice
    mov rsi, [rbx + PyListObject.ob_size]
    call slice_indices
    ; rax = start, rdx = stop, rcx = step
    mov r13, rax           ; r13 = start
    mov r14, rdx           ; r14 = stop
    mov r15, rcx           ; r15 = step

    ; Only support step=1 for now
    cmp r15, 1
    jne .las_step_error

    ; Clamp: if stop < start, set stop = start
    cmp r14, r13
    jge .las_stop_ok
    mov r14, r13
.las_stop_ok:

    ; old_len = stop - start (number of items being replaced)
    mov rcx, r14
    sub rcx, r13           ; rcx = old_len

    ; Get new items from value (must be a list)
    ; r12 = value (the new items list/iterable)
    ; For simplicity, require value to be a list
    test qword [rbp - LAS_VTAG], TAG_RC_BIT
    jz .las_type_error         ; non-heap value (SmallInt etc.) → type error
    mov rax, [r12 + PyObject.ob_type]
    lea rdx, [rel list_type]
    cmp rax, rdx
    jne .las_try_tuple

    ; Value is a list
    mov r8, [r12 + PyListObject.ob_size]   ; r8 = new_len
    mov r9, [r12 + PyListObject.ob_item]   ; r9 = new items ptr
    mov r15, 16                             ; r15 = source stride (fat list = 16)
    jmp .las_have_items

.las_try_tuple:
    extern tuple_type
    lea rdx, [rel tuple_type]
    cmp rax, rdx
    jne .las_type_error

    ; Value is a tuple (fat: 16-byte stride)
    mov r8, [r12 + PyTupleObject.ob_size]
    lea r9, [r12 + PyTupleObject.ob_item]
    mov r15, 16                             ; r15 = source stride (fat tuple = 16)
    jmp .las_have_items

.las_have_items:
    ; rcx = old_len (items being removed)
    ; r8 = new_len (items being inserted)
    ; r9 = pointer to new items array
    ; r13 = start, r14 = stop
    ; rbx = list

    ; Save new items info on stack
    push r8                ; [rsp+0] = new_len
    push r9                ; [rsp+0] = new_items_ptr, [rsp+8] = new_len
    push rcx               ; [rsp+0] = old_len

    ; 1. DECREF old items in slice range [start..stop)
    mov rcx, r13           ; i = start
.las_decref_loop:
    cmp rcx, r14           ; i < stop?
    jge .las_decref_done
    push rcx
    mov rax, [rbx + PyListObject.ob_item]
    mov rdx, rcx
    shl rdx, 4                ; index * 16
    mov rdi, [rax + rdx]      ; payload
    mov rsi, [rax + rdx + 8]  ; tag
    XDECREF_VAL rdi, rsi
    pop rcx
    inc rcx
    jmp .las_decref_loop
.las_decref_done:

    ; 2. Shift elements if old_len != new_len
    pop rcx                ; old_len
    pop r9                 ; new_items_ptr
    pop r8                 ; new_len

    mov rax, r8
    sub rax, rcx           ; delta = new_len - old_len
    test rax, rax
    jz .las_no_shift

    ; New list size
    mov rdi, [rbx + PyListObject.ob_size]
    add rdi, rax           ; new_size = ob_size + delta
    push rdi               ; save new_size
    push r8                ; save new_len
    push r9                ; save new_items_ptr
    push rax               ; save delta

    ; Ensure capacity
    cmp rdi, [rbx + PyListObject.allocated]
    jle .las_no_realloc

    ; Grow: at least new_size, double if bigger
    mov rsi, [rbx + PyListObject.allocated]
    shl rsi, 1             ; double
    cmp rdi, rsi
    jle .las_use_double
    mov rsi, rdi           ; use new_size if larger
.las_use_double:
    mov [rbx + PyListObject.allocated], rsi
    mov rdi, [rbx + PyListObject.ob_item]
    shl rsi, 4             ; bytes (capacity * 16)
    call ap_realloc
    mov [rbx + PyListObject.ob_item], rax

.las_no_realloc:
    pop rax                ; delta
    pop r9                 ; new_items_ptr
    pop r8                 ; new_len
    pop rdi                ; new_size

    ; Shift tail: memmove(items[start+new_len], items[stop], tail_count * 16)
    ; tail_count = ob_size - stop
    push r8
    push r9
    push rdi               ; new_size

    mov rcx, [rbx + PyListObject.ob_size]
    sub rcx, r14           ; tail_count = ob_size - stop

    test rcx, rcx
    jz .las_shift_done

    mov rdi, [rbx + PyListObject.ob_item]
    ; dst = items + (start + new_len) * 16
    mov rax, r13
    add rax, r8
    shl rax, 4
    add rdi, rax
    ; src = items + stop * 16
    mov rsi, [rbx + PyListObject.ob_item]
    mov rax, r14
    shl rax, 4
    add rsi, rax
    ; count in 16-byte slots
    ; Use forward or backward copy depending on direction
    push rcx
    cmp rdi, rsi
    jb .las_copy_fwd
    ; Backward copy (dst > src, overlap)
    dec rcx
.las_copy_bwd_loop:
    cmp rcx, 0
    jl .las_copy_done
    mov rax, rcx
    shl rax, 4                ; offset = i * 16
    mov r10, [rsi + rax]      ; payload
    mov r11, [rsi + rax + 8]  ; tag
    mov [rdi + rax], r10
    mov [rdi + rax + 8], r11
    dec rcx
    jmp .las_copy_bwd_loop
.las_copy_fwd:
    xor edx, edx
.las_copy_fwd_loop:
    cmp rdx, rcx
    jge .las_copy_done
    mov rax, rdx
    shl rax, 4                ; offset = i * 16
    mov r10, [rsi + rax]
    mov r11, [rsi + rax + 8]
    mov [rdi + rax], r10
    mov [rdi + rax + 8], r11
    inc rdx
    jmp .las_copy_fwd_loop
.las_copy_done:
    pop rcx

.las_shift_done:
    pop rdi                ; new_size
    mov [rbx + PyListObject.ob_size], rdi
    pop r9
    pop r8
    jmp .las_copy_new

.las_no_shift:
    ; Size stays the same, already correct

.las_copy_new:
    ; 3. Copy new items into [start..start+new_len), INCREF each
    xor ecx, ecx          ; i = 0
.las_insert_loop:
    cmp rcx, r8            ; i < new_len?
    jge .las_insert_done
    push rcx
    ; Compute source offset: rcx * stride (r15 = 16 for both fat list and fat tuple)
    imul rax, rcx, 1       ; rax = i
    imul rax, r15           ; rax = i * stride
    mov r10, [r9 + rax]    ; payload from source
    mov r11, [r9 + rax + 8] ; tag from source
    INCREF_VAL r10, r11
    mov rdx, [rbx + PyListObject.ob_item]
    mov rdi, r13
    add rdi, rcx           ; start + i
    shl rdi, 4             ; (start + i) * 16
    mov [rdx + rdi], r10   ; payload
    mov [rdx + rdi + 8], r11 ; tag
    pop rcx
    inc rcx
    jmp .las_insert_loop

.las_insert_done:
    add rsp, 8             ; undo alignment
    pop r15
    pop r14
    pop r13
    pop r12
    pop rbx
    leave
    ret

.las_step_error:
    extern exc_ValueError_type
    add rsp, 8
    pop r15
    pop r14
    pop r13
    lea rdi, [rel exc_TypeError_type]
    CSTRING rsi, "extended slice assignment not supported"
    call raise_exception

.las_type_error:
    extern exc_TypeError_type
    add rsp, 8
    pop r15
    pop r14
    pop r13
    lea rdi, [rel exc_TypeError_type]
    CSTRING rsi, "can only assign an iterable"
    call raise_exception
END_FUNC list_ass_subscript

;; ============================================================================
;; list_len(PyObject *self) -> int64_t
;; ============================================================================
DEF_FUNC_BARE list_len
    mov rax, [rdi + PyListObject.ob_size]
    ret
END_FUNC list_len

;; ============================================================================
;; list_contains(PyListObject *list, PyObject *value, int value_tag) -> int (0/1)
;; sq_contains: linear scan with payload + tag equality
;; ============================================================================
DEF_FUNC list_contains
    push rbx
    push r12
    push r13
    push r14

    mov rbx, rdi               ; list
    mov r12, rsi               ; value payload
    mov r13d, edx              ; value tag
    mov r14, [rbx + PyListObject.ob_size]

    xor ecx, ecx
.loop:
    cmp rcx, r14
    jge .not_found
    mov rax, [rbx + PyListObject.ob_item]
    mov rdx, rcx
    shl rdx, 4
    cmp r12, [rax + rdx]      ; payload match? (16-byte stride)
    jne .next
    cmp r13d, [rax + rdx + 8] ; tag match?
    je .found
.next:
    inc rcx
    jmp .loop

.found:
    mov eax, 1
    pop r14
    pop r13
    pop r12
    pop rbx
    leave
    ret

.not_found:
    xor eax, eax
    pop r14
    pop r13
    pop r12
    pop rbx
    leave
    ret
END_FUNC list_contains

;; ============================================================================
;; list_dealloc(PyObject *self)
;; DECREF all items, free items array, free list
;; ============================================================================
DEF_FUNC list_dealloc
    push rbx
    push r12
    push r13

    mov rbx, rdi
    mov r12, [rbx + PyListObject.ob_size]
    xor r13d, r13d

.dealloc_loop:
    cmp r13, r12
    jge .free_items
    mov rax, [rbx + PyListObject.ob_item]
    mov rcx, r13
    shl rcx, 4                ; index * 16
    mov rdi, [rax + rcx]      ; payload
    mov rsi, [rax + rcx + 8]  ; tag
    XDECREF_VAL rdi, rsi
    inc r13
    jmp .dealloc_loop

.free_items:
    mov rdi, [rbx + PyListObject.ob_item]
    call ap_free

    mov rdi, rbx
    call ap_free

    pop r13
    pop r12
    pop rbx
    leave
    ret
END_FUNC list_dealloc

; list_repr is in src/repr.asm
extern list_repr

;; ============================================================================
;; list_bool(PyObject *self) -> int (0/1)
;; ============================================================================
DEF_FUNC_BARE list_bool
    cmp qword [rdi + PyListObject.ob_size], 0
    setne al
    movzx eax, al
    ret
END_FUNC list_bool

;; ============================================================================
;; list_getslice(PyListObject *list, PySliceObject *slice) -> PyListObject*
;; Creates a new list from a slice of the original.
;; ============================================================================
DEF_FUNC list_getslice
    push rbx
    push r12
    push r13
    push r14
    push r15
    sub rsp, 8                 ; align

    mov rbx, rdi               ; list
    mov r12, rsi               ; slice

    ; Get slice indices
    mov rdi, r12               ; slice
    mov rsi, [rbx + PyListObject.ob_size]  ; length
    call slice_indices
    ; rax = start, rdx = stop, rcx = step
    mov r13, rax               ; r13 = start
    mov r14, rdx               ; r14 = stop
    mov r15, rcx               ; r15 = step

    ; Compute slicelength
    test r15, r15
    jg .lgs_pos_step
    ; Negative step
    mov rax, r13
    sub rax, r14               ; start - stop
    dec rax                    ; start - stop - 1
    mov rcx, r15
    neg rcx                    ; abs(step)
    xor edx, edx
    div rcx                    ; (start-stop-1) / abs(step)
    inc rax                    ; +1
    jmp .lgs_have_len

.lgs_pos_step:
    mov rax, r14
    sub rax, r13               ; stop - start
    jle .lgs_empty
    dec rax                    ; stop - start - 1
    xor edx, edx
    div r15                    ; (stop-start-1) / step
    inc rax                    ; +1
    jmp .lgs_have_len

.lgs_empty:
    xor eax, eax

.lgs_have_len:
    ; rax = slicelength
    push rax                   ; save slicelength [rbp-56]
    mov rdi, rax
    test rdi, rdi
    jnz .lgs_alloc
    mov rdi, 4                 ; min capacity
.lgs_alloc:
    call list_new
    push rax                   ; save new list [rbp-64]

    ; Fill items: for i = 0..slicelength-1, idx = start + i*step
    ; Set new list size to slicelength (capacity already >= slicelength)
    mov rcx, [rsp + 8]        ; slicelength
    mov rdi, [rsp]             ; new list
    mov [rdi + PyListObject.ob_size], rcx

    xor ecx, ecx              ; i = 0
.lgs_loop:
    cmp rcx, [rsp + 8]        ; slicelength
    jge .lgs_done
    ; idx = start + i * step
    mov rax, rcx
    imul rax, r15              ; i * step
    add rax, r13               ; start + i * step
    ; Get fat item from source list (16-byte stride)
    mov rdx, [rbx + PyListObject.ob_item]
    shl rax, 4                ; index * 16
    mov r8, [rdx + rax]       ; item payload
    mov r9, [rdx + rax + 8]   ; item tag
    INCREF_VAL r8, r9
    ; Store fat item into new list (16-byte stride)
    mov rdi, [rsp]             ; new list
    mov rdi, [rdi + PyListObject.ob_item]
    mov rax, rcx
    shl rax, 4                ; dest index * 16
    mov [rdi + rax], r8       ; payload
    mov [rdi + rax + 8], r9   ; tag
    inc rcx
    jmp .lgs_loop

.lgs_done:
    pop rax                    ; new list
    add rsp, 8                 ; discard slicelength

    add rsp, 8                 ; undo alignment
    pop r15
    pop r14
    pop r13
    pop r12
    pop rbx
    mov edx, TAG_PTR
    leave
    ret
END_FUNC list_getslice

;; ============================================================================
;; list_concat(PyListObject *a, PyObject *b) -> PyListObject*
;; Concatenate two lists: [1,2] + [3,4] -> [1,2,3,4]
;; ============================================================================
DEF_FUNC list_concat
    push rbx
    push r12
    push r13
    push r14

    mov rbx, rdi            ; rbx = list a
    mov r12, rsi            ; r12 = list b

    ; Get sizes
    mov r13, [rbx + PyListObject.ob_size]   ; r13 = len(a)
    mov r14, [r12 + PyListObject.ob_size]   ; r14 = len(b)

    ; Allocate new list with total capacity
    lea rdi, [r13 + r14]
    call list_new
    push rax                ; save new list

    ; Set size
    lea rcx, [r13 + r14]
    mov [rax + PyListObject.ob_size], rcx

    ; Copy items from a
    mov rdi, [rax + PyListObject.ob_item]   ; dest
    mov rsi, [rbx + PyListObject.ob_item]   ; src
    xor ecx, ecx
.copy_a:
    cmp rcx, r13
    jge .copy_b_start
    mov rax, rcx
    shl rax, 4                ; i * 16
    mov rdx, [rsi + rax]      ; payload from source
    mov r8, [rsi + rax + 8]   ; tag from source
    mov [rdi + rax], rdx      ; payload to dest
    mov [rdi + rax + 8], r8   ; tag to dest
    INCREF_VAL rdx, r8
    inc rcx
    jmp .copy_a

.copy_b_start:
    ; Copy items from b
    mov rsi, [r12 + PyListObject.ob_item]
    xor ecx, ecx
.copy_b:
    cmp rcx, r14
    jge .concat_done
    mov rax, rcx
    shl rax, 4                ; i * 16
    mov rdx, [rsi + rax]      ; payload from source b
    mov r8, [rsi + rax + 8]   ; tag from source b
    ; Destination offset = (r13 + rcx) * 16
    lea r9, [r13 + rcx]
    shl r9, 4
    mov r10, [rsp]            ; new list
    mov r10, [r10 + PyListObject.ob_item]
    mov [r10 + r9], rdx       ; payload
    mov [r10 + r9 + 8], r8   ; tag
    INCREF_VAL rdx, r8
    inc rcx
    jmp .copy_b

.concat_done:
    pop rax                 ; return new list
    mov edx, TAG_PTR
    pop r14
    pop r13
    pop r12
    pop rbx
    leave
    ret
END_FUNC list_concat

;; ============================================================================
;; list_repeat(PyListObject *list, PyObject *count) -> PyListObject*
;; Repeat a list: [1,2] * 3 -> [1,2,1,2,1,2]
;; ============================================================================
DEF_FUNC list_repeat
    push rbx
    push r12
    push r13
    push r14

    mov rbx, rdi            ; rbx = list
    mov rdi, rsi            ; count (int payload)
    mov edx, ecx            ; count tag (right operand)
    call int_to_i64
    mov r12, rax             ; r12 = repeat count

    ; Clamp negative to 0
    test r12, r12
    jg .rep_positive
    xor r12d, r12d
.rep_positive:

    mov r13, [rbx + PyListObject.ob_size]   ; r13 = len(list)
    imul r14, r13, 1                         ; r14 = original length
    imul r14, r12                            ; r14 = total items

    ; Allocate new list
    mov rdi, r14
    test rdi, rdi
    jnz .rep_has_size
    mov rdi, 1              ; min capacity
.rep_has_size:
    call list_new
    push rax                ; save new list
    mov [rax + PyListObject.ob_size], r14

    ; Copy list r12 times
    mov rdi, [rax + PyListObject.ob_item]
    xor ecx, ecx            ; ecx = repeat counter
.rep_outer:
    cmp rcx, r12
    jge .rep_done
    push rcx
    ; Copy all items from source list
    mov rsi, [rbx + PyListObject.ob_item]
    xor edx, edx
.rep_inner:
    cmp rdx, r13
    jge .rep_inner_done
    mov rax, rdx
    shl rax, 4
    mov r8, [rsi + rax]       ; payload
    mov r9, [rsi + rax + 8]   ; tag
    mov [rdi], r8             ; payload to dest
    mov [rdi + 8], r9         ; tag to dest
    INCREF_VAL r8, r9
    add rdi, 16               ; advance dest by 16
    inc rdx
    jmp .rep_inner
.rep_inner_done:
    pop rcx
    inc rcx
    jmp .rep_outer

.rep_done:
    pop rax                 ; return new list
    mov edx, TAG_PTR
    pop r14
    pop r13
    pop r12
    pop rbx
    leave
    ret
END_FUNC list_repeat

;; ============================================================================
;; list_type_call(PyTypeObject *type, PyObject **args, int64_t nargs) -> PyListObject*
;; Constructor: list() or list(iterable)
;; ============================================================================
; Frame layout
LTC_LIST    equ 8       ; new list object
LTC_ITER    equ 16      ; iterator object
LTC_FRAME   equ 24

DEF_FUNC list_type_call, LTC_FRAME
    push rbx
    push r12
    push r13

    mov r12, rsi            ; args
    mov r13, rdx            ; nargs

    ; list() — no args: return empty list
    test r13, r13
    jz .ltc_empty

    ; list(iterable) — exactly 1 arg
    cmp r13, 1
    jne .ltc_error

    ; Create empty list, then extend from iterable
    xor edi, edi
    call list_new
    mov [rbp - LTC_LIST], rax
    mov rbx, rax            ; rbx = new list

    ; Get iterator from arg
    mov rdi, [r12]          ; iterable
    mov rax, [rdi + PyObject.ob_type]
    mov rax, [rax + PyTypeObject.tp_iter]
    test rax, rax
    jz .ltc_not_iterable
    call rax                ; tp_iter(iterable) -> iterator
    test rax, rax
    jz .ltc_not_iterable
    mov [rbp - LTC_ITER], rax

    ; Iterate and append
.ltc_loop:
    mov rdi, [rbp - LTC_ITER]
    mov rax, [rdi + PyObject.ob_type]
    mov rax, [rax + PyTypeObject.tp_iternext]
    test rax, rax
    jz .ltc_done
    mov rdi, [rbp - LTC_ITER]
    call rax                ; tp_iternext(iter) -> item or NULL
    test edx, edx
    jz .ltc_done            ; StopIteration

    ; Append item to list
    push rax                ; save item
    mov rdi, rbx
    mov rsi, rax
    mov edx, TAG_PTR
    call list_append
    ; DECREF item (list_append INCREFs internally)
    pop rdi
    call obj_decref
    jmp .ltc_loop

.ltc_done:
    ; DECREF iterator
    mov rdi, [rbp - LTC_ITER]
    call obj_decref

    mov rax, rbx            ; return the list
    mov edx, TAG_PTR
    pop r13
    pop r12
    pop rbx
    leave
    ret

.ltc_empty:
    xor edi, edi
    call list_new
    mov edx, TAG_PTR
    pop r13
    pop r12
    pop rbx
    leave
    ret

.ltc_not_iterable:
    extern exc_TypeError_type
    lea rdi, [rel exc_TypeError_type]
    CSTRING rsi, "list() argument must be an iterable"
    call raise_exception

.ltc_error:
    lea rdi, [rel exc_TypeError_type]
    CSTRING rsi, "list expected at most 1 argument"
    call raise_exception
END_FUNC list_type_call

;; ============================================================================
;; Data section
;; ============================================================================
section .data

list_name_str: db "list", 0
; list_repr_str removed - repr now in src/repr.asm

; List number methods (just bool)
align 8
list_number_methods:
    dq list_concat          ; nb_add (list concatenation)
    dq 0                    ; nb_subtract
    dq list_repeat          ; nb_multiply (list repetition)
    dq 0                    ; nb_remainder
    dq 0                    ; nb_divmod
    dq 0                    ; nb_power
    dq 0                    ; nb_negative
    dq 0                    ; nb_positive
    dq 0                    ; nb_absolute
    dq list_bool            ; nb_bool
    dq 0                    ; nb_invert
    dq 0                    ; nb_lshift
    dq 0                    ; nb_rshift
    dq 0                    ; nb_and
    dq 0                    ; nb_xor
    dq 0                    ; nb_or
    dq 0                    ; nb_int
    dq 0                    ; nb_float
    dq 0                    ; nb_floor_divide
    dq 0                    ; nb_true_divide
    dq 0                    ; nb_index

; List sequence methods
align 8
list_sequence_methods:
    dq list_len             ; sq_length
    dq list_concat          ; sq_concat
    dq list_repeat          ; sq_repeat
    dq list_getitem         ; sq_item
    dq list_setitem         ; sq_ass_item
    dq list_contains        ; sq_contains
    dq 0                    ; sq_inplace_concat
    dq 0                    ; sq_inplace_repeat

; List mapping methods
align 8
list_mapping_methods:
    dq list_len             ; mp_length
    dq list_subscript       ; mp_subscript
    dq list_ass_subscript   ; mp_ass_subscript

; List type object
align 8
global list_type
list_type:
    dq 1                    ; ob_refcnt (immortal)
    dq type_type            ; ob_type
    dq list_name_str        ; tp_name
    dq PyListObject_size    ; tp_basicsize
    dq list_dealloc         ; tp_dealloc
    dq list_repr            ; tp_repr
    dq list_repr            ; tp_str
    dq 0                    ; tp_hash (unhashable)
    dq list_type_call       ; tp_call
    dq 0                    ; tp_getattr
    dq 0                    ; tp_setattr
    dq 0                    ; tp_richcompare
    dq 0                    ; tp_iter (set by iter_obj.asm)
    dq 0                    ; tp_iternext
    dq 0                    ; tp_init
    dq 0                    ; tp_new
    dq list_number_methods  ; tp_as_number
    dq list_sequence_methods ; tp_as_sequence
    dq list_mapping_methods ; tp_as_mapping
    dq 0                    ; tp_base
    dq 0                    ; tp_dict
    dq 0                    ; tp_mro
    dq TYPE_FLAG_LIST_SUBCLASS ; tp_flags
    dq 0                    ; tp_bases
