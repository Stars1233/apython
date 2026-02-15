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

    ; Allocate items array: capacity * 8
    lea rdi, [r12 * 8]
    call ap_malloc
    mov [rbx + PyListObject.ob_item], rax

    mov rax, rbx
    pop r12
    pop rbx
    leave
    ret
END_FUNC list_new

;; ============================================================================
;; list_append(PyListObject *list, PyObject *item)
;; Append item, grow if needed. INCREF item.
;; ============================================================================
DEF_FUNC list_append
    push rbx
    push r12

    mov rbx, rdi               ; list
    mov r12, rsi               ; item

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
    shl rsi, 3                 ; new_cap * 8
    call ap_realloc
    mov [rbx + PyListObject.ob_item], rax

.no_grow:
    ; Append item
    mov rax, [rbx + PyListObject.ob_size]
    mov rcx, [rbx + PyListObject.ob_item]
    mov [rcx + rax*8], r12

    ; INCREF item
    INCREF r12

    ; Increment size
    inc qword [rbx + PyListObject.ob_size]

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

    ; Return item with INCREF
    mov rax, [rdi + PyListObject.ob_item]
    mov rax, [rax + rsi*8]
    INCREF rax

    leave
    ret

.index_error:
    lea rdi, [rel exc_IndexError_type]
    CSTRING rsi, "list index out of range"
    call raise_exception
END_FUNC list_getitem

;; ============================================================================
;; list_setitem(PyListObject *list, int64_t index, PyObject *value)
;; sq_ass_item: set item at index, DECREF old, INCREF new
;; ============================================================================
DEF_FUNC list_setitem
    push rbx
    push r12

    mov rbx, rdi               ; list
    mov r12, rdx               ; new value

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

    ; DECREF old value
    mov rax, [rbx + PyListObject.ob_item]
    mov rdi, [rax + rsi*8]
    push rax
    push rsi
    call obj_decref
    pop rsi
    pop rax

    ; Store new value and INCREF
    mov [rax + rsi*8], r12
    INCREF r12

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

    ; Check if key is a slice
    test rsi, rsi
    js .ls_int                 ; SmallInt -> int path
    mov rax, [rsi + PyObject.ob_type]
    lea rcx, [rel slice_type]
    cmp rax, rcx
    je .ls_slice

.ls_int:
    ; Convert key to i64
    mov rdi, rsi
    call int_to_i64
    mov rsi, rax

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
;; list_ass_subscript(PyListObject *list, PyObject *key, PyObject *value)
;; mp_ass_subscript: set with int or slice key
;; ============================================================================
DEF_FUNC list_ass_subscript
    push rbx
    push r12

    mov rbx, rdi               ; list
    mov r12, rdx               ; value

    ; Check if key is a slice
    test rsi, rsi
    js .las_int                ; SmallInt -> int path
    mov rax, [rsi + PyObject.ob_type]
    lea rcx, [rel slice_type]
    cmp rax, rcx
    je .las_slice

.las_int:
    ; Convert key to i64
    mov rdi, rsi
    call int_to_i64
    mov rsi, rax

    ; Call list_setitem
    mov rdi, rbx
    mov rdx, r12
    call list_setitem

    pop r12
    pop rbx
    leave
    ret

.las_slice:
    ; Slice assignment: not yet supported, raise TypeError
    lea rdi, [rel exc_IndexError_type]
    extern exc_TypeError_type
    lea rdi, [rel exc_TypeError_type]
    CSTRING rsi, "slice assignment not yet supported"
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
;; list_contains(PyListObject *list, PyObject *value) -> int (0/1)
;; sq_contains: linear scan with pointer equality
;; ============================================================================
DEF_FUNC list_contains
    push rbx
    push r12
    push r13

    mov rbx, rdi               ; list
    mov r12, rsi               ; value
    mov r13, [rbx + PyListObject.ob_size]

    xor ecx, ecx
.loop:
    cmp rcx, r13
    jge .not_found
    mov rax, [rbx + PyListObject.ob_item]
    cmp r12, [rax + rcx*8]    ; pointer equality
    je .found
    inc rcx
    jmp .loop

.found:
    mov eax, 1
    pop r13
    pop r12
    pop rbx
    leave
    ret

.not_found:
    xor eax, eax
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
    mov rdi, [rax + r13*8]
    test rdi, rdi
    jz .dealloc_next
    call obj_decref
.dealloc_next:
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
    xor ecx, ecx              ; i = 0
.lgs_loop:
    cmp rcx, [rsp + 8]        ; slicelength
    jge .lgs_done
    push rcx                   ; save i
    ; idx = start + i * step
    mov rax, rcx
    imul rax, r15              ; i * step
    add rax, r13               ; start + i * step
    ; Get item from source list
    mov rdx, [rbx + PyListObject.ob_item]
    mov rsi, [rdx + rax*8]    ; item
    ; Append to new list (list_append does INCREF)
    mov rdi, [rsp + 8]        ; new list
    call list_append
    pop rcx
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
    mov rdx, [rsi + rcx*8]
    mov [rdi + rcx*8], rdx
    INCREF rdx
    inc rcx
    jmp .copy_a

.copy_b_start:
    ; Copy items from b
    mov rsi, [r12 + PyListObject.ob_item]
    xor ecx, ecx
.copy_b:
    cmp rcx, r14
    jge .concat_done
    mov rdx, [rsi + rcx*8]
    lea rax, [r13 + rcx]
    mov r8, [rsp]           ; get new list
    mov r8, [r8 + PyListObject.ob_item]
    mov [r8 + rax*8], rdx
    INCREF rdx
    inc rcx
    jmp .copy_b

.concat_done:
    pop rax                 ; return new list
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
    mov rdi, rsi            ; count (int obj)
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
    mov rax, [rsi + rdx*8]
    mov [rdi], rax
    INCREF rax
    add rdi, 8
    inc rdx
    jmp .rep_inner
.rep_inner_done:
    pop rcx
    inc rcx
    jmp .rep_outer

.rep_done:
    pop rax                 ; return new list
    pop r14
    pop r13
    pop r12
    pop rbx
    leave
    ret
END_FUNC list_repeat

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
    dq 0                    ; ob_type
    dq list_name_str        ; tp_name
    dq PyListObject_size    ; tp_basicsize
    dq list_dealloc         ; tp_dealloc
    dq list_repr            ; tp_repr
    dq list_repr            ; tp_str
    dq 0                    ; tp_hash (unhashable)
    dq 0                    ; tp_call
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
