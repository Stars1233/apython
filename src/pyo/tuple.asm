; tuple_obj.asm - Tuple type implementation
; Fat tuples: each element is 16 bytes (payload + tag) inline

%include "macros.inc"
%include "object.inc"
%include "types.inc"

extern ap_malloc
extern ap_free
extern obj_decref
extern obj_dealloc
extern str_from_cstr
extern obj_hash
extern int_to_i64
extern fatal_error
extern raise_exception
extern exc_IndexError_type
extern obj_incref
extern slice_type
extern slice_indices
extern type_type
extern obj_is_true
extern float_compare
extern int_type
extern str_type
extern bool_type
extern none_type

; tuple_new(int64_t size) -> PyTupleObject*
; Allocate a tuple with room for 'size' fat items (16 bytes each), zero-filled
DEF_FUNC tuple_new
    push rbx
    push r12

    mov r12, rdi                ; r12 = size (item count)

    ; Allocate: header (32) + size * 16
    mov rdi, r12
    shl rdi, 4                  ; size * 16
    add rdi, PyTupleObject.ob_item
    call ap_malloc
    mov rbx, rax                ; rbx = new tuple

    ; Fill header
    mov qword [rbx + PyObject.ob_refcnt], 1
    lea rax, [rel tuple_type]
    mov [rbx + PyObject.ob_type], rax
    mov [rbx + PyTupleObject.ob_size], r12
    mov qword [rbx + PyTupleObject.ob_hash], -1  ; not computed

    ; Zero-fill the ob_item array (16 bytes per slot)
    test r12, r12
    jz .done
    lea rdi, [rbx + PyTupleObject.ob_item]
    xor eax, eax
    mov rcx, r12
    shl rcx, 1                  ; count * 2 (qwords per slot)
.zero_loop:
    mov [rdi], rax
    add rdi, 8
    dec rcx
    jnz .zero_loop

.done:
    mov rax, rbx
    pop r12
    pop rbx
    leave
    ret
END_FUNC tuple_new

; tuple_getitem(PyTupleObject *tuple, int64_t index) -> (rax=payload, rdx=tag)
; sq_item: Return fat tuple element with bounds check and INCREF_VAL
DEF_FUNC_BARE tuple_getitem
    ; Handle negative index
    test rsi, rsi
    jns .positive
    add rsi, [rdi + PyTupleObject.ob_size]
.positive:
    ; Bounds check
    cmp rsi, [rdi + PyTupleObject.ob_size]
    jge .index_error
    cmp rsi, 0
    jl .index_error
    shl rsi, 4                  ; index * 16
    mov rax, [rdi + PyTupleObject.ob_item + rsi]
    mov rdx, [rdi + PyTupleObject.ob_item + rsi + 8]
    INCREF_VAL rax, rdx
    ret
.index_error:
    lea rdi, [rel exc_IndexError_type]
    CSTRING rsi, "tuple index out of range"
    call raise_exception
END_FUNC tuple_getitem

; tuple_subscript(PyTupleObject *tuple, PyObject *key) -> PyObject*
; mp_subscript: index with int or slice key (for BINARY_SUBSCR)
; Returns (rax=payload, edx=tag) fat value
DEF_FUNC tuple_subscript
    push rbx
    mov rbx, rdi               ; save tuple

    ; Check if key is a SmallInt (edx = key tag from caller)
    cmp edx, TAG_SMALLINT
    je .ts_int                 ; SmallInt -> int path
    mov rax, [rsi + PyObject.ob_type]
    lea rcx, [rel slice_type]
    cmp rax, rcx
    je .ts_slice

.ts_int:
    mov rdi, rsi               ; key
    call int_to_i64
    mov rsi, rax               ; index
    mov rdi, rbx
    call tuple_getitem         ; returns (rax=payload, rdx=tag)
    pop rbx
    leave
    ret

.ts_slice:
    mov rdi, rbx
    ; rsi = slice
    call tuple_getslice
    pop rbx
    leave
    ret
END_FUNC tuple_subscript

; tuple_len(PyTupleObject *tuple) -> int64_t
; Return tuple->ob_size
DEF_FUNC_BARE tuple_len
    mov rax, [rdi + PyTupleObject.ob_size]
    ret
END_FUNC tuple_len

; tuple_dealloc(PyObject *self)
; DECREF_VAL each fat item, then free self
DEF_FUNC tuple_dealloc
    push rbx
    push r12
    push r13

    mov rbx, rdi                ; rbx = tuple
    mov r12, [rbx + PyTupleObject.ob_size]  ; r12 = item count
    xor r13d, r13d              ; r13 = index

.decref_loop:
    cmp r13, r12
    jge .free_self
    mov rax, r13
    shl rax, 4                  ; index * 16
    mov rdi, [rbx + PyTupleObject.ob_item + rax]
    mov rsi, [rbx + PyTupleObject.ob_item + rax + 8]
    DECREF_VAL rdi, rsi
    inc r13
    jmp .decref_loop

.free_self:
    mov rdi, rbx
    call ap_free

    pop r13
    pop r12
    pop rbx
    leave
    ret
END_FUNC tuple_dealloc

; tuple_repr is in src/repr.asm
extern tuple_repr

; tuple_hash(PyObject *self) -> int64
; Combines item hashes using a simple multiply-xor scheme
; TAG_SMALLINT: hash = payload. TAG_PTR: obj_hash(payload).
DEF_FUNC tuple_hash
    push rbx
    push r12
    push r13
    push r14

    mov rbx, rdi                ; rbx = tuple

    ; Check cached hash
    mov rax, [rbx + PyTupleObject.ob_hash]
    cmp rax, -1
    jne .cached

    mov r12, [rbx + PyTupleObject.ob_size]  ; r12 = item count
    xor r13d, r13d              ; r13 = index
    mov r14, 0x345678            ; r14 = hash accumulator

.hash_loop:
    cmp r13, r12
    jge .finalize
    mov rax, r13
    shl rax, 4                  ; index * 16
    mov rdi, [rbx + PyTupleObject.ob_item + rax]
    mov rsi, [rbx + PyTupleObject.ob_item + rax + 8]
    ; Check tag
    cmp esi, TAG_SMALLINT
    je .hash_smallint
    cmp esi, TAG_NULL
    je .skip_null
    ; TAG_PTR or other: call obj_hash on payload
    call obj_hash               ; rax = hash of item
    jmp .hash_combine
.hash_smallint:
    mov rax, rdi                ; hash = payload value
.hash_combine:
    ; Combine: hash = hash * 1000003 ^ item_hash
    imul r14, r14, 1000003
    xor r14, rax
.skip_null:
    inc r13
    jmp .hash_loop

.finalize:
    mov rax, r14
    ; Add length to hash
    xor rax, r12

    ; Ensure hash is never -1
    cmp rax, -1
    jne .store
    mov rax, -2
.store:
    mov [rbx + PyTupleObject.ob_hash], rax

.cached:
    pop r14
    pop r13
    pop r12
    pop rbx
    leave
    ret
END_FUNC tuple_hash

;; ============================================================================
;; tuple_getslice(PyTupleObject *tuple, PySliceObject *slice) -> PyTupleObject*
;; Creates a new tuple from a slice of the original. Fat 16-byte slots.
;; ============================================================================
DEF_FUNC tuple_getslice
    push rbx
    push r12
    push r13
    push r14
    push r15
    sub rsp, 16                ; [rbp-56]=slicelength, [rbp-48]=newtuple, align

    mov rbx, rdi               ; tuple
    mov r12, rsi               ; slice

    ; Get slice indices
    mov rdi, r12
    mov rsi, [rbx + PyTupleObject.ob_size]
    call slice_indices
    mov r13, rax               ; start
    mov r14, rdx               ; stop
    mov r15, rcx               ; step

    ; Compute slicelength
    test r15, r15
    jg .tgs_pos_step
    ; Negative step
    mov rax, r13
    sub rax, r14
    dec rax
    mov rcx, r15
    neg rcx
    xor edx, edx
    div rcx
    inc rax
    jmp .tgs_have_len

.tgs_pos_step:
    mov rax, r14
    sub rax, r13
    jle .tgs_empty
    dec rax
    xor edx, edx
    div r15
    inc rax
    jmp .tgs_have_len

.tgs_empty:
    xor eax, eax

.tgs_have_len:
    mov [rbp-56], rax          ; slicelength
    mov rdi, rax
    call tuple_new
    mov [rbp-48], rax          ; new tuple

    ; Fill items (16-byte fat slots)
    xor ecx, ecx
.tgs_loop:
    cmp rcx, [rbp-56]
    jge .tgs_done
    ; src_idx = start + i * step
    mov rax, rcx
    imul rax, r15
    add rax, r13
    ; Load fat element from source
    shl rax, 4                 ; src_idx * 16
    mov rdx, [rbx + PyTupleObject.ob_item + rax]       ; payload
    mov r8, [rbx + PyTupleObject.ob_item + rax + 8]    ; tag
    ; Store in new tuple
    mov rsi, [rbp-48]
    mov rax, rcx
    shl rax, 4                 ; dest_idx * 16
    mov [rsi + PyTupleObject.ob_item + rax], rdx
    mov [rsi + PyTupleObject.ob_item + rax + 8], r8
    ; INCREF_VAL
    push rcx
    INCREF_VAL rdx, r8
    pop rcx
    inc rcx
    jmp .tgs_loop

.tgs_done:
    mov rax, [rbp-48]

    add rsp, 16
    pop r15
    pop r14
    pop r13
    pop r12
    pop rbx
    mov edx, TAG_PTR
    leave
    ret
END_FUNC tuple_getslice

;; ============================================================================
;; tuple_contains(PyTupleObject *self, PyObject *value, int value_tag) -> int (0 or 1)
;; Linear scan with payload + tag equality (fat 16-byte slots).
;; ============================================================================
DEF_FUNC tuple_contains
    push rbx
    push r12
    push r13
    push r14

    mov rbx, rdi               ; tuple
    mov r12, rsi               ; value payload
    mov r13d, edx              ; value tag
    mov r14, [rbx + PyTupleObject.ob_size]

    xor ecx, ecx
.tc_loop:
    cmp rcx, r14
    jge .tc_not_found
    mov rax, rcx
    shl rax, 4                 ; index * 16
    cmp r12, [rbx + PyTupleObject.ob_item + rax]  ; payload match?
    jne .tc_next
    cmp r13, [rbx + PyTupleObject.ob_item + rax + 8]   ; tag match? (64-bit for SmallStr bit 63)
    je .tc_found
.tc_next:
    inc rcx
    jmp .tc_loop

.tc_found:
    mov eax, 1
    pop r14
    pop r13
    pop r12
    pop rbx
    leave
    ret

.tc_not_found:
    xor eax, eax
    pop r14
    pop r13
    pop r12
    pop rbx
    leave
    ret
END_FUNC tuple_contains

;; ============================================================================
;; tuple_concat(PyTupleObject *a, PyObject *b) -> PyTupleObject*
;; Concatenate two tuples with fat 16-byte slots.
;; ============================================================================
DEF_FUNC tuple_concat
    push rbx
    push r12
    push r13
    push r14

    mov rbx, rdi            ; rbx = tuple a
    mov r12, rsi            ; r12 = tuple b

    mov r13, [rbx + PyTupleObject.ob_size]   ; r13 = len(a)
    mov r14, [r12 + PyTupleObject.ob_size]   ; r14 = len(b)

    ; Allocate new tuple
    lea rdi, [r13 + r14]
    call tuple_new
    push rax                ; save new tuple

    ; Copy fat items from a
    xor ecx, ecx
.copy_a:
    cmp rcx, r13
    jge .copy_b_start
    mov rax, rcx
    shl rax, 4              ; index * 16
    mov rdx, [rbx + PyTupleObject.ob_item + rax]       ; payload
    mov r8, [rbx + PyTupleObject.ob_item + rax + 8]    ; tag
    mov r9, [rsp]           ; new tuple
    mov [r9 + PyTupleObject.ob_item + rax], rdx
    mov [r9 + PyTupleObject.ob_item + rax + 8], r8
    INCREF_VAL rdx, r8
    inc rcx
    jmp .copy_a

.copy_b_start:
    xor ecx, ecx
.copy_b:
    cmp rcx, r14
    jge .concat_done
    mov rax, rcx
    shl rax, 4              ; src index * 16
    mov rdx, [r12 + PyTupleObject.ob_item + rax]       ; payload
    mov r8, [r12 + PyTupleObject.ob_item + rax + 8]    ; tag
    lea rax, [r13 + rcx]    ; dest index
    shl rax, 4              ; dest index * 16
    mov r9, [rsp]           ; new tuple
    mov [r9 + PyTupleObject.ob_item + rax], rdx
    mov [r9 + PyTupleObject.ob_item + rax + 8], r8
    INCREF_VAL rdx, r8
    inc rcx
    jmp .copy_b

.concat_done:
    pop rax
    mov edx, TAG_PTR
    pop r14
    pop r13
    pop r12
    pop rbx
    leave
    ret
END_FUNC tuple_concat

;; ============================================================================
;; tuple_repeat(PyTupleObject *tuple, PyObject *count) -> PyTupleObject*
;; Repeat a tuple with fat 16-byte slots.
;; ============================================================================
DEF_FUNC tuple_repeat
    push rbx
    push r12
    push r13
    push r14

    mov rbx, rdi            ; rbx = tuple
    mov rdi, rsi            ; count (int payload)
    mov edx, ecx            ; count tag (right operand)
    call int_to_i64
    mov r12, rax             ; r12 = repeat count

    test r12, r12
    jg .rep_positive
    xor r12d, r12d
.rep_positive:

    mov r13, [rbx + PyTupleObject.ob_size]   ; r13 = len(tuple)
    imul r14, r13, 1
    imul r14, r12            ; r14 = total items

    ; Allocate new tuple
    mov rdi, r14
    call tuple_new
    push rax                ; save new tuple

    ; Copy tuple r12 times
    xor ecx, ecx            ; repeat counter
    xor r8d, r8d            ; dest index
.rep_outer:
    cmp rcx, r12
    jge .rep_done
    push rcx
    xor edx, edx
.rep_inner:
    cmp rdx, r13
    jge .rep_inner_done
    mov rax, rdx
    shl rax, 4              ; src index * 16
    mov r9, [rbx + PyTupleObject.ob_item + rax]        ; payload
    mov r10, [rbx + PyTupleObject.ob_item + rax + 8]   ; tag
    mov rax, r8
    shl rax, 4              ; dest index * 16
    mov rcx, [rsp + 8]      ; get new tuple (past pushed rcx)
    mov [rcx + PyTupleObject.ob_item + rax], r9
    mov [rcx + PyTupleObject.ob_item + rax + 8], r10
    INCREF_VAL r9, r10
    inc r8
    inc rdx
    jmp .rep_inner
.rep_inner_done:
    pop rcx
    inc rcx
    jmp .rep_outer

.rep_done:
    pop rax
    mov edx, TAG_PTR
    pop r14
    pop r13
    pop r12
    pop rbx
    leave
    ret
END_FUNC tuple_repeat

;; ============================================================================
;; tuple_richcompare(left, right, op, left_tag, right_tag) -> (rax, edx)
;; Compare two tuples. Returns bool fat value.
;; Supports EQ, NE, LT, LE, GT, GE (lexicographic for ordering).
;; ============================================================================
TRC_LEFT     equ 8
TRC_RIGHT    equ 16
TRC_OP       equ 24
TRC_IDX      equ 32
TRC_MINLEN   equ 40
TRC_FRAME    equ 40

global tuple_richcompare
DEF_FUNC tuple_richcompare, TRC_FRAME
    ; Verify right is TAG_PTR and a tuple
    cmp r8d, TAG_PTR
    jne .trc_not_impl
    mov rax, [rsi + PyObject.ob_type]
    lea r9, [rel tuple_type]
    cmp rax, r9
    jne .trc_not_impl

    mov [rbp - TRC_LEFT], rdi
    mov [rbp - TRC_RIGHT], rsi
    mov [rbp - TRC_OP], edx

    ; Get lengths
    mov rcx, [rdi + PyTupleObject.ob_size]   ; left_len
    mov r8, [rsi + PyTupleObject.ob_size]    ; right_len

    ; min_len = min(left_len, right_len)
    mov rax, rcx
    cmp rax, r8
    jle .trc_have_min
    mov rax, r8
.trc_have_min:
    mov [rbp - TRC_MINLEN], rax

    ; Compare elements 0..min_len-1
    mov qword [rbp - TRC_IDX], 0

.trc_elem_loop:
    mov rax, [rbp - TRC_IDX]
    cmp rax, [rbp - TRC_MINLEN]
    jge .trc_elements_equal

    ; Get left[i] and right[i] (fat values inline, 16-byte stride)
    shl rax, 4
    mov rdi, [rbp - TRC_LEFT]
    mov rcx, [rdi + PyTupleObject.ob_item + rax + 8]   ; left_tag
    mov rdi, [rdi + PyTupleObject.ob_item + rax]        ; left_payload

    mov rsi, [rbp - TRC_RIGHT]
    mov r8, [rsi + PyTupleObject.ob_item + rax + 8]    ; right_tag
    mov rsi, [rsi + PyTupleObject.ob_item + rax]        ; right_payload

    ; Fast path: both same tag and same payload → elements equal, skip
    cmp rcx, r8
    jne .trc_elem_compare
    cmp rdi, rsi
    je .trc_elem_next

.trc_elem_compare:
    ; Compare elements for EQ using element type's tp_richcompare
    push rdi                        ; left_payload
    push rcx                        ; left_tag
    push rsi                        ; right_payload
    push r8                         ; right_tag

    ; Float coercion: if either is TAG_FLOAT, use float_compare
    cmp ecx, TAG_FLOAT
    je .trc_elem_float
    cmp r8d, TAG_FLOAT
    je .trc_elem_float

    ; Resolve left type
    cmp ecx, TAG_SMALLINT
    je .trc_elem_int_type
    cmp ecx, TAG_BOOL
    je .trc_elem_bool_type
    cmp ecx, TAG_NONE
    je .trc_elem_none_type
    test rcx, rcx
    js .trc_elem_str_type           ; SmallStr
    ; TAG_PTR: get ob_type
    mov rax, [rdi + PyObject.ob_type]
    jmp .trc_elem_have_type

.trc_elem_int_type:
    lea rax, [rel int_type]
    jmp .trc_elem_have_type
.trc_elem_bool_type:
    lea rax, [rel bool_type]
    jmp .trc_elem_have_type
.trc_elem_none_type:
    lea rax, [rel none_type]
    jmp .trc_elem_have_type
.trc_elem_str_type:
    lea rax, [rel str_type]

.trc_elem_have_type:
    mov rax, [rax + PyTypeObject.tp_richcompare]
    test rax, rax
    jz .trc_elem_not_equal          ; no richcompare → not equal

    ; Call tp_richcompare(left, right, PY_EQ, left_tag, right_tag)
    pop r8                          ; right_tag
    pop rsi                         ; right_payload
    pop rcx                         ; left_tag
    pop rdi                         ; left_payload
    mov edx, PY_EQ
    call rax
    ; Check for NotImplemented (NULL return = tag 0)
    test edx, edx
    jz .trc_elem_not_equal_nopop

    ; Check result for truthiness
    push rax
    push rdx
    mov rdi, rax
    mov rsi, rdx
    call obj_is_true
    mov ecx, eax                    ; ecx = truthiness (0/1)
    pop rdx                         ; result tag
    pop rdi                         ; result payload
    push rcx                        ; save truthiness
    mov rsi, rdx
    DECREF_VAL rdi, rsi
    pop rcx                         ; restore truthiness
    test ecx, ecx
    jnz .trc_elem_next              ; equal → continue

    ; Elements not equal
    jmp .trc_elem_not_equal_nopop

.trc_elem_float:
    pop r8
    pop rsi
    pop rcx
    pop rdi
    mov edx, PY_EQ
    call float_compare
    ; Check for NotImplemented (NULL return = tag 0)
    test edx, edx
    jz .trc_elem_not_equal_nopop
    push rax
    push rdx
    mov rdi, rax
    mov rsi, rdx
    call obj_is_true
    mov ecx, eax
    pop rdx
    pop rdi
    push rcx
    mov rsi, rdx
    DECREF_VAL rdi, rsi
    pop rcx
    test ecx, ecx
    jnz .trc_elem_next
    jmp .trc_elem_not_equal_nopop

.trc_elem_not_equal:
    add rsp, 32                     ; clean up 4 pushes
.trc_elem_not_equal_nopop:
    ; Elements at index i differ.
    ; For EQ: return False. For NE: return True.
    ; For ordering: compare these elements with the requested op.
    mov ecx, [rbp - TRC_OP]
    cmp ecx, PY_EQ
    je .trc_return_false
    cmp ecx, PY_NE
    je .trc_return_true

    ; Ordering ops: compare the differing elements with the actual op
    mov rax, [rbp - TRC_IDX]
    shl rax, 4

    mov rdi, [rbp - TRC_LEFT]
    mov rcx, [rdi + PyTupleObject.ob_item + rax + 8]   ; left_tag
    mov rdi, [rdi + PyTupleObject.ob_item + rax]        ; left_payload

    mov rsi, [rbp - TRC_RIGHT]
    mov r8, [rsi + PyTupleObject.ob_item + rax + 8]    ; right_tag
    mov rsi, [rsi + PyTupleObject.ob_item + rax]        ; right_payload

    ; Resolve left type (again)
    push rcx
    push r8
    ; Float coercion: if either operand is TAG_FLOAT, use float_compare
    cmp ecx, TAG_FLOAT
    je .trc_order_float
    cmp r8d, TAG_FLOAT
    je .trc_order_float
    cmp ecx, TAG_SMALLINT
    je .trc_order_int_type
    cmp ecx, TAG_BOOL
    je .trc_order_bool_type
    cmp ecx, TAG_NONE
    je .trc_order_none_type
    test rcx, rcx
    js .trc_order_str_type
    mov rax, [rdi + PyObject.ob_type]
    jmp .trc_order_have_type
.trc_order_int_type:
    lea rax, [rel int_type]
    jmp .trc_order_have_type
.trc_order_bool_type:
    lea rax, [rel bool_type]
    jmp .trc_order_have_type
.trc_order_none_type:
    lea rax, [rel none_type]
    jmp .trc_order_have_type
.trc_order_str_type:
    lea rax, [rel str_type]
.trc_order_have_type:
    mov r10, rax                    ; save type ptr
    mov rax, [rax + PyTypeObject.tp_richcompare]
    test rax, rax
    jz .trc_order_fallback
    pop r8
    pop rcx
    mov edx, [rbp - TRC_OP]
    call rax
    leave
    ret
.trc_order_float:
    pop r8
    pop rcx
    mov edx, [rbp - TRC_OP]
    call float_compare
    leave
    ret
.trc_order_fallback:
    ; tp_richcompare is NULL — check if heaptype with dunders
    mov rax, [r10 + PyTypeObject.tp_flags]
    test rax, TYPE_FLAG_HEAPTYPE
    jz .trc_order_notimpl           ; not heaptype → return NotImplemented
    ; Heaptype: try dunder for ordering op
    pop r8                          ; right_tag
    pop rcx                         ; left_tag (unused, dunder_call_2 uses ecx=right_tag)
    mov ecx, r8d                    ; ecx = right_tag for dunder_call_2
    ; rdi = left_payload (still set from line 698)
    ; rsi = right_payload (still set from line 702)
    mov eax, [rbp - TRC_OP]
    cmp eax, PY_LT
    je .trc_order_dunder_lt
    cmp eax, PY_LE
    je .trc_order_dunder_le
    cmp eax, PY_GT
    je .trc_order_dunder_gt
    cmp eax, PY_GE
    je .trc_order_dunder_ge
    jmp .trc_order_notimpl_nopop    ; shouldn't reach here
.trc_order_dunder_lt:
    extern dunder_lt
    lea rdx, [rel dunder_lt]
    jmp .trc_order_dunder_call
.trc_order_dunder_le:
    extern dunder_le
    lea rdx, [rel dunder_le]
    jmp .trc_order_dunder_call
.trc_order_dunder_gt:
    extern dunder_gt
    lea rdx, [rel dunder_gt]
    jmp .trc_order_dunder_call
.trc_order_dunder_ge:
    extern dunder_ge
    lea rdx, [rel dunder_ge]
.trc_order_dunder_call:
    extern dunder_call_2
    call dunder_call_2
    leave
    ret
.trc_order_notimpl:
    add rsp, 16                     ; clean up 2 pushes
.trc_order_notimpl_nopop:
    RET_NULL
    leave
    ret

.trc_elem_next:
    inc qword [rbp - TRC_IDX]
    jmp .trc_elem_loop

.trc_elements_equal:
    ; All min_len elements are equal.
    ; Result depends on lengths and comparison op.
    mov rcx, [rbp - TRC_LEFT]
    mov rcx, [rcx + PyTupleObject.ob_size]    ; left_len
    mov r8, [rbp - TRC_RIGHT]
    mov r8, [r8 + PyTupleObject.ob_size]      ; right_len
    mov edx, [rbp - TRC_OP]

    cmp edx, PY_EQ
    je .trc_len_eq
    cmp edx, PY_NE
    je .trc_len_ne
    cmp edx, PY_LT
    je .trc_len_lt
    cmp edx, PY_LE
    je .trc_len_le
    cmp edx, PY_GT
    je .trc_len_gt
    ; PY_GE
    cmp rcx, r8
    jge .trc_return_true
    jmp .trc_return_false

.trc_len_eq:
    cmp rcx, r8
    je .trc_return_true
    jmp .trc_return_false
.trc_len_ne:
    cmp rcx, r8
    jne .trc_return_true
    jmp .trc_return_false
.trc_len_lt:
    cmp rcx, r8
    jl .trc_return_true
    jmp .trc_return_false
.trc_len_le:
    cmp rcx, r8
    jle .trc_return_true
    jmp .trc_return_false
.trc_len_gt:
    cmp rcx, r8
    jg .trc_return_true
    jmp .trc_return_false

.trc_return_true:
    mov eax, 1
    mov edx, TAG_BOOL
    leave
    ret

.trc_return_false:
    xor eax, eax
    mov edx, TAG_BOOL
    leave
    ret

.trc_not_impl:
    ; Return NotImplemented (NULL) so COMPARE_OP can try right operand
    RET_NULL
    leave
    ret

END_FUNC tuple_richcompare

;; ============================================================================
;; tuple_type_call(PyTypeObject *type, PyObject **args, int64_t nargs)
;; Constructor: tuple() or tuple(iterable)
;; ============================================================================
TTC_LIST    equ 8       ; temp list
TTC_ITER    equ 16      ; iterator
TTC_FRAME   equ 24

global tuple_type_call
DEF_FUNC tuple_type_call, TTC_FRAME
    push rbx
    push r12
    push r13

    mov r12, rsi            ; args
    mov r13, rdx            ; nargs

    ; tuple() — no args: return empty tuple
    test r13, r13
    jz .ttc_empty

    ; tuple(iterable) — exactly 1 arg
    cmp r13, 1
    jne .ttc_error

    ; If arg is already a tuple, return a copy (or just INCREF)
    ; For now: always iterate

    ; Create empty list, iterate into it, convert to tuple
    xor edi, edi
    extern list_new
    call list_new
    mov [rbp - TTC_LIST], rax
    mov rbx, rax            ; rbx = temp list

    ; Get iterator from arg
    mov rdi, [r12]          ; iterable payload
    mov esi, [r12 + 8]      ; iterable tag
    extern get_iterator
    call get_iterator
    mov [rbp - TTC_ITER], rax

    ; Iterate and append to list
.ttc_loop:
    mov rdi, [rbp - TTC_ITER]
    extern call_iternext
    call call_iternext
    test edx, edx
    jz .ttc_done

    push rax                ; save item payload
    push rdx                ; save item tag
    mov rdi, rbx
    mov rsi, rax
    ; edx = tag
    extern list_append
    call list_append
    pop rsi                 ; item tag
    pop rdi                 ; item payload
    DECREF_VAL rdi, rsi
    jmp .ttc_loop

.ttc_done:
    ; DECREF iterator
    mov rdi, [rbp - TTC_ITER]
    call obj_decref

    ; Check for pending exception
    extern current_exception
    mov rax, [rel current_exception]
    test rax, rax
    jnz .ttc_exc_cleanup

    ; Convert list to tuple
    mov rcx, [rbx + PyListObject.ob_size]
    mov rsi, [rbx + PyListObject.ob_item]
    push rbx                ; save list for DECREF

    mov rdi, rcx
    push rcx
    push rsi
    extern tuple_new
    call tuple_new
    pop rsi                 ; items ptr
    pop rcx                 ; count
    mov r12, rax             ; r12 = new tuple

    ; Copy items from list to tuple, INCREF each
    xor edx, edx
.ttc_copy_loop:
    cmp rdx, rcx
    jge .ttc_copy_done
    push rcx
    push rdx
    push rsi

    mov r8, rdx
    shl r8, 4               ; index * 16
    mov rdi, [rsi + r8]     ; payload from list
    mov r9, [rsi + r8 + 8]  ; tag from list
    mov [r12 + PyTupleObject.ob_item + r8], rdi
    mov [r12 + PyTupleObject.ob_item + r8 + 8], r9
    INCREF_VAL rdi, r9

    pop rsi
    pop rdx
    pop rcx
    inc rdx
    jmp .ttc_copy_loop

.ttc_copy_done:
    ; DECREF the temp list
    pop rdi                 ; temp list
    call obj_decref

    mov rax, r12
    mov edx, TAG_PTR
    pop r13
    pop r12
    pop rbx
    leave
    ret

.ttc_exc_cleanup:
    ; DECREF the temp list, return NULL
    mov rdi, rbx
    call obj_decref
    RET_NULL
    pop r13
    pop r12
    pop rbx
    leave
    ret

.ttc_empty:
    xor edi, edi
    extern tuple_new
    call tuple_new
    mov edx, TAG_PTR
    pop r13
    pop r12
    pop rbx
    leave
    ret

.ttc_error:
    extern exc_TypeError_type
    lea rdi, [rel exc_TypeError_type]
    CSTRING rsi, "tuple expected at most 1 argument"
    call raise_exception
END_FUNC tuple_type_call

section .data

tuple_name_str: db "tuple", 0
; tuple_repr_str removed - repr now in src/repr.asm

; Tuple sequence methods
align 8
tuple_sequence_methods:
    dq tuple_len            ; sq_length
    dq tuple_concat         ; sq_concat
    dq tuple_repeat         ; sq_repeat
    dq tuple_getitem        ; sq_item
    dq 0                    ; sq_ass_item
    dq tuple_contains       ; sq_contains
    dq 0                    ; sq_inplace_concat
    dq 0                    ; sq_inplace_repeat

; Tuple mapping methods
align 8
tuple_mapping_methods:
    dq tuple_len            ; mp_length
    dq tuple_subscript      ; mp_subscript
    dq 0                    ; mp_ass_subscript

; tuple type object
align 8
global tuple_type
tuple_type:
    dq 1                    ; ob_refcnt
    dq type_type            ; ob_type
    dq tuple_name_str       ; tp_name
    dq PyTupleObject.ob_item ; tp_basicsize (header, without items)
    dq tuple_dealloc        ; tp_dealloc
    dq tuple_repr           ; tp_repr
    dq tuple_repr           ; tp_str
    dq tuple_hash           ; tp_hash
    dq tuple_type_call      ; tp_call
    dq 0                    ; tp_getattr
    dq 0                    ; tp_setattr
    dq tuple_richcompare    ; tp_richcompare
    dq 0                    ; tp_iter (set by init_iter_types)
    dq 0                    ; tp_iternext
    dq 0                    ; tp_init
    dq 0                    ; tp_new
    dq 0                    ; tp_as_number
    dq tuple_sequence_methods ; tp_as_sequence
    dq tuple_mapping_methods ; tp_as_mapping
    dq 0                    ; tp_base
    dq 0                    ; tp_dict
    dq 0                    ; tp_mro
    dq TYPE_FLAG_TUPLE_SUBCLASS ; tp_flags
    dq 0                    ; tp_bases
