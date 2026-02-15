; tuple_obj.asm - Tuple type implementation
; Immutable sequence of PyObject* pointers with inline storage

%include "macros.inc"
%include "object.inc"
%include "types.inc"

extern ap_malloc
extern ap_free
extern obj_decref
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

; tuple_new(int64_t size) -> PyTupleObject*
; Allocate a tuple with room for 'size' items, zero-filled
DEF_FUNC tuple_new
    push rbx
    push r12

    mov r12, rdi                ; r12 = size (item count)

    ; Allocate: header (PyTupleObject.ob_item = 32) + size*8
    lea rdi, [r12*8 + PyTupleObject.ob_item]
    call ap_malloc
    mov rbx, rax                ; rbx = new tuple

    ; Fill header
    mov qword [rbx + PyObject.ob_refcnt], 1
    lea rax, [rel tuple_type]
    mov [rbx + PyObject.ob_type], rax
    mov [rbx + PyTupleObject.ob_size], r12
    mov qword [rbx + PyTupleObject.ob_hash], -1  ; not computed

    ; Zero-fill the ob_item array
    test r12, r12
    jz .done
    lea rdi, [rbx + PyTupleObject.ob_item]
    xor eax, eax
    mov rcx, r12
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

; tuple_getitem(PyTupleObject *tuple, int64_t index) -> PyObject*
; sq_item: Return tuple->ob_item[index] with bounds check and INCREF
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
    mov rax, [rdi + PyTupleObject.ob_item + rsi*8]
    INCREF rax
    ret
.index_error:
    lea rdi, [rel exc_IndexError_type]
    CSTRING rsi, "tuple index out of range"
    call raise_exception
END_FUNC tuple_getitem

; tuple_subscript(PyTupleObject *tuple, PyObject *key) -> PyObject*
; mp_subscript: index with int or slice key (for BINARY_SUBSCR)
DEF_FUNC tuple_subscript
    push rbx
    mov rbx, rdi               ; save tuple

    ; Check if key is a slice
    test rsi, rsi
    js .ts_int                 ; SmallInt -> int path
    mov rax, [rsi + PyObject.ob_type]
    lea rcx, [rel slice_type]
    cmp rax, rcx
    je .ts_slice

.ts_int:
    mov rdi, rsi               ; key
    call int_to_i64
    mov rsi, rax               ; index
    mov rdi, rbx
    call tuple_getitem
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
; DECREF each item, then free self
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
    mov rdi, [rbx + PyTupleObject.ob_item + r13*8]
    test rdi, rdi
    jz .next
    call obj_decref
.next:
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
    mov rdi, [rbx + PyTupleObject.ob_item + r13*8]
    test rdi, rdi
    jz .skip_null
    call obj_hash               ; rax = hash of item
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
;; Creates a new tuple from a slice of the original.
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

    ; Fill items
    xor ecx, ecx
.tgs_loop:
    cmp rcx, [rbp-56]
    jge .tgs_done
    ; idx = start + i * step
    mov rax, rcx
    imul rax, r15
    add rax, r13
    ; Get item
    mov rdx, [rbx + PyTupleObject.ob_item + rax*8]
    ; Store in new tuple
    mov rsi, [rbp-48]
    mov [rsi + PyTupleObject.ob_item + rcx*8], rdx
    ; INCREF item
    push rcx
    mov rdi, rdx
    call obj_incref
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
    leave
    ret
END_FUNC tuple_getslice

;; ============================================================================
;; tuple_contains(PyTupleObject *self, PyObject *value) -> int (0 or 1)
;; Linear scan with pointer equality + value comparison fallback.
;; ============================================================================
DEF_FUNC tuple_contains
    push rbx
    push r12
    push r13

    mov rbx, rdi               ; tuple
    mov r12, rsi               ; value to find
    mov r13, [rbx + PyTupleObject.ob_size]

    xor ecx, ecx
.tc_loop:
    cmp rcx, r13
    jge .tc_not_found
    mov rax, [rbx + PyTupleObject.ob_item + rcx*8]
    cmp r12, rax               ; pointer equality
    je .tc_found
    ; Value comparison for SmallInts (common case for int 'in' tuple)
    mov rdx, r12
    and rdx, rax
    jns .tc_next               ; not both SmallInt → skip
    cmp r12, rax               ; both SmallInt, already compared above
    ; Both SmallInt with same tag but not same pointer can't happen
    ; (SmallInts with same value have same tagged pointer)
.tc_next:
    inc rcx
    jmp .tc_loop

.tc_found:
    mov eax, 1
    pop r13
    pop r12
    pop rbx
    leave
    ret

.tc_not_found:
    xor eax, eax
    pop r13
    pop r12
    pop rbx
    leave
    ret
END_FUNC tuple_contains

;; ============================================================================
;; tuple_concat(PyTupleObject *a, PyObject *b) -> PyTupleObject*
;; Concatenate two tuples: (1,2) + (3,4) -> (1,2,3,4)
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

    ; Copy items from a
    xor ecx, ecx
.copy_a:
    cmp rcx, r13
    jge .copy_b_start
    mov rdx, [rbx + PyTupleObject.ob_item + rcx*8]
    mov rax, [rsp]
    mov [rax + PyTupleObject.ob_item + rcx*8], rdx
    INCREF rdx
    inc rcx
    jmp .copy_a

.copy_b_start:
    xor ecx, ecx
.copy_b:
    cmp rcx, r14
    jge .concat_done
    mov rdx, [r12 + PyTupleObject.ob_item + rcx*8]
    lea rax, [r13 + rcx]
    mov r8, [rsp]
    mov [r8 + PyTupleObject.ob_item + rax*8], rdx
    INCREF rdx
    inc rcx
    jmp .copy_b

.concat_done:
    pop rax
    pop r14
    pop r13
    pop r12
    pop rbx
    leave
    ret
END_FUNC tuple_concat

;; ============================================================================
;; tuple_repeat(PyTupleObject *tuple, PyObject *count) -> PyTupleObject*
;; Repeat a tuple: (1,2) * 3 -> (1,2,1,2,1,2)
;; ============================================================================
DEF_FUNC tuple_repeat
    push rbx
    push r12
    push r13
    push r14

    mov rbx, rdi            ; rbx = tuple
    mov rdi, rsi            ; count
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
    mov rax, [rbx + PyTupleObject.ob_item + rdx*8]
    mov rcx, [rsp + 8]     ; get new tuple (past pushed rcx)
    mov [rcx + PyTupleObject.ob_item + r8*8], rax
    INCREF rax
    inc r8
    inc rdx
    jmp .rep_inner
.rep_inner_done:
    pop rcx
    inc rcx
    jmp .rep_outer

.rep_done:
    pop rax
    pop r14
    pop r13
    pop r12
    pop rbx
    leave
    ret
END_FUNC tuple_repeat

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
    dq 0                    ; tp_call
    dq 0                    ; tp_getattr
    dq 0                    ; tp_setattr
    dq 0                    ; tp_richcompare
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
