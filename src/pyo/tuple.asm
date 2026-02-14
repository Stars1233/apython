; tuple_obj.asm - Tuple type implementation
; Immutable sequence of PyObject* pointers with inline storage

%include "macros.inc"
%include "object.inc"
%include "types.inc"

section .note.GNU-stack noalloc noexec nowrite progbits

section .text

extern ap_malloc
extern ap_free
extern obj_decref
extern str_from_cstr
extern obj_hash
extern int_to_i64
extern fatal_error

; tuple_new(int64_t size) -> PyTupleObject*
; Allocate a tuple with room for 'size' items, zero-filled
global tuple_new
tuple_new:
    push rbp
    mov rbp, rsp
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
    pop rbp
    ret

; tuple_getitem(PyTupleObject *tuple, int64_t index) -> PyObject*
; sq_item: Return tuple->ob_item[index] with bounds check and INCREF
global tuple_getitem
tuple_getitem:
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
    CSTRING rdi, "IndexError: tuple index out of range"
    call fatal_error

; tuple_subscript(PyTupleObject *tuple, PyObject *key) -> PyObject*
; mp_subscript: index with int key (for BINARY_SUBSCR)
global tuple_subscript
tuple_subscript:
    push rbp
    mov rbp, rsp
    push rbx
    mov rbx, rdi               ; save tuple
    mov rdi, rsi               ; key
    call int_to_i64
    mov rsi, rax               ; index
    mov rdi, rbx
    call tuple_getitem
    pop rbx
    pop rbp
    ret

; tuple_len(PyTupleObject *tuple) -> int64_t
; Return tuple->ob_size
global tuple_len
tuple_len:
    mov rax, [rdi + PyTupleObject.ob_size]
    ret

; tuple_dealloc(PyObject *self)
; DECREF each item, then free self
global tuple_dealloc
tuple_dealloc:
    push rbp
    mov rbp, rsp
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
    pop rbp
    ret

; tuple_repr(PyObject *self) -> PyStrObject*
; Stub: returns "(....)"
global tuple_repr
tuple_repr:
    lea rdi, [rel tuple_repr_str]
    jmp str_from_cstr

; tuple_hash(PyObject *self) -> int64
; Combines item hashes using a simple multiply-xor scheme
global tuple_hash
tuple_hash:
    push rbp
    mov rbp, rsp
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
    pop rbp
    ret

section .data

tuple_name_str: db "tuple", 0
tuple_repr_str: db "(...)", 0

; Tuple sequence methods
align 8
tuple_sequence_methods:
    dq tuple_len            ; sq_length
    dq 0                    ; sq_concat
    dq 0                    ; sq_repeat
    dq tuple_getitem        ; sq_item
    dq 0                    ; sq_ass_item
    dq 0                    ; sq_contains
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
    dq 0                    ; ob_type
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
