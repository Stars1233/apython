; list_obj.asm - List type implementation
; Phase 9: dynamic array with amortized O(1) append

%include "macros.inc"
%include "object.inc"
%include "types.inc"

section .text

extern ap_malloc
extern ap_free
extern ap_realloc
extern obj_decref
extern obj_dealloc
extern str_from_cstr
extern str_new
extern obj_repr
extern memcpy
extern fatal_error
extern int_to_i64
extern bool_true
extern bool_false

;; ============================================================================
;; list_new(int64_t capacity) -> PyListObject*
;; Allocate a new empty list with given initial capacity
;; ============================================================================
global list_new
list_new:
    push rbp
    mov rbp, rsp
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
    pop rbp
    ret

;; ============================================================================
;; list_append(PyListObject *list, PyObject *item)
;; Append item, grow if needed. INCREF item.
;; ============================================================================
global list_append
list_append:
    push rbp
    mov rbp, rsp
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
    pop rbp
    ret

;; ============================================================================
;; list_getitem(PyListObject *list, int64_t index) -> PyObject*
;; sq_item: return item at index with bounds check and negative index support
;; ============================================================================
global list_getitem
list_getitem:
    push rbp
    mov rbp, rsp

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

    pop rbp
    ret

.index_error:
    CSTRING rdi, "IndexError: list index out of range"
    call fatal_error

;; ============================================================================
;; list_setitem(PyListObject *list, int64_t index, PyObject *value)
;; sq_ass_item: set item at index, DECREF old, INCREF new
;; ============================================================================
global list_setitem
list_setitem:
    push rbp
    mov rbp, rsp
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
    pop rbp
    ret

.index_error:
    CSTRING rdi, "IndexError: list assignment index out of range"
    call fatal_error

;; ============================================================================
;; list_subscript(PyListObject *list, PyObject *key) -> PyObject*
;; mp_subscript: index with int key (for BINARY_SUBSCR)
;; ============================================================================
global list_subscript
list_subscript:
    push rbp
    mov rbp, rsp
    push rbx

    mov rbx, rdi               ; save list

    ; Convert key to i64
    mov rdi, rsi
    call int_to_i64
    mov rsi, rax

    ; Call list_getitem
    mov rdi, rbx
    call list_getitem

    pop rbx
    pop rbp
    ret

;; ============================================================================
;; list_ass_subscript(PyListObject *list, PyObject *key, PyObject *value)
;; mp_ass_subscript: set with int key
;; ============================================================================
global list_ass_subscript
list_ass_subscript:
    push rbp
    mov rbp, rsp
    push rbx
    push r12

    mov rbx, rdi               ; list
    mov r12, rdx               ; value

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
    pop rbp
    ret

;; ============================================================================
;; list_len(PyObject *self) -> int64_t
;; ============================================================================
global list_len
list_len:
    mov rax, [rdi + PyListObject.ob_size]
    ret

;; ============================================================================
;; list_contains(PyListObject *list, PyObject *value) -> int (0/1)
;; sq_contains: linear scan with pointer equality
;; ============================================================================
global list_contains
list_contains:
    push rbp
    mov rbp, rsp
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
    pop rbp
    ret

.not_found:
    xor eax, eax
    pop r13
    pop r12
    pop rbx
    pop rbp
    ret

;; ============================================================================
;; list_dealloc(PyObject *self)
;; DECREF all items, free items array, free list
;; ============================================================================
global list_dealloc
list_dealloc:
    push rbp
    mov rbp, rsp
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
    pop rbp
    ret

;; ============================================================================
;; list_repr(PyObject *self) -> PyStrObject*
;; Returns "[...]" placeholder
;; ============================================================================
global list_repr
list_repr:
    push rbp
    mov rbp, rsp
    lea rdi, [rel list_repr_str]
    call str_from_cstr
    pop rbp
    ret

;; ============================================================================
;; list_bool(PyObject *self) -> int (0/1)
;; ============================================================================
global list_bool
list_bool:
    cmp qword [rdi + PyListObject.ob_size], 0
    setne al
    movzx eax, al
    ret

;; ============================================================================
;; Data section
;; ============================================================================
section .data

list_name_str: db "list", 0
list_repr_str: db "[...]", 0

; List number methods (just bool)
align 8
list_number_methods:
    dq 0                    ; nb_add
    dq 0                    ; nb_subtract
    dq 0                    ; nb_multiply
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
    dq 0                    ; sq_concat
    dq 0                    ; sq_repeat
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
