; bytes_obj.asm - Bytes type implementation
; Immutable sequence of raw bytes with inline storage

%include "macros.inc"
%include "object.inc"
%include "types.inc"

extern ap_malloc
extern ap_free
extern str_from_cstr
extern str_new
extern ap_memcpy
extern type_type
extern obj_incref
extern obj_decref
extern obj_dealloc
extern raise_exception
extern exc_IndexError_type
extern exc_TypeError_type
extern int_to_i64
extern slice_type
extern slice_indices
extern ap_strcmp
extern builtin_func_new
extern method_new

section .text

;; ============================================================================
;; bytes_new(int64_t size) -> PyBytesObject*
;; Allocate a bytes object with room for 'size' bytes
;; ============================================================================
DEF_FUNC bytes_new
    push rbx
    push r12

    mov r12, rdi                ; r12 = size

    ; Allocate: header (PyBytesObject.data = 24) + size
    lea rdi, [r12 + PyBytesObject.data]
    call ap_malloc
    mov rbx, rax                ; rbx = new bytes obj

    ; Fill header
    mov qword [rbx + PyObject.ob_refcnt], 1
    lea rax, [rel bytes_type]
    mov [rbx + PyObject.ob_type], rax
    mov [rbx + PyBytesObject.ob_size], r12

    mov rax, rbx
    pop r12
    pop rbx
    leave
    ret
END_FUNC bytes_new

;; ============================================================================
;; bytes_from_data(const void *data, int64_t size) -> PyBytesObject*
;; Allocate a bytes object and copy data into it
;; ============================================================================
DEF_FUNC bytes_from_data
    push rbx
    push r12
    push r13

    mov r12, rdi                ; r12 = source data ptr
    mov r13, rsi                ; r13 = size

    ; Allocate: header + size
    lea rdi, [r13 + PyBytesObject.data]
    call ap_malloc
    mov rbx, rax                ; rbx = new bytes obj

    ; Fill header
    mov qword [rbx + PyObject.ob_refcnt], 1
    lea rax, [rel bytes_type]
    mov [rbx + PyObject.ob_type], rax
    mov [rbx + PyBytesObject.ob_size], r13

    ; Copy data in
    lea rdi, [rbx + PyBytesObject.data]
    mov rsi, r12                ; source
    mov rdx, r13                ; length
    call ap_memcpy

    mov rax, rbx
    pop r13
    pop r12
    pop rbx
    leave
    ret
END_FUNC bytes_from_data

;; ============================================================================
;; bytes_dealloc(PyObject *self)
;; Data is inline, just free the object
;; ============================================================================
DEF_FUNC_BARE bytes_dealloc
    jmp ap_free
END_FUNC bytes_dealloc

;; ============================================================================
;; bytes_len(PyObject *self) -> int64_t
;; ============================================================================
DEF_FUNC_BARE bytes_len
    mov rax, [rdi + PyBytesObject.ob_size]
    ret
END_FUNC bytes_len

;; ============================================================================
;; bytes_getitem(PyBytesObject *self, int64_t index) -> PyObject* (SmallInt 0-255)
;; sq_item: return byte at index as integer
;; ============================================================================
DEF_FUNC_BARE bytes_getitem
    ; Handle negative index
    test rsi, rsi
    jns .positive
    add rsi, [rdi + PyBytesObject.ob_size]
.positive:
    ; Bounds check
    cmp rsi, [rdi + PyBytesObject.ob_size]
    jge .index_error
    cmp rsi, 0
    jl .index_error

    ; Get byte and return as SmallInt
    movzx eax, byte [rdi + PyBytesObject.data + rsi]
    RET_TAG_SMALLINT
    ret

.index_error:
    push rdi
    lea rdi, [rel exc_IndexError_type]
    CSTRING rsi, "index out of range"
    call raise_exception
END_FUNC bytes_getitem

;; ============================================================================
;; bytes_subscript(PyBytesObject *self, PyObject *key) -> PyObject*
;; mp_subscript: handles both int and slice keys
;; ============================================================================
DEF_FUNC bytes_subscript
    push rbx
    push r12

    mov rbx, rdi               ; bytes obj
    mov r12, rsi               ; key

    ; Check if key is a SmallInt (edx = key tag from caller)
    cmp edx, TAG_SMALLINT
    je .bs_int                 ; SmallInt → int path
    mov rax, [r12 + PyObject.ob_type]
    lea rcx, [rel slice_type]
    cmp rax, rcx
    je .bs_slice

.bs_int:
    ; Convert key to i64
    mov rdi, r12
    call int_to_i64
    ; Call bytes_getitem
    mov rdi, rbx
    mov rsi, rax
    call bytes_getitem

    pop r12
    pop rbx
    leave
    ret

.bs_slice:
    ; Slice: create new bytes from slice
    push r13
    push r14
    push r15

    mov rdi, r12               ; slice
    mov rsi, [rbx + PyBytesObject.ob_size]
    call slice_indices
    ; rax = start, rdx = stop, rcx = step
    mov r13, rax               ; start
    mov r14, rdx               ; stop
    mov r15, rcx               ; step

    ; For step=1, simple case
    cmp r15, 1
    jne .bs_step_slice

    ; Compute length
    mov rdi, r14
    sub rdi, r13
    jle .bs_empty

    ; Create new bytes
    push rdi                   ; save length
    call bytes_new
    pop rdi                    ; length
    push rax                   ; save new bytes

    ; Copy data
    lea rsi, [rbx + PyBytesObject.data]
    add rsi, r13               ; src = data + start
    lea rdi, [rax + PyBytesObject.data]  ; dst
    mov rdx, r14
    sub rdx, r13               ; count
    call ap_memcpy

    pop rax                    ; new bytes
    jmp .bs_slice_done

.bs_step_slice:
    ; Extended slice: compute length
    test r15, r15
    jg .bs_pos_step
    ; Negative step
    mov rax, r13
    sub rax, r14
    dec rax
    mov rcx, r15
    neg rcx
    xor edx, edx
    div rcx
    inc rax
    jmp .bs_have_slen

.bs_pos_step:
    mov rax, r14
    sub rax, r13
    jle .bs_empty
    dec rax
    xor edx, edx
    div r15
    inc rax
    jmp .bs_have_slen

.bs_have_slen:
    push rax                   ; slicelength
    mov rdi, rax
    call bytes_new
    push rax                   ; new bytes obj

    ; Fill items
    xor ecx, ecx              ; i = 0
.bs_step_loop:
    cmp rcx, [rsp + 8]        ; slicelength
    jge .bs_step_done
    ; idx = start + i * step
    mov rax, rcx
    imul rax, r15
    add rax, r13
    movzx edx, byte [rbx + PyBytesObject.data + rax]
    mov rdi, [rsp]             ; new bytes obj
    mov [rdi + PyBytesObject.data + rcx], dl
    inc rcx
    jmp .bs_step_loop

.bs_step_done:
    pop rax                    ; new bytes
    add rsp, 8                 ; discard slicelength
    jmp .bs_slice_done

.bs_empty:
    xor edi, edi
    call bytes_new

.bs_slice_done:
    pop r15
    pop r14
    pop r13
    pop r12
    pop rbx
    leave
    ret
END_FUNC bytes_subscript

;; ============================================================================
;; bytes_contains(PyBytesObject *self, PyObject *value) -> int (0/1)
;; sq_contains: check if byte value is in bytes
;; ============================================================================
DEF_FUNC bytes_contains
    push rbx

    mov rbx, rdi               ; bytes obj

    ; Value must be an int (0-255)
    mov rdi, rsi
    call int_to_i64
    ; rax = byte value

    ; Search
    mov rcx, [rbx + PyBytesObject.ob_size]
    lea rdx, [rbx + PyBytesObject.data]
    xor r8d, r8d               ; index
.bc_loop:
    cmp r8, rcx
    jge .bc_not_found
    movzx edi, byte [rdx + r8]
    cmp rdi, rax
    je .bc_found
    inc r8
    jmp .bc_loop

.bc_found:
    mov eax, 1
    pop rbx
    leave
    ret

.bc_not_found:
    xor eax, eax
    pop rbx
    leave
    ret
END_FUNC bytes_contains

;; ============================================================================
;; bytes_repr(PyObject *self) -> PyStrObject*
;; Returns b'...' representation with hex escapes for non-printable bytes
;; ============================================================================
DEF_FUNC bytes_repr, 1024
    push rbx
    push r12
    push r13

    mov rbx, rdi               ; bytes obj
    mov r12, [rbx + PyBytesObject.ob_size]  ; length

    ; Build repr in local buffer
    lea r13, [rbp - 1024]      ; buffer on stack

    ; Write "b'"
    mov byte [r13], 'b'
    mov byte [r13 + 1], 0x27   ; single quote
    mov ecx, 2                 ; output pos

    ; Iterate bytes
    xor edx, edx               ; input pos
.br_loop:
    cmp rdx, r12
    jge .br_close
    cmp ecx, 1000
    jge .br_close              ; safety limit

    movzx eax, byte [rbx + PyBytesObject.data + rdx]

    ; Printable ASCII (32-126, excluding backslash and quote)?
    cmp eax, 32
    jl .br_hex
    cmp eax, 127
    jge .br_hex
    cmp eax, 0x27              ; single quote
    je .br_escape_quote
    cmp eax, 0x5C              ; backslash
    je .br_escape_bs

    ; Printable: emit directly
    mov [r13 + rcx], al
    inc ecx
    inc edx
    jmp .br_loop

.br_escape_quote:
    mov byte [r13 + rcx], 0x5C     ; backslash
    mov byte [r13 + rcx + 1], 0x27 ; quote
    add ecx, 2
    inc edx
    jmp .br_loop

.br_escape_bs:
    mov byte [r13 + rcx], 0x5C
    mov byte [r13 + rcx + 1], 0x5C
    add ecx, 2
    inc edx
    jmp .br_loop

.br_hex:
    ; Non-printable: emit \xHH
    ; Common escapes first
    cmp eax, 0x0A
    je .br_escape_n
    cmp eax, 0x0D
    je .br_escape_r
    cmp eax, 0x09
    je .br_escape_t
    cmp eax, 0x00
    je .br_escape_0

    ; General \xHH
    mov byte [r13 + rcx], 0x5C     ; backslash
    mov byte [r13 + rcx + 1], 'x'
    push rdx
    ; High nibble
    mov edx, eax
    shr edx, 4
    lea rsi, [rel hex_digits]
    movzx edx, byte [rsi + rdx]
    mov [r13 + rcx + 2], dl
    ; Low nibble
    and eax, 0x0F
    movzx eax, byte [rsi + rax]
    mov [r13 + rcx + 3], al
    pop rdx
    add ecx, 4
    inc edx
    jmp .br_loop

.br_escape_n:
    mov byte [r13 + rcx], 0x5C
    mov byte [r13 + rcx + 1], 'n'
    add ecx, 2
    inc edx
    jmp .br_loop
.br_escape_r:
    mov byte [r13 + rcx], 0x5C
    mov byte [r13 + rcx + 1], 'r'
    add ecx, 2
    inc edx
    jmp .br_loop
.br_escape_t:
    mov byte [r13 + rcx], 0x5C
    mov byte [r13 + rcx + 1], 't'
    add ecx, 2
    inc edx
    jmp .br_loop
.br_escape_0:
    mov byte [r13 + rcx], 0x5C
    mov byte [r13 + rcx + 1], 'x'
    mov byte [r13 + rcx + 2], '0'
    mov byte [r13 + rcx + 3], '0'
    add ecx, 4
    inc edx
    jmp .br_loop

.br_close:
    mov byte [r13 + rcx], 0x27     ; closing quote
    mov byte [r13 + rcx + 1], 0    ; null terminator

    ; Create str from buffer
    mov rdi, r13
    call str_from_cstr
    mov edx, TAG_PTR

    pop r13
    pop r12
    pop rbx
    leave
    ret
END_FUNC bytes_repr

;; ============================================================================
;; bytes_decode(PyBytesObject *self) -> PyStrObject*
;; Decode bytes as UTF-8 string
;; ============================================================================
DEF_FUNC bytes_decode
    ; Simply create a string from the bytes data
    ; Assumes UTF-8 encoding
    lea rdi, [rdi + PyBytesObject.data]
    mov rsi, [rdi - PyBytesObject.data + PyBytesObject.ob_size]
    ; rdi = data ptr, rsi = length
    ; str_new(data, length)
    call str_new
    leave
    ret
END_FUNC bytes_decode

;; ============================================================================
;; bytes_getattr(PyBytesObject *self, PyObject *name) -> PyObject*
;; Attribute lookup for bytes: handles decode, hex, etc.
;; ============================================================================
DEF_FUNC bytes_getattr
    push rbx
    push r12

    mov rbx, rdi               ; self
    mov r12, rsi               ; name

    lea rdi, [r12 + PyStrObject.data]

    ; Check "decode"
    CSTRING rsi, "decode"
    call ap_strcmp
    test eax, eax
    jz .bga_decode

    ; Not found
    RET_NULL
    pop r12
    pop rbx
    leave
    ret

.bga_decode:
    call _get_bytes_decode_builtin
    mov rdi, rax
    call obj_incref
    mov edx, TAG_PTR
    pop r12
    pop rbx
    leave
    ret
END_FUNC bytes_getattr

;; ============================================================================
;; _bytes_decode_impl(args, nargs) — b.decode()
;; ============================================================================
DEF_FUNC _bytes_decode_impl
    ; nargs should be 1 (just self)
    mov rdi, [rdi]             ; self = args[0]
    call bytes_decode
    leave
    ret
END_FUNC _bytes_decode_impl

;; ============================================================================
;; Lazy-init helper for bytes.decode builtin
;; ============================================================================
DEF_FUNC_LOCAL _get_bytes_decode_builtin
    mov rax, [rel _bytes_decode_cache]
    test rax, rax
    jnz .ret
    lea rdi, [rel _bytes_decode_impl]
    CSTRING rsi, "decode"
    call builtin_func_new
    mov [rel _bytes_decode_cache], rax
.ret:
    leave
    ret
END_FUNC _get_bytes_decode_builtin

;; ============================================================================
;; bytes_tp_iter(PyBytesObject *self) -> PyBytesIterObject*
;; Create an iterator for bytes
;; ============================================================================
DEF_FUNC bytes_tp_iter
    push rbx

    mov rbx, rdi               ; save bytes obj

    mov edi, PyBytesIterObject_size
    call ap_malloc

    mov qword [rax + PyObject.ob_refcnt], 1
    lea rcx, [rel bytes_iter_type]
    mov [rax + PyObject.ob_type], rcx
    mov [rax + PyBytesIterObject.it_seq], rbx
    mov qword [rax + PyBytesIterObject.it_index], 0

    INCREF rbx

    pop rbx
    leave
    ret
END_FUNC bytes_tp_iter

;; ============================================================================
;; bytes_iter_next(PyBytesIterObject *self) -> PyObject* or NULL
;; Return next byte as SmallInt, or NULL if exhausted
;; ============================================================================
DEF_FUNC_BARE bytes_iter_next
    mov rax, [rdi + PyBytesIterObject.it_seq]   ; bytes obj
    mov rcx, [rdi + PyBytesIterObject.it_index] ; index

    ; Check bounds
    cmp rcx, [rax + PyBytesObject.ob_size]
    jge .exhausted

    ; Get byte and return as SmallInt
    movzx eax, byte [rax + PyBytesObject.data + rcx]
    RET_TAG_SMALLINT

    ; Advance index
    inc qword [rdi + PyBytesIterObject.it_index]
    ret

.exhausted:
    RET_NULL
    ret
END_FUNC bytes_iter_next

;; ============================================================================
;; bytes_iter_dealloc(PyObject *self)
;; ============================================================================
DEF_FUNC bytes_iter_dealloc
    push rbx
    mov rbx, rdi

    ; DECREF the bytes obj
    mov rdi, [rbx + PyBytesIterObject.it_seq]
    call obj_decref

    ; Free self
    mov rdi, rbx
    call ap_free

    pop rbx
    leave
    ret
END_FUNC bytes_iter_dealloc

;; ============================================================================
;; iter_self - tp_iter for iterators: return self with INCREF
;; ============================================================================
bytes_iter_self:
    inc qword [rdi + PyObject.ob_refcnt]
    mov rax, rdi
    ret
END_FUNC bytes_iter_self

;; ============================================================================
;; bytes_compare(PyObject *a, PyObject *b, int op) -> PyObject*
;; Rich comparison for bytes (supports == and !=)
;; ============================================================================
extern bool_true
extern bool_false
DEF_FUNC bytes_compare
    ; rdi=a, rsi=b, edx=op
    push rbx
    mov ebx, edx              ; save op in ebx

    ; Check if b is also bytes
    lea rax, [rel bytes_type]
    cmp [rsi + PyObject.ob_type], rax
    jne .bytes_cmp_not_impl

    ; Compare lengths
    mov rcx, [rdi + PyBytesObject.ob_size]
    cmp rcx, [rsi + PyBytesObject.ob_size]
    jne .bytes_cmp_neq

    ; Same length — compare data
    lea r8, [rdi + PyBytesObject.data]
    lea r9, [rsi + PyBytesObject.data]
    xor eax, eax
.bytes_cmp_loop:
    cmp rax, rcx
    jge .bytes_cmp_eq
    movzx edx, byte [r8 + rax]
    cmp dl, [r9 + rax]
    jne .bytes_cmp_neq
    inc rax
    jmp .bytes_cmp_loop

.bytes_cmp_eq:
    ; Equal: return True for ==, False for !=
    cmp ebx, 2                ; PyCmp_EQ = 2
    je .bytes_ret_true
    cmp ebx, 3                ; PyCmp_NE = 3
    je .bytes_ret_false
    jmp .bytes_cmp_not_impl

.bytes_cmp_neq:
    ; Not equal: return False for ==, True for !=
    cmp ebx, 2
    je .bytes_ret_false
    cmp ebx, 3
    je .bytes_ret_true
    jmp .bytes_cmp_not_impl

.bytes_ret_true:
    lea rax, [rel bool_true]
    inc qword [rax + PyObject.ob_refcnt]
    mov edx, TAG_PTR
    pop rbx
    leave
    ret
.bytes_ret_false:
    lea rax, [rel bool_false]
    inc qword [rax + PyObject.ob_refcnt]
    mov edx, TAG_PTR
    pop rbx
    leave
    ret
.bytes_cmp_not_impl:
    extern none_singleton
    lea rax, [rel none_singleton]
    inc qword [rax + PyObject.ob_refcnt]
    mov edx, TAG_PTR
    pop rbx
    leave
    ret
END_FUNC bytes_compare

;; ============================================================================
;; Data section
;; ============================================================================
section .data

bytes_name_str: db "bytes", 0
bytes_iter_name_str: db "bytes_iterator", 0
hex_digits: db "0123456789abcdef"

; Cached builtin for bytes.decode
_bytes_decode_cache: dq 0

; bytes sequence methods
align 8
bytes_sequence_methods:
    dq bytes_len            ; sq_length       +0
    dq 0                    ; sq_concat       +8
    dq 0                    ; sq_repeat       +16
    dq bytes_getitem        ; sq_item         +24
    dq 0                    ; sq_ass_item     +32
    dq bytes_contains       ; sq_contains     +40
    dq 0                    ; sq_inplace_concat +48
    dq 0                    ; sq_inplace_repeat +56

section .text

;; ============================================================================
;; bytes_mod(PyBytesObject *fmt, PyObject *args) -> PyBytesObject*
;; nb_remainder: implements b"fmt" % args
;; Strategy: convert bytes fmt to str, call str_mod, convert result to bytes
;; ============================================================================
BM_FMT   equ 8
BM_ARGS  equ 16
BM_FRAME equ 16

DEF_FUNC bytes_mod, BM_FRAME
    push rbx
    push r12

    mov [rbp-BM_FMT], rdi     ; fmt bytes obj
    mov [rbp-BM_ARGS], rsi    ; args
    ; Convert bytes to str
    mov rsi, [rdi + PyBytesObject.ob_size]
    lea rdi, [rdi + PyBytesObject.data]
    extern str_new
    call str_new
    mov rbx, rax               ; rbx = temp str

    ; Call str_mod(temp_str, args)
    extern str_mod
    mov rdi, rbx               ; temp str
    mov rsi, [rbp-BM_ARGS]    ; args
    call str_mod
    mov r12, rax               ; r12 = result str

    ; DECREF temp fmt str
    mov rdi, rbx
    DECREF_REG rdi

    ; Convert result str to bytes
    mov rdi, [r12 + PyStrObject.ob_size]
    extern bytes_new
    call bytes_new
    mov rbx, rax               ; rbx = bytes result
    ; Copy str data into bytes
    lea rdi, [rax + PyBytesObject.data]
    lea rsi, [r12 + PyStrObject.data]
    mov rdx, [r12 + PyStrObject.ob_size]
    extern ap_memcpy
    call ap_memcpy

    ; DECREF result str
    mov rdi, r12
    DECREF_REG rdi

    mov rax, rbx               ; return bytes
    mov edx, TAG_PTR
    pop r12
    pop rbx
    leave
    ret
END_FUNC bytes_mod

section .data

; bytes number methods (for % formatting)
align 8
bytes_number_methods:
    dq 0                    ; nb_add          +0
    dq 0                    ; nb_subtract     +8
    dq 0                    ; nb_multiply     +16
    dq bytes_mod            ; nb_remainder    +24

; bytes mapping methods (for subscript with int/slice)
align 8
bytes_mapping_methods:
    dq bytes_len            ; mp_length       +0
    dq bytes_subscript      ; mp_subscript    +8
    dq 0                    ; mp_ass_subscript +16

section .text

;; ============================================================================
;; bytes_type_call(type, args, nargs) -> PyBytesObject*
;; Constructor: bytes(bytes_obj) — copies data, uses passed-in type for subclass
;; ============================================================================
global bytes_type_call
BTC_TYPE  equ 8
BTC_FRAME equ 8
DEF_FUNC bytes_type_call, BTC_FRAME
    ; rdi=type, rsi=args, rdx=nargs
    mov [rbp - BTC_TYPE], rdi
    cmp rdx, 1
    jne .btc_error
    mov rdi, [rsi]              ; arg0 payload
    cmp dword [rsi + 8], TAG_SMALLINT
    je .btc_error               ; SmallInt → error
    mov rax, [rdi + PyObject.ob_type]
    lea rcx, [rel bytes_type]
    cmp rax, rcx
    jne .btc_error

    ; Copy bytes data into new object
    mov rcx, [rdi + PyBytesObject.ob_size]
    push rdi
    push rcx
    lea rdi, [rcx + PyBytesObject.data + 1]
    call ap_malloc
    pop rcx
    pop rsi

    mov qword [rax + PyBytesObject.ob_refcnt], 1
    mov rdx, [rbp - BTC_TYPE]
    mov [rax + PyBytesObject.ob_type], rdx
    mov [rax + PyBytesObject.ob_size], rcx
    ; INCREF the type (needed for heap type subclasses)
    inc qword [rdx + PyObject.ob_refcnt]

    push rax
    lea rdi, [rax + PyBytesObject.data]
    lea rsi, [rsi + PyBytesObject.data]
    mov rdx, rcx
    call ap_memcpy
    ; Null-terminate (for C string compat)
    pop rax
    mov rcx, [rax + PyBytesObject.ob_size]
    mov byte [rax + PyBytesObject.data + rcx], 0
    mov edx, TAG_PTR
    leave
    ret

.btc_error:
    lea rdi, [rel exc_TypeError_type]
    CSTRING rsi, "bytes() argument must be a bytes object"
    call raise_exception
END_FUNC bytes_type_call

section .data

; bytes type object
align 8
global bytes_type
bytes_type:
    dq 1                    ; ob_refcnt
    dq type_type            ; ob_type
    dq bytes_name_str       ; tp_name
    dq PyBytesObject.data   ; tp_basicsize (header, without data)
    dq bytes_dealloc        ; tp_dealloc
    dq bytes_repr           ; tp_repr
    dq bytes_repr           ; tp_str
    dq 0                    ; tp_hash
    dq 0                    ; tp_call
    dq bytes_getattr        ; tp_getattr
    dq 0                    ; tp_setattr
    dq bytes_compare        ; tp_richcompare
    dq bytes_tp_iter        ; tp_iter
    dq 0                    ; tp_iternext
    dq 0                    ; tp_init
    dq 0                    ; tp_new
    dq bytes_number_methods  ; tp_as_number
    dq bytes_sequence_methods ; tp_as_sequence
    dq bytes_mapping_methods  ; tp_as_mapping
    dq 0                    ; tp_base
    dq 0                    ; tp_dict
    dq 0                    ; tp_mro
    dq TYPE_FLAG_BASETYPE   ; tp_flags
    dq 0                    ; tp_bases

; bytes_iter type object
align 8
bytes_iter_type:
    dq 1                        ; ob_refcnt
    dq type_type                ; ob_type
    dq bytes_iter_name_str      ; tp_name
    dq PyBytesIterObject_size   ; tp_basicsize
    dq bytes_iter_dealloc       ; tp_dealloc
    dq 0                        ; tp_repr
    dq 0                        ; tp_str
    dq 0                        ; tp_hash
    dq 0                        ; tp_call
    dq 0                        ; tp_getattr
    dq 0                        ; tp_setattr
    dq 0                        ; tp_richcompare
    dq bytes_iter_self          ; tp_iter
    dq bytes_iter_next          ; tp_iternext
    dq 0                        ; tp_init
    dq 0                        ; tp_new
    dq 0                        ; tp_as_number
    dq 0                        ; tp_as_sequence
    dq 0                        ; tp_as_mapping
    dq 0                        ; tp_base
    dq 0                        ; tp_dict
    dq 0                        ; tp_mro
    dq 0                        ; tp_flags
    dq 0                        ; tp_bases
