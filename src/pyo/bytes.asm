; pyo/bytes.asm - Bytes type implementation
; Immutable sequence of raw bytes with inline storage

%include "macros.inc"
%include "object.inc"

extern ap_malloc
extern ap_free
extern gc_alloc
extern gc_track
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
extern exc_ValueError_type
extern int_type
extern obj_as_index
extern bool_type
extern int_to_i64
extern slice_type
extern slice_indices
extern ap_strcmp
extern builtin_func_new

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
;; bytes_getitem(PyBytesObject *self, int64_t index) -> rax = Value (SmallInt 0-255)
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
    V_PACK rax, rdx             ; return one Value
    ret

.index_error:
    push rdi
    RAISE exc_IndexError_type, "index out of range"
END_FUNC bytes_getitem

;; ============================================================================
;; bytes_subscript(PyBytesObject *self, PyObject *key) -> rax = Value
;; mp_subscript: handles both int and slice keys
;; ============================================================================
DEF_FUNC bytes_subscript
    V_UNPACK rsi, rdx           ; key Value -> (payload, tag)
    push rbx
    push r12

    mov rbx, rdi               ; bytes obj
    mov r12, rsi               ; key

    ; Check if key is a SmallInt (edx = key tag from caller)
    cmp edx, TAG_SMALLINT
    je .bs_int                 ; SmallInt → int path
    cmp edx, TAG_PTR            ; a float key is neither: classify
    jne .bs_type_error          ; fully before dereferencing, or raw
                                ; f64 bits get used as an address
    mov rax, [r12 + PyObject.ob_type]
    lea rcx, [rel slice_type]
    cmp rax, rcx
    je .bs_slice

.bs_int:
    ; obj_as_index covers int, bool, an int subclass and __index__, and
    ; raises for anything else.
    mov rdi, r12
    call obj_as_index
    ; Call bytes_getitem
    mov rdi, rbx
    mov rsi, rax
    call bytes_getitem          ; already returns a Value

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
    mov edx, TAG_PTR           ; bytes_new doesn't set tag; ap_memcpy clobbers rdx
    pop r15
    pop r14
    pop r13
    pop r12
    pop rbx
    leave
    V_PACK rax, rdx             ; return one Value
    ret

.bs_type_error:
    RAISE exc_TypeError_type, "byte indices must be integers or slices"
END_FUNC bytes_subscript

;; ============================================================================
;; bytes_contains(PyBytesObject *self, PyObject *value) -> int (0/1)
;; sq_contains: check if byte value is in bytes
;; ============================================================================
DEF_FUNC bytes_contains
    V_UNPACK rsi, rdx           ; decode the operand Value
    push rbx
    push r12
    push r13
    push r14

    mov rbx, rdi               ; bytes obj

    ; An int (or bool) searches for a single byte; a bytes searches for a
    ; subsequence.  The operand used to go straight to int_to_i64, which
    ; reads PyIntObject.compact unconditionally -- so 1.5 in b"ab" read raw
    ; f64 bits as an address -- and the subsequence form was missing
    ; entirely, so b"a" in b"xaby" was False.
    cmp edx, TAG_SMALLINT
    je .bc_byte
    cmp edx, TAG_PTR
    jne .bc_type_error
    mov rax, [rsi + PyObject.ob_type]
    lea rcx, [rel bytes_type]
    cmp rax, rcx
    je .bc_sub
    REQUIRE_INT_TYPE rax, rcx, .bc_type_error

.bc_byte:
    mov rdi, rsi                ; int_to_i64 takes the payload plus the tag
    call int_to_i64             ; in edx, not a packed Value
    ; A byte value outside 0..255 can never be present, and CPython raises
    ; for it rather than answering False.
    cmp rax, 255
    ja .bc_range_error

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

.bc_sub:
    ; Naive subsequence search: needle is short in practice.
    mov r12, [rsi + PyBytesObject.ob_size]      ; needle length
    mov r13, [rbx + PyBytesObject.ob_size]      ; haystack length
    lea r14, [rsi + PyBytesObject.data]         ; needle data
    test r12, r12
    jz .bc_found                                ; b"" is in everything
    mov rax, r13
    sub rax, r12
    js .bc_not_found                            ; needle longer than haystack
    xor r8d, r8d                                ; start offset
.bc_sub_outer:
    cmp r8, rax
    jg .bc_not_found
    xor r9d, r9d                                ; offset within the needle
.bc_sub_inner:
    cmp r9, r12
    jge .bc_found
    mov rcx, r8
    add rcx, r9
    movzx edi, byte [rbx + PyBytesObject.data + rcx]
    cmp dil, [r14 + r9]
    jne .bc_sub_next
    inc r9
    jmp .bc_sub_inner
.bc_sub_next:
    inc r8
    jmp .bc_sub_outer

.bc_found:
    mov eax, 1
    pop r14
    pop r13
    pop r12
    pop rbx
    leave
    ret

.bc_not_found:
    xor eax, eax
    pop r14
    pop r13
    pop r12
    pop rbx
    leave
    ret

.bc_type_error:
    RAISE exc_TypeError_type, "a bytes-like object is required"

.bc_range_error:
    RAISE exc_ValueError_type, "byte must be in range(0, 256)"
END_FUNC bytes_contains

;; ============================================================================
;; bytes_repr(PyObject *self) -> PyStrObject*
;; Returns b'...' representation with hex escapes for non-printable bytes
;; ============================================================================
;; bytes and bytearray have identical layouts -- ob_size at +16, data at +24 --
;; so one implementation serves both; only the wrapper text differs.
global bytearray_repr
DEF_FUNC bytes_repr
    xor esi, esi               ; 0 = b'...'
    call bytes_repr_impl
    leave
    ret
END_FUNC bytes_repr

DEF_FUNC bytearray_repr
    mov esi, 1                 ; 1 = bytearray(b'...')
    call bytes_repr_impl
    leave
    ret
END_FUNC bytearray_repr

BRI_BUF   equ 1024          ; render buffer, on the stack
DEF_FUNC_LOCAL bytes_repr_impl, 1024
    push rbx
    push r12
    push r13
    push r14
    push r15

    mov rbx, rdi               ; bytes obj
    mov r12, [rbx + PyBytesObject.ob_size]  ; length
    mov r14d, esi              ; wrap flag, preserved across the loop

    ; Pick the delimiter the way CPython does: a single quote normally, but a
    ; double quote when the data contains ' and no ", so the quote inside
    ; needs no backslash.
    mov r15d, 0x27
    xor eax, eax               ; saw a single quote?
    xor edx, edx
.br_scan:
    cmp rdx, r12
    jge .br_scan_done
    movzx ecx, byte [rbx + PyBytesObject.data + rdx]
    cmp ecx, 0x22              ; a double quote rules the switch out
    je .br_scan_done_squote
    cmp ecx, 0x27
    jne .br_scan_next
    mov eax, 1
.br_scan_next:
    inc rdx
    jmp .br_scan
.br_scan_done:
    test eax, eax
    jz .br_scan_done_squote
    mov r15d, 0x22
.br_scan_done_squote:

    ; Build repr in local buffer
    lea r13, [rbp - BRI_BUF]      ; buffer on stack

    xor ecx, ecx               ; output pos
    test r14d, r14d
    jz .br_prefix_b
    mov byte [r13 + 0], 'b'
    mov byte [r13 + 1], 'y'
    mov byte [r13 + 2], 't'
    mov byte [r13 + 3], 'e'
    mov byte [r13 + 4], 'a'
    mov byte [r13 + 5], 'r'
    mov byte [r13 + 6], 'r'
    mov byte [r13 + 7], 'a'
    mov byte [r13 + 8], 'y'
    mov byte [r13 + 9], '('
    mov ecx, 10
.br_prefix_b:
    mov byte [r13 + rcx], 'b'
    mov [r13 + rcx + 1], r15b        ; opening delimiter
    add ecx, 2

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
    cmp eax, r15d              ; the delimiter in use
    je .br_escape_quote
    test r14d, r14d            ; CPython's bytearray_repr escapes a single
    jz .br_not_squote          ; quote even under a double-quote delimiter,
    cmp eax, 0x27              ; where its bytes_repr does not
    je .br_escape_squote
.br_not_squote:
    cmp eax, 0x5c              ; backslash
    je .br_escape_bs

    ; Printable: emit directly
    mov [r13 + rcx], al
    inc ecx
    inc edx
    jmp .br_loop

.br_escape_squote:
    mov byte [r13 + rcx], 0x5c     ; backslash
    mov byte [r13 + rcx + 1], 0x27
    add ecx, 2
    inc edx
    jmp .br_loop

.br_escape_quote:
    mov byte [r13 + rcx], 0x5c     ; backslash
    mov [r13 + rcx + 1], r15b      ; the delimiter in use
    add ecx, 2
    inc edx
    jmp .br_loop

.br_escape_bs:
    mov byte [r13 + rcx], 0x5c
    mov byte [r13 + rcx + 1], 0x5c
    add ecx, 2
    inc edx
    jmp .br_loop

.br_hex:
    ; Non-printable: emit \xHH
    ; Common escapes first
    cmp eax, 0x0a
    je .br_escape_n
    cmp eax, 0x0d
    je .br_escape_r
    cmp eax, 0x09
    je .br_escape_t
    cmp eax, 0x00
    je .br_escape_0

    ; General \xHH
    mov byte [r13 + rcx], 0x5c     ; backslash
    mov byte [r13 + rcx + 1], 'x'
    push rdx
    ; High nibble
    mov edx, eax
    shr edx, 4
    lea rsi, [rel hex_digits]
    movzx edx, byte [rsi + rdx]
    mov [r13 + rcx + 2], dl
    ; Low nibble
    and eax, 0x0f
    movzx eax, byte [rsi + rax]
    mov [r13 + rcx + 3], al
    pop rdx
    add ecx, 4
    inc edx
    jmp .br_loop

.br_escape_n:
    mov byte [r13 + rcx], 0x5c
    mov byte [r13 + rcx + 1], 'n'
    add ecx, 2
    inc edx
    jmp .br_loop
.br_escape_r:
    mov byte [r13 + rcx], 0x5c
    mov byte [r13 + rcx + 1], 'r'
    add ecx, 2
    inc edx
    jmp .br_loop
.br_escape_t:
    mov byte [r13 + rcx], 0x5c
    mov byte [r13 + rcx + 1], 't'
    add ecx, 2
    inc edx
    jmp .br_loop
.br_escape_0:
    mov byte [r13 + rcx], 0x5c
    mov byte [r13 + rcx + 1], 'x'
    mov byte [r13 + rcx + 2], '0'
    mov byte [r13 + rcx + 3], '0'
    add ecx, 4
    inc edx
    jmp .br_loop

.br_close:
    mov [r13 + rcx], r15b          ; closing delimiter
    inc ecx
    test r14d, r14d
    jz .br_terminate
    mov byte [r13 + rcx], ')'
    inc ecx
.br_terminate:
    mov byte [r13 + rcx], 0        ; null terminator

    ; Create str from buffer
    mov rdi, r13
    call str_from_cstr

    pop r15
    pop r14
    pop r13
    pop r12
    pop rbx
    leave
    ret
END_FUNC bytes_repr_impl


;; ============================================================================
;; bytes_getattr(PyBytesObject *self, PyObject *name) -> rax = Value
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
    V_PACK rax, rdx             ; return one Value
    ret

.bga_decode:
    call _get_bytes_decode_builtin
    mov rdi, rax
    call obj_incref
    mov edx, TAG_PTR
    pop r12
    pop rbx
    leave
    V_PACK rax, rdx             ; return one Value
    ret
END_FUNC bytes_getattr

;; ============================================================================
;; _bytes_decode_impl(args, nargs) — b.decode([encoding[, errors]])
;;
;; utf-8 is the bytes as they stand; ascii is that with a range check; latin-1
;; turns each byte into a code point, which is where the byte string and the
;; text stop being the same thing.
;; ============================================================================
BD_SELF  equ 8
BD_OUT   equ 16
BD_POS   equ 24
BD_FRAME equ 32
DEF_FUNC _bytes_decode_impl, BD_FRAME
    push rbx
    push r12
    mov rbx, [rdi]
    mov [rbp - BD_SELF], rbx
    mov r12, [rbx + PyBytesObject.ob_size]

    ; decode([encoding[, errors]]).  An encoding that is not a str is a
    ; TypeError in CPython, not a silent fall back to utf-8.
    cmp rsi, 3
    jg .bd_too_many
    xor eax, eax
    cmp rsi, 2
    jl .bd_have_enc
    mov rax, [rdi + 8]
    extern none_singleton
    lea rcx, [rel none_singleton]
    cmp rax, rcx
    je .bd_default_enc
    V_TEST_PTR rax, rcx
    ja .bd_bad_enc
    test rax, rax
    jz .bd_bad_enc
    mov rcx, [rax + PyObject.ob_type]
    lea rdx, [rel str_type]
    cmp rcx, rdx
    jne .bd_bad_enc
    jmp .bd_have_enc
.bd_default_enc:
    xor eax, eax
.bd_have_enc:
    mov rdi, rax
    extern codec_id
    call codec_id
    cmp eax, 1
    je .bd_ascii
    cmp eax, 2
    je .bd_latin1

.bd_utf8:
    mov rbx, [rbp - BD_SELF]
    lea rdi, [rbx + PyBytesObject.data]
    mov rsi, r12
    call str_new
    pop r12
    pop rbx
    leave
    V_PACK rax, rdx
    ret

.bd_ascii:
    xor ecx, ecx
.bd_ascii_scan:
    cmp rcx, r12
    jge .bd_utf8
    movzx eax, byte [rbx + PyBytesObject.data + rcx]
    test al, 0x80
    jnz .bd_not_decodable
    inc rcx
    jmp .bd_ascii_scan

.bd_latin1:
    ; Each byte is one code point, so a byte at or above 0x80 becomes two
    ; bytes of UTF-8: the result can be twice as long.
    lea rdi, [r12 + r12]
    add rdi, PyStrObject.data + 8
    call ap_malloc
    mov [rbp - BD_OUT], rax
    mov qword [rax + PyObject.ob_refcnt], 1
    lea rcx, [rel str_type]
    mov [rax + PyObject.ob_type], rcx
    mov qword [rax + PyStrObject.ob_hash], -1
    mov [rax + PyStrObject.ob_length], r12
    mov qword [rbp - BD_POS], 0
    xor ecx, ecx
.bd_l1_loop:
    cmp rcx, r12
    jge .bd_l1_done
    mov rbx, [rbp - BD_SELF]
    movzx eax, byte [rbx + PyBytesObject.data + rcx]
    mov rdx, [rbp - BD_OUT]
    mov r8, [rbp - BD_POS]
    test al, 0x80
    jnz .bd_l1_two
    mov [rdx + PyStrObject.data + r8], al
    inc qword [rbp - BD_POS]
    jmp .bd_l1_next
.bd_l1_two:
    mov r9d, eax
    shr r9d, 6
    or r9b, 0xc0
    mov [rdx + PyStrObject.data + r8], r9b
    and eax, 0x3f
    or al, 0x80
    mov [rdx + PyStrObject.data + r8 + 1], al
    add qword [rbp - BD_POS], 2
.bd_l1_next:
    inc rcx
    jmp .bd_l1_loop
.bd_l1_done:
    mov rax, [rbp - BD_OUT]
    mov rcx, [rbp - BD_POS]
    mov [rax + PyStrObject.ob_size], rcx
    mov qword [rax + PyStrObject.data + rcx], 0
    mov edx, TAG_PTR
    pop r12
    pop rbx
    leave
    V_PACK rax, rdx
    ret

.bd_bad_enc:
    extern raise_type_error_with_name
    mov rsi, rax
    CSTRING rdi, `decode() argument 'encoding' must be str, not \x01`
    call raise_type_error_with_name
.bd_too_many:
    RAISE exc_TypeError_type, "decode() takes at most 2 arguments"

.bd_not_decodable:
    extern exc_UnicodeDecodeError_type
    RAISE exc_UnicodeDecodeError_type, "byte not in range for this encoding"
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

    ; Get byte and return as an int immediate (0..255 always fits)
    movzx eax, byte [rax + PyBytesObject.data + rcx]
    add rax, [rel v_int_bias]

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
DEF_FUNC_BARE bytes_iter_self
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
    V_UNPACK rdi, rcx           ; left  Value -> (payload, tag)
    V_UNPACK rsi, r8            ; right Value -> (payload, tag)
    ; rdi=a, rsi=b, edx=op
    push rbx
    mov ebx, edx              ; save op in ebx

    ; Check if b is also bytes
    cmp r8d, TAG_PTR          ; b may be an int or float immediate, whose
    jne .bytes_cmp_not_impl   ; payload is not an address
    lea rax, [rel bytes_type]
    cmp [rsi + PyObject.ob_type], rax
    jne .bytes_cmp_not_impl

    ; Lexicographic three-way compare, as CPython does: walk the common
    ; prefix, and if that matches, the shorter operand sorts first.  Only
    ; == and != were implemented before, so every ordering comparison
    ; between two bytes fell through to NotImplemented.
    mov rcx, [rdi + PyBytesObject.ob_size]   ; rcx = len(a)
    mov rdx, [rsi + PyBytesObject.ob_size]   ; rdx = len(b)
    mov r11, rcx
    cmp r11, rdx
    jle .bytes_have_min
    mov r11, rdx
.bytes_have_min:                             ; r11 = min(len(a), len(b))
    lea r8, [rdi + PyBytesObject.data]
    lea r9, [rsi + PyBytesObject.data]
    xor eax, eax
.bytes_cmp_loop:
    cmp rax, r11
    jge .bytes_prefix_equal
    movzx r10d, byte [r8 + rax]
    cmp r10b, [r9 + rax]                     ; bytes compare unsigned
    jb .bytes_cmp_lt
    ja .bytes_cmp_gt
    inc rax
    jmp .bytes_cmp_loop

.bytes_prefix_equal:
    cmp rcx, rdx
    jb .bytes_cmp_lt
    ja .bytes_cmp_gt
    ; fall through: the two are equal

.bytes_cmp_eq:
    cmp ebx, PY_EQ
    je .bytes_ret_true
    cmp ebx, PY_LE
    je .bytes_ret_true
    cmp ebx, PY_GE
    je .bytes_ret_true
    jmp .bytes_ret_false

.bytes_cmp_lt:
    cmp ebx, PY_LT
    je .bytes_ret_true
    cmp ebx, PY_LE
    je .bytes_ret_true
    cmp ebx, PY_NE
    je .bytes_ret_true
    jmp .bytes_ret_false

.bytes_cmp_gt:
    cmp ebx, PY_GT
    je .bytes_ret_true
    cmp ebx, PY_GE
    je .bytes_ret_true
    cmp ebx, PY_NE
    je .bytes_ret_true
    jmp .bytes_ret_false

.bytes_ret_true:
    RET_TRUE
    pop rbx
    leave
    ret
.bytes_ret_false:
    RET_FALSE
    pop rbx
    leave
    ret
.bytes_cmp_not_impl:
    ; NotImplemented is a NULL return, so op_compare_op can try the reflected
    ; operand and then fall back to identity.  Returning None instead made
    ; b"a" == 5 and every ordering comparison evaluate to None.
    RET_NULL
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
align 8
_bytes_decode_cache: dq 0

; bytes sequence methods
align 8
bytes_sequence_methods:
    dq bytes_len            ; sq_length       +0
    dq bytes_concat         ; sq_concat       +8
    dq bytes_repeat         ; sq_repeat       +16
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
    V_UNPACK rdi, rdx           ; left  Value -> (payload, tag)
    V_UNPACK rsi, rcx           ; right Value -> (payload, tag)
    push rbx
    push r12

    mov [rbp-BM_FMT], rdi     ; fmt bytes obj
    mov [rbp-BM_ARGS], rsi    ; args
    ; Convert bytes to str
    mov rsi, [rdi + PyBytesObject.ob_size]
    lea rdi, [rdi + PyBytesObject.data]
    extern str_new_heap
    call str_new_heap
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
    V_PACK rax, rdx             ; return one Value
    ret
END_FUNC bytes_mod

section .data

; bytes number methods (for % formatting)
align 8
bytes_number_methods:
    dq bytes_concat         ; nb_add          +0
    dq 0                    ; nb_subtract     +8
    dq bytes_repeat         ; nb_multiply     +16
    dq bytes_mod            ; nb_remainder    +24
    dq 0                    ; nb_divmod       +32
    dq 0                    ; nb_power        +40
    dq 0                    ; nb_negative     +48
    dq 0                    ; nb_positive     +56
    dq 0                    ; nb_absolute     +64
    dq 0                    ; nb_bool         +72
    dq 0                    ; nb_invert       +80
    dq 0                    ; nb_lshift       +88
    dq 0                    ; nb_rshift       +96
    dq 0                    ; nb_and          +104
    dq 0                    ; nb_xor          +112
    dq 0                    ; nb_or           +120
    dq 0                    ; nb_int          +128
    dq 0                    ; nb_float        +136
    dq 0                    ; nb_floor_divide +144
    dq 0                    ; nb_true_divide  +152
    dq 0                    ; nb_index        +160
    dq 0                        ; nb_iadd         +168
    dq 0                        ; nb_isub         +176
    dq 0                        ; nb_imul         +184
    dq 0                        ; nb_irem         +192
    dq 0                        ; nb_ipow         +200
    dq 0                        ; nb_ilshift      +208
    dq 0                        ; nb_irshift      +216
    dq 0                        ; nb_iand         +224
    dq 0                        ; nb_ixor         +232
    dq 0                        ; nb_ior          +240
    dq 0                        ; nb_ifloor_divide +248
    dq 0                        ; nb_itrue_divide +256
    dq 0 ; nb_matmul
    dq 0 ; nb_imatmul

; bytes mapping methods (for subscript with int/slice)
align 8
bytes_mapping_methods:
    dq bytes_len            ; mp_length       +0
    dq bytes_subscript      ; mp_subscript    +8
    dq 0                    ; mp_ass_subscript +16

section .text

;; ============================================================================
;; bytes_concat(left Value, right Value) -> Value
;; bytes had neither sq_concat nor nb_add, so `x + y` on two bytes variables
;; raised TypeError; only the constant-folded form worked.
;; ============================================================================
DEF_FUNC bytes_concat
    push rbx
    push r12
    push r13
    mov rbx, rdi
    mov r12, rsi

    V_TEST_PTR rbx, rax
    ja .bc_type_error
    V_TEST_PTR r12, rax
    ja .bc_type_error
    mov rax, [rbx + PyObject.ob_type]
    lea rcx, [rel bytes_type]
    cmp rax, rcx
    jne .bc_type_error
    mov rax, [r12 + PyObject.ob_type]
    cmp rax, rcx
    jne .bc_type_error

    mov r13, [rbx + PyBytesObject.ob_size]
    add r13, [r12 + PyBytesObject.ob_size]
    lea rdi, [r13 + PyBytesObject.data + 8]
    call ap_malloc
    mov qword [rax + PyObject.ob_refcnt], 1
    lea rcx, [rel bytes_type]
    mov [rax + PyObject.ob_type], rcx
    mov [rax + PyBytesObject.ob_size], r13

    push rax
    lea rdi, [rax + PyBytesObject.data]
    lea rsi, [rbx + PyBytesObject.data]
    mov rdx, [rbx + PyBytesObject.ob_size]
    call ap_memcpy
    mov rax, [rsp]
    mov rdi, [rbx + PyBytesObject.ob_size]
    lea rdi, [rax + PyBytesObject.data + rdi]
    lea rsi, [r12 + PyBytesObject.data]
    mov rdx, [r12 + PyBytesObject.ob_size]
    call ap_memcpy
    pop rax
    mov qword [rax + PyBytesObject.data + r13], 0
    mov edx, TAG_PTR
    pop r13
    pop r12
    pop rbx
    leave
    ret

.bc_type_error:
    mov rsi, r12
    CSTRING rdi, `can't concat \x01 to bytes`
    extern raise_type_error_with_name
    call raise_type_error_with_name
END_FUNC bytes_concat

;; ============================================================================
;; bytes_repeat(bytes Value, count Value) -> Value
;; ============================================================================
DEF_FUNC bytes_repeat
    push rbx
    push r12
    push r13
    push r14
    mov rbx, rdi
    mov r14, rsi

    mov rsi, r14
    extern seq_repeat_check_count
    call seq_repeat_check_count

    mov rdi, r14
    V_UNPACK rdi, rdx
    push rdi
    push rdx
    extern int_fits_i64
    call int_fits_i64
    pop rdx
    pop rdi
    test eax, eax
    jz .brep_overflow
    extern int_to_i64
    call int_to_i64
    mov r12, rax
    test r12, r12
    jg .brep_positive
    xor r12d, r12d
.brep_positive:

    mov r13, [rbx + PyBytesObject.ob_size]
    mov r14, r13
    imul r14, r12
    jo .brep_overflow
    cmp r14, 0x10000000
    ja .brep_overflow

    lea rdi, [r14 + PyBytesObject.data + 8]
    call ap_malloc
    push rax
    mov qword [rax + PyObject.ob_refcnt], 1
    lea rcx, [rel bytes_type]
    mov [rax + PyObject.ob_type], rcx
    mov [rax + PyBytesObject.ob_size], r14

    lea rdi, [rax + PyBytesObject.data]
    xor ecx, ecx
.brep_loop:
    cmp rcx, r12
    jge .brep_done
    push rcx
    push rdi
    lea rsi, [rbx + PyBytesObject.data]
    mov rdx, r13
    call ap_memcpy
    pop rdi
    pop rcx
    add rdi, r13
    inc rcx
    jmp .brep_loop
.brep_done:
    pop rax
    mov qword [rax + PyBytesObject.data + r14], 0
    mov edx, TAG_PTR
    pop r14
    pop r13
    pop r12
    pop rbx
    leave
    ret

.brep_overflow:
    extern exc_OverflowError_type
    RAISE exc_OverflowError_type, "repeated bytes are too long"
END_FUNC bytes_repeat

;; ============================================================================
;; bytes_type_call(type, args, nargs) -> PyBytesObject*
;; Constructor: bytes(bytes_obj) — copies data, uses passed-in type for subclass
;; ============================================================================
global bytes_type_call

;; ============================================================================
;; byteslike_source(rdi = args, rsi = nargs, rdx = name cstr)
;;   -> rax = ap_malloc'd buffer (0 when empty), rdx = length
;;
;; The raw bytes a bytes() or bytearray() call asks for, gathered before
;; anything is allocated.  CPython accepts no argument, a count, another
;; bytes-like, or any iterable of ints; the constructors here used to accept
;; a bytes object and nothing else, which is what stopped _collections_abc
;; at `type(iter(bytearray()))` and with it every module behind it.
;;
;; The buffer is over-allocated by 8 so bytes objects can zero-terminate.
;; ============================================================================

;; ============================================================================
;; bls_item_byte(rdi = item Value) -> eax = the byte 0..255,
;;                                    -1 not an integer,
;;                                    -2 an integer outside range(0, 256)
;;
;; One item of the iterable handed to bytes() or bytearray().  It used to test
;; V_IS_INT and nothing else, so only an int *immediate* was accepted: every
;; heap PyIntObject -- which under INT_STRESS is every int at all, and
;; otherwise any value past 2^50 -- was reported as "'int' object cannot be
;; interpreted as an integer".  Nothing caught it because `make INT_STRESS=1`
;; silently relinked the unstressed binary.
;;
;; __index__ is honoured because CPython honours it here: bytes([C()]) is b'A'
;; when C.__index__ returns 65.  A value too wide for int64 is out of range
;; rather than a type error, matching bytes([2**100]) -> ValueError.
;; ============================================================================
BIB_ITEM  equ 8
BIB_FRAME equ 16            ; + 0 pushes = 16
DEF_FUNC_LOCAL bls_item_byte, BIB_FRAME
    mov [rbp - BIB_ITEM], rdi
    V_IS_INT rdi, rdx
    jae .bib_immediate

    V_TEST_PTR rdi, rdx
    ja .bib_not_int
    mov rax, [rdi + PyObject.ob_type]
    REQUIRE_INT_TYPE rax, rcx, .bib_try_index

    mov edx, TAG_PTR
    call int_fits_i64
    test eax, eax
    jz .bib_range               ; wider than int64 is certainly not a byte
    mov rdi, [rbp - BIB_ITEM]
    mov edx, TAG_PTR
    call int_to_i64
    jmp .bib_check

.bib_immediate:
    V_TO_I64 rdi
    mov rax, rdi
    jmp .bib_check

    ; Not an int, but __index__ makes an object usable wherever one is wanted.
    ; obj_as_index raises on anything else, so the protocol is checked first
    ; and a bare object falls out as a type error with the buffer still owned
    ; by the caller.
.bib_try_index:
    mov rax, [rdi + PyObject.ob_type]
    mov rax, [rax + PyTypeObject.tp_as_number]
    test rax, rax
    jz .bib_not_int
    cmp qword [rax + PyNumberMethods.nb_index], 0
    je .bib_not_int
    mov edx, TAG_PTR
    call obj_as_index

.bib_check:
    cmp rax, 0
    jl .bib_range
    cmp rax, 255
    jg .bib_range
    leave
    ret

.bib_not_int:
    mov eax, -1
    leave
    ret

.bib_range:
    mov eax, -2
    leave
    ret
END_FUNC bls_item_byte

;; ============================================================================
;; (continued) bytes_load_source
;; ============================================================================
BLS_ARGS  equ 8
BLS_RANGEMSG equ 16
BLS_BADITEM equ 56
BLS_BUF   equ 32
BLS_LIST  equ 40
BLS_FRAME equ 64
DEF_FUNC byteslike_source, BLS_FRAME
    push rbx
    push r12
    mov [rbp - BLS_ARGS], rdi
    mov [rbp - BLS_RANGEMSG], rdx
    mov qword [rbp - BLS_LIST], 0
    mov qword [rbp - BLS_BUF], 0
    test rsi, rsi
    jz .bls_empty
    cmp rsi, 1
    jne .bls_too_many

    mov rdi, [rdi]              ; the one argument, as a Value
    V_IS_INT rdi, rax
    jae .bls_count
    V_TEST_PTR rdi, rax
    ja .bls_bad_type
    test rdi, rdi
    jz .bls_bad_type

    mov rax, [rdi + PyObject.ob_type]
    lea rcx, [rel bytes_type]
    cmp rax, rcx
    je .bls_copy_bytes
    lea rcx, [rel bytearray_type]
    cmp rax, rcx
    je .bls_copy_bytearray
    extern int_type
    lea rcx, [rel int_type]
    cmp rax, rcx
    je .bls_count_obj
    extern str_type
    lea rcx, [rel str_type]
    cmp rax, rcx
    je .bls_need_encoding
    jmp .bls_iterable

.bls_empty:
    xor eax, eax
    xor edx, edx
    pop r12
    pop rbx
    leave
    ret

    ; bytes(n) / bytearray(n): n zero bytes.
.bls_count:
    V_TO_I64 rdi
    mov rbx, rdi
    jmp .bls_count_have
.bls_count_obj:
    call int_to_i64
    mov rbx, rax
.bls_count_have:
    test rbx, rbx
    js .bls_negative
    jz .bls_empty
    lea rdi, [rbx + 8]
    call ap_malloc
    mov r12, rax
    mov rdi, rax
    xor esi, esi
    lea rdx, [rbx + 8]
    extern ap_memset
    call ap_memset
    mov rax, r12
    mov rdx, rbx
    pop r12
    pop rbx
    leave
    ret

.bls_copy_bytes:
    mov rbx, [rdi + PyBytesObject.ob_size]
    lea r12, [rdi + PyBytesObject.data]
    jmp .bls_copy

.bls_copy_bytearray:
    mov rbx, [rdi + PyByteArrayObject.ob_size]
    lea r12, [rdi + PyByteArrayObject.data]

.bls_copy:
    test rbx, rbx
    jz .bls_empty
    lea rdi, [rbx + 8]
    call ap_malloc
    push rax
    mov rdi, rax
    mov rsi, r12
    mov rdx, rbx
    call ap_memcpy
    pop rax
    mov qword [rax + rbx], 0
    mov rdx, rbx
    pop r12
    pop rbx
    leave
    ret

    ; Any other iterable: materialise it as a list, then take one byte per
    ; item.  Going through list() rather than the iterator protocol directly
    ; keeps __iter__/__next__ on heap types working for free.
.bls_iterable:
    extern list_type
    extern list_type_call
    lea rdi, [rel list_type]
    mov rsi, [rbp - BLS_ARGS]
    mov edx, 1
    call list_type_call
    V_UNPACK rax, rdx
    test rax, rax
    jz .bls_bad_type
    mov [rbp - BLS_LIST], rax
    mov rbx, [rax + PyListObject.ob_size]
    test rbx, rbx
    jz .bls_iter_empty
    lea rdi, [rbx + 8]
    call ap_malloc
    mov [rbp - BLS_BUF], rax
    mov r12, rax
    mov rax, [rbp - BLS_LIST]
    mov rax, [rax + PyListObject.ob_item]
    xor ecx, ecx
.bls_iter_loop:
    cmp rcx, rbx
    jge .bls_iter_done
    mov rdi, [rax + rcx*8]
    push rax                    ; bls_item_byte clobbers the caller-saved regs
    push rcx                    ; two pushes, so rsp stays 16-byte aligned
    call bls_item_byte
    mov edx, eax                ; the result, before `pop rax` overwrites it
    pop rcx
    pop rax
    test edx, edx
    js .bls_iter_reject
    mov [r12 + rcx], dl
    inc rcx
    jmp .bls_iter_loop

.bls_iter_reject:
    ; -1 = not an integer at all, -2 = an integer outside range(0, 256).
    mov rdi, [rax + rcx*8]      ; the offending item, for the message
    cmp edx, -2
    je .bls_iter_range
    jmp .bls_iter_bad
.bls_iter_done:
    mov qword [r12 + rbx], 0
    mov rdi, [rbp - BLS_LIST]
    call obj_decref
    mov rax, r12
    mov rdx, rbx
    pop r12
    pop rbx
    leave
    ret
.bls_iter_empty:
    mov rdi, [rbp - BLS_LIST]
    call obj_decref
    jmp .bls_empty
    ; raise_exception abandons this frame, so the list and the buffer have to
    ; go first or a loop that keeps catching the error keeps leaking them.
.bls_release:
    mov rdi, [rbp - BLS_LIST]
    test rdi, rdi
    jz .bls_release_buf
    mov qword [rbp - BLS_LIST], 0
    call obj_decref
.bls_release_buf:
    mov rdi, [rbp - BLS_BUF]
    test rdi, rdi
    jz .bls_released
    mov qword [rbp - BLS_BUF], 0
    call ap_free
.bls_released:
    ret

.bls_iter_bad:
    ; A non-integer item is a TypeError, as it is in CPython; only an integer
    ; that will not fit in a byte is a ValueError.
    mov [rbp - BLS_BADITEM], rdi
    call .bls_release
    extern raise_type_error_with_name
    CSTRING rdi, `'\x01' object cannot be interpreted as an integer`
    mov rsi, [rbp - BLS_BADITEM]
    call raise_type_error_with_name
.bls_iter_range:
    call .bls_release
    lea rdi, [rel exc_ValueError_type]
    ; bytes() and bytearray() word this differently; the caller says which.
    mov rsi, [rbp - BLS_RANGEMSG]
    call raise_exception

.bls_negative:
    RAISE exc_ValueError_type, "negative count"
.bls_need_encoding:
    RAISE exc_TypeError_type, "string argument without an encoding"
.bls_too_many:
    RAISE exc_TypeError_type, "encoding and errors arguments are not supported"
.bls_bad_type:
    RAISE exc_TypeError_type, "cannot convert this object to bytes"
END_FUNC byteslike_source

BTC_TYPE  equ 8
BTC_BUF   equ 16
BTC_LEN   equ 24
BTC_FRAME equ 32
DEF_FUNC bytes_type_call, BTC_FRAME
    ; rdi=type, rsi=args, rdx=nargs
    push rbx
    mov [rbp - BTC_TYPE], rdi
    mov rdi, rsi
    mov rsi, rdx
    lea rdx, [rel bytes_range_msg]
    call byteslike_source
    mov [rbp - BTC_BUF], rax
    mov [rbp - BTC_LEN], rdx

    mov rcx, rdx
    mov rdx, [rbp - BTC_TYPE]
    lea rdi, [rcx + PyBytesObject.data + 8]
    ; A subclass carrying a __dict__ at its tail needs one more word for it.
    cmp qword [rdx + PyTypeObject.tp_dictoffset], TP_DICT_AT_TAIL
    jne .btc_no_tail
    add rdi, 8
.btc_no_tail:
    test qword [rdx + PyTypeObject.tp_flags], TYPE_FLAG_HAVE_GC
    jz .btc_plain_alloc
    mov rsi, rdx
    call gc_alloc
    jmp .btc_alloc_done
.btc_plain_alloc:
    call ap_malloc
    mov qword [rax + PyBytesObject.ob_refcnt], 1
    mov rdx, [rbp - BTC_TYPE]
    mov [rax + PyBytesObject.ob_type], rdx
.btc_alloc_done:
    mov rbx, rax
    mov rcx, [rbp - BTC_LEN]
    mov [rbx + PyBytesObject.ob_size], rcx
    mov rdx, [rbp - BTC_TYPE]
    inc qword [rdx + PyObject.ob_refcnt]

    test rcx, rcx
    jz .btc_no_copy
    lea rdi, [rbx + PyBytesObject.data]
    mov rsi, [rbp - BTC_BUF]
    mov rdx, rcx
    call ap_memcpy
.btc_no_copy:
    mov rcx, [rbp - BTC_LEN]
    mov qword [rbx + PyBytesObject.data + rcx], 0
    mov rdx, [rbp - BTC_TYPE]
    cmp qword [rdx + PyTypeObject.tp_dictoffset], TP_DICT_AT_TAIL
    jne .btc_no_tail_zero
    mov qword [rbx + PyBytesObject.data + rcx + 8], 0    ; the tail __dict__
.btc_no_tail_zero:
    mov rdi, [rbp - BTC_BUF]
    test rdi, rdi
    jz .btc_no_free
    call ap_free
.btc_no_free:

    mov rdx, [rbp - BTC_TYPE]
    test qword [rdx + PyTypeObject.tp_flags], TYPE_FLAG_HAVE_GC
    jz .btc_no_track
    mov rdi, rbx
    call gc_track
.btc_no_track:
    mov rax, rbx
    mov edx, TAG_PTR
    pop rbx
    leave
    ret
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
    dq 0                        ; tp_traverse
    dq 0                        ; tp_clear
    dq 0 ; tp_dictoffset

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
    dq 0                        ; tp_traverse
    dq 0                        ; tp_clear
    dq 0 ; tp_dictoffset

section .rodata
bytes_range_msg: db "bytes must be in range(0, 256)", 0
global bytearray_range_msg
bytearray_range_msg: db "byte must be in range(0, 256)", 0

;; ============================================================================
;; (was src/pyo/bytearray.asm)
;; ============================================================================

section .text

extern ap_malloc
extern ap_free
extern ap_memcpy
extern gc_alloc
extern gc_track
extern type_type
extern raise_exception
extern exc_TypeError_type
extern obj_decref
extern v_int_bias

section .text

;; ============================================================================
;; bytearray_type_call(type, args, nargs) -> PyByteArrayObject*
;; Constructor: bytearray(bytes_obj)
;; ============================================================================
global bytearray_type_call
BA_TYPE  equ 8
BA_BUF   equ 16
BA_LEN   equ 24
BA_FRAME equ 32
DEF_FUNC bytearray_type_call, BA_FRAME
    ; rdi=type, rsi=args, rdx=nargs
    push rbx
    mov [rbp - BA_TYPE], rdi           ; save type
    mov rdi, rsi
    mov rsi, rdx
    lea rdx, [rel bytearray_range_msg]
    call byteslike_source
    mov [rbp - BA_BUF], rax
    mov [rbp - BA_LEN], rdx

    mov rcx, rdx
    mov rdx, [rbp - BA_TYPE]
    test qword [rdx + PyTypeObject.tp_flags], TYPE_FLAG_HAVE_GC
    lea rdi, [rcx + PyByteArrayObject.data]
    jz .ba_plain_alloc
    mov rsi, rdx
    call gc_alloc
    jmp .ba_alloc_done
.ba_plain_alloc:
    call ap_malloc
    mov qword [rax + PyByteArrayObject.ob_refcnt], 1
    mov rdx, [rbp - BA_TYPE]
    mov [rax + PyByteArrayObject.ob_type], rdx
.ba_alloc_done:
    mov rbx, rax
    mov rcx, [rbp - BA_LEN]
    mov [rbx + PyByteArrayObject.ob_size], rcx
    mov rdx, [rbp - BA_TYPE]
    inc qword [rdx + PyObject.ob_refcnt]

    test rcx, rcx
    jz .ba_no_copy
    lea rdi, [rbx + PyByteArrayObject.data]
    mov rsi, [rbp - BA_BUF]
    mov rdx, rcx
    call ap_memcpy
.ba_no_copy:
    mov rdi, [rbp - BA_BUF]
    test rdi, rdi
    jz .ba_no_free
    call ap_free
.ba_no_free:

    mov rdx, [rbp - BA_TYPE]
    test qword [rdx + PyTypeObject.tp_flags], TYPE_FLAG_HAVE_GC
    jz .ba_no_track
    mov rdi, rbx
    call gc_track
.ba_no_track:
    mov rax, rbx
    mov edx, TAG_PTR
    pop rbx
    leave
    ret
END_FUNC bytearray_type_call

;; ============================================================================
;; bytearray_dealloc(obj)
;; ============================================================================
DEF_FUNC_BARE bytearray_dealloc
    jmp ap_free
END_FUNC bytearray_dealloc

;; ============================================================================
;; bytearray_len(obj) -> int64
;; ============================================================================
DEF_FUNC_BARE bytearray_len
    mov rax, [rdi + PyByteArrayObject.ob_size]
    ret
END_FUNC bytearray_len

;; ============================================================================
;; bytearray_tp_iter / bytearray_iter_next
;; A bytearray was not iterable at all, so `for b in ba` and every stdlib
;; census that does type(iter(bytearray())) stopped here.  Same shape as the
;; bytes iterator; the index is checked against the current length each time,
;; so a bytearray that shrinks under the iterator just ends early.
;; ============================================================================
DEF_FUNC bytearray_tp_iter
    push rbx
    mov rbx, rdi
    mov edi, PyBytesIterObject_size
    call ap_malloc
    mov qword [rax + PyObject.ob_refcnt], 1
    lea rcx, [rel bytearray_iter_type]
    mov [rax + PyObject.ob_type], rcx
    mov [rax + PyBytesIterObject.it_seq], rbx
    mov qword [rax + PyBytesIterObject.it_index], 0
    inc qword [rbx + PyObject.ob_refcnt]
    pop rbx
    leave
    ret
END_FUNC bytearray_tp_iter

DEF_FUNC_BARE bytearray_iter_next
    mov rax, [rdi + PyBytesIterObject.it_seq]
    mov rcx, [rdi + PyBytesIterObject.it_index]
    cmp rcx, [rax + PyByteArrayObject.ob_size]
    jge .exhausted
    movzx eax, byte [rax + PyByteArrayObject.data + rcx]
    add rax, [rel v_int_bias]
    inc qword [rdi + PyBytesIterObject.it_index]
    ret
.exhausted:
    xor eax, eax
    ret
END_FUNC bytearray_iter_next

DEF_FUNC bytearray_iter_self
    inc qword [rdi + PyObject.ob_refcnt]
    mov rax, rdi
    leave
    ret
END_FUNC bytearray_iter_self

DEF_FUNC bytearray_iter_dealloc
    push rbx
    mov rbx, rdi
    mov rdi, [rbx + PyBytesIterObject.it_seq]
    test rdi, rdi
    jz .free
    call obj_decref
.free:
    mov rdi, rbx
    call ap_free
    pop rbx
    leave
    ret
END_FUNC bytearray_iter_dealloc

;; ============================================================================
;; Type object
;; ============================================================================
section .data

align 8
ba_name_str:  db "bytearray", 0

align 8
bytearray_seq_methods:
    dq bytearray_len       ; +0: sq_length
    dq 0                   ; +8: sq_concat
    dq 0                   ; +16: sq_repeat
    dq 0                   ; +24: sq_item
    dq 0                   ; +32: sq_ass_item
    dq 0                   ; +40: sq_contains
    dq 0                   ; +48: sq_inplace_concat
    dq 0                   ; +56: sq_inplace_repeat

align 8
global bytearray_type
bytearray_type:
    dq 1                            ; ob_refcnt
    dq type_type                    ; ob_type
    dq ba_name_str                  ; tp_name
    dq PyByteArrayObject.data       ; tp_basicsize
    dq bytearray_dealloc            ; tp_dealloc
    dq bytearray_repr               ; tp_repr
    dq bytearray_repr               ; tp_str
    dq 0                            ; tp_hash
    dq 0                            ; tp_call (set by add_builtin_type)
    dq 0                            ; tp_getattr
    dq 0                            ; tp_setattr
    dq 0                            ; tp_richcompare
    dq bytearray_tp_iter            ; tp_iter
    dq 0                            ; tp_iternext
    dq 0                            ; tp_init
    dq 0                            ; tp_new
    dq 0                            ; tp_as_number
    dq bytearray_seq_methods        ; tp_as_sequence
    dq 0                            ; tp_as_mapping
    dq 0                            ; tp_base
    dq 0                            ; tp_dict
    dq 0                            ; tp_mro
    dq TYPE_FLAG_BASETYPE           ; tp_flags (allow subclassing)
    dq 0                            ; tp_bases
    dq 0                        ; tp_traverse
    dq 0                        ; tp_clear
    dq 0 ; tp_dictoffset

align 8
ba_iter_name_str: db "bytearray_iterator", 0

align 8
global bytearray_iter_type
bytearray_iter_type:
    dq 1                            ; ob_refcnt
    dq type_type                    ; ob_type
    dq ba_iter_name_str             ; tp_name
    dq PyBytesIterObject_size       ; tp_basicsize
    dq bytearray_iter_dealloc       ; tp_dealloc
    dq 0                            ; tp_repr
    dq 0                            ; tp_str
    dq 0                            ; tp_hash
    dq 0                            ; tp_call
    dq 0                            ; tp_getattr
    dq 0                            ; tp_setattr
    dq 0                            ; tp_richcompare
    dq bytearray_iter_self          ; tp_iter
    dq bytearray_iter_next          ; tp_iternext
    dq 0                            ; tp_init
    dq 0                            ; tp_new
    dq 0                            ; tp_as_number
    dq 0                            ; tp_as_sequence
    dq 0                            ; tp_as_mapping
    dq 0                            ; tp_base
    dq 0                            ; tp_dict
    dq 0                            ; tp_mro
    dq 0                            ; tp_flags
    dq 0                            ; tp_bases
    dq 0                            ; tp_traverse
    dq 0                            ; tp_clear
    dq 0                            ; tp_dictoffset

;; ============================================================================
;; (was src/pyo/memview.asm)
;; ============================================================================

section .text

extern ap_malloc
extern ap_free
extern ap_memcpy
extern type_type
extern obj_incref
extern obj_decref
extern raise_exception
extern exc_TypeError_type
extern int_type
extern bool_type
extern exc_IndexError_type
extern int_to_i64
extern slice_type
extern slice_indices

section .text

;; ============================================================================
;; memoryview_type_call(type, args, nargs) -> PyMemoryViewObject*
;; Constructor: memoryview(bytes_obj)
;; ============================================================================
global memoryview_type_call
MV_FRAME equ 8
DEF_FUNC memoryview_type_call, MV_FRAME
    ; rdi=type, rsi=args, rdx=nargs
    cmp rdx, 1
    jne .mv_error
    mov rdi, [rsi]                     ; arg0 payload
    ; Must be a bytes-like object (reject all non-pointer tags)
    V_TEST_PTR_M [rsi], r11      ; args[0] a pointer?
    ja .mv_error
    mov rax, [rdi + PyObject.ob_type]
    lea rcx, [rel bytes_type]
    cmp rax, rcx
    jne .mv_check_bytearray

.mv_from_bytes:
    ; rdi = bytes obj
    push rdi
    mov edi, PyMemoryViewObject_size
    call ap_malloc
    pop rdi                            ; source bytes

    ; Init header
    mov qword [rax + PyMemoryViewObject.ob_refcnt], 1
    lea rcx, [rel memoryview_type]
    mov [rax + PyMemoryViewObject.ob_type], rcx
    mov [rax + PyMemoryViewObject.mv_source], rdi
    push rax                           ; save result
    push rdi                           ; save for INCREF
    INCREF rdi
    pop rdi
    pop rax
    ; Set buffer pointer and length
    mov rcx, [rdi + PyBytesObject.ob_size]
    mov [rax + PyMemoryViewObject.mv_len], rcx
    lea rcx, [rdi + PyBytesObject.data]
    mov [rax + PyMemoryViewObject.mv_buf], rcx
    mov edx, TAG_PTR
    leave
    ret

.mv_check_bytearray:
    lea rcx, [rel bytearray_type]
    cmp rax, rcx
    je .mv_from_bytes                  ; same layout as bytes
    jmp .mv_error

.mv_error:
    RAISE exc_TypeError_type, "memoryview: a bytes-like object is required"
END_FUNC memoryview_type_call


;; Proper dealloc:
DEF_FUNC memoryview_dealloc_proper
    push rdi                           ; save self
    mov rdi, [rdi + PyMemoryViewObject.mv_source]
    call obj_decref
    pop rdi                            ; restore self
    call ap_free
    leave
    ret
END_FUNC memoryview_dealloc_proper

;; ============================================================================
;; memoryview_subscript(obj, key) -> PyMemoryViewObject* (slice)
;; ============================================================================
MS_OBJ   equ 8
MS_KEY   equ 16
MS_FRAME equ 16
DEF_FUNC memoryview_subscript, MS_FRAME
    V_UNPACK rsi, rdx           ; key Value -> (payload, tag)
    mov [rbp - MS_OBJ], rdi
    mov [rbp - MS_KEY], rsi

    ; Check if key is a SmallInt (edx = key tag from caller)
    cmp edx, TAG_SMALLINT
    je .ms_int_index                   ; SmallInt index
    cmp edx, TAG_PTR            ; a float key is neither: classify
    jne .ms_type_error          ; fully before dereferencing, or raw
                                ; f64 bits get used as an address
    mov rax, [rsi + PyObject.ob_type]
    lea rcx, [rel slice_type]
    cmp rax, rcx
    jne .ms_int_index_heap

    ; Slice: call slice_indices(slice, length) -> rax=start, rdx=stop, rcx=step
    mov rdi, rsi                       ; slice obj
    mov rsi, [rbp - MS_OBJ]
    mov rsi, [rsi + PyMemoryViewObject.mv_len]  ; length
    call slice_indices
    ; rax=start, rdx=stop, rcx=step

    ; Only support step=1 for now
    cmp rcx, 1
    jne .ms_step_error

    mov r8, rax                        ; start
    sub rdx, rax
    mov r9, rdx                        ; slicelength = stop - start

    ; Create new memoryview pointing to slice of source
    push r8                            ; save start
    push r9                            ; save slicelength
    mov edi, PyMemoryViewObject_size
    call ap_malloc
    pop r9                             ; slicelength
    pop r8                             ; start

    ; Init the new memoryview
    mov qword [rax + PyMemoryViewObject.ob_refcnt], 1
    lea rcx, [rel memoryview_type]
    mov [rax + PyMemoryViewObject.ob_type], rcx

    ; Share the same source object
    mov rdi, [rbp - MS_OBJ]
    mov rcx, [rdi + PyMemoryViewObject.mv_source]
    mov [rax + PyMemoryViewObject.mv_source], rcx

    ; Buffer = original buffer + start
    mov rdx, [rdi + PyMemoryViewObject.mv_buf]
    add rdx, r8
    mov [rax + PyMemoryViewObject.mv_buf], rdx

    ; Length = slicelength
    mov [rax + PyMemoryViewObject.mv_len], r9

    ; INCREF source
    push rax
    mov rdi, rcx
    INCREF rdi
    pop rax

    mov edx, TAG_PTR
    leave
    V_PACK rax, rdx             ; return one Value
    ret

.ms_int_index:
    ; SmallInt index — return single byte as SmallInt
    mov rdi, [rbp - MS_OBJ]
    ; Handle negative index
    mov rcx, [rdi + PyMemoryViewObject.mv_len]
    test rsi, rsi
    jns .ms_check_bounds
    add rsi, rcx
.ms_check_bounds:
    cmp rsi, 0
    jl .ms_index_error
    cmp rsi, rcx
    jge .ms_index_error
    mov rdx, [rdi + PyMemoryViewObject.mv_buf]
    movzx eax, byte [rdx + rsi]
    RET_TAG_SMALLINT
    leave
    V_PACK rax, rdx             ; return one Value
    ret

.ms_int_index_heap:
    ; Heap int index — convert to i64
    mov rax, [rsi + PyObject.ob_type]   ; int_to_i64 reads PyIntObject.compact
    REQUIRE_INT_TYPE rax, rcx, .ms_type_error   ; unconditionally
    push rdi
    mov rdi, rsi
    call int_to_i64
    mov rsi, rax
    pop rdi
    jmp .ms_check_bounds

.ms_index_error:
    RAISE exc_IndexError_type, "index out of range"

.ms_step_error:
    RAISE exc_TypeError_type, "memoryview: unsupported step"

.ms_type_error:
    RAISE exc_TypeError_type, "memoryview: invalid slice key"
END_FUNC memoryview_subscript

;; ============================================================================
;; memoryview_len(obj) -> int64
;; ============================================================================
DEF_FUNC_BARE memoryview_len
    mov rax, [rdi + PyMemoryViewObject.mv_len]
    ret
END_FUNC memoryview_len

;; ============================================================================
;; Type object
;; ============================================================================
section .data

align 8
mv_name_str:  db "memoryview", 0

align 8
memoryview_seq_methods:
    dq memoryview_len       ; +0: sq_length
    dq 0                    ; +8: sq_concat
    dq 0                    ; +16: sq_repeat
    dq 0                    ; +24: sq_item
    dq 0                    ; +32: sq_ass_item
    dq 0                    ; +40: sq_contains
    dq 0                    ; +48: sq_inplace_concat
    dq 0                    ; +56: sq_inplace_repeat

align 8
memoryview_mapping_methods:
    dq memoryview_len       ; +0: mp_length
    dq memoryview_subscript ; +8: mp_subscript
    dq 0                    ; +16: mp_ass_subscript

align 8
global memoryview_type
memoryview_type:
    dq 1                             ; ob_refcnt
    dq type_type                     ; ob_type
    dq mv_name_str                   ; tp_name
    dq PyMemoryViewObject_size       ; tp_basicsize
    dq memoryview_dealloc_proper     ; tp_dealloc
    dq 0                             ; tp_repr
    dq 0                             ; tp_str
    dq 0                             ; tp_hash
    dq 0                             ; tp_call (set by add_builtin_type)
    dq 0                             ; tp_getattr
    dq 0                             ; tp_setattr
    dq 0                             ; tp_richcompare
    dq 0                             ; tp_iter
    dq 0                             ; tp_iternext
    dq 0                             ; tp_init
    dq 0                             ; tp_new
    dq 0                             ; tp_as_number
    dq memoryview_seq_methods        ; tp_as_sequence
    dq memoryview_mapping_methods    ; tp_as_mapping
    dq 0                             ; tp_base
    dq 0                             ; tp_dict
    dq 0                             ; tp_mro
    dq 0                             ; tp_flags
    dq 0                             ; tp_bases
    dq 0                        ; tp_traverse
    dq 0                        ; tp_clear
    dq 0 ; tp_dictoffset
