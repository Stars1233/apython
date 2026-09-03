; pyo/bytes.asm - Bytes type implementation
; Immutable sequence of raw bytes with inline storage

%include "macros.inc"
%include "object.inc"

extern exc_UnicodeDecodeError_type
extern exc_new
extern exc_setattr
extern tuple_type
extern tuple_new
extern bytes_method_hex
extern str_from_cstr_heap
extern tuple_new
extern list_append
extern list_new
extern int_is_integer
extern type_is_subtype
extern hash_not_implemented
extern io_buffer_released
extern io_buffer_acquired
extern ap_memcmp
extern ap_memmove
extern exc_MemoryError_type
extern exc_BufferError_type
extern set_exception
extern ap_realloc
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
extern exc_NotImplementedError_type
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

    ; Allocate: header + size + a NUL.  The terminator is not decoration: a
    ; bytes reaches the kernel as a path, and posix_path_arg compares
    ; ap_strlen against ob_size to refuse an embedded one.  Without it that
    ; comparison read past the end of the object and the answer depended on
    ; whatever byte the allocator happened to leave there -- so
    ; posix.stat(b"/some/path") raised "embedded null byte" or not, by luck.
    ; CPython terminates its bytes for the same reason.
    lea rdi, [r12 + PyBytesObject.data + 1]
    call ap_malloc
    mov rbx, rax                ; rbx = new bytes obj

    ; Fill header
    mov qword [rbx + PyObject.ob_refcnt], 1
    lea rax, [rel bytes_type]
    mov [rbx + PyObject.ob_type], rax
    mov [rbx + PyBytesObject.ob_size], r12
    mov byte [rbx + PyBytesObject.data + r12], 0

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

    ; Allocate: header + size + a NUL -- see bytes_new for why.
    lea rdi, [r13 + PyBytesObject.data + 1]
    call ap_malloc
    mov rbx, rax                ; rbx = new bytes obj

    ; Fill header
    mov qword [rbx + PyObject.ob_refcnt], 1
    lea rax, [rel bytes_type]
    mov [rbx + PyObject.ob_type], rax
    mov [rbx + PyBytesObject.ob_size], r13
    mov byte [rbx + PyBytesObject.data + r13], 0

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
;; bytes and bytearray no longer share a layout -- a bytearray's data is out
;; of line -- so each hands the renderer its own (pointer, length) pair.
DEF_FUNC bytes_repr
    mov rsi, [rdi + PyBytesObject.ob_size]
    lea rdi, [rdi + PyBytesObject.data]
    xor edx, edx               ; 0 = b'...'
    call bytes_repr_impl
    leave
    ret
END_FUNC bytes_repr

DEF_FUNC bytearray_repr
    push rbx
    mov rbx, rdi
    call bytearray_data
    mov rdi, rax
    mov rsi, [rbx + PyByteArrayObject.ob_size]
    mov edx, 1                 ; 1 = bytearray(b'...')
    call bytes_repr_impl
    pop rbx
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

    mov rbx, rdi               ; the data pointer
    mov r12, rsi               ; length
    mov r14d, edx              ; wrap flag, preserved across the loop

    ; Pick the delimiter the way CPython does: a single quote normally, but a
    ; double quote when the data contains ' and no ", so the quote inside
    ; needs no backslash.
    mov r15d, 0x27
    xor eax, eax               ; saw a single quote?
    xor edx, edx
.br_scan:
    cmp rdx, r12
    jge .br_scan_done
    movzx ecx, byte [rbx + rdx]
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

    movzx eax, byte [rbx + rdx]

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
BD_SELF   equ 8
BD_OUT    equ 16
BD_POS    equ 24
BD_ERRORS equ 32            ; the errors= argument, a Value
BD_WHY    equ 40            ; which of the three malformations
BD_ERRID  equ 48            ; 1 = ignore, 2 = replace
BD_READ   equ 56            ; the read cursor, while rebuilding
BD_SPAN   equ 64            ; how many bytes the bad subpart covers
BD_FRAME  equ 80            ; + 2 pushes = 96, 16-aligned
;; ============================================================================
;; bytes_utf8_check(rdi = data, rsi = length) -> rax = the index of the first
;;   byte that is not part of a well-formed sequence, or -1; edx = why
;;
;; 0 = invalid start byte, 1 = invalid continuation byte, 2 = unexpected end
;; of data -- CPython's three reasons, in its words -- and r8 = how many bytes
;; the offending subpart spans, which is 1 for everything except a sequence
;; cut short by the end of the input.  CPython reports that one as a RANGE and
;; replaces the whole of it with a single U+FFFD.  The ranges are UTF-8's
;; and not "anything with the high bit set": overlong forms, surrogates and
;; anything past U+10FFFF are rejected too, which is what makes `strict` mean
;; something.
;; ============================================================================
;; bytes_check_errors_type(rdi = the errors= Value) -> returns, or raises
;; Absent is "strict"; anything present must be a str, None included -- which
;; is what CPython requires here, unlike open()'s errors=.
DEF_FUNC bytes_check_errors_type
    test rdi, rdi
    jz .bcet_ok                 ; the argument was not passed at all
    V_TEST_PTR rdi, rax
    ja .bcet_bad
    mov rax, [rdi + PyObject.ob_type]
    lea rcx, [rel str_type]
    cmp rax, rcx
    je .bcet_ok
    test qword [rax + PyTypeObject.tp_flags], TYPE_FLAG_STR_SUBCLASS
    jnz .bcet_ok
.bcet_bad:
    ; CPython names the type: "must be str, not None".
    push rdi
    lea rdi, [rel bd_msgbuf]
    lea rsi, [rel bd_msg_errtype]
    call bd_copy
    pop rsi
    mov rdi, rax
    call bd_append_typename
    lea rdi, [rel exc_TypeError_type]
    lea rsi, [rel bd_msgbuf]
    call raise_exception
    ud2
.bcet_ok:
    leave
    ret
END_FUNC bytes_check_errors_type

;; ============================================================================
;; bytes_raise_decode_error(rdi = the bytes, rsi = position, rdx = reason)
;;
;; "'utf-8' codec can't decode byte 0xff in position 3: invalid start byte" --
;; CPython's wording, built here because str() of a UnicodeDecodeError does
;; not render its fields (bugs.md).  Without the text the exception says
;; nothing about which byte or where.
;; ============================================================================
BRD_POS   equ 8
BRD_SPAN  equ 16
BRD_SELF  equ 24
BRD_CODEC equ 32
BRD_REASON equ 40
BRD_FRAME equ 48            ; + 1 push = 56, not 16-aligned

DEF_FUNC bytes_raise_decode_error, BRD_FRAME
    push rbx
    mov [rbp - BRD_POS], rsi
    mov [rbp - BRD_SPAN], rcx
    mov [rbp - BRD_SELF], rdi
    mov rbx, rdx                ; the reason
    ; The codec is a parameter now: the ascii arm raised
    ; "byte not in range for this encoding", which says neither which byte
    ; nor where, and named no codec at all.
    lea rax, [rel bd_codec_utf8]
    test r8, r8
    jz .brd_have_codec
    mov rax, r8
.brd_have_codec:
    mov [rbp - BRD_CODEC], rax
    cmp rcx, 1
    jg .brd_range               ; more than one byte is reported as a range

    ; The offending byte, as two lowercase hex digits.
    movzx eax, byte [rdi + PyBytesObject.data + rsi]
    mov rcx, rax
    shr rcx, 4
    and eax, 0x0f
    lea rdx, [rel bd_hexdigits]
    movzx ecx, byte [rdx + rcx]     ; the high digit
    movzx eax, byte [rdx + rax]     ; the low one
    push rcx
    push rax
    lea rdi, [rel bd_msgbuf]
    lea rsi, [rel bd_quote]
    call bd_copy
    mov rdi, rax
    mov rsi, [rbp - BRD_CODEC]
    call bd_copy
    mov rdi, rax
    lea rsi, [rel bd_quote]
    call bd_copy
    mov rdi, rax
    lea rsi, [rel bd_msg_head]
    call bd_copy
    pop rdx                         ; low
    pop rcx                         ; high
    mov [rax], cl
    mov [rax + 1], dl
    add rax, 2
    mov byte [rax], 0
    mov rdi, rax
    lea rsi, [rel bd_msg_inpos]
    call bd_copy
    mov rdi, rax
    mov rsi, [rbp - BRD_POS]
    call bd_append_i64
    mov rdi, rax
    lea rsi, [rel bd_msg_colon]
    call bd_copy
    mov rdi, rax
    call .brd_reason_text
    mov [rbp - BRD_REASON], rsi
    call bd_copy
    jmp .brd_finish

.brd_range:
    ; "can't decode bytes in position 0-1", which is what CPython says when
    ; the input ends in the middle of a sequence.
    lea rdi, [rel bd_msgbuf]
    lea rsi, [rel bd_quote]
    call bd_copy
    mov rdi, rax
    mov rsi, [rbp - BRD_CODEC]
    call bd_copy
    mov rdi, rax
    lea rsi, [rel bd_quote]
    call bd_copy
    mov rdi, rax
    lea rsi, [rel bd_msg_bytes]
    call bd_copy
    mov rdi, rax
    mov rsi, [rbp - BRD_POS]
    call bd_append_i64
    mov rdi, rax
    lea rsi, [rel bd_msg_dash]
    call bd_copy
    mov rdi, rax
    mov rsi, [rbp - BRD_POS]
    add rsi, [rbp - BRD_SPAN]
    dec rsi
    call bd_append_i64
    mov rdi, rax
    lea rsi, [rel bd_msg_colon]
    call bd_copy
    mov rdi, rax
    call .brd_reason_text
    mov [rbp - BRD_REASON], rsi
    call bd_copy

.brd_finish:
    ; The five fields CPython puts on a UnicodeDecodeError.  The stdlib's
    ; error handlers read every one of them: `e.start`, `e.end` and
    ; `e.object` are how a replacement handler knows what to replace.  This
    ; raised a bare message with none of them.
    lea rdi, [rel bd_msgbuf]
    call str_from_cstr_heap
    push rax
    sub rsp, 8
    mov rsi, rax
    lea rdi, [rel exc_UnicodeDecodeError_type]
    mov edx, TAG_PTR
    call exc_new
    add rsp, 8
    pop rdi
    push rax                    ; the exception
    sub rsp, 8
    call obj_decref             ; exc_new took its own reference to the message
    mov rbx, [rsp + 8]

    CSTRING rdi, "encoding"
    mov rsi, [rbp - BRD_CODEC]
    call .brd_set_str_field
    CSTRING rdi, "reason"
    mov rsi, [rbp - BRD_REASON]
    call .brd_set_str_field

    CSTRING rdi, "object"
    mov rsi, [rbp - BRD_SELF]
    INCREF rsi
    call .brd_set_field

    CSTRING rdi, "start"
    mov rsi, [rbp - BRD_POS]
    V_PACK_I64 rsi, rcx
    call .brd_set_field
    CSTRING rdi, "end"
    mov rsi, [rbp - BRD_POS]
    add rsi, [rbp - BRD_SPAN]
    V_PACK_I64 rsi, rcx
    call .brd_set_field

    add rsp, 8
    pop rdi
    extern raise_exception_obj
    jmp raise_exception_obj     ; takes the reference; does not return

;; .brd_reason_text -- rbx = the reason id -> rsi = the text
.brd_reason_text:
    lea rsi, [rel bd_reason_start]
    cmp rbx, 1
    jne .brd_reason_2
    lea rsi, [rel bd_reason_cont]
    ret
.brd_reason_2:
    cmp rbx, 2
    jne .brd_reason_3
    lea rsi, [rel bd_reason_end]
    ret
.brd_reason_3:
    cmp rbx, 3
    jne .brd_reason_done
    lea rsi, [rel bd_reason_ascii]
.brd_reason_done:
    ret

;; .brd_set_str_field(rdi = the field's name, rsi = a C string) -- makes the
;; string and hands it to .brd_set_field, which takes over the reference.
.brd_set_str_field:
    push rdi
    sub rsp, 8
    mov rdi, rsi
    call str_from_cstr_heap
    mov rsi, rax
    add rsp, 8
    pop rdi
    ; fall through

;; .brd_set_field(rdi = the field's name, rsi = an owned Value)
;; rbx holds the exception.
.brd_set_field:
    push rsi                    ; [rsp] = the value
    push rdi
    sub rsp, 8
    mov rdi, [rsp + 8]
    call str_from_cstr_heap
    add rsp, 8
    pop rdi                     ; the name cstring, done with
    push rax                    ; [rsp] = the key, [rsp+8] = the value
    sub rsp, 8
    mov rdi, rbx
    mov rsi, [rsp + 8]
    mov rdx, [rsp + 16]
    xor ecx, ecx
    call exc_setattr
    add rsp, 8
    pop rdi
    call obj_decref             ; the key
    pop rdi
    DECREF_V rdi, rcx           ; exc_setattr took its own reference
    ret
END_FUNC bytes_raise_decode_error

DEF_FUNC_LOCAL bd_append_typename   ; (rdi = dest, rsi = a Value) -> rax
    V_TEST_PTR rsi, rax
    ja .bdt_immediate
    test rsi, rsi
    jz .bdt_int
    LOAD_NONE rax
    cmp rsi, rax
    je .bdt_none                ; CPython prints "not None", not "not NoneType"
    mov rsi, [rsi + PyObject.ob_type]
    mov rsi, [rsi + PyTypeObject.tp_name]
    jmp .bdt_have
.bdt_immediate:
    V_IS_FLOAT rsi, rax
    ja .bdt_int
    lea rsi, [rel bd_name_float]
    jmp .bdt_have
.bdt_none:
    lea rsi, [rel bd_name_none]
    jmp .bdt_have
.bdt_int:
    lea rsi, [rel bd_name_int]
.bdt_have:
    call bd_copy
    leave
    ret
END_FUNC bd_append_typename

DEF_FUNC_LOCAL bd_copy          ; (rdi = dest, rsi = src) -> rax = the NUL
    xor ecx, ecx
.bdc_loop:
    cmp rcx, 100
    jge .bdc_done
    mov al, [rsi + rcx]
    test al, al
    jz .bdc_done
    mov [rdi + rcx], al
    inc rcx
    jmp .bdc_loop
.bdc_done:
    lea rax, [rdi + rcx]
    mov byte [rax], 0
    leave
    ret
END_FUNC bd_copy

DEF_FUNC_LOCAL bd_append_i64    ; (rdi = dest, rsi = n) -> rax = the NUL
    mov rax, rsi
    lea r8, [rel bd_numbuf + 24]
    mov byte [r8], 0
    mov r9, 10
.bda_loop:
    xor edx, edx
    div r9
    dec r8
    add dl, '0'
    mov [r8], dl
    test rax, rax
    jnz .bda_loop
    mov rsi, r8
    call bd_copy
    leave
    ret
END_FUNC bd_append_i64

DEF_FUNC_BARE bytes_utf8_check
    xor rcx, rcx                ; index
.buc_loop:
    cmp rcx, rsi
    jge .buc_valid
    movzx eax, byte [rdi + rcx]
    cmp al, 0x80
    jb .buc_one                 ; ASCII
    cmp al, 0xc2
    jb .buc_bad_start           ; a continuation byte, or an overlong C0/C1
    cmp al, 0xe0
    jb .buc_two
    cmp al, 0xf0
    jb .buc_three
    cmp al, 0xf5
    jb .buc_four
.buc_bad_start:
    jmp .buc_bad_start_out

.buc_one:
    inc rcx
    jmp .buc_loop

.buc_two:
    lea r8, [rcx + 1]
    cmp r8, rsi
    jge .buc_short
    movzx r9d, byte [rdi + r8]
    cmp r9d, 0x80
    jb .buc_bad_cont
    cmp r9d, 0xbf
    ja .buc_bad_cont
    add rcx, 2
    jmp .buc_loop

.buc_three:
    ; The second byte's range narrows for E0 (overlong) and ED (surrogates).
    mov r10d, 0x80
    mov r11d, 0xbf
    cmp al, 0xe0
    jne .buc_three_ed
    mov r10d, 0xa0
    jmp .buc_three_go
.buc_three_ed:
    cmp al, 0xed
    jne .buc_three_go
    mov r11d, 0x9f
.buc_three_go:
    lea r8, [rcx + 1]
    cmp r8, rsi
    jge .buc_short
    movzx r9d, byte [rdi + r8]
    cmp r9d, r10d
    jb .buc_bad_cont
    cmp r9d, r11d
    ja .buc_bad_cont
    lea r8, [rcx + 2]
    cmp r8, rsi
    jge .buc_short
    movzx r9d, byte [rdi + r8]
    cmp r9d, 0x80
    jb .buc_bad_cont
    cmp r9d, 0xbf
    ja .buc_bad_cont
    add rcx, 3
    jmp .buc_loop

.buc_four:
    ; F0 is overlong below U+10000; F4 runs past U+10FFFF above 0x8F.
    mov r10d, 0x80
    mov r11d, 0xbf
    cmp al, 0xf0
    jne .buc_four_f4
    mov r10d, 0x90
    jmp .buc_four_go
.buc_four_f4:
    cmp al, 0xf4
    jne .buc_four_go
    mov r11d, 0x8f
.buc_four_go:
    lea r8, [rcx + 1]
    cmp r8, rsi
    jge .buc_short
    movzx r9d, byte [rdi + r8]
    cmp r9d, r10d
    jb .buc_bad_cont
    cmp r9d, r11d
    ja .buc_bad_cont
    lea r8, [rcx + 2]
    cmp r8, rsi
    jge .buc_short
    movzx r9d, byte [rdi + r8]
    cmp r9d, 0x80
    jb .buc_bad_cont
    cmp r9d, 0xbf
    ja .buc_bad_cont
    lea r8, [rcx + 3]
    cmp r8, rsi
    jge .buc_short
    movzx r9d, byte [rdi + r8]
    cmp r9d, 0x80
    jb .buc_bad_cont
    cmp r9d, 0xbf
    ja .buc_bad_cont
    add rcx, 4
    jmp .buc_loop

.buc_bad_start_out:
    mov rax, rcx
    xor edx, edx
    mov r8d, 1
    ret
.buc_bad_cont:
    ; The subpart is the lead byte plus every continuation already accepted,
    ; which is what CPython reports and replaces with ONE U+FFFD.  Counting
    ; it as a single byte gave "byte 0xf0 in position 2" where CPython says
    ; "bytes in position 2-4", and replace emitted three U+FFFD where CPython
    ; emits one.  r8 is the index of the offending continuation, so the run
    ; is however far past the lead it had got.
    mov rax, rcx
    mov edx, 1
    sub r8, rcx
    cmp r8, 1
    jge .buc_cont_span
    mov r8d, 1
.buc_cont_span:
    ret
.buc_short:
    mov rax, rcx
    mov edx, 2
    mov r8, rsi
    sub r8, rcx                 ; everything that is there of the sequence
    ret
.buc_valid:
    mov rax, -1
    xor edx, edx
    mov r8d, 1
    ret
END_FUNC bytes_utf8_check

DEF_FUNC _bytes_decode_impl, BD_FRAME
    push rbx
    push r12
    mov qword [rbp - BD_ERRORS], 0
    cmp rsi, 3
    jl .bd_no_errors
    mov rax, [rdi + 16]
    mov [rbp - BD_ERRORS], rax
.bd_no_errors:
    ; Checked here rather than on the error path.  It used to be validated
    ; only once a malformation had been found, so a clean decode never looked
    ; at it and b"ab".decode("utf-8", 5) answered 'ab' where CPython raises.
    push rdi
    sub rsp, 8
    mov rdi, [rbp - BD_ERRORS]
    call bytes_check_errors_type
    add rsp, 8
    pop rdi

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
    ; Validate before building.  str_new copied the bytes through untouched,
    ; so an invalid sequence became an invalid str: errors="strict" never
    ; raised, and every text file opened here accepted corrupt input in
    ; silence, which is the dangerous half of ignoring the handler.
    mov rbx, [rbp - BD_SELF]
    lea rdi, [rbx + PyBytesObject.data]
    mov rsi, r12
    call bytes_utf8_check
    cmp rax, -1
    je .bd_utf8_ok
    mov [rbp - BD_POS], rax
    mov [rbp - BD_WHY], rdx
    mov [rbp - BD_SPAN], r8
    ; The type was checked in the prologue: codec_error_id answers -1 for
    ; anything that is not one of the three names, including a non-str, and
    ; the message builder then read PyStrObject.data off it.
    mov rdi, [rbp - BD_ERRORS]
    call codec_error_id         ; 0 strict, 1 ignore, 2 replace, -1 unknown
    cmp eax, -1
    je .bd_bad_errors
    test eax, eax
    jz .bd_utf8_strict
    mov [rbp - BD_ERRID], rax
    jmp .bd_utf8_fixup

.bd_utf8_ok:
    mov rbx, [rbp - BD_SELF]
    lea rdi, [rbx + PyBytesObject.data]
    mov rsi, r12
    call str_new
    pop r12
    pop rbx
    leave
    V_PACK rax, rdx
    ret

.bd_utf8_strict:
    mov rdi, [rbp - BD_SELF]
    mov rsi, [rbp - BD_POS]
    mov rdx, [rbp - BD_WHY]
    mov rcx, [rbp - BD_SPAN]
    xor r8d, r8d                ; the default codec name, 'utf-8'
    call bytes_raise_decode_error
    ud2

.bd_utf8_fixup:
    ; ignore drops each offending byte, replace puts U+FFFD where it was, so
    ; the result can be three times as long as the input.
    lea rdi, [r12 + r12*2]
    add rdi, PyStrObject.data + 8
    call ap_malloc
    test rax, rax
    jz .bd_nomem
    mov [rbp - BD_OUT], rax
    mov qword [rax + PyObject.ob_refcnt], 1
    lea rcx, [rel str_type]
    mov [rax + PyObject.ob_type], rcx
    mov qword [rax + PyStrObject.ob_hash], -1
    mov qword [rbp - BD_POS], 0     ; write cursor
    mov qword [rbp - BD_READ], 0    ; read cursor

.bd_fix_loop:
    mov rbx, [rbp - BD_SELF]
    lea rdi, [rbx + PyBytesObject.data]
    add rdi, [rbp - BD_READ]
    mov rsi, r12
    sub rsi, [rbp - BD_READ]
    jle .bd_fix_finish
    call bytes_utf8_check
    cmp rax, -1
    je .bd_fix_tail
    mov [rbp - BD_SPAN], r8

    ; The good run up to the bad byte, then the substitution.
    mov [rbp - BD_WHY], rax         ; the run length, reusing the slot
    mov rdi, [rbp - BD_OUT]
    add rdi, PyStrObject.data
    add rdi, [rbp - BD_POS]
    mov rbx, [rbp - BD_SELF]
    lea rsi, [rbx + PyBytesObject.data]
    add rsi, [rbp - BD_READ]
    mov rdx, rax
    call ap_memcpy
    mov rax, [rbp - BD_WHY]
    add [rbp - BD_POS], rax
    add [rbp - BD_READ], rax
    mov rax, [rbp - BD_SPAN]
    add [rbp - BD_READ], rax        ; step over the whole offending subpart
    cmp qword [rbp - BD_ERRID], 2
    jne .bd_fix_loop
    mov rdx, [rbp - BD_OUT]
    mov r8, [rbp - BD_POS]
    mov byte [rdx + PyStrObject.data + r8], 0xef
    mov byte [rdx + PyStrObject.data + r8 + 1], 0xbf
    mov byte [rdx + PyStrObject.data + r8 + 2], 0xbd
    add qword [rbp - BD_POS], 3
    jmp .bd_fix_loop

.bd_fix_tail:
    ; Everything from the cursor on is well formed.
    mov rdi, [rbp - BD_OUT]
    add rdi, PyStrObject.data
    add rdi, [rbp - BD_POS]
    mov rbx, [rbp - BD_SELF]
    lea rsi, [rbx + PyBytesObject.data]
    add rsi, [rbp - BD_READ]
    mov rdx, r12
    sub rdx, [rbp - BD_READ]
    mov [rbp - BD_WHY], rdx
    call ap_memcpy
    mov rax, [rbp - BD_WHY]
    add [rbp - BD_POS], rax

.bd_fix_finish:
    mov rax, [rbp - BD_OUT]
    mov rcx, [rbp - BD_POS]
    mov [rax + PyStrObject.ob_size], rcx
    mov qword [rax + PyStrObject.data + rcx], 0
    mov rdi, rax
    call str_set_length
    mov rax, [rbp - BD_OUT]
    mov edx, TAG_PTR
    pop r12
    pop rbx
    leave
    V_PACK rax, rdx
    ret

.bd_nomem:
    RAISE exc_MemoryError_type, "out of memory"
.bd_bad_errors:
    ; The name is the useful half: "unknown error handler name" leaves the
    ; caller to guess which of their arguments was wrong.
    lea rdi, [rel bd_msgbuf]
    lea rsi, [rel bd_msg_handler]
    call bd_copy
    mov rdi, rax
    mov rsi, [rbp - BD_ERRORS]
    lea rsi, [rsi + PyStrObject.data]
    call bd_copy
    mov byte [rax], 0x27        ; the closing quote
    mov byte [rax + 1], 0
    lea rdi, [rel exc_LookupError_type]
    lea rsi, [rel bd_msgbuf]
    call raise_exception
    ud2

.bd_ascii:
    xor ecx, ecx
.bd_ascii_scan:
    cmp rcx, r12
    jge .bd_utf8
    movzx eax, byte [rbx + PyBytesObject.data + rcx]
    test al, 0x80
    jnz .bd_ascii_bad
    inc rcx
    jmp .bd_ascii_scan

.bd_ascii_bad:
    ; The handler, looked up only now that something has actually failed.
    ; This arm jumped straight to the raise, so `b"a\xffb".decode("ascii",
    ; "ignore")` raised where CPython answers 'ab' -- and an unknown handler
    ; name was never reported as a LookupError on this path either.
    mov [rbp - BD_POS], rcx     ; where it failed, for the message
    mov rdi, [rbp - BD_ERRORS]
    call codec_error_id         ; 0 strict, 1 ignore, 2 replace, -1 unknown
    cmp eax, -1
    je .bd_bad_errors
    test eax, eax
    jz .bd_not_decodable
    mov [rbp - BD_ERRID], rax

    ; replace writes U+FFFD, three bytes, for each byte dropped -- so the
    ; result can be three times as long as the input.
    lea rdi, [r12 + r12*2]
    add rdi, PyStrObject.data + 8
    call ap_malloc
    test rax, rax
    jz .bd_nomem
    mov [rbp - BD_OUT], rax
    mov qword [rax + PyObject.ob_refcnt], 1
    lea rcx, [rel str_type]
    mov [rax + PyObject.ob_type], rcx
    mov qword [rax + PyStrObject.ob_hash], -1
    mov qword [rbp - BD_POS], 0
    mov rbx, [rbp - BD_SELF]
    xor ecx, ecx
.bd_af_loop:
    cmp rcx, r12
    jge .bd_af_done
    movzx eax, byte [rbx + PyBytesObject.data + rcx]
    test al, 0x80
    jnz .bd_af_bad
    mov rdx, [rbp - BD_OUT]
    mov r8, [rbp - BD_POS]
    mov [rdx + PyStrObject.data + r8], al
    inc qword [rbp - BD_POS]
    inc rcx
    jmp .bd_af_loop
.bd_af_bad:
    inc rcx
    cmp qword [rbp - BD_ERRID], 2
    jne .bd_af_loop             ; ignore
    mov rdx, [rbp - BD_OUT]
    mov r8, [rbp - BD_POS]
    mov byte [rdx + PyStrObject.data + r8], 0xef
    mov byte [rdx + PyStrObject.data + r8 + 1], 0xbf
    mov byte [rdx + PyStrObject.data + r8 + 2], 0xbd
    add qword [rbp - BD_POS], 3
    jmp .bd_af_loop
.bd_af_done:
    mov rax, [rbp - BD_OUT]
    mov rcx, [rbp - BD_POS]
    mov [rax + PyStrObject.ob_size], rcx
    mov qword [rax + PyStrObject.data + rcx], 0
    mov rdi, rax
    extern str_set_length
    call str_set_length
    mov rax, [rbp - BD_OUT]
    mov edx, TAG_PTR
    pop r12
    pop rbx
    leave
    V_PACK rax, rdx
    ret

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
    ; "'ascii' codec can't decode byte 0xff in position 2: ordinal not in
    ; range(128)".  This said "byte not in range for this encoding", which
    ; names neither the codec, nor the byte, nor where it was.
    mov rdi, [rbp - BD_SELF]
    mov rsi, [rbp - BD_POS]
    mov edx, 3                  ; "ordinal not in range(128)"
    mov ecx, 1
    lea r8, [rel bd_codec_ascii]
    call bytes_raise_decode_error
    ud2
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

;; ============================================================================
;; bytes_like_ptr_len(rdi = a pointer) -> rax = data, r10 = length, ecx = 1
;;   ecx = 0 when it is neither bytes nor bytearray.
;;
;; The two keep their data in different places -- bytes inline, bytearray out
;; of line -- so anything that reads both goes through here.
;; ============================================================================
DEF_FUNC bytes_like_ptr_len
    push rbx
    ; A Value, which may be an int or a float immediate -- sq_concat and
    ; tp_richcompare are both called with whatever the other operand was.
    V_TEST_PTR rdi, rax
    ja .bpl_no
    test rdi, rdi
    jz .bpl_no
    mov rbx, rdi
    mov rax, [rdi + PyObject.ob_type]
    lea rcx, [rel bytes_type]
    cmp rax, rcx
    je .bpl_bytes
    lea rcx, [rel bytearray_type]
    cmp rax, rcx
    je .bpl_bytearray
    lea rcx, [rel memoryview_type]
    cmp rax, rcx
    je .bpl_memoryview

    ; A subclass counts.  There is no family flag for either, so the answer
    ; comes from the MRO -- and without this `B(bytes)(b"xy") == b"xy"` was
    ; False, because the comparison declined and identity took over.
    mov rdi, rax
    lea rsi, [rel bytes_type]
    call type_is_subtype
    test eax, eax
    jnz .bpl_bytes_sub
    mov rdi, [rbx + PyObject.ob_type]
    lea rsi, [rel bytearray_type]
    call type_is_subtype
    test eax, eax
    jz .bpl_no
    mov rdi, rbx
    jmp .bpl_bytearray_have

.bpl_bytes_sub:
    mov rdi, rbx
.bpl_bytes:
    mov r10, [rdi + PyBytesObject.ob_size]
    lea rax, [rdi + PyBytesObject.data]
    jmp .bpl_yes
.bpl_bytearray:
.bpl_bytearray_have:
    mov r10, [rdi + PyByteArrayObject.ob_size]
    mov rax, [rdi + PyByteArrayObject.ob_bytes]
    test rax, rax
    jnz .bpl_yes
    lea rax, [rel bytearray_empty_data]
    jmp .bpl_yes
.bpl_memoryview:
    ; release() zeroes mv_buf and leaves mv_len alone, so a released view
    ; would hand back a NULL pointer with the old length -- and every caller
    ; here reads through it.  It is not bytes-like any more; the callers that
    ; should raise instead of declining call memoryview_check first.
    mov rax, [rdi + PyMemoryViewObject.mv_buf]
    cmp rax, MV_RELEASED
    je .bpl_no
    mov r10, [rdi + PyMemoryViewObject.mv_len]
.bpl_yes:
    mov ecx, 1
    pop rbx
    leave
    ret
.bpl_no:
    xor ecx, ecx
    pop rbx
    leave
    ret
END_FUNC bytes_like_ptr_len

DEF_FUNC bytes_compare
    V_UNPACK rdi, rcx           ; left  Value -> (payload, tag)
    V_UNPACK rsi, r8            ; right Value -> (payload, tag)
    ; rdi=a, rsi=b, edx=op
    push rbx
    mov ebx, edx              ; save op in ebx

    ; Either side may be bytes OR bytearray -- `b"ab" == bytearray(b"ab")` is
    ; True in CPython, and this function is now both types' tp_richcompare.
    ; The two layouts differ, so each side is reduced to a (pointer, length)
    ; pair before the walk.
    cmp r8d, TAG_PTR          ; b may be an int or float immediate, whose
    jne .bytes_cmp_not_impl   ; payload is not an address
    push rsi
    push rdi
    mov rdi, rsi
    call bytes_like_ptr_len
    pop rdi
    pop rsi
    test ecx, ecx
    jz .bytes_cmp_not_impl
    mov r9, rax               ; b's data
    mov rdx, r10              ; len(b)
    push rdx
    push r9
    call bytes_like_ptr_len   ; rdi is still a
    pop r9
    pop rdx
    test ecx, ecx
    jz .bytes_cmp_not_impl
    mov r8, rax               ; a's data
    mov rcx, r10              ; len(a)

    ; Lexicographic three-way compare, as CPython does: walk the common
    ; prefix, and if that matches, the shorter operand sorts first.  Only
    ; == and != were implemented before, so every ordering comparison
    ; between two bytes fell through to NotImplemented.
    mov r11, rcx
    cmp r11, rdx
    jle .bytes_have_min
    mov r11, rdx
.bytes_have_min:                             ; r11 = min(len(a), len(b))
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

;; bytes_latin1_to_str(rdi = data, rsi = length) -> a new str, or 0
;; One code point per byte, which is what makes the round trip through
;; str_mod exact: bytes_mod re-encodes the result the same way.
BL1_SRC   equ 8
BL1_LEN   equ 16
BL1_OUT   equ 24
BL1_POS   equ 32
BL1_FRAME equ 48            ; + 0 pushes = 48

DEF_FUNC_LOCAL bytes_latin1_to_str, BL1_FRAME
    mov [rbp - BL1_SRC], rdi
    mov [rbp - BL1_LEN], rsi
    lea rdi, [rsi + rsi]
    add rdi, PyStrObject.data + 8
    call ap_malloc
    test rax, rax
    jz .bl1_fail
    mov [rbp - BL1_OUT], rax
    mov qword [rax + PyObject.ob_refcnt], 1
    lea rcx, [rel str_type]
    mov [rax + PyObject.ob_type], rcx
    mov qword [rax + PyStrObject.ob_hash], -1
    mov rcx, [rbp - BL1_LEN]
    mov [rax + PyStrObject.ob_length], rcx
    mov qword [rbp - BL1_POS], 0
    xor ecx, ecx
.bl1_loop:
    cmp rcx, [rbp - BL1_LEN]
    jge .bl1_done
    mov rdx, [rbp - BL1_SRC]
    movzx eax, byte [rdx + rcx]
    mov rdx, [rbp - BL1_OUT]
    mov r8, [rbp - BL1_POS]
    test al, 0x80
    jnz .bl1_two
    mov [rdx + PyStrObject.data + r8], al
    inc qword [rbp - BL1_POS]
    jmp .bl1_next
.bl1_two:
    mov r9d, eax
    shr r9d, 6
    or r9b, 0xc0
    mov [rdx + PyStrObject.data + r8], r9b
    and eax, 0x3f
    or al, 0x80
    mov [rdx + PyStrObject.data + r8 + 1], al
    add qword [rbp - BL1_POS], 2
.bl1_next:
    inc rcx
    jmp .bl1_loop
.bl1_done:
    mov rax, [rbp - BL1_OUT]
    mov rcx, [rbp - BL1_POS]
    mov [rax + PyStrObject.ob_size], rcx
    mov qword [rax + PyStrObject.data + rcx], 0
    leave
    ret
.bl1_fail:
    xor eax, eax
    leave
    ret
END_FUNC bytes_latin1_to_str

;; bytes_mod_is_byteslike(rdi = a Value) -> eax = 1 when %s would insert its
;; bytes rather than its repr
;; bytes_mod_reject_wide(rdi = a Value)
;; A str argument survives this pipeline only if it is pure ASCII: everything
;; goes out through a latin-1 re-encode, so a code point above 0x7f would come
;; back as a single mangled byte.  CPython rejects a str for %s outright
;; ("%b requires a bytes-like object..."); it accepts one for %r and %a, which
;; is why only the wide ones are refused here rather than all of them.
DEF_FUNC bytes_mod_reject_wide
    V_TEST_PTR rdi, rax
    ja .bmw_ok
    test rdi, rdi
    jz .bmw_ok
    mov rax, [rdi + PyObject.ob_type]
    lea rcx, [rel str_type]
    cmp rax, rcx
    je .bmw_is_str
    ; A subclass has str's layout and str's data, so it reaches the same
    ; latin-1 re-encode.  Testing the type pointer alone let one straight
    ; past this guard and into the overflow it exists to prevent:
    ; `class S(str): pass` then b"%s" % (S("中"*20),) wrote past the
    ; allocation, because the size came from code points and the re-encode
    ; assumed two bytes for each of them.
    test qword [rax + PyTypeObject.tp_flags], TYPE_FLAG_STR_SUBCLASS
    jz .bmw_ok
.bmw_is_str:
    mov rax, [rdi + PyStrObject.ob_size]
    cmp rax, [rdi + PyStrObject.ob_length]
    jne .bmw_wide               ; more bytes than code points: not ASCII
.bmw_ok:
    leave
    ret
.bmw_wide:
    ; The class, not the literal word "str": CPython names the argument's own
    ; type, which for a subclass is the subclass.
    mov rsi, rdi
    CSTRING rdi, `%b requires a bytes-like object, or an object that implements __bytes__, not '\x01'`
    call raise_type_error_with_name
END_FUNC bytes_mod_reject_wide

global bytes_mod_is_byteslike
DEF_FUNC_BARE bytes_mod_is_byteslike
    xor eax, eax
    V_TEST_PTR rdi, rcx
    ja .bmi_out
    test rdi, rdi
    jz .bmi_out
    mov rcx, [rdi + PyObject.ob_type]
    lea rdx, [rel bytes_type]
    cmp rcx, rdx
    je .bmi_yes
    lea rdx, [rel bytearray_type]
    cmp rcx, rdx
    je .bmi_yes
    lea rdx, [rel memoryview_type]
    cmp rcx, rdx
    jne .bmi_out
.bmi_yes:
    mov eax, 1
.bmi_out:
    ret
END_FUNC bytes_mod_is_byteslike

;; ============================================================================
;; bytes_mod_prepare_args(rdi = the right operand Value) -> a Value to format
;; bytes_mod_release_args(rdi = what it returned)
;;
;; %s on a bytes format means "insert these bytes", not "insert str(x)".
;; Decoding each bytes-like argument as latin-1 turns it into a str whose code
;; points are its bytes, and bytes_mod re-encodes the result the same way, so
;; the round trip is exact.  Anything else is passed through untouched.
;;
;; A tuple is rebuilt only if it contains something bytes-like; a lone
;; argument is converted in place.  Nothing else here allocates, so
;; release_args frees exactly what prepare_args made.
;; ============================================================================
BMP_ARGS  equ 8
BMP_OUT   equ 16
BMP_I     equ 24
BMP_N     equ 32
BMP_FRAME equ 48            ; + 0 pushes = 48

DEF_FUNC_LOCAL bytes_mod_prepare_args, BMP_FRAME
    mov [rbp - BMP_ARGS], rdi
    V_TEST_PTR rdi, rax
    ja .bmp_asis
    test rdi, rdi
    jz .bmp_asis
    mov rax, [rdi + PyObject.ob_type]
    lea rcx, [rel tuple_type]
    cmp rax, rcx
    je .bmp_tuple

    ; A single argument.
    call bytes_mod_reject_wide
    mov rdi, [rbp - BMP_ARGS]
    call bytes_mod_as_str
    test rax, rax
    jnz .bmp_out
.bmp_asis:
    mov rax, [rbp - BMP_ARGS]
    leave
    ret
.bmp_out:
    leave
    ret

.bmp_tuple:
    mov rcx, [rdi + PyTupleObject.ob_size]
    mov [rbp - BMP_N], rcx
    ; Only rebuild if there is something to convert.  Building a copy
    ; unconditionally meant that when str_mod raised -- which it does for a
    ; wrong argument count, and a raise abandons the C stack -- the copy was
    ; never released.
    mov rax, [rdi + PyTupleObject.ob_item]
    xor rdx, rdx
.bmp_scan:
    cmp rdx, rcx
    jge .bmp_asis
    mov rdi, [rax + rdx*8]
    push rax
    push rcx
    push rdx
    sub rsp, 8
    call bytes_mod_reject_wide
    mov rax, [rsp + 8 + 16]
    mov rcx, [rsp + 8 + 8]
    mov rdx, [rsp + 8]
    mov rdi, [rax + rdx*8]
    call bytes_mod_is_byteslike
    add rsp, 8
    pop rdx
    pop rcx
    pop rax
    test eax, eax
    jnz .bmp_rebuild
    inc rdx
    jmp .bmp_scan

.bmp_rebuild:
    mov rdi, [rbp - BMP_N]
    call tuple_new
    test rax, rax
    jz .bmp_asis
    mov [rbp - BMP_OUT], rax
    mov qword [rbp - BMP_I], 0
.bmp_loop:
    mov rcx, [rbp - BMP_I]
    cmp rcx, [rbp - BMP_N]
    jge .bmp_tuple_done
    mov rax, [rbp - BMP_ARGS]
    mov rax, [rax + PyTupleObject.ob_item]
    mov rdi, [rax + rcx*8]
    call bytes_mod_as_str
    test rax, rax
    jnz .bmp_store
    mov rax, [rbp - BMP_ARGS]           ; not bytes-like: keep it, with a
    mov rax, [rax + PyTupleObject.ob_item]   ; reference of its own
    mov rcx, [rbp - BMP_I]
    mov rax, [rax + rcx*8]
    push rax
    INCREF_V rax, rcx
    pop rax
.bmp_store:
    mov rcx, [rbp - BMP_OUT]
    mov rcx, [rcx + PyTupleObject.ob_item]
    mov rdx, [rbp - BMP_I]
    mov [rcx + rdx*8], rax
    inc qword [rbp - BMP_I]
    jmp .bmp_loop
.bmp_tuple_done:
    mov rax, [rbp - BMP_OUT]
    leave
    ret
END_FUNC bytes_mod_prepare_args

;; ============================================================================
;; bytes_latin1_from_str(rdi = a str whose code points are all below 256)
;;   -> rax = a new bytes, one byte per code point
;;
;; The inverse of bytes_latin1_to_str.  str_mod_impl needs it for a %(name)s
;; key in a BYTES format: the format was decoded to a str to be scanned, so the
;; key comes out as a str, and the mapping is keyed by bytes.
;; ============================================================================
global bytes_latin1_from_str
DEF_FUNC bytes_latin1_from_str
    push rbx
    push r12
    mov r12, rdi
    mov rdi, [r12 + PyStrObject.ob_length]
    call bytes_new
    test rax, rax
    jz .blf_out
    mov rbx, rax
    lea rdi, [rax + PyBytesObject.data]
    lea rsi, [r12 + PyStrObject.data]
    mov rdx, [r12 + PyStrObject.ob_size]
    xor rcx, rcx
    xor r8, r8
.blf_loop:
    cmp rcx, rdx
    jge .blf_done
    movzx eax, byte [rsi + rcx]
    test al, 0x80
    jz .blf_one
    and eax, 0x1f
    shl eax, 6
    movzx r9d, byte [rsi + rcx + 1]
    and r9d, 0x3f
    or eax, r9d
    add rcx, 2
    jmp .blf_store
.blf_one:
    inc rcx
.blf_store:
    mov [rdi + r8], al
    inc r8
    jmp .blf_loop
.blf_done:
    mov [rbx + PyBytesObject.ob_size], r8
    mov rax, rbx
.blf_out:
    pop r12
    pop rbx
    leave
    ret
END_FUNC bytes_latin1_from_str

;; bytes_mod_as_str(rdi = a Value) -> a new str, or 0 if it is not bytes-like
;; str_mod_impl calls it for %s and %b, which is where the conversion is known.
global bytes_mod_as_str
DEF_FUNC bytes_mod_as_str
    V_TEST_PTR rdi, rax
    ja .bma_no
    test rdi, rdi
    jz .bma_no
    mov rax, [rdi + PyObject.ob_type]
    lea rcx, [rel bytes_type]
    cmp rax, rcx
    je .bma_yes
    lea rcx, [rel bytearray_type]
    cmp rax, rcx
    je .bma_yes
    lea rcx, [rel memoryview_type]
    cmp rax, rcx
    jne .bma_no
.bma_yes:
    call bytes_like_ptr_len     ; rax = data, r10 = length
    test ecx, ecx
    jz .bma_no
    mov rdi, rax
    mov rsi, r10
    call bytes_latin1_to_str
    leave
    ret
.bma_no:
    xor eax, eax
    leave
    ret
END_FUNC bytes_mod_as_str

DEF_FUNC_LOCAL bytes_mod_release_args
    ; Only what prepare_args ALLOCATED: it hands back the caller's own object
    ; when there was nothing to convert, and releasing that decrefs a tuple
    ; nobody gave us -- which surfaced as free() complaining, later, about a
    ; constant.
    cmp rdi, rsi
    je .bmr_out
    V_TEST_PTR rdi, rax
    ja .bmr_out
    test rdi, rdi
    jz .bmr_out
    call obj_decref
.bmr_out:
    leave
    ret
END_FUNC bytes_mod_release_args

;; ============================================================================
;; bytes_mod(PyBytesObject *fmt, PyObject *args) -> PyBytesObject*
;; nb_remainder: implements b"fmt" % args
;; Strategy: convert bytes fmt to str, call str_mod, convert result to bytes
;; ============================================================================
BM_FMT   equ 8
BM_ARGS  equ 16
BM_ORIG  equ 24             ; the caller's own args, to tell a copy from it
BM_FRAME equ 32             ; + 2 pushes = 48, 16-aligned

DEF_FUNC bytes_mod, BM_FRAME
    ; The right operand stays a Value.  It used to be V_UNPACK'd here and the
    ; raw payload handed to str_mod -- which is itself an nb_remainder slot and
    ; unpacks its arguments again.  For `b"%d" % 5` the bare 5 has a zero
    ; high16, so the second unpack read it as a pointer and dereferenced
    ; address 0x5.  A heap int survived by luck, a pointer being its own Value,
    ; which is why tests/cpython/test_int.py:843 never caught it.
    mov [rbp-BM_ARGS], rsi      ; args, still a Value
    V_TEST_PTR rdi, rax         ; ja == not a pointer, so not bytes either
    ja .bm_decline
    mov rax, [rdi + PyObject.ob_type]
    lea rcx, [rel bytes_type]
    cmp rax, rcx
    jne .bm_decline
    push rbx
    push r12

    mov [rbp-BM_FMT], rdi     ; fmt bytes obj
    ; The FORMAT is latin-1 decoded too, exactly as the arguments are below.
    ; Copying its bytes verbatim into the temp str left the two halves of the
    ; round trip disagreeing: the arguments went in as one code point per
    ; byte and the result came back out the same way, but a format byte above
    ; 0x7f had arrived as part of a multi-byte sequence, so the re-encode
    ; wrote MORE bytes than the code-point count it had sized the result by.
    ; b"\xe4\xb8\xad" % () overran the allocation and aborted the process.
    ; The arguments are prepared BEFORE the format is decoded, because
    ; preparing them can raise -- a wide str for %s -- and a raise abandons
    ; the C stack.  With the temp str built first it was the thing abandoned,
    ; one leaked str per `b"%s" % ("\u4e2d",)`.
    ;
    ; Any bytes-like argument becomes the str its bytes decode to under
    ; latin-1, one code point per byte.  The result is re-encoded the same way
    ; below, so the bytes come through untouched -- where handing the object
    ; itself to str_mod applied str() to it and `b"%s" % (b"abc",)` produced
    ; b"b'abc'".
    ; The arguments are NOT pre-converted any more.  Doing it here could not
    ; tell %s from %r from %c -- the conversion is not known until str_mod_impl
    ; reads it -- so b"%r" % (b"x",) answered b"'x'" where CPython gives
    ; b"b'x'", and b"%s" % 5 answered b"5" where it is a TypeError.

    mov rdi, [rbp-BM_FMT]
    mov rsi, [rdi + PyBytesObject.ob_size]
    lea rdi, [rdi + PyBytesObject.data]
    call bytes_latin1_to_str
    test rax, rax
    jz .bm_failed_fmt
    mov rbx, rax               ; rbx = temp str

    ; Call str_mod(temp_str, args)
    extern str_mod
    mov rdi, rbx               ; temp str
    mov rsi, [rbp-BM_ARGS]    ; args, a Value -- str_mod is a slot and unpacks
    mov edx, 1                 ; and this one is a BYTES format
    extern str_mod_impl
    call str_mod_impl
    mov r12, rax               ; r12 = result str Value (a str is a pointer)

    ; DECREF temp fmt str
    mov rdi, rbx
    DECREF_REG rdi

    ; str_mod raises rather than declining, but a NULL here would be read as a
    ; PyStrObject below, so refuse it rather than trusting the callee.
    test r12, r12
    jz .bm_failed

    ; Convert the result str back to bytes, one BYTE per code point -- the
    ; reverse of the latin-1 decoding the arguments went through.  Copying
    ; the str's UTF-8 out verbatim turned every byte above 0x7f back into the
    ; two bytes that encode it, so b"%s" % (b"\xff",) came out as b"\xc3\xbf".
    mov rdi, [r12 + PyStrObject.ob_length]
    call bytes_new
    mov rbx, rax
    lea rdi, [rax + PyBytesObject.data]
    lea rsi, [r12 + PyStrObject.data]
    mov rdx, [r12 + PyStrObject.ob_size]
    xor rcx, rcx                ; read cursor
    xor r8, r8                  ; write cursor
.bm_latin1_loop:
    cmp rcx, rdx
    jge .bm_latin1_done
    movzx eax, byte [rsi + rcx]
    test al, 0x80
    jz .bm_latin1_one
    ; A two-byte sequence encodes one code point below 0x100 here.
    and eax, 0x1f
    shl eax, 6
    movzx r9d, byte [rsi + rcx + 1]
    and r9d, 0x3f
    or eax, r9d
    add rcx, 2
    jmp .bm_latin1_store
.bm_latin1_one:
    inc rcx
.bm_latin1_store:
    mov [rdi + r8], al
    inc r8
    jmp .bm_latin1_loop
.bm_latin1_done:
    ; bytes_new already sized the block and wrote the terminator, and the
    ; loop writes exactly the code-point count it was given: adding a NUL
    ; here put one byte past the end of the allocation.
    mov [rbx + PyBytesObject.ob_size], r8

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

.bm_failed_fmt:
    ; The arguments were prepared first, so they are ours to release here.
    mov rdi, [rbp-BM_ARGS]
    mov rsi, [rbp-BM_ORIG]
    call bytes_mod_release_args
    xor eax, eax
    xor edx, edx
    pop r12
    pop rbx
    leave
    ret

.bm_failed:
    ; str_mod left an exception pending; propagate the NULL Value.
    xor eax, eax
    pop r12
    pop rbx
    leave
    ret

.bm_decline:
    ; Reached before the pushes, so there is no mirror to unwind.
    xor eax, eax                ; NULL Value = NotImplemented
    leave
    ret
END_FUNC bytes_mod

;; ============================================================================
;; bytearray_mod(rdi = self Value, rsi = args Value) -> a bytearray Value
;;
;; bytearray_type.tp_as_number was 0 and nothing supplied nb_remainder, so
;; `bytearray(b"%d") % 5` was a TypeError where CPython answers
;; bytearray(b'5').  The work is bytes_mod's: a temporary bytes of the same
;; bytes, the shared body, and the result re-wrapped -- which is how
;; bytearray_repr already borrows bytes_repr_impl.
;; ============================================================================
BMOD_ARGS  equ 8
BMOD_TMP   equ 16
BMOD_FRAME equ 32            ; + 1 push = 40, not 16-aligned

DEF_FUNC bytearray_mod, BMOD_FRAME
    push rbx
    mov [rbp - BMOD_ARGS], rsi
    V_TEST_PTR rdi, rax
    ja .bam_decline
    mov rax, [rdi + PyObject.ob_type]
    lea rcx, [rel bytearray_type]
    cmp rax, rcx
    jne .bam_decline

    push rdi
    call bytearray_data         ; rax = the payload, rdx = the length
    pop rdi
    mov rsi, [rdi + PyByteArrayObject.ob_size]
    mov rdi, rax
    call bytes_from_data
    mov [rbp - BMOD_TMP], rax

    mov rdi, rax
    mov rsi, [rbp - BMOD_ARGS]
    call bytes_mod
    mov rbx, rax

    mov rdi, [rbp - BMOD_TMP]
    call obj_decref
    test rbx, rbx
    jz .bam_out                 ; declined or raised; hand it on as it is

    ; bytearray_new copies a (pointer, length) range, which is what the
    ; result bytes is.  bytearray_from_bytes lives in methods/bytes.asm and
    ; would be a circular dependency from here.
    lea rdi, [rbx + PyBytesObject.data]
    mov rsi, [rbx + PyBytesObject.ob_size]
    call bytearray_new
    push rax
    mov rdi, rbx
    call obj_decref
    pop rbx
.bam_out:
    mov rax, rbx
    pop rbx
    leave
    ret

.bam_decline:
    xor eax, eax                ; NULL Value = NotImplemented
    pop rbx
    leave
    ret
END_FUNC bytearray_mod

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
    BINOP_REQUIRE_LEFT bytes_type, TYPE_FLAG_BYTES_SUBCLASS, 1
    push rbx
    push r12
    push r13
    push r14
    mov rbx, rdi
    mov r12, rsi

    V_TEST_PTR rbx, rax
    ja .bc_type_error
    V_TEST_PTR r12, rax
    ja .bc_type_error
    mov rax, [rbx + PyObject.ob_type]
    lea rcx, [rel bytes_type]
    cmp rax, rcx
    je .bc_left_ok
    ; A subclass instance has bytes' layout, which is all the copies below
    ; need; BINOP_REQUIRE_LEFT above already let it through.
    test qword [rax + PyTypeObject.tp_flags], TYPE_FLAG_BYTES_SUBCLASS
    jz .bc_type_error
.bc_left_ok:
    ; The right operand may be any bytes-like: `b"ab" + bytearray(b"cd")` is
    ; b'abcd' in CPython -- bytes, from the left operand -- and requiring an
    ; exact bytes on the right refused it.  r14 carries the right side's
    ; length and r12 becomes its DATA pointer, so the copies below read one
    ; layout whichever type arrived.
    mov rax, [r12 + PyObject.ob_type]
    cmp rax, rcx
    je .bc_right_bytes
    ; A bytes subclass on the right has bytes' layout too.
    test qword [rax + PyTypeObject.tp_flags], TYPE_FLAG_BYTES_SUBCLASS
    jz .bc_right_bytearray
.bc_right_bytes:
    mov r14, [r12 + PyBytesObject.ob_size]
    lea r12, [r12 + PyBytesObject.data]
    jmp .bc_right_done
.bc_right_bytearray:
    lea rcx, [rel bytearray_type]
    cmp rax, rcx
    je .bc_right_ba
    test qword [rax + PyTypeObject.tp_flags], TYPE_FLAG_BYTEARRAY_SUBCLASS
    jz .bc_type_error
.bc_right_ba:
    mov r14, [r12 + PyByteArrayObject.ob_size]
    mov rdi, r12
    call bytearray_data
    mov r12, rax
.bc_right_done:

    mov r13, [rbx + PyBytesObject.ob_size]
    add r13, r14
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
    mov rsi, r12
    mov rdx, r14
    call ap_memcpy
    pop rax
    mov qword [rax + PyBytesObject.data + r13], 0
    mov edx, TAG_PTR
    pop r14
    pop r13
    pop r12
    pop rbx
    leave
    ret

.bc_type_error:
    ; raise_type_error_with_name does not return, so the pushes above are
    ; abandoned with the rest of the C stack; nothing to restore.
    mov rsi, r12
    CSTRING rdi, `can't concat \x01 to bytes`
    extern raise_type_error_with_name
    call raise_type_error_with_name
END_FUNC bytes_concat

;; ============================================================================
;; bytes_repeat(bytes Value, count Value) -> Value
;; ============================================================================
DEF_FUNC bytes_repeat
    BINOP_REQUIRE_LEFT bytes_type, TYPE_FLAG_BYTES_SUBCLASS, 1
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
    ja .brep_toobig

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

.brep_toobig:
    ; Too large to allocate is a MemoryError in CPython; only a count that
    ; does not fit an index is an OverflowError.
    RAISE exc_MemoryError_type, ""
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
BLS_NARGS equ 24
BLS_BADITEM equ 56
BLS_BUF   equ 32
BLS_LIST  equ 40
BLS_TMP   equ 48
BLS_ENCMSG equ 64
BLS_FRAME equ 80            ; + 2 pushes = 96
DEF_FUNC byteslike_source, BLS_FRAME
    push rbx
    push r12
    mov [rbp - BLS_ARGS], rdi
    mov [rbp - BLS_RANGEMSG], rdx
    mov [rbp - BLS_ENCMSG], rcx
    mov [rbp - BLS_NARGS], rsi
    mov qword [rbp - BLS_LIST], 0
    mov qword [rbp - BLS_BUF], 0
    test rsi, rsi
    jz .bls_empty
    cmp rsi, 1
    jb .bls_empty
    ja .bls_with_encoding

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
extern codec_error_id
extern exc_LookupError_type
extern str_set_length
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
    call bytearray_data
    mov r12, rax

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

    ; bytes(s, encoding[, errors]) / bytearray(s, encoding[, errors]).  The
    ; arguments are str.encode's, in str.encode's order, so that is what runs
    ; them -- one decoder table, not two.
.bls_with_encoding:
    cmp rsi, 3
    ja .bls_too_many
    ; The encoding argument is checked before the subject, as CPython's is:
    ; bytes(1, 2) complains about the 2, not about the 1.
    mov rax, [rdi + 8]
    V_TEST_PTR rax, rcx
    ja .bls_enc_bad_encoding
    test rax, rax
    jz .bls_enc_bad_encoding
    mov rcx, [rax + PyObject.ob_type]
    lea rdx, [rel str_type]
    cmp rcx, rdx
    jne .bls_enc_bad_encoding
    mov rdi, [rdi]
    V_TEST_PTR rdi, rax
    ja .bls_enc_no_str
    test rdi, rdi
    jz .bls_enc_no_str
    mov rax, [rdi + PyObject.ob_type]
    lea rcx, [rel str_type]
    cmp rax, rcx
    je .bls_encode
    mov [rbp - BLS_TMP], rdi
    mov rdi, rax
    lea rsi, [rel str_type]
    extern type_is_subtype
    call type_is_subtype
    test eax, eax
    jz .bls_enc_no_str

.bls_encode:
    mov rdi, [rbp - BLS_ARGS]
    mov rsi, [rbp - BLS_NARGS]
    extern str_method_encode
    call str_method_encode
    V_UNPACK rax, rdx
    test rax, rax
    jz .bls_bad_type
    mov [rbp - BLS_LIST], rax   ; the release slot: it wants a decref too
    mov rbx, [rax + PyBytesObject.ob_size]
    lea r12, [rax + PyBytesObject.data]
    lea rdi, [rbx + 8]
    call ap_malloc
    mov [rbp - BLS_BUF], rax
    mov rdi, rax
    mov rsi, r12
    mov rdx, rbx
    call ap_memcpy
    mov rax, [rbp - BLS_BUF]
    mov qword [rax + rbx], 0
    mov rdi, [rbp - BLS_LIST]
    mov qword [rbp - BLS_LIST], 0
    call obj_decref
    mov rax, [rbp - BLS_BUF]
    mov rdx, rbx
    pop r12
    pop rbx
    leave
    ret

.bls_negative:
    RAISE exc_ValueError_type, "negative count"
.bls_enc_no_str:
    RAISE exc_TypeError_type, "encoding without a string argument"
.bls_enc_bad_encoding:
    mov rdi, [rbp - BLS_ARGS]
    mov rsi, [rdi + 8]
    mov rdi, [rbp - BLS_ENCMSG]
    extern raise_type_error_with_name
    call raise_type_error_with_name
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
BTC_FRAME equ 32            ; + 1 push = 40, not 16-aligned
DEF_FUNC bytes_type_call, BTC_FRAME
    ; rdi=type, rsi=args, rdx=nargs
    push rbx
    mov [rbp - BTC_TYPE], rdi
    mov rdi, rsi
    mov rsi, rdx
    lea rdx, [rel bytes_range_msg]
    lea rcx, [rel bytes_enc_msg]
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

;; ============================================================================
;; bytes_hash(rdi = PyBytesObject *) -> rax = the hash
;;
;; bytes had no tp_hash at all, so obj_hash fell through to its address: two
;; equal bytes objects hashed differently, and every dict and set holding
;; them was silently wrong.  A small dict hid it, because dict_lookup probes
;; and compares keys -- but 200 distinct byte strings gave 0 lookups found
;; and a set of 400.  posix.environ is a dict[bytes, bytes], which is how
;; this surfaced.
;;
;; FNV-1a, as str_hash computes it, and equal to str_hash's answer for the
;; same bytes.  That is legal -- unequal objects may share a hash, and
;; bytes_compare keeps them apart -- and it is what makes this a transcription
;; rather than a second algorithm to keep in step.
;;
;; No cache: PyBytesObject has no ob_hash field, and adding one means every
;; constructor has to initialise it or a new object reads a stale value.
;; Nothing hashes bytes in a hot loop.
;; ============================================================================
DEF_FUNC bytes_hash
    mov rcx, [rdi + PyBytesObject.ob_size]
    lea rsi, [rdi + PyBytesObject.data]
    mov rax, 0xcbf29ce484222325     ; FNV offset basis
    mov rdx, 0x100000001b3          ; FNV prime
.bh_loop4:
    cmp rcx, 4
    jb .bh_tail
    movzx r8d, byte [rsi]
    xor rax, r8
    imul rax, rdx
    movzx r8d, byte [rsi+1]
    xor rax, r8
    imul rax, rdx
    movzx r8d, byte [rsi+2]
    xor rax, r8
    imul rax, rdx
    movzx r8d, byte [rsi+3]
    xor rax, r8
    imul rax, rdx
    add rsi, 4
    sub rcx, 4
    jmp .bh_loop4
.bh_tail:
    test rcx, rcx
    jz .bh_done
    movzx r8d, byte [rsi]
    xor rax, r8
    imul rax, rdx
    inc rsi
    dec rcx
    jmp .bh_tail
.bh_done:
    ; -1 is the "no hash yet" marker everywhere it is stored.
    cmp rax, -1
    jne .bh_out
    mov rax, -2
.bh_out:
    leave
    ret
END_FUNC bytes_hash

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
    dq bytes_hash           ; tp_hash
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
    dq TYPE_FLAG_BASETYPE | TYPE_FLAG_BYTES_SUBCLASS   ; tp_flags -- the
                            ; subclass bit is carried by the base too, as
                            ; bytearray's is, so type_from_parts inherits it
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

bd_hexdigits:     db "0123456789abcdef"
bd_codec_utf8:    db "utf-8", 0
bd_codec_ascii:   db "ascii", 0
bd_quote:         db "'", 0
bd_msg_head:      db " codec can't decode byte 0x", 0
bd_msg_inpos:     db " in position ", 0
bd_msg_colon:     db ": ", 0
bd_msg_bytes:     db " codec can't decode bytes in position ", 0
bd_msg_dash:      db "-", 0
bd_msg_handler:   db "unknown error handler name '", 0
bd_msg_errtype:   db "decode() argument 'errors' must be str, not ", 0
bd_reason_start:  db "invalid start byte", 0
bd_reason_cont:   db "invalid continuation byte", 0
bd_reason_end:    db "unexpected end of data", 0
bd_reason_ascii:  db "ordinal not in range(128)", 0
bd_name_int:      db "int", 0
bd_name_float:    db "float", 0
bd_name_none:     db "None", 0

section .bss
bd_msgbuf: resb 192
bd_numbuf: resb 32

section .rodata
bytes_range_msg: db "bytes must be in range(0, 256)", 0
global bytearray_range_msg
bytearray_range_msg: db "byte must be in range(0, 256)", 0
; The \x01 is raise_type_error_with_name's placeholder for the argument's type.
bytes_enc_msg: db `bytes() argument 'encoding' must be str, not \x01`, 0
bytearray_enc_msg: db `bytearray() argument 'encoding' must be str, not \x01`, 0

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
BA_SIZE  equ 32
BA_FRAME equ 48             ; + 1 push = 56, not 16-aligned
DEF_FUNC bytearray_type_call, BA_FRAME
    ; rdi=type, rsi=args, rdx=nargs
    push rbx
    mov [rbp - BA_TYPE], rdi           ; save type
    mov rdi, rsi
    mov rsi, rdx
    lea rdx, [rel bytearray_range_msg]
    lea rcx, [rel bytearray_enc_msg]
    call byteslike_source
    mov [rbp - BA_BUF], rax
    mov [rbp - BA_LEN], rdx

    ; The object is a fixed size now; the bytes live in their own allocation.
    ; The SIZE comes from the type, not from the struct: a subclass carries a
    ; dict word and its __slots__ past this layout, and allocating the base's
    ; size meant the first attribute write landed past the end of the block.
    mov rdx, [rbp - BA_TYPE]
    mov rdi, [rdx + PyTypeObject.tp_basicsize]
    cmp rdi, PyByteArrayObject_size
    jge .ba_size_ok
    mov edi, PyByteArrayObject_size
.ba_size_ok:
    mov [rbp - BA_SIZE], rdi
    test qword [rdx + PyTypeObject.tp_flags], TYPE_FLAG_HAVE_GC
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
    mov qword [rbx + PyByteArrayObject.ob_size], 0
    mov qword [rbx + PyByteArrayObject.ob_cap], 0
    mov qword [rbx + PyByteArrayObject.ob_bytes], 0
    mov qword [rbx + PyByteArrayObject.ob_exports], 0
    ; Zero whatever the subclass added: a dict slot and any __slots__ values
    ; are read as Values by instance_dealloc and by the collector.
    mov rcx, [rbp - BA_SIZE]
    sub rcx, PyByteArrayObject_size
    jle .ba_no_tail
    lea rdi, [rbx + PyByteArrayObject_size]
    shr rcx, 3
    xor eax, eax
    rep stosq
.ba_no_tail:
    mov rdx, [rbp - BA_TYPE]
    inc qword [rdx + PyObject.ob_refcnt]

    mov rdi, rbx
    mov rsi, [rbp - BA_LEN]
    call bytearray_resize       ; allocates and NUL-terminates
    test eax, eax
    jz .ba_oom
    mov rcx, [rbp - BA_LEN]
    test rcx, rcx
    jz .ba_no_copy
    mov rdi, [rbx + PyByteArrayObject.ob_bytes]
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

.ba_oom:
    RAISE exc_MemoryError_type, "out of memory"
END_FUNC bytearray_type_call

;; ============================================================================
;; bytearray_resize(rdi = self, rsi = the new length) -> eax = 1 on success
;;
;; Grows the buffer to hold at least the new length, doubling so that n
;; appends cost O(n) copies in total, and never shrinks the allocation --
;; only ob_size moves down.  One byte past ob_size is always a NUL, so the
;; data pointer can be handed to anything expecting a C string.
;;
;; The bytes between the old and new length are NOT initialised; the caller
;; fills them.  bytearray(n) zeroes them itself.
;; ============================================================================
; Several bytearray mutators move bytes with ap_memmove BEFORE they call
; bytearray_resize, and call it only to settle ob_size afterwards.  For those
; the guard inside resize fires too late -- the view's bytes have already
; shifted under it -- so they ask up front instead.
%macro BA_REFUSE_IF_EXPORTED 1  ; %1 = register holding the bytearray
    cmp qword [%1 + PyByteArrayObject.ob_exports], 0
    jle %%ok
    RAISE exc_BufferError_type, "Existing exports of data: object cannot be re-sized"
%%ok:
%endmacro

BRS_SELF  equ 8
BRS_NEW   equ 16
BRS_CAP   equ 24
BRS_FRAME equ 32            ; + 0 pushes = 32

DEF_FUNC bytearray_resize, BRS_FRAME
    mov [rbp - BRS_SELF], rdi
    mov [rbp - BRS_NEW], rsi

    ; A live memoryview points straight into ob_bytes, so a length change
    ; leaves it dangling.  CPython refuses one; there was nothing here to
    ; refuse it with, and `m = memoryview(ba); ba.extend(b'x'*200)` corrupted
    ; the heap outright.  The same length is always allowed, which is what
    ; keeps reverse() and a same-size slice assignment legal with a view out.
    ;
    ; SET_EXC, not RAISE: bytearray_ass_subscript holds an owned source buffer
    ; and an owned temp bytearray across this call, and a RAISE abandons the C
    ; stack and both of them.  The 0 return already means failure and cannot
    ; be confused with anything -- its only other producer would be
    ; ap_realloc, which calls fatal_error rather than returning NULL.
    cmp rsi, [rdi + PyByteArrayObject.ob_size]
    je .brs_exports_ok
    cmp qword [rdi + PyByteArrayObject.ob_exports], 0
    jle .brs_exports_ok
    SET_EXC exc_BufferError_type, "Existing exports of data: object cannot be re-sized"
    xor eax, eax
    leave
    ret
.brs_exports_ok:
    mov rdi, [rbp - BRS_SELF]
    mov rsi, [rbp - BRS_NEW]
    mov rcx, [rdi + PyByteArrayObject.ob_cap]
    ; A fresh object has no buffer at all, and the NUL below has to go
    ; somewhere -- so allocate even when the requested length is 0.
    cmp qword [rdi + PyByteArrayObject.ob_bytes], 0
    je .brs_grow
    cmp rsi, rcx
    jbe .brs_fits
.brs_grow:

    ; Grow: at least double, and at least the requested size, with a floor
    ; so that a handful of appends does not reallocate every time.
    lea rax, [rcx + rcx]
    cmp rax, rsi
    jae .brs_have_cap
    mov rax, rsi
.brs_have_cap:
    cmp rax, 16
    jae .brs_cap_ok
    mov eax, 16
.brs_cap_ok:
    mov [rbp - BRS_CAP], rax
    mov rdi, [rdi + PyByteArrayObject.ob_bytes]
    lea rsi, [rax + 1]          ; + 1 for the NUL
    call ap_realloc             ; ap_realloc(NULL, n) is malloc(n)
    test rax, rax
    jz .brs_fail
    mov rdi, [rbp - BRS_SELF]
    mov [rdi + PyByteArrayObject.ob_bytes], rax
    mov rcx, [rbp - BRS_CAP]
    mov [rdi + PyByteArrayObject.ob_cap], rcx

.brs_fits:
    mov rdi, [rbp - BRS_SELF]
    mov rsi, [rbp - BRS_NEW]
    mov [rdi + PyByteArrayObject.ob_size], rsi
    mov rax, [rdi + PyByteArrayObject.ob_bytes]
    mov byte [rax + rsi], 0
    mov eax, 1
    leave
    ret

.brs_fail:
    xor eax, eax
    leave
    ret
END_FUNC bytearray_resize

;; ============================================================================
;; bytearray_data(rdi = self) -> rax = the buffer, never NULL
;;
;; An empty bytearray built by something other than the constructor could
;; have a NULL ob_bytes; every reader would then dereference it.  This hands
;; back a pointer to a static NUL instead.
;; ============================================================================
;; ============================================================================
;; bytearray_getitem(rdi = self, rsi = index) -> a Value, the byte as an int
;; sq_item, the counterpart of bytes_getitem.
;; ============================================================================
DEF_FUNC_BARE bytearray_getitem
    test rsi, rsi
    jns .bag_positive
    add rsi, [rdi + PyByteArrayObject.ob_size]
.bag_positive:
    cmp rsi, [rdi + PyByteArrayObject.ob_size]
    jge .bag_index_error
    cmp rsi, 0
    jl .bag_index_error

    mov rax, [rdi + PyByteArrayObject.ob_bytes]
    test rax, rax
    jz .bag_index_error         ; empty: every index is out of range
    movzx eax, byte [rax + rsi]
    RET_TAG_SMALLINT
    V_PACK rax, rdx
    ret

.bag_index_error:
    RAISE exc_IndexError_type, "bytearray index out of range"
END_FUNC bytearray_getitem

DEF_FUNC_BARE bytearray_data
    mov rax, [rdi + PyByteArrayObject.ob_bytes]
    test rax, rax
    jnz .bad_have
    lea rax, [rel bytearray_empty_data]
.bad_have:
    ret
END_FUNC bytearray_data

;; ============================================================================
;; bytearray_index_arg(rdi = Value) -> rax = 0..255, or it raises
;;
;; The value side of `b[i] = v` and of append(): CPython takes any object with
;; __index__ and refuses anything outside a byte.
;; ============================================================================
DEF_FUNC bytearray_index_arg
    V_UNPACK rdi, rdx           ; obj_as_index takes the pair, not the Value
    call obj_as_index
    cmp rax, 0
    jl .bia_range
    cmp rax, 255
    jg .bia_range
    leave
    ret
.bia_range:
    RAISE exc_ValueError_type, "byte must be in range(0, 256)"
END_FUNC bytearray_index_arg

;; ============================================================================
;; bytearray_new(rdi = data or NULL, rsi = length) -> rax = a new bytearray
;;
;; The one place a bytearray is built from a byte range.  Everything that
;; returns a new bytearray -- a slice, copy(), the concatenations -- goes
;; through it rather than repeating the allocate-resize-copy triple.
;; ============================================================================
BAN_SRC   equ 8
BAN_LEN   equ 16
BAN_OBJ   equ 24
BAN_FRAME equ 32            ; + 0 pushes = 32

DEF_FUNC bytearray_new, BAN_FRAME
    mov [rbp - BAN_SRC], rdi
    mov [rbp - BAN_LEN], rsi
    mov edi, PyByteArrayObject_size
    call ap_malloc
    test rax, rax
    jz .ban_fail
    mov [rbp - BAN_OBJ], rax
    mov qword [rax + PyByteArrayObject.ob_refcnt], 1
    lea rcx, [rel bytearray_type]
    mov [rax + PyByteArrayObject.ob_type], rcx
    mov qword [rax + PyByteArrayObject.ob_size], 0
    mov qword [rax + PyByteArrayObject.ob_cap], 0
    mov qword [rax + PyByteArrayObject.ob_bytes], 0
    mov qword [rax + PyByteArrayObject.ob_exports], 0
    inc qword [rcx + PyObject.ob_refcnt]

    mov rdi, rax
    mov rsi, [rbp - BAN_LEN]
    call bytearray_resize
    test eax, eax
    jz .ban_fail_free

    mov rdx, [rbp - BAN_LEN]
    test rdx, rdx
    jz .ban_done
    mov rsi, [rbp - BAN_SRC]
    test rsi, rsi
    jz .ban_done                ; no source: the caller fills it
    mov rax, [rbp - BAN_OBJ]
    mov rdi, [rax + PyByteArrayObject.ob_bytes]
    call ap_memcpy
.ban_done:
    mov rax, [rbp - BAN_OBJ]
    leave
    ret
.ban_fail_free:
    mov rdi, [rbp - BAN_OBJ]
    call ap_free
.ban_fail:
    xor eax, eax
    leave
    ret
END_FUNC bytearray_new

;; ============================================================================
;; bytearray_subscript(rdi = self, rsi = key Value) -> rax = Value
;;
;; An int gives the byte as an int; a slice gives a new BYTEARRAY, as CPython
;; does -- bytes gives bytes and bytearray gives bytearray.
;; ============================================================================
BSU_SELF  equ 8
BSU_KEY   equ 16
BSU_START equ 24
BSU_STEP  equ 32
BSU_LEN   equ 40
BSU_OUT   equ 48
BSU_KEYTAG equ 56
BSU_FRAME equ 64            ; + 0 pushes = 64

DEF_FUNC bytearray_subscript, BSU_FRAME
    mov [rbp - BSU_SELF], rdi
    ; The key arrives as a Value; obj_as_index and the slice test both want
    ; the (payload, tag) pair, as bytes_subscript unpacks it too.
    V_UNPACK rsi, rdx
    mov [rbp - BSU_KEY], rsi
    mov [rbp - BSU_KEYTAG], rdx

    cmp edx, TAG_PTR
    jne .bsu_int                ; an immediate: an int index
    mov rax, [rsi + PyObject.ob_type]
    lea rcx, [rel slice_type]
    cmp rax, rcx
    je .bsu_slice

.bsu_int:
    mov rdi, [rbp - BSU_KEY]
    mov rdx, [rbp - BSU_KEYTAG]
    call obj_as_index
    mov rcx, [rbp - BSU_SELF]
    mov rdx, [rcx + PyByteArrayObject.ob_size]
    test rax, rax
    jns .bsu_have_index
    add rax, rdx                ; a negative index counts from the end
.bsu_have_index:
    cmp rax, 0
    jl .bsu_range
    cmp rax, rdx
    jge .bsu_range
    push rax
    mov rdi, rcx
    call bytearray_data
    pop rcx
    movzx eax, byte [rax + rcx]
    V_PACK_I64 rax, rcx
    leave
    ret

.bsu_slice:
    mov rdi, [rbp - BSU_KEY]
    mov rcx, [rbp - BSU_SELF]
    mov rsi, [rcx + PyByteArrayObject.ob_size]
    call slice_indices          ; rax = start, rdx = stop, rcx = step
    mov [rbp - BSU_START], rax
    mov [rbp - BSU_STEP], rcx
    ; The element count, which slice_length computes for any step.
    mov rdi, rax
    mov rsi, rdx
    call slice_length           ; rdi = start, rsi = stop, rcx = step
    mov [rbp - BSU_LEN], rax

    xor edi, edi                ; no source: filled below
    mov rsi, rax
    call bytearray_new
    test rax, rax
    jz .bsu_fail
    mov [rbp - BSU_OUT], rax

    mov rdi, [rbp - BSU_SELF]
    call bytearray_data
    mov rsi, rax                ; the source bytes
    mov rdx, [rbp - BSU_OUT]
    mov rdx, [rdx + PyByteArrayObject.ob_bytes]
    mov r8, [rbp - BSU_START]
    mov r9, [rbp - BSU_STEP]
    xor ecx, ecx
.bsu_copy:
    cmp rcx, [rbp - BSU_LEN]
    jge .bsu_copied
    movzx eax, byte [rsi + r8]
    mov [rdx + rcx], al
    add r8, r9
    inc rcx
    jmp .bsu_copy
.bsu_copied:
    mov rax, [rbp - BSU_OUT]
    leave
    ret

.bsu_fail:
    xor eax, eax
    leave
    ret
.bsu_range:
    RAISE exc_IndexError_type, "bytearray index out of range"
END_FUNC bytearray_subscript

;; ============================================================================
;; slice_length(rdi = start, rsi = stop, rcx = step) -> rax = element count
;;
;; The count slice_indices does not return.  Shared by every bytearray slice
;; operation, each of which needs it before it can size a buffer.
;; ============================================================================
DEF_FUNC_BARE slice_length
    test rcx, rcx
    js .sln_negative
    mov rax, rsi
    sub rax, rdi                ; stop - start
    jle .sln_zero
    ; ceil(span / step)
    add rax, rcx
    dec rax
    xor edx, edx
    div rcx
    ret
.sln_negative:
    mov rax, rdi
    sub rax, rsi                ; start - stop
    jle .sln_zero
    mov r8, rcx
    neg r8
    add rax, r8
    dec rax
    xor edx, edx
    div r8
    ret
.sln_zero:
    xor eax, eax
    ret
END_FUNC slice_length

;; ============================================================================
;; bytearray_ass_subscript(rdi = self, rsi = key Value, rdx = value Value)
;;   -> rax = 0 on success
;;
;; b[i] = v, b[i:j] = v, b[i:j:k] = v, and all three deletions -- a NULL value
;; Value means del, which is how mp_ass_subscript spells it everywhere.
;;
;; The simple-slice case is the one CPython's regex compiler needs
;; (`data[0:0] = ...`), and it is the only one that changes the length: the
;; tail is moved and the buffer grown or the size dropped.
;; ============================================================================
BAS_SELF  equ 8
BAS_KEY   equ 16
BAS_KTAG  equ 24
BAS_VAL   equ 32
BAS_SRC   equ 40            ; the replacement bytes
BAS_SLEN  equ 48            ; and how many
BAS_START equ 56
BAS_STOP  equ 64
BAS_STEP  equ 72
BAS_N     equ 80            ; the slice's element count
BAS_TMP   equ 88            ; a copy, when source and target are the same object
BAS_SPAN  equ 96            ; bytes the span gives up, across the two calls below
BAS_FRAME equ 96            ; + 1 push = 104... padded to 112 below

DEF_FUNC bytearray_ass_subscript, 104
    push rbx
    mov [rbp - BAS_SELF], rdi
    mov [rbp - BAS_VAL], rdx
    mov qword [rbp - BAS_TMP], 0
    V_UNPACK rsi, rcx
    mov [rbp - BAS_KEY], rsi
    mov [rbp - BAS_KTAG], rcx

    cmp ecx, TAG_PTR
    jne .bas_int
    mov rax, [rsi + PyObject.ob_type]
    lea rcx, [rel slice_type]
    cmp rax, rcx
    je .bas_slice

;; --- b[i] = v, and del b[i] ------------------------------------------------
.bas_int:
    mov rdi, [rbp - BAS_KEY]
    mov rdx, [rbp - BAS_KTAG]
    call obj_as_index
    mov rbx, rax
    mov rcx, [rbp - BAS_SELF]
    mov rdx, [rcx + PyByteArrayObject.ob_size]
    test rbx, rbx
    jns .bas_int_bounds
    add rbx, rdx
.bas_int_bounds:
    cmp rbx, 0
    jl .bas_range
    cmp rbx, rdx
    jge .bas_range

    cmp qword [rbp - BAS_VAL], 0
    je .bas_del_one

    mov rdi, [rbp - BAS_VAL]
    call bytearray_index_arg    ; 0..255, or it raises
    mov rcx, rax
    mov rdi, [rbp - BAS_SELF]
    push rcx
    call bytearray_data
    pop rcx
    mov [rax + rbx], cl
    xor eax, eax
    pop rbx
    leave
    ret

.bas_del_one:
    mov rdi, [rbp - BAS_SELF]
    BA_REFUSE_IF_EXPORTED rdi   ; the gap is closed before the resize
    ; Close the gap, then drop the length by one.
    mov rdi, [rbp - BAS_SELF]
    call bytearray_data
    mov rcx, [rbp - BAS_SELF]
    mov rdx, [rcx + PyByteArrayObject.ob_size]
    lea rdi, [rax + rbx]
    lea rsi, [rdi + 1]
    sub rdx, rbx
    dec rdx
    call ap_memmove
    mov rdi, [rbp - BAS_SELF]
    mov rsi, [rdi + PyByteArrayObject.ob_size]
    dec rsi
    call bytearray_resize
    xor eax, eax
    pop rbx
    leave
    ret

;; --- the slice forms -------------------------------------------------------
.bas_slice:
    mov rdi, [rbp - BAS_KEY]
    mov rcx, [rbp - BAS_SELF]
    mov rsi, [rcx + PyByteArrayObject.ob_size]
    call slice_indices
    mov [rbp - BAS_START], rax
    mov [rbp - BAS_STOP], rdx
    mov [rbp - BAS_STEP], rcx
    mov rdi, rax
    mov rsi, rdx
    call slice_length
    mov [rbp - BAS_N], rax

    ; The replacement, as a byte range.  A deletion has none.
    mov qword [rbp - BAS_SRC], 0
    mov qword [rbp - BAS_SLEN], 0
    cmp qword [rbp - BAS_VAL], 0
    je .bas_have_src

    ; Assigning a bytearray to a slice of ITSELF would read the buffer while
    ; moving it, so take a copy first.
    mov rdi, [rbp - BAS_VAL]
    cmp rdi, [rbp - BAS_SELF]
    jne .bas_src_from_value
    call bytearray_data
    mov rdi, rax
    mov rcx, [rbp - BAS_SELF]
    mov rsi, [rcx + PyByteArrayObject.ob_size]
    call bytearray_new
    test rax, rax
    jz .bas_fail
    mov [rbp - BAS_TMP], rax
    mov rdi, rax

.bas_src_from_value:
    ; byteslike_source normalises bytes, bytearray, memoryview and any
    ; iterable of ints into a buffer it owns.
    mov [rbp - BAS_VAL], rdi
    lea rdi, [rbp - BAS_VAL]
    mov esi, 1
    lea rdx, [rel bytearray_range_msg]
    lea rcx, [rel bytearray_enc_msg]
    call byteslike_source
    mov [rbp - BAS_SRC], rax
    mov [rbp - BAS_SLEN], rdx

.bas_have_src:
    cmp qword [rbp - BAS_STEP], 1
    jne .bas_extended

;; --- b[i:j] = v: the only form that changes the length ---------------------
    mov r8, [rbp - BAS_STOP]
    mov r9, [rbp - BAS_START]
    cmp r8, r9
    jge .bas_span_ok
    mov r8, r9                  ; an empty span, as slice_indices allows
.bas_span_ok:
    sub r8, r9                  ; how many bytes go away
    ; The only arm that changes the length, and it moves the tail before it
    ; resizes.  Refused here, with the two owned things released first --
    ; .bas_ext_mismatch below does the same for the same reason.
    mov rcx, [rbp - BAS_SELF]
    cmp qword [rcx + PyByteArrayObject.ob_exports], 0
    jle .bas_span_exports_ok
    mov rdx, [rcx + PyByteArrayObject.ob_size]
    sub rdx, r8
    add rdx, [rbp - BAS_SLEN]   ; the length this assignment would produce
    cmp rdx, [rcx + PyByteArrayObject.ob_size]
    je .bas_span_exports_ok     ; same size: legal with a view out
    jmp .bas_span_exported
.bas_span_exports_ok:
    ; To the frame, not left in r8: bytearray_resize and bytearray_data are
    ; both ahead, both ordinary calls, and r8 is caller-saved.  rdx and rcx
    ; were already spilled around each of them; this one was not, so the
    ; memmove below read its source from a garbage offset.
    mov [rbp - BAS_SPAN], r8
    mov rcx, [rbp - BAS_SLEN]   ; rcx = how many arrive
    mov rdi, [rbp - BAS_SELF]
    mov rdx, [rdi + PyByteArrayObject.ob_size]
    mov rbx, rdx
    sub rbx, r8
    sub rbx, r9                 ; rbx = the length of the tail after the span
    add rdx, rcx
    sub rdx, r8                 ; rdx = the new length

    ; Grow first when the buffer has to be bigger, so the tail is moved into
    ; space that exists.
    cmp rdx, [rdi + PyByteArrayObject.ob_size]
    jbe .bas_no_grow
    mov rsi, rdx
    push rdx
    push rcx
    call bytearray_resize
    pop rcx
    pop rdx
    test eax, eax
    jz .bas_fail
.bas_no_grow:
    ; Move the tail to where it belongs.
    mov rdi, [rbp - BAS_SELF]
    push rdx
    push rcx
    call bytearray_data
    pop rcx
    pop rdx
    mov rsi, rax
    mov r9, [rbp - BAS_START]
    lea rdi, [rax + r9]
    add rdi, rcx                ; the tail's new home
    lea rsi, [rax + r9]
    add rsi, [rbp - BAS_SPAN]   ; where it is now
    push rdx
    push rcx
    mov rdx, rbx
    call ap_memmove
    pop rcx
    pop rdx

    ; Then drop the replacement in.
    test rcx, rcx
    jz .bas_no_fill
    mov rdi, [rbp - BAS_SELF]
    push rdx
    push rcx
    call bytearray_data
    pop rcx
    pop rdx
    mov r9, [rbp - BAS_START]
    lea rdi, [rax + r9]
    mov rsi, [rbp - BAS_SRC]
    push rdx
    mov rdx, rcx
    call ap_memcpy
    pop rdx
.bas_no_fill:
    mov rdi, [rbp - BAS_SELF]
    mov rsi, rdx
    call bytearray_resize
    jmp .bas_done

;; --- b[i:j:k] = v, which must match the slice length exactly ---------------
.bas_extended:
    cmp qword [rbp - BAS_VAL], 0
    je .bas_ext_delete
    mov rax, [rbp - BAS_SLEN]
    ; An EMPTY right-hand side is the one length that does not have to match:
    ; b[::2] = b'' removes those positions, exactly as del b[::2] does, and
    ; bytearray(b'abcd') becomes bytearray(b'bd').  This is bytearray's alone
    ; -- a list raises for L[::2] = [] in CPython too -- and it read as a
    ; length mismatch here because only a NULL value counted as a delete.
    test rax, rax
    jz .bas_ext_delete
    cmp rax, [rbp - BAS_N]
    jne .bas_ext_mismatch
    mov rdi, [rbp - BAS_SELF]
    call bytearray_data
    mov rdx, [rbp - BAS_SRC]
    mov r8, [rbp - BAS_START]
    mov r9, [rbp - BAS_STEP]
    xor ecx, ecx
.bas_ext_loop:
    cmp rcx, [rbp - BAS_N]
    jge .bas_done
    movzx esi, byte [rdx + rcx]
    mov [rax + r8], sil
    add r8, r9
    inc rcx
    jmp .bas_ext_loop

.bas_ext_delete:
    ; An empty selection removes nothing and never resizes.
    cmp qword [rbp - BAS_N], 0
    jle .bas_ext_delete_go
    mov rdi, [rbp - BAS_SELF]
    BA_REFUSE_IF_EXPORTED rdi   ; the compaction runs before the resize
.bas_ext_delete_go:
    ; Walk forward, copying the bytes that survive down over the gaps.
    mov rdi, [rbp - BAS_SELF]
    call bytearray_data
    mov rcx, [rbp - BAS_SELF]
    mov r10, [rcx + PyByteArrayObject.ob_size]
    mov r8, [rbp - BAS_START]
    mov r9, [rbp - BAS_STEP]
    ; The walk below is forward, so a negative step has to be turned into the
    ; same set of indices counted upward: {start, start+step, ...} with step
    ; negative is {start+(n-1)*step, ..., start} with it positive.  Without
    ; this only the first index ever matched, and `del b[::-2]` removed one
    ; byte instead of three.
    test r9, r9
    jns .bas_extdel_ready
    mov rdx, [rbp - BAS_N]
    dec rdx
    jl .bas_extdel_ready        ; an empty slice deletes nothing
    imul rdx, r9
    add r8, rdx
    neg r9
.bas_extdel_ready:
    xor ecx, ecx                ; how many deleted so far
    xor rsi, rsi                ; the read cursor
    xor rdi, rdi                ; the write cursor
.bas_extdel_loop:
    cmp rsi, r10
    jge .bas_extdel_done
    ; Is this index one of the slice's?
    cmp rcx, [rbp - BAS_N]
    jge .bas_extdel_keep
    cmp rsi, r8
    jne .bas_extdel_keep
    add r8, r9
    inc rcx
    inc rsi
    jmp .bas_extdel_loop
.bas_extdel_keep:
    movzx edx, byte [rax + rsi]
    mov [rax + rdi], dl
    inc rdi
    inc rsi
    jmp .bas_extdel_loop
.bas_extdel_done:
    mov rsi, rdi
    mov rdi, [rbp - BAS_SELF]
    call bytearray_resize

.bas_done:
    mov rdi, [rbp - BAS_SRC]
    test rdi, rdi
    jz .bas_no_src_free
    call ap_free
.bas_no_src_free:
    mov rdi, [rbp - BAS_TMP]
    test rdi, rdi
    jz .bas_ok
    call obj_decref
.bas_ok:
    xor eax, eax
    pop rbx
    leave
    ret

.bas_fail:
    mov rdi, [rbp - BAS_SRC]
    test rdi, rdi
    jz .bas_fail_out
    call ap_free
.bas_fail_out:
    mov eax, -1
    pop rbx
    leave
    ret

.bas_range:
    RAISE exc_IndexError_type, "bytearray index out of range"

.bas_span_exported:
    mov rdi, [rbp - BAS_SRC]
    test rdi, rdi
    jz .bas_span_exported_raise
    call ap_free
.bas_span_exported_raise:
    mov rdi, [rbp - BAS_TMP]
    test rdi, rdi
    jz .bas_span_exported_raise2
    call obj_decref
.bas_span_exported_raise2:
    RAISE exc_BufferError_type, "Existing exports of data: object cannot be re-sized"

.bas_ext_mismatch:
    ; RAISE abandons the C stack, so the buffer byteslike_source owns has to
    ; go first -- .bas_done below never runs from here.
    mov rdi, [rbp - BAS_SRC]
    test rdi, rdi
    jz .bas_mismatch_raise
    call ap_free
.bas_mismatch_raise:
    mov rdi, [rbp - BAS_TMP]
    test rdi, rdi
    jz .bas_mismatch_raise2
    call obj_decref
.bas_mismatch_raise2:
    ; "attempt to assign bytes of size 3 to extended slice of size 2", the two
    ; numbers being BAS_SLEN and BAS_N, both still in the frame.
    sub rsp, 128
    mov rdi, rsp
    lea rsi, [rel bas_msg_size]
    extern rbt_append_cstr
    call rbt_append_cstr
    mov rdi, rax
    mov rsi, [rbp - BAS_SLEN]
    extern msg_append_i64
    call msg_append_i64
    mov rdi, rax
    lea rsi, [rel bas_msg_to]
    call rbt_append_cstr
    mov rdi, rax
    mov rsi, [rbp - BAS_N]
    call msg_append_i64
    lea rdi, [rel exc_ValueError_type]
    mov rsi, rsp
    call raise_exception
    ud2
END_FUNC bytearray_ass_subscript

;; ============================================================================
;; bytearray_contains(rdi = self, rsi = needle Value) -> eax = 0/1
;;
;; An int looks for that byte; a bytes-like looks for the subsequence -- the
;; same two meanings `in` has for bytes.
;; ============================================================================
BCT_SELF  equ 8
BCT_VAL   equ 16
BCT_SRC   equ 24
BCT_SLEN  equ 32
BCT_FRAME equ 32            ; + 2 pushes = 48

DEF_FUNC bytearray_contains, BCT_FRAME
    push rbx
    push r12
    mov [rbp - BCT_SELF], rdi
    mov [rbp - BCT_VAL], rsi

    ; An int looks for the byte -- and "an int" is int_is_integer's answer,
    ; not a tag test: under INT_STRESS=1 every int is a heap object, and a
    ; pointer test sent them all down the bytes-like path.
    mov rdi, rsi
    V_UNPACK rdi, rdx
    call int_is_integer
    test eax, eax
    jz .bct_object
    mov rdi, [rbp - BCT_VAL]
    V_UNPACK rdi, rdx
    call obj_as_index
    mov rdi, rax
    cmp rdi, 0
    jl .bct_range
    cmp rdi, 255
    jg .bct_range
    mov rbx, rdi
    mov rdi, [rbp - BCT_SELF]
    mov r12, [rdi + PyByteArrayObject.ob_size]
    call bytearray_data
    xor ecx, ecx
.bct_byte_loop:
    cmp rcx, r12
    jge .bct_no
    movzx edx, byte [rax + rcx]
    cmp rdx, rbx
    je .bct_yes
    inc rcx
    jmp .bct_byte_loop

.bct_object:
    ; A bytes-like: look for the subsequence.
    lea rdi, [rbp - BCT_VAL]
    mov esi, 1
    lea rdx, [rel bytearray_range_msg]
    lea rcx, [rel bytearray_enc_msg]
    call byteslike_source
    mov [rbp - BCT_SRC], rax
    mov [rbp - BCT_SLEN], rdx

    mov rdi, [rbp - BCT_SELF]
    mov r12, [rdi + PyByteArrayObject.ob_size]
    call bytearray_data
    mov rbx, rax
    mov r8, [rbp - BCT_SLEN]
    test r8, r8
    jz .bct_yes_free            ; the empty subsequence is in everything
    cmp r8, r12
    ja .bct_no_free
    xor ecx, ecx
.bct_scan:
    mov rax, r12
    sub rax, rcx
    cmp rax, r8
    jl .bct_no_free
    lea rdi, [rbx + rcx]
    mov rsi, [rbp - BCT_SRC]
    mov rdx, r8
    push rcx
    push r8
    call ap_memcmp
    pop r8
    pop rcx
    test eax, eax
    jz .bct_yes_free
    inc rcx
    jmp .bct_scan

.bct_yes_free:
    mov rdi, [rbp - BCT_SRC]
    test rdi, rdi
    jz .bct_yes
    call ap_free
.bct_yes:
    mov eax, 1
    pop r12
    pop rbx
    leave
    ret
.bct_no_free:
    mov rdi, [rbp - BCT_SRC]
    test rdi, rdi
    jz .bct_no
    call ap_free
.bct_no:
    xor eax, eax
    pop r12
    pop rbx
    leave
    ret
.bct_range:
    RAISE exc_ValueError_type, "byte must be in range(0, 256)"
.bct_type_error:
    RAISE exc_TypeError_type, "a bytes-like object is required"
END_FUNC bytearray_contains

;; ============================================================================
;; The mutators.  Each takes (rdi = args Value[], rsi = nargs) with args[0] as
;; self, which is the method calling convention everywhere here.
;;
;; They share one shape: work out the new length, resize, then move bytes.
;; bytearray_resize never shrinks the allocation, so removing and re-adding
;; does not thrash.
;; ============================================================================
BAM_SELF  equ 8
BAM_ARG   equ 16
BAM_SRC   equ 24
BAM_SLEN  equ 32
BAM_OLD   equ 40
BAM_IDX   equ 48
BAM_FRAME equ 48            ; + 0 pushes = 48

;; bytearray.append(b)
DEF_FUNC bytearray_method_append, BAM_FRAME
    cmp rsi, 2
    jne .bap_argerr
    mov rax, [rdi]
    mov [rbp - BAM_SELF], rax
    mov rdi, [rdi + 8]
    call bytearray_index_arg
    mov [rbp - BAM_ARG], rax
    mov rdi, [rbp - BAM_SELF]
    mov rsi, [rdi + PyByteArrayObject.ob_size]
    mov [rbp - BAM_OLD], rsi
    inc rsi
    call bytearray_resize
    test eax, eax
    jz .bap_oom
    mov rdi, [rbp - BAM_SELF]
    call bytearray_data
    mov rcx, [rbp - BAM_OLD]
    mov rdx, [rbp - BAM_ARG]
    mov [rax + rcx], dl
    LOAD_NONE rax
    mov edx, TAG_PTR
    leave
    ret
.bap_oom:
    ; bytearray_resize's 0 now means "a live memoryview blocks this", with a
    ; BufferError already recorded -- raising a MemoryError over it would
    ; bury the real one.  Propagate the pending exception instead.
    xor eax, eax
    xor edx, edx
    leave
    ret
.bap_argerr:
    RAISE exc_TypeError_type, "append() takes exactly one argument"
END_FUNC bytearray_method_append

;; bytearray.extend(iterable) -- and the body sq_inplace_concat shares.
;;
;; bytearray_extend_from(rdi = self, rsi = a Value) -> eax = 1 on success
DEF_FUNC bytearray_extend_from, BAM_FRAME
    mov [rbp - BAM_SELF], rdi
    mov [rbp - BAM_ARG], rsi
    ; byteslike_source takes an args array, so hand it the one slot.
    lea rdi, [rbp - BAM_ARG]
    mov esi, 1
    lea rdx, [rel bytearray_range_msg]
    lea rcx, [rel bytearray_enc_msg]
    call byteslike_source
    mov [rbp - BAM_SRC], rax
    mov [rbp - BAM_SLEN], rdx

    mov rdi, [rbp - BAM_SELF]
    mov rsi, [rdi + PyByteArrayObject.ob_size]
    mov [rbp - BAM_OLD], rsi
    add rsi, rdx
    call bytearray_resize
    test eax, eax
    jz .bef_fail

    mov rcx, [rbp - BAM_SLEN]
    test rcx, rcx
    jz .bef_done
    mov rdi, [rbp - BAM_SELF]
    call bytearray_data
    mov rdi, rax
    add rdi, [rbp - BAM_OLD]
    mov rsi, [rbp - BAM_SRC]
    mov rdx, [rbp - BAM_SLEN]
    call ap_memcpy
.bef_done:
    mov rdi, [rbp - BAM_SRC]
    test rdi, rdi
    jz .bef_ok
    call ap_free
.bef_ok:
    mov eax, 1
    leave
    ret
.bef_fail:
    mov rdi, [rbp - BAM_SRC]
    test rdi, rdi
    jz .bef_fail_out
    call ap_free
.bef_fail_out:
    xor eax, eax
    leave
    ret
END_FUNC bytearray_extend_from

DEF_FUNC bytearray_method_extend, BAM_FRAME
    cmp rsi, 2
    jne .bex_argerr
    mov rsi, [rdi + 8]
    mov rdi, [rdi]
    call bytearray_extend_from
    test eax, eax
    jz .bex_oom
    LOAD_NONE rax
    mov edx, TAG_PTR
    leave
    ret
.bex_oom:
    ; bytearray_resize's 0 now means "a live memoryview blocks this", with a
    ; BufferError already recorded -- raising a MemoryError over it would
    ; bury the real one.  Propagate the pending exception instead.
    xor eax, eax
    xor edx, edx
    leave
    ret
.bex_argerr:
    RAISE exc_TypeError_type, "extend() takes exactly one argument"
END_FUNC bytearray_method_extend

;; bytearray.insert(i, b)
DEF_FUNC bytearray_method_insert, BAM_FRAME
    cmp rsi, 3
    jne .bin_argerr
    mov rax, [rdi]
    mov [rbp - BAM_SELF], rax
    mov [rbp - BAM_ARG], rdi
    mov rdi, [rdi + 8]
    V_UNPACK rdi, rdx
    call obj_as_index
    mov [rbp - BAM_IDX], rax
    mov rdi, [rbp - BAM_ARG]
    mov rdi, [rdi + 16]
    call bytearray_index_arg
    mov [rbp - BAM_ARG], rax

    ; Clamp, as list.insert does: any index past either end lands at that end.
    mov rdi, [rbp - BAM_SELF]
    mov rdx, [rdi + PyByteArrayObject.ob_size]
    mov rcx, [rbp - BAM_IDX]
    test rcx, rcx
    jns .bin_positive
    add rcx, rdx
    jns .bin_positive
    xor ecx, ecx
.bin_positive:
    cmp rcx, rdx
    jle .bin_have_idx
    mov rcx, rdx
.bin_have_idx:
    mov [rbp - BAM_IDX], rcx
    mov [rbp - BAM_OLD], rdx
    lea rsi, [rdx + 1]
    call bytearray_resize
    test eax, eax
    jz .bin_oom

    mov rdi, [rbp - BAM_SELF]
    call bytearray_data
    mov rcx, [rbp - BAM_IDX]
    lea rdi, [rax + rcx + 1]
    lea rsi, [rax + rcx]
    mov rdx, [rbp - BAM_OLD]
    sub rdx, rcx
    call ap_memmove
    mov rdi, [rbp - BAM_SELF]
    call bytearray_data
    mov rcx, [rbp - BAM_IDX]
    mov rdx, [rbp - BAM_ARG]
    mov [rax + rcx], dl
    LOAD_NONE rax
    mov edx, TAG_PTR
    leave
    ret
.bin_oom:
    ; bytearray_resize's 0 now means "a live memoryview blocks this", with a
    ; BufferError already recorded -- raising a MemoryError over it would
    ; bury the real one.  Propagate the pending exception instead.
    xor eax, eax
    xor edx, edx
    leave
    ret
.bin_argerr:
    RAISE exc_TypeError_type, "insert() takes exactly 2 arguments"
END_FUNC bytearray_method_insert

;; bytearray.pop([i]) -> the byte removed
DEF_FUNC bytearray_method_pop, BAM_FRAME
    cmp rsi, 1
    jl .bpo_argerr
    cmp rsi, 2
    jg .bpo_argerr
    mov rax, [rdi]
    mov [rbp - BAM_SELF], rax
    BA_REFUSE_IF_EXPORTED rax   ; the memmove below runs before the resize
    mov rdx, [rax + PyByteArrayObject.ob_size]
    test rdx, rdx
    jz .bpo_empty
    mov rcx, rdx
    dec rcx                     ; the default is the last byte
    cmp rsi, 2
    jne .bpo_have_idx
    push rdx
    mov rdi, [rdi + 8]
    V_UNPACK rdi, rdx
    call obj_as_index
    pop rdx
    mov rcx, rax
    test rcx, rcx
    jns .bpo_have_idx
    add rcx, rdx
.bpo_have_idx:
    cmp rcx, 0
    jl .bpo_range
    cmp rcx, rdx
    jge .bpo_range
    mov [rbp - BAM_IDX], rcx
    mov [rbp - BAM_OLD], rdx

    mov rdi, [rbp - BAM_SELF]
    call bytearray_data
    mov rcx, [rbp - BAM_IDX]
    movzx edx, byte [rax + rcx]
    mov [rbp - BAM_ARG], rdx    ; the answer, before the move
    lea rdi, [rax + rcx]
    lea rsi, [rdi + 1]
    mov rdx, [rbp - BAM_OLD]
    sub rdx, rcx
    dec rdx
    call ap_memmove
    mov rdi, [rbp - BAM_SELF]
    mov rsi, [rbp - BAM_OLD]
    dec rsi
    call bytearray_resize
    mov rax, [rbp - BAM_ARG]
    V_PACK_I64 rax, rcx
    mov edx, TAG_PTR
    leave
    ret
.bpo_empty:
    RAISE exc_IndexError_type, "pop from empty bytearray"
.bpo_range:
    RAISE exc_IndexError_type, "pop index out of range"
.bpo_argerr:
    RAISE exc_TypeError_type, "pop() takes at most 1 argument"
END_FUNC bytearray_method_pop

;; bytearray.remove(b) -- the first occurrence, or ValueError
DEF_FUNC bytearray_method_remove, BAM_FRAME
    cmp rsi, 2
    jne .brm_argerr
    mov rax, [rdi]
    mov [rbp - BAM_SELF], rax
    BA_REFUSE_IF_EXPORTED rax   ; the memmove below runs before the resize
    mov rdi, [rdi + 8]
    call bytearray_index_arg
    mov [rbp - BAM_ARG], rax

    mov rdi, [rbp - BAM_SELF]
    mov rdx, [rdi + PyByteArrayObject.ob_size]
    mov [rbp - BAM_OLD], rdx
    call bytearray_data
    mov rcx, [rbp - BAM_ARG]
    xor esi, esi
.brm_scan:
    cmp rsi, [rbp - BAM_OLD]
    jge .brm_missing
    movzx edx, byte [rax + rsi]
    cmp rdx, rcx
    je .brm_found
    inc rsi
    jmp .brm_scan
.brm_found:
    lea rdi, [rax + rsi]
    push rsi
    lea rsi, [rdi + 1]
    mov rdx, [rbp - BAM_OLD]
    pop rcx
    sub rdx, rcx
    dec rdx
    call ap_memmove
    mov rdi, [rbp - BAM_SELF]
    mov rsi, [rbp - BAM_OLD]
    dec rsi
    call bytearray_resize
    LOAD_NONE rax
    mov edx, TAG_PTR
    leave
    ret
.brm_missing:
    RAISE exc_ValueError_type, "value not found in bytearray"
.brm_argerr:
    RAISE exc_TypeError_type, "remove() takes exactly one argument"
END_FUNC bytearray_method_remove

;; bytearray.clear()
DEF_FUNC bytearray_method_clear, BAM_FRAME
    mov rdi, [rdi]
    xor esi, esi
    call bytearray_resize
    test eax, eax
    jz .bcl_failed              ; the return was ignored outright
    LOAD_NONE rax
    mov edx, TAG_PTR
    leave
    ret
.bcl_failed:
    xor eax, eax                ; a NULL Value, with the exception pending
    xor edx, edx
    leave
    ret
END_FUNC bytearray_method_clear

;; bytearray.reverse()
DEF_FUNC bytearray_method_reverse, BAM_FRAME
    mov rdi, [rdi]
    mov rsi, [rdi + PyByteArrayObject.ob_size]
    call bytearray_data
    xor ecx, ecx
    mov rdx, rsi
    dec rdx
.brv_loop:
    cmp rcx, rdx
    jge .brv_done
    movzx esi, byte [rax + rcx]
    movzx edi, byte [rax + rdx]
    mov [rax + rcx], dil
    mov [rax + rdx], sil
    inc rcx
    dec rdx
    jmp .brv_loop
.brv_done:
    LOAD_NONE rax
    mov edx, TAG_PTR
    leave
    ret
END_FUNC bytearray_method_reverse

;; bytearray.copy() -> a new bytearray
DEF_FUNC bytearray_method_copy, BAM_FRAME
    mov rdi, [rdi]
    mov [rbp - BAM_SELF], rdi
    mov rsi, [rdi + PyByteArrayObject.ob_size]
    mov [rbp - BAM_SLEN], rsi
    call bytearray_data
    mov rdi, rax
    mov rsi, [rbp - BAM_SLEN]
    call bytearray_new
    mov edx, TAG_PTR
    leave
    ret
END_FUNC bytearray_method_copy

;; ============================================================================
;; The operators: +, *, += and *=.
;;
;; sq_concat and sq_repeat build a new bytearray; the inplace pair mutate and
;; hand back the same object, which is what makes `b += x` in a loop O(n).
;; ============================================================================
BAO_SELF  equ 8
BAO_ARG   equ 16
BAO_SRC   equ 24
BAO_SLEN  equ 32
BAO_OUT   equ 40
BAO_OWNED equ 48
BAO_FRAME equ 64            ; + 0 pushes = 64

DEF_FUNC bytearray_concat, BAO_FRAME
    mov [rbp - BAO_SELF], rdi
    mov [rbp - BAO_ARG], rsi
    ; A bytes-like only.  byteslike_source is the CONSTRUCTOR's rule and takes
    ; any iterable of ints, so using it here made `bytearray(b"ab") + [1, 2]`
    ; succeed where CPython raises.  extend() keeps the looser rule.
    mov rdi, rsi
    call bytes_like_ptr_len
    test ecx, ecx
    jz .bco_not_impl
    mov [rbp - BAO_SRC], rax
    mov [rbp - BAO_SLEN], r10
    mov qword [rbp - BAO_OWNED], 0

    mov rdi, [rbp - BAO_SELF]
    mov rsi, [rdi + PyByteArrayObject.ob_size]
    add rsi, [rbp - BAO_SLEN]
    xor edi, edi
    call bytearray_new
    test rax, rax
    jz .bco_fail
    mov [rbp - BAO_OUT], rax

    mov rdi, [rbp - BAO_SELF]
    mov r8, [rdi + PyByteArrayObject.ob_size]
    push r8
    call bytearray_data
    pop r8
    mov rsi, rax
    mov rax, [rbp - BAO_OUT]
    mov rdi, [rax + PyByteArrayObject.ob_bytes]
    mov rdx, r8
    push r8
    push r8
    call ap_memcpy
    pop r8
    pop r8
    mov rax, [rbp - BAO_OUT]
    mov rdi, [rax + PyByteArrayObject.ob_bytes]
    add rdi, r8
    mov rsi, [rbp - BAO_SRC]
    mov rdx, [rbp - BAO_SLEN]
    call ap_memcpy
.bco_done:
    mov rax, [rbp - BAO_OUT]
    mov edx, TAG_PTR
    leave
    ret
.bco_not_impl:
    ; A NULL Value is NotImplemented, so the protocol tries the other side.
.bco_fail:
    xor eax, eax
    xor edx, edx
    leave
    ret
END_FUNC bytearray_concat

DEF_FUNC bytearray_repeat, BAO_FRAME
    mov [rbp - BAO_SELF], rdi
    ; sq_repeat is handed two VALUES, not a count -- op_binary_op packs both
    ; operands before the call, as bytes_repeat's own V_UNPACK shows.
    ;
    ; The count goes through the three checks bytes_repeat and list_repeat
    ; both make, none of which this one had.  It called obj_as_index straight
    ; away, so a non-int with an __index__ was accepted where CPython raises;
    ; int_to_i64 truncated a count past 2^63 rather than refusing it, which is
    ; how bytearray(b'x') * (2**70) answered bytearray(b''); and the product
    ; was neither checked for overflow nor capped, so bytearray(b'xy') *
    ; (2**40) went to the allocator and aborted the process.
    mov [rbp - BAO_ARG], rsi        ; the count, still a Value
    call seq_repeat_check_count     ; rsi = the count; raises for a non-int

    mov rdi, [rbp - BAO_ARG]
    V_UNPACK rdi, rdx
    push rdi
    push rdx
    extern int_fits_i64
    call int_fits_i64
    pop rdx
    pop rdi
    test eax, eax
    jz .brp_overflow
    extern int_to_i64
    call int_to_i64
    mov rsi, rax

    mov rdi, [rbp - BAO_SELF]
    mov rdx, [rdi + PyByteArrayObject.ob_size]
    mov [rbp - BAO_SLEN], rdx
    mov rax, rsi
    test rax, rax
    jns .brp_count_ok
    xor eax, eax
.brp_count_ok:
    mov [rbp - BAO_ARG], rax
    imul rax, rdx
    jo .brp_overflow
    cmp rax, 0x10000000
    ja .brp_toobig
    mov rsi, rax
    xor edi, edi
    call bytearray_new
    test rax, rax
    jz .brp_fail
    mov [rbp - BAO_OUT], rax

    mov rdi, [rbp - BAO_SELF]
    call bytearray_data
    mov r9, rax                 ; the source
    mov rax, [rbp - BAO_OUT]
    mov r10, [rax + PyByteArrayObject.ob_bytes]
    xor ecx, ecx
.brp_loop:
    cmp rcx, [rbp - BAO_ARG]
    jge .brp_done
    mov rax, rcx
    imul rax, [rbp - BAO_SLEN]
    lea rdi, [r10 + rax]
    mov rsi, r9
    mov rdx, [rbp - BAO_SLEN]
    push rcx
    push r9
    push r10
    push r10
    call ap_memcpy
    pop r10
    pop r10
    pop r9
    pop rcx
    inc rcx
    jmp .brp_loop
.brp_done:
    mov rax, [rbp - BAO_OUT]
    mov edx, TAG_PTR
    leave
    ret
.brp_fail:
    xor eax, eax
    xor edx, edx
    leave
    ret
.brp_toobig:
    RAISE exc_MemoryError_type, ""
.brp_overflow:
    RAISE exc_OverflowError_type, "repeated bytes are too long"
END_FUNC bytearray_repeat

DEF_FUNC bytearray_inplace_concat, BAO_FRAME
    mov [rbp - BAO_SELF], rdi
    ; As sq_concat: a bytes-like only.
    mov [rbp - BAO_ARG], rsi
    mov rdi, rsi
    call bytes_like_ptr_len
    test ecx, ecx
    jz .bic2_not_impl
    ; Refused here rather than deeper down: bytearray_extend_from reports the
    ; block with a 0 return, and op_binary_op reads a slot's NULL as
    ; "declined" and reports "unsupported operand type(s)" -- burying the
    ; BufferError.  Only when there is actually something to append; `ba +=
    ; b""` changes no length and stays legal, as it does in CPython.
    test r10, r10
    jz .bic2_do_extend
    mov rdi, [rbp - BAO_SELF]
    BA_REFUSE_IF_EXPORTED rdi
.bic2_do_extend:
    mov rdi, [rbp - BAO_SELF]
    mov rsi, [rbp - BAO_ARG]
    call bytearray_extend_from
    test eax, eax
    jz .bic2_fail
    mov rax, [rbp - BAO_SELF]
    inc qword [rax + PyObject.ob_refcnt]
    mov edx, TAG_PTR
    leave
    ret
.bic2_not_impl:
.bic2_fail:
    xor eax, eax
    xor edx, edx
    leave
    ret
END_FUNC bytearray_inplace_concat

DEF_FUNC bytearray_inplace_repeat, BAO_FRAME
    mov [rbp - BAO_SELF], rdi
    BA_REFUSE_IF_EXPORTED rdi   ; its shrink-to-0 arm ignores the resize result
    mov rdi, rsi                ; a Value, as in bytearray_repeat
    V_UNPACK rdi, rdx
    call obj_as_index
    mov rsi, rax
    mov [rbp - BAO_ARG], rsi
    mov rdi, [rbp - BAO_SELF]
    mov rdx, [rdi + PyByteArrayObject.ob_size]
    mov [rbp - BAO_SLEN], rdx
    test rsi, rsi
    jg .bir_grow
    ; Zero or negative empties it, as it does for a list.
    mov rdi, [rbp - BAO_SELF]
    xor esi, esi
    call bytearray_resize
    jmp .bir_done
.bir_grow:
    mov rax, rsi
    imul rax, rdx
    mov rdi, [rbp - BAO_SELF]
    mov rsi, rax
    call bytearray_resize
    test eax, eax
    jz .bir_fail
    mov rdi, [rbp - BAO_SELF]
    call bytearray_data
    mov r10, rax
    mov ecx, 1                  ; copy 0 is already in place
.bir_loop:
    cmp rcx, [rbp - BAO_ARG]
    jge .bir_done
    mov rax, rcx
    imul rax, [rbp - BAO_SLEN]
    lea rdi, [r10 + rax]
    mov rsi, r10
    mov rdx, [rbp - BAO_SLEN]
    push rcx
    push r10
    call ap_memcpy
    pop r10
    pop rcx
    inc rcx
    jmp .bir_loop
.bir_done:
    mov rax, [rbp - BAO_SELF]
    inc qword [rax + PyObject.ob_refcnt]
    mov edx, TAG_PTR
    leave
    ret
.bir_fail:
    xor eax, eax
    xor edx, edx
    leave
    ret
END_FUNC bytearray_inplace_repeat




;; ============================================================================
;; bytearray_export_acquired(rdi = any source object or Value)
;; bytearray_export_released(rdi = the same)
;;
;; The BytesIO pair next door, for the other resizable buffer a memoryview can
;; point into.  Both are no-ops unless the source really is a bytearray, so
;; every memoryview site can call them unconditionally beside the io_buffer_*
;; ones; the two are mutually exclusive by type.
;; ============================================================================
DEF_FUNC_BARE bytearray_export_acquired
    test rdi, rdi
    jz .bea_out
    V_TEST_PTR rdi, rax
    ja .bea_out
    lea rax, [rel bytearray_type]
    cmp [rdi + PyObject.ob_type], rax
    jne .bea_out
    inc qword [rdi + PyByteArrayObject.ob_exports]
.bea_out:
    ret
END_FUNC bytearray_export_acquired

DEF_FUNC_BARE bytearray_export_released
    test rdi, rdi
    jz .ber_out
    V_TEST_PTR rdi, rax
    ja .ber_out
    lea rax, [rel bytearray_type]
    cmp [rdi + PyObject.ob_type], rax
    jne .ber_out
    cmp qword [rdi + PyByteArrayObject.ob_exports], 0
    jle .ber_out
    dec qword [rdi + PyByteArrayObject.ob_exports]
.ber_out:
    ret
END_FUNC bytearray_export_released

;; ============================================================================
;; bytearray_dealloc(obj)
;; ============================================================================
;; The buffer is a separate allocation now, so freeing the object is not
;; enough.  ob_bytes is NULL for an object whose constructor failed part way.
DEF_FUNC bytearray_dealloc
    push rbx
    mov rbx, rdi
    mov rdi, [rbx + PyByteArrayObject.ob_bytes]
    test rdi, rdi
    jz .bad_no_buf
    call ap_free
    mov qword [rbx + PyByteArrayObject.ob_bytes], 0
.bad_no_buf:
    mov rdi, rbx
    pop rbx
    leave
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
    mov rax, [rax + PyByteArrayObject.ob_bytes]
    movzx eax, byte [rax + rcx]
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
align 8
bytearray_mapping_methods:
    dq bytearray_len            ; mp_length       +0
    dq bytearray_subscript      ; mp_subscript    +8
    dq bytearray_ass_subscript  ; mp_ass_subscript +16

align 8
bytearray_seq_methods:
    dq bytearray_len       ; +0: sq_length
    dq bytearray_concat    ; +8: sq_concat
    dq bytearray_repeat    ; +16: sq_repeat
    ; sq_item is what reversed() looks for: it walks a sequence backwards
    ; through sq_length and sq_item, and declines outright when either is 0.
    ; mp_subscript covers indexing itself, but not that.
    dq bytearray_getitem   ; +24: sq_item
    dq 0                   ; +32: sq_ass_item (mp_ass_subscript covers it)
    dq bytearray_contains  ; +40: sq_contains
    dq bytearray_inplace_concat ; +48
    dq bytearray_inplace_repeat ; +56

align 8
; bytearray's only numeric slot: `%`.  Everything else it does through
; tp_as_sequence.
bytearray_number_methods:
    dq 0                        ; nb_add          +0
    dq 0                        ; nb_subtract     +8
    dq 0                        ; nb_multiply     +16
    dq bytearray_mod            ; nb_remainder    +24
    times 32 dq 0               ; through nb_imatmul

align 8
global bytearray_type
bytearray_type:
    dq 1                            ; ob_refcnt
    dq type_type                    ; ob_type
    dq ba_name_str                  ; tp_name
    dq PyByteArrayObject_size       ; tp_basicsize (the data is out of line)
    dq bytearray_dealloc            ; tp_dealloc
    dq bytearray_repr               ; tp_repr
    dq bytearray_repr               ; tp_str
    ; Mutable, therefore unhashable.  A 0 here is not the same thing: obj_hash
    ; falls through to the object's ADDRESS, so `{bytearray(b"ab"): 1}` was
    ; accepted and the key could never be found again.
    dq hash_not_implemented         ; tp_hash
    dq 0                            ; tp_call (set by add_builtin_type)
    dq 0                            ; tp_getattr
    dq 0                            ; tp_setattr
    dq bytes_compare                ; tp_richcompare (shared with bytes)
    dq bytearray_tp_iter            ; tp_iter
    dq 0                            ; tp_iternext
    dq 0                            ; tp_init
    dq 0                            ; tp_new
    dq bytearray_number_methods     ; tp_as_number (just nb_remainder)
    dq bytearray_seq_methods        ; tp_as_sequence
    dq bytearray_mapping_methods    ; tp_as_mapping
    dq 0                            ; tp_base
    dq 0                            ; tp_dict
    dq 0                            ; tp_mro
    dq TYPE_FLAG_BASETYPE | TYPE_FLAG_BYTEARRAY_SUBCLASS           ; tp_flags (allow subclassing)
    dq 0                            ; tp_bases
    dq 0                        ; tp_traverse
    dq 0                        ; tp_clear
    dq 0 ; tp_dictoffset

; What bytearray_data hands back when ob_bytes is NULL, so no reader has to
; test for it.
align 8
bytearray_empty_data: db 0

; The one-character format codes a view can carry.  cast() accepts only the
; unsigned ones, which is what memoryview_item_value reads.
global mv_format_B
mv_format_B: db "B", 0
mv_format_H: db "H", 0
mv_format_I: db "I", 0
mv_format_L: db "L", 0
mv_format_Q: db "Q", 0

align 8
ba_iter_name_str: db "bytearray_iterator", 0

align 8
mv_iter_name_str: db "memory_iterator", 0

align 8
global memoryview_iter_type
memoryview_iter_type:
    dq 1                            ; ob_refcnt
    dq type_type                    ; ob_type
    dq mv_iter_name_str             ; tp_name
    dq PyBytesIterObject_size       ; tp_basicsize
    dq bytearray_iter_dealloc       ; tp_dealloc (the same shape)
    dq 0                            ; tp_repr
    dq 0                            ; tp_str
    dq 0                            ; tp_hash
    dq 0                            ; tp_call
    dq 0                            ; tp_getattr
    dq 0                            ; tp_setattr
    dq 0                            ; tp_richcompare
    dq bytearray_iter_self          ; tp_iter
    dq memoryview_iter_next         ; tp_iternext
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
MV_FRAME equ 8              ; + 0 pushes = 8, not 16-aligned
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
    ; rdi = the source, bytes or bytearray
    push rdi
    mov edi, PyMemoryViewObject_size
    call ap_malloc
    pop rdi                            ; source

    ; Init header
    mov qword [rax + PyMemoryViewObject.ob_refcnt], 1
    lea rcx, [rel memoryview_type]
    mov [rax + PyMemoryViewObject.ob_type], rcx
    mov [rax + PyMemoryViewObject.mv_source], rdi
    push rax                           ; save result
    push rdi                           ; save for INCREF
    INCREF rdi
    ; A bytearray source counts this view, so a later resize can refuse.  The
    ; BytesIO sites below already do the equivalent through io_buffer_*; this
    ; arm called neither, which is the hole.
    call bytearray_export_acquired
    pop rdi
    pop rax

    ; A bytes keeps its data inline and a bytearray does not, so the two
    ; cannot share one read -- which is what this did while the layouts
    ; happened to match.
    push rax
    call bytes_like_ptr_len            ; rax = data, r10 = length
    mov rcx, rax
    pop rax
    mov [rax + PyMemoryViewObject.mv_buf], rcx
    mov [rax + PyMemoryViewObject.mv_len], r10

    ; A view starts out over single bytes, and is read-only exactly when its
    ; source is.
    mov qword [rax + PyMemoryViewObject.mv_itemsize], 1
    lea rcx, [rel mv_format_B]
    mov [rax + PyMemoryViewObject.mv_format], rcx
    mov rcx, [rdi + PyObject.ob_type]
    lea rdx, [rel bytes_type]
    cmp rcx, rdx
    sete cl
    movzx ecx, cl
    mov [rax + PyMemoryViewObject.mv_readonly], rcx

    mov edx, TAG_PTR
    leave
    ret

.mv_check_bytearray:
    lea rcx, [rel bytearray_type]
    cmp rax, rcx
    je .mv_from_bytes
    lea rcx, [rel memoryview_type]
    cmp rax, rcx
    je .mv_from_view
    jmp .mv_error

.mv_from_view:
    ; memoryview(memoryview) shares the same window, as CPython's does.
    push rdi
    mov edi, PyMemoryViewObject_size
    call ap_malloc
    pop rdi
    mov qword [rax + PyMemoryViewObject.ob_refcnt], 1
    lea rcx, [rel memoryview_type]
    mov [rax + PyMemoryViewObject.ob_type], rcx
    mov rcx, [rdi + PyMemoryViewObject.mv_source]
    mov [rax + PyMemoryViewObject.mv_source], rcx
    test rcx, rcx
    jz .mv_view_no_src
    inc qword [rcx + PyObject.ob_refcnt]
    push rax
    push rdi
    mov rdi, rcx
    call bytearray_export_acquired
    call io_buffer_acquired     ; a second view over a BytesIO is a second
    pop rdi                     ; export, and its release will decrement
    pop rax
.mv_view_no_src:
    mov rcx, [rdi + PyMemoryViewObject.mv_buf]
    mov [rax + PyMemoryViewObject.mv_buf], rcx
    mov rcx, [rdi + PyMemoryViewObject.mv_len]
    mov [rax + PyMemoryViewObject.mv_len], rcx
    mov rcx, [rdi + PyMemoryViewObject.mv_itemsize]
    mov [rax + PyMemoryViewObject.mv_itemsize], rcx
    mov rcx, [rdi + PyMemoryViewObject.mv_format]
    mov [rax + PyMemoryViewObject.mv_format], rcx
    mov rcx, [rdi + PyMemoryViewObject.mv_readonly]
    mov [rax + PyMemoryViewObject.mv_readonly], rcx
    mov edx, TAG_PTR
    leave
    ret

.mv_error:
    RAISE exc_TypeError_type, "memoryview: a bytes-like object is required"
END_FUNC memoryview_type_call


;; Proper dealloc:
DEF_FUNC memoryview_dealloc_proper
    push rdi                           ; save self
    mov rdi, [rdi + PyMemoryViewObject.mv_source]
    test rdi, rdi
    jz .mvd_no_source                  ; already released
    push rdi
    call bytearray_export_released
    call io_buffer_released
    pop rdi
    call obj_decref
.mvd_no_source:
    pop rdi                            ; restore self
    call ap_free
    leave
    ret
END_FUNC memoryview_dealloc_proper

;; ============================================================================
;; memoryview_check(rdi = self) -> returns, or raises
;;
;; Every operation on a released view raises, which is the whole point of
;; release(): it lets the buffer's owner resize again, so anything still
;; pointing into the old buffer has to be refused rather than read.
;; ============================================================================
DEF_FUNC_BARE memoryview_check
    cmp qword [rdi + PyMemoryViewObject.mv_buf], MV_RELEASED
    je memoryview_released_error
    ret
END_FUNC memoryview_check

DEF_FUNC memoryview_released_error
    RAISE exc_ValueError_type, "operation forbidden on released memoryview object"
END_FUNC memoryview_released_error

;; ============================================================================
;; memoryview_item_value(rdi = self, rsi = item index) -> rax = the item Value
;;
;; itemsize bytes, read little-endian and unsigned -- which is all the
;; formats below need, since cast() accepts only the unsigned ones.
;; ============================================================================
DEF_FUNC_BARE memoryview_item_value
    mov rcx, [rdi + PyMemoryViewObject.mv_itemsize]
    mov rax, [rdi + PyMemoryViewObject.mv_buf]
    imul rsi, rcx
    add rax, rsi
    cmp rcx, 1
    je .miv_1
    cmp rcx, 2
    je .miv_2
    cmp rcx, 4
    je .miv_4
    mov rax, [rax]
    ret
.miv_1:
    movzx eax, byte [rax]
    ret
.miv_2:
    movzx eax, word [rax]
    ret
.miv_4:
    mov eax, [rax]
    ret
END_FUNC memoryview_item_value

;; ============================================================================
;; memoryview_getattr(rdi = self, rsi = name str) -> rax = Value, or NULL
;;
;; tp_getattr was 0, so a memoryview had no attributes and no methods at all.
;; _pyio reads nbytes and readonly, calls tobytes and cast, and uses `with
;; memoryview(b) as view:` around every readinto.
;;
;; NULL for an unknown name rather than a raise, so op_load_attr falls through
;; to the MRO's tp_dicts -- the contract every other tp_getattr keeps.
;; ============================================================================
MVG_SELF  equ 8
MVG_NAME  equ 16
MVG_FRAME equ 32            ; + 0 pushes = 32

%macro MVG_NAME_IS 2            ; %1 = the C string, %2 = the label
    mov rdi, [rbp - MVG_NAME]
    lea rdi, [rdi + PyStrObject.data]
    CSTRING rsi, %1
    call ap_strcmp
    test eax, eax
    jz %2
%endmacro

DEF_FUNC memoryview_getattr, MVG_FRAME
    mov [rbp - MVG_SELF], rdi
    mov [rbp - MVG_NAME], rsi

    MVG_NAME_IS "nbytes",       .mvg_nbytes
    MVG_NAME_IS "itemsize",     .mvg_itemsize
    MVG_NAME_IS "format",       .mvg_format
    MVG_NAME_IS "readonly",     .mvg_readonly
    MVG_NAME_IS "obj",          .mvg_obj
    MVG_NAME_IS "ndim",         .mvg_ndim
    MVG_NAME_IS "shape",        .mvg_shape
    MVG_NAME_IS "strides",      .mvg_strides
    MVG_NAME_IS "suboffsets",   .mvg_suboffsets
    MVG_NAME_IS "c_contiguous", .mvg_true
    MVG_NAME_IS "f_contiguous", .mvg_true
    MVG_NAME_IS "contiguous",   .mvg_true

    ; Not an attribute of ours: the methods live in tp_dict.
    RET_NULL
    leave
    V_PACK rax, rdx
    ret

.mvg_nbytes:
    mov rdi, [rbp - MVG_SELF]
    call memoryview_check
    mov rdi, [rbp - MVG_SELF]
    mov rax, [rdi + PyMemoryViewObject.mv_len]
    jmp .mvg_int

.mvg_itemsize:
    mov rdi, [rbp - MVG_SELF]
    call memoryview_check
    mov rdi, [rbp - MVG_SELF]
    mov rax, [rdi + PyMemoryViewObject.mv_itemsize]
    jmp .mvg_int

.mvg_ndim:
    mov eax, 1
.mvg_int:
    V_PACK_I64 rax, rcx
    mov edx, TAG_PTR
    leave
    ret

.mvg_format:
    mov rdi, [rbp - MVG_SELF]
    call memoryview_check
    mov rdi, [rbp - MVG_SELF]
    mov rdi, [rdi + PyMemoryViewObject.mv_format]
    call str_from_cstr_heap
    leave
    ret

.mvg_readonly:
    mov rdi, [rbp - MVG_SELF]
    call memoryview_check
    mov rdi, [rbp - MVG_SELF]
    cmp qword [rdi + PyMemoryViewObject.mv_readonly], 0
    jne .mvg_true
    lea rax, [rel bool_false]
    jmp .mvg_bool_out
.mvg_true:
    lea rax, [rel bool_true]
.mvg_bool_out:
    inc qword [rax + PyObject.ob_refcnt]
    mov edx, TAG_PTR
    leave
    ret

.mvg_obj:
    mov rdi, [rbp - MVG_SELF]
    mov rax, [rdi + PyMemoryViewObject.mv_source]
    test rax, rax
    jz .mvg_none
    inc qword [rax + PyObject.ob_refcnt]
    mov edx, TAG_PTR
    leave
    ret
.mvg_none:
    LOAD_NONE rax
    mov edx, TAG_PTR
    leave
    ret

.mvg_shape:
    ; One dimension, so a 1-tuple of the item count.
    mov rdi, [rbp - MVG_SELF]
    call memoryview_check
    mov edi, 1
    call tuple_new
    test rax, rax
    jz .mvg_none
    mov [rbp - MVG_NAME], rax          ; the name is finished with
    mov rdi, [rbp - MVG_SELF]
    mov rax, [rdi + PyMemoryViewObject.mv_len]
    xor edx, edx
    div qword [rdi + PyMemoryViewObject.mv_itemsize]
    V_PACK_I64 rax, rcx
    mov rcx, [rbp - MVG_NAME]
    mov rcx, [rcx + PyTupleObject.ob_item]
    mov [rcx], rax
    mov rax, [rbp - MVG_NAME]
    mov edx, TAG_PTR
    leave
    ret

.mvg_strides:
    ; Contiguous, so the stride is the item size.
    mov rdi, [rbp - MVG_SELF]
    call memoryview_check
    mov edi, 1
    call tuple_new
    test rax, rax
    jz .mvg_none
    mov [rbp - MVG_NAME], rax
    mov rdi, [rbp - MVG_SELF]
    mov rax, [rdi + PyMemoryViewObject.mv_itemsize]
    V_PACK_I64 rax, rcx
    mov rcx, [rbp - MVG_NAME]
    mov rcx, [rcx + PyTupleObject.ob_item]
    mov [rcx], rax
    mov rax, [rbp - MVG_NAME]
    mov edx, TAG_PTR
    leave
    ret

.mvg_suboffsets:
    xor edi, edi
    call tuple_new
    mov edx, TAG_PTR
    leave
    ret
END_FUNC memoryview_getattr

;; ============================================================================
;; memoryview_repr(rdi = self, edx = tag) -> a str
;;
;; tp_repr was 0, so printing one reached obj_repr's fallback and raised
;; "build_string expects str".
;; ============================================================================
DEF_FUNC memoryview_repr
    cmp qword [rdi + PyMemoryViewObject.mv_buf], MV_RELEASED
    je .mvr_released
    CSTRING rdi, "<memory>"
    call str_from_cstr_heap
    leave
    ret
.mvr_released:
    CSTRING rdi, "<released memory>"
    call str_from_cstr_heap
    leave
    ret
END_FUNC memoryview_repr

;; ============================================================================
;; The methods.  Each takes (rdi = args Value[], rsi = nargs), args[0] = self.
;; ============================================================================
MVM_SELF  equ 8
MVM_ARG   equ 16
MVM_TMP   equ 24
MVM_FRAME equ 32            ; + 0 pushes = 32

DEF_FUNC memoryview_method_tobytes, MVM_FRAME
    test rsi, rsi
    jz .mvt_argerr
    mov rdi, [rdi]
    mov [rbp - MVM_SELF], rdi
    call memoryview_check
    mov rdi, [rbp - MVM_SELF]
    mov rsi, [rdi + PyMemoryViewObject.mv_len]
    mov rdi, [rdi + PyMemoryViewObject.mv_buf]
    call bytes_from_data
    mov edx, TAG_PTR
    leave
    ret
.mvt_argerr:
    RAISE exc_TypeError_type, "tobytes() takes no arguments"
END_FUNC memoryview_method_tobytes

DEF_FUNC memoryview_method_release, MVM_FRAME
    test rsi, rsi
    jz .mvrl_argerr
    mov rdi, [rdi]
    ; Releasing twice is not an error, as CPython has it.
    cmp qword [rdi + PyMemoryViewObject.mv_buf], MV_RELEASED
    je .mvrl_done
    mov qword [rdi + PyMemoryViewObject.mv_buf], MV_RELEASED
    mov rax, [rdi + PyMemoryViewObject.mv_source]
    mov qword [rdi + PyMemoryViewObject.mv_source], 0
    test rax, rax
    jz .mvrl_done
    push rax                    ; io_buffer_released returns in rax, so the
    mov rdi, rax                ; source has to survive the call in a slot
    call bytearray_export_released
    call io_buffer_released     ; a BytesIO counts its live views
    pop rdi
    call obj_decref
.mvrl_done:
    LOAD_NONE rax
    mov edx, TAG_PTR
    leave
    ret
.mvrl_argerr:
    RAISE exc_TypeError_type, "release() takes no arguments"
END_FUNC memoryview_method_release

;; __enter__ hands the view back; __exit__ releases it.  `with memoryview(b)
;; as view:` is how _pyio wraps every readinto.
DEF_FUNC memoryview_method_enter, MVM_FRAME
    test rsi, rsi
    jz .mve_argerr
    mov rax, [rdi]
    push rax
    mov rdi, rax
    call memoryview_check
    pop rax
    inc qword [rax + PyObject.ob_refcnt]
    mov edx, TAG_PTR
    leave
    ret
.mve_argerr:
    RAISE exc_TypeError_type, "__enter__() takes no arguments"
END_FUNC memoryview_method_enter

DEF_FUNC memoryview_method_exit, MVM_FRAME
    test rsi, rsi
    jz .mvx_argerr
    mov esi, 1
    call memoryview_method_release
    LOAD_NONE rax
    mov edx, TAG_PTR
    leave
    ret
.mvx_argerr:
    RAISE exc_TypeError_type, "__exit__() takes three arguments"
END_FUNC memoryview_method_exit

;; memoryview.cast(fmt) -- what re._compiler._bytes_to_codes calls with 'I'.
MVC_SELF  equ 8
MVC_FMT   equ 16
MVC_SIZE  equ 24
MVC_STR   equ 32
MVC_FRAME equ 48            ; + 0 pushes = 48

DEF_FUNC memoryview_method_cast, MVC_FRAME
    cmp rsi, 2
    jl .mvc_argerr
    mov rax, [rdi]
    mov [rbp - MVC_SELF], rax
    mov rcx, [rdi + 8]
    mov [rbp - MVC_STR], rcx
    mov rdi, rax
    call memoryview_check

    ; One character, and only the unsigned formats: those are what
    ; memoryview_item_value reads, and what CPython's own callers use here.
    mov rcx, [rbp - MVC_STR]
    V_TEST_PTR rcx, rax
    ja .mvc_badfmt
    mov rax, [rcx + PyObject.ob_type]
    lea rdx, [rel str_type]
    cmp rax, rdx
    jne .mvc_badfmt
    cmp qword [rcx + PyStrObject.ob_size], 1
    jne .mvc_badfmt
    movzx eax, byte [rcx + PyStrObject.data]

    lea rdx, [rel mv_format_B]
    mov esi, 1
    cmp al, 'B'
    je .mvc_have_fmt
    cmp al, 'b'
    je .mvc_have_fmt
    lea rdx, [rel mv_format_H]
    mov esi, 2
    cmp al, 'H'
    je .mvc_have_fmt
    lea rdx, [rel mv_format_I]
    mov esi, 4
    cmp al, 'I'
    je .mvc_have_fmt
    lea rdx, [rel mv_format_L]
    mov esi, 8
    cmp al, 'L'
    je .mvc_have_fmt
    lea rdx, [rel mv_format_Q]
    mov esi, 8
    cmp al, 'Q'
    jne .mvc_badfmt
.mvc_have_fmt:
    mov [rbp - MVC_SIZE], rsi
    mov [rbp - MVC_FMT], rdx

    ; The byte length must divide evenly, as CPython requires.
    mov rdi, [rbp - MVC_SELF]
    mov rax, [rdi + PyMemoryViewObject.mv_len]
    xor edx, edx
    div qword [rbp - MVC_SIZE]
    test rdx, rdx
    jnz .mvc_badlen

    mov edi, PyMemoryViewObject_size
    call ap_malloc
    test rax, rax
    jz .mvc_badlen
    mov qword [rax + PyMemoryViewObject.ob_refcnt], 1
    lea rcx, [rel memoryview_type]
    mov [rax + PyMemoryViewObject.ob_type], rcx
    mov rdi, [rbp - MVC_SELF]
    mov rcx, [rdi + PyMemoryViewObject.mv_source]
    mov [rax + PyMemoryViewObject.mv_source], rcx
    test rcx, rcx
    jz .mvc_no_src
    inc qword [rcx + PyObject.ob_refcnt]
    ; cast() is the fourth place a view takes a share of another's source, and
    ; release and dealloc decrement for every view that has one -- so without
    ; the matching acquire this view's release drove a BytesIO's export count
    ; below what is outstanding, and the next write reallocated the storage
    ; underneath it.  lib/_io.py's readinto does `b = b.cast("B")`, so it is
    ; on the ordinary path, not a corner.
    push rax
    push rdi
    mov rdi, rcx
    call bytearray_export_acquired
    call io_buffer_acquired
    pop rdi
    pop rax
.mvc_no_src:
    mov rcx, [rdi + PyMemoryViewObject.mv_buf]
    mov [rax + PyMemoryViewObject.mv_buf], rcx
    mov rcx, [rdi + PyMemoryViewObject.mv_len]
    mov [rax + PyMemoryViewObject.mv_len], rcx
    mov rcx, [rdi + PyMemoryViewObject.mv_readonly]
    mov [rax + PyMemoryViewObject.mv_readonly], rcx
    mov rcx, [rbp - MVC_SIZE]
    mov [rax + PyMemoryViewObject.mv_itemsize], rcx
    mov rcx, [rbp - MVC_FMT]
    mov [rax + PyMemoryViewObject.mv_format], rcx
    mov edx, TAG_PTR
    leave
    ret

.mvc_badfmt:
    RAISE exc_ValueError_type, "memoryview: destination format must be a native single character format prefixed with an optional '@'"
.mvc_badlen:
    RAISE exc_TypeError_type, "memoryview: length is not a multiple of itemsize"
.mvc_argerr:
    RAISE exc_TypeError_type, "cast() takes at least 1 argument"
END_FUNC memoryview_method_cast

;; memoryview.tolist() and .hex()
MVL_SELF  equ 8
MVL_OUT   equ 16
MVL_N     equ 24
MVL_FRAME equ 32            ; + 1 push = 40

DEF_FUNC memoryview_method_tolist, 40
    push rbx
    test rsi, rsi
    jz .mvtl_argerr
    mov rdi, [rdi]
    mov [rbp - MVL_SELF], rdi
    call memoryview_check
    mov rdi, [rbp - MVL_SELF]
    mov rax, [rdi + PyMemoryViewObject.mv_len]
    xor edx, edx
    div qword [rdi + PyMemoryViewObject.mv_itemsize]
    mov [rbp - MVL_N], rax
    xor edi, edi
    call list_new
    test rax, rax
    jz .mvtl_fail
    mov [rbp - MVL_OUT], rax
    xor ebx, ebx
.mvtl_loop:
    cmp rbx, [rbp - MVL_N]
    jge .mvtl_done
    mov rdi, [rbp - MVL_SELF]
    mov rsi, rbx
    call memoryview_item_value
    V_PACK_I64 rax, rcx
    mov rsi, rax
    mov rdi, [rbp - MVL_OUT]
    push rsi
    push rsi
    call list_append
    pop rsi
    pop rsi
    DECREF_V rsi, rcx           ; V_PACK may have boxed it
    inc rbx
    jmp .mvtl_loop
.mvtl_done:
    mov rax, [rbp - MVL_OUT]
    mov edx, TAG_PTR
    pop rbx
    leave
    ret
.mvtl_fail:
    xor eax, eax
    xor edx, edx
    pop rbx
    leave
    ret
.mvtl_argerr:
    RAISE exc_TypeError_type, "tolist() takes no arguments"
END_FUNC memoryview_method_tolist

;; ============================================================================
;; memoryview iteration.  tp_iter was 0, so `for b in mv` and list(mv) both
;; failed -- and _pyio iterates a view in more than one place.  The index is
;; checked against the current length each time, as the bytes iterator does.
;; ============================================================================
DEF_FUNC memoryview_tp_iter
    push rbx
    mov rbx, rdi
    call memoryview_check
    mov edi, PyBytesIterObject_size
    call ap_malloc
    mov qword [rax + PyObject.ob_refcnt], 1
    lea rcx, [rel memoryview_iter_type]
    mov [rax + PyObject.ob_type], rcx
    mov [rax + PyBytesIterObject.it_seq], rbx
    mov qword [rax + PyBytesIterObject.it_index], 0
    inc qword [rbx + PyObject.ob_refcnt]
    pop rbx
    leave
    ret
END_FUNC memoryview_tp_iter

;; memoryview.hex() -- through a temporary bytes, as bytearray's read-only
;; methods do, for the same reason: bytes_method_hex reads a bytes layout.
MVH_TMP   equ 8
MVH_FRAME equ 16            ; + 0 pushes = 16

DEF_FUNC memoryview_method_hex, MVH_FRAME
    test rsi, rsi
    jz .mvh_argerr
    mov rdi, [rdi]
    call memoryview_check
    call memoryview_method_hex_self
    leave
    ret
.mvh_argerr:
    RAISE exc_TypeError_type, "hex() takes no arguments"
END_FUNC memoryview_method_hex

DEF_FUNC memoryview_method_hex_self, MVH_FRAME
    mov rsi, [rdi + PyMemoryViewObject.mv_len]
    mov rdi, [rdi + PyMemoryViewObject.mv_buf]
    call bytes_from_data
    test rax, rax
    jz .mvhs_fail
    mov [rbp - MVH_TMP], rax
    sub rsp, 16
    mov [rsp], rax
    mov rdi, rsp
    mov esi, 1
    call bytes_method_hex
    add rsp, 16
    push rax
    push rax
    mov rdi, [rbp - MVH_TMP]
    call obj_decref
    pop rax
    pop rax
    mov edx, TAG_PTR
    leave
    ret
.mvhs_fail:
    xor eax, eax
    xor edx, edx
    leave
    ret
END_FUNC memoryview_method_hex_self

DEF_FUNC_BARE memoryview_iter_next
    mov rax, [rdi + PyBytesIterObject.it_seq]
    cmp qword [rax + PyMemoryViewObject.mv_buf], MV_RELEASED
    je .mvin_done
    mov rcx, [rdi + PyBytesIterObject.it_index]
    mov rdx, [rax + PyMemoryViewObject.mv_len]
    push rdi
    push rax
    mov r8, [rax + PyMemoryViewObject.mv_itemsize]
    mov rax, rdx
    xor edx, edx
    div r8                      ; the item count
    pop rdi                     ; the view
    cmp rcx, rax
    jge .mvin_pop_done
    mov rsi, rcx
    call memoryview_item_value
    pop rdi                     ; the iterator
    inc qword [rdi + PyBytesIterObject.it_index]
    V_PACK_I64 rax, rcx
    ret
.mvin_pop_done:
    pop rdi
.mvin_done:
    xor eax, eax
    ret
END_FUNC memoryview_iter_next




;; ============================================================================
;; memoryview_subscript(obj, key) -> PyMemoryViewObject* (slice)
;; ============================================================================
MS_OBJ   equ 8
MS_KEY   equ 16
MS_START equ 24
MS_COUNT equ 32
MS_FRAME equ 32             ; + 0 pushes = 32
DEF_FUNC memoryview_subscript, MS_FRAME
    ; A view indexes in ITEMS, not bytes.  They are the same thing until
    ; cast() has been called, which is why the byte length stood in for the
    ; item count here and the count was one divide away from being wrong.
    mov [rbp - MS_OBJ], rdi
    call memoryview_check
    V_UNPACK rsi, rdx           ; key Value -> (payload, tag)
    mov [rbp - MS_KEY], rsi

    cmp edx, TAG_SMALLINT
    je .ms_int_index
    cmp edx, TAG_PTR            ; a float key is neither: classify
    jne .ms_type_error          ; fully before dereferencing, or raw
                                ; f64 bits get used as an address
    mov rax, [rsi + PyObject.ob_type]
    lea rcx, [rel slice_type]
    cmp rax, rcx
    jne .ms_int_index_heap

    ; --- slice ---
    mov rdi, [rbp - MS_OBJ]            ; the length slice_indices clamps
    call memoryview_nitems             ; against is the VIEW's, in items
    mov rsi, rax
    mov rdi, [rbp - MS_KEY]            ; slice obj
    call slice_indices                 ; rax=start, rdx=stop, rcx=step

    ; A step other than 1 needs a stride, which this view does not carry;
    ; CPython answers with a non-contiguous view.  Recorded in bugs.md.
    cmp rcx, 1
    jne .ms_step_error

    sub rdx, rax                       ; item count
    jns .ms_have_count
    xor edx, edx                       ; an empty slice, not a negative one
.ms_have_count:
    mov [rbp - MS_START], rax
    mov [rbp - MS_COUNT], rdx

    mov edi, PyMemoryViewObject_size
    call ap_malloc
    test rax, rax
    jz .ms_fail

    mov qword [rax + PyMemoryViewObject.ob_refcnt], 1
    lea rcx, [rel memoryview_type]
    mov [rax + PyMemoryViewObject.ob_type], rcx

    ; Every field the source carries has to come across.  A missed itemsize
    ; left the new view with 0, and memoryview_len divides by it.
    mov rdi, [rbp - MS_OBJ]
    mov rcx, [rdi + PyMemoryViewObject.mv_itemsize]
    mov [rax + PyMemoryViewObject.mv_itemsize], rcx
    mov rdx, [rdi + PyMemoryViewObject.mv_format]
    mov [rax + PyMemoryViewObject.mv_format], rdx
    mov rdx, [rdi + PyMemoryViewObject.mv_readonly]
    mov [rax + PyMemoryViewObject.mv_readonly], rdx

    mov rdx, [rbp - MS_START]
    imul rdx, rcx
    add rdx, [rdi + PyMemoryViewObject.mv_buf]
    mov [rax + PyMemoryViewObject.mv_buf], rdx
    mov rdx, [rbp - MS_COUNT]
    imul rdx, rcx
    mov [rax + PyMemoryViewObject.mv_len], rdx

    ; The slice shares the ORIGINAL owner, not the view it came from: a
    ; chain of slices would otherwise keep every intermediate alive.
    mov rcx, [rdi + PyMemoryViewObject.mv_source]
    mov [rax + PyMemoryViewObject.mv_source], rcx
    test rcx, rcx
    jz .ms_no_source
    inc qword [rcx + PyObject.ob_refcnt]
    push rax
    mov rdi, rcx
    call bytearray_export_acquired
    call io_buffer_acquired
    pop rax
.ms_no_source:
    mov edx, TAG_PTR
    leave
    V_PACK rax, rdx
    ret

.ms_fail:
    xor eax, eax
    xor edx, edx
    leave
    ret

.ms_int_index:
    ; rsi = the index, as an i64
    mov rdi, [rbp - MS_OBJ]
    push rsi
    call memoryview_nitems             ; rax = item count
    pop rsi
    mov rcx, rax
    test rsi, rsi
    jns .ms_check_bounds
    add rsi, rcx
.ms_check_bounds:
    cmp rsi, 0
    jl .ms_index_error
    cmp rsi, rcx
    jge .ms_index_error
    mov rdi, [rbp - MS_OBJ]
    call memoryview_item_value
    mov edx, TAG_SMALLINT
    leave
    V_PACK rax, rdx
    ret

.ms_int_index_heap:
    mov rax, [rsi + PyObject.ob_type]   ; int_to_i64 reads PyIntObject.compact
    REQUIRE_INT_TYPE rax, rcx, .ms_type_error   ; unconditionally
    mov rdi, rsi
    call int_to_i64
    mov rsi, rax
    jmp .ms_int_index

.ms_index_error:
    RAISE exc_IndexError_type, "index out of range"

.ms_step_error:
    RAISE exc_NotImplementedError_type, "memoryview: only step 1 is supported"

.ms_type_error:
    RAISE exc_TypeError_type, "memoryview: invalid slice key"
END_FUNC memoryview_subscript

;; ============================================================================
;; memoryview_nitems(rdi = self) -> rax = the length in ITEMS
;; ============================================================================
DEF_FUNC_BARE memoryview_nitems
    mov rax, [rdi + PyMemoryViewObject.mv_len]
    mov r8, [rdi + PyMemoryViewObject.mv_itemsize]
    cmp r8, 1
    je .mvn_done
    push rdx
    xor edx, edx
    div r8
    pop rdx
.mvn_done:
    ret
END_FUNC memoryview_nitems

;; ============================================================================
;; memoryview_ass_subscript(rdi = self, rsi = key Value, rdx = value Value)
;;
;; mp_ass_subscript was 0, so a view over a bytearray was read-only in
;; practice -- and readinto(), which is the whole reason _pyio takes a view,
;; is nothing but writes through one.
;;
;; A slice assignment must be the same size: a view cannot resize its owner.
;; ============================================================================
MA_OBJ    equ 8
MA_KEY    equ 16
MA_VAL    equ 24
MA_START  equ 32
MA_COUNT  equ 40
MA_FRAME  equ 48            ; + 0 pushes = 48

DEF_FUNC memoryview_ass_subscript, MA_FRAME
    mov [rbp - MA_OBJ], rdi
    mov [rbp - MA_KEY], rsi
    mov [rbp - MA_VAL], rdx
    call memoryview_check
    test rdx, rdx
    jz .ma_del_error
    cmp qword [rdi + PyMemoryViewObject.mv_readonly], 0
    jne .ma_readonly

    mov rsi, [rbp - MA_KEY]
    V_TEST_PTR rsi, rax
    jbe .ma_maybe_slice
.ma_int_key:
    ; An item assignment: one integer, range-checked like bytearray's.
    mov rdi, [rbp - MA_KEY]
    V_UNPACK rdi, rdx
    call obj_as_index
    mov rsi, rax
    mov rdi, [rbp - MA_OBJ]
    push rsi
    call memoryview_nitems
    pop rsi
    mov rcx, rax
    test rsi, rsi
    jns .ma_bounds
    add rsi, rcx
.ma_bounds:
    cmp rsi, 0
    jl .ma_index_error
    cmp rsi, rcx
    jge .ma_index_error
    mov [rbp - MA_START], rsi

    mov rdi, [rbp - MA_VAL]
    V_UNPACK rdi, rdx
    call int_is_integer         ; not a tag test: a heap int, a bool and an
    test eax, eax               ; int subclass are all integers here
    jz .ma_value_type
    mov rdi, [rbp - MA_VAL]
    V_UNPACK rdi, rdx
    call obj_as_index
    cmp rax, 0
    jl .ma_value_range
    cmp rax, 255
    jg .ma_value_range

    mov rdi, [rbp - MA_OBJ]
    mov rsi, [rbp - MA_START]
    mov rcx, [rdi + PyMemoryViewObject.mv_itemsize]
    imul rsi, rcx
    add rsi, [rdi + PyMemoryViewObject.mv_buf]
    cmp rcx, 1
    je .ma_store1
    cmp rcx, 2
    je .ma_store2
    cmp rcx, 4
    je .ma_store4
    mov [rsi], rax
    jmp .ma_ok
.ma_store1:
    mov [rsi], al
    jmp .ma_ok
.ma_store2:
    mov [rsi], ax
    jmp .ma_ok
.ma_store4:
    mov [rsi], eax
.ma_ok:
    xor eax, eax
    leave
    ret

.ma_maybe_slice:
    test rsi, rsi
    jz .ma_key_type
    mov rax, [rsi + PyObject.ob_type]
    lea rcx, [rel slice_type]
    cmp rax, rcx
    jne .ma_int_key

    mov rdi, [rbp - MA_OBJ]
    call memoryview_nitems
    mov rsi, rax
    mov rdi, [rbp - MA_KEY]
    call slice_indices                 ; rax=start, rdx=stop, rcx=step
    cmp rcx, 1
    jne .ma_step_error
    sub rdx, rax
    jns .ma_have_count
    xor edx, edx
.ma_have_count:
    mov [rbp - MA_START], rax
    mov [rbp - MA_COUNT], rdx

    mov rdi, [rbp - MA_VAL]
    call bytes_like_ptr_len            ; rax = data, r10 = length, ecx = ok
    test ecx, ecx
    jz .ma_value_type
    mov rdi, [rbp - MA_OBJ]
    mov rdx, [rbp - MA_COUNT]
    imul rdx, [rdi + PyMemoryViewObject.mv_itemsize]
    cmp r10, rdx
    jne .ma_size_error

    mov rsi, rax                       ; source
    mov rdi, [rbp - MA_START]
    mov rax, [rbp - MA_OBJ]
    imul rdi, [rax + PyMemoryViewObject.mv_itemsize]
    add rdi, [rax + PyMemoryViewObject.mv_buf]
    call ap_memcpy
    jmp .ma_ok

.ma_del_error:
    RAISE exc_TypeError_type, "cannot delete memory"
.ma_readonly:
    RAISE exc_TypeError_type, "cannot modify read-only memory"
.ma_index_error:
    RAISE exc_IndexError_type, "index out of range"
.ma_value_range:
    RAISE exc_ValueError_type, "memoryview: invalid value for format 'B'"
.ma_value_type:
    RAISE exc_TypeError_type, "memoryview: invalid type for assignment"
.ma_size_error:
    RAISE exc_ValueError_type, "memoryview assignment: lvalue and rvalue have different structures"
.ma_step_error:
    RAISE exc_NotImplementedError_type, "memoryview: only step 1 is supported"
.ma_key_type:
    RAISE exc_TypeError_type, "memoryview: invalid slice key"
END_FUNC memoryview_ass_subscript

;; ============================================================================
;; memoryview_len(obj) -> int64
;; ============================================================================
;; len() counts ITEMS, so a view cast to 'I' is a quarter as long as its
;; bytes; and a released view answers no questions at all.
DEF_FUNC memoryview_len
    call memoryview_check
    call memoryview_nitems
    leave
    ret
END_FUNC memoryview_len

;; The by-name half of the two slots above.  A slot with no matching entry in
;; tp_dict answers hasattr() and getattr() wrong, and _pyio reaches
;; __setitem__ through the abstract base classes rather than the syntax.
DEF_FUNC memoryview_dunder_getitem
    REQUIRE_SELF memoryview_type, "__getitem__"
    cmp rsi, 2
    jne .mdg_bad
    mov rsi, [rdi + 8]
    mov rdi, [rdi]
    call memoryview_subscript
    leave
    ret
.mdg_bad:
    RAISE exc_TypeError_type, "expected exactly one argument"
END_FUNC memoryview_dunder_getitem

DEF_FUNC memoryview_dunder_setitem
    REQUIRE_SELF memoryview_type, "__setitem__"
    cmp rsi, 3
    jne .mds_bad
    mov rdx, [rdi + 16]
    mov rsi, [rdi + 8]
    mov rdi, [rdi]
    call memoryview_ass_subscript
    LOAD_NONE rax
    mov edx, TAG_PTR
    leave
    ret
.mds_bad:
    RAISE exc_TypeError_type, "expected exactly two arguments"
END_FUNC memoryview_dunder_setitem

DEF_FUNC memoryview_dunder_len
    REQUIRE_SELF memoryview_type, "__len__"
    test rsi, rsi
    jz .mdl_bad
    mov rdi, [rdi]
    call memoryview_len
    mov rdx, rax
    V_PACK_I64 rdx, rcx
    mov rax, rdx
    leave
    ret
.mdl_bad:
    RAISE exc_TypeError_type, "__len__() takes no arguments"
END_FUNC memoryview_dunder_len

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
    dq memoryview_ass_subscript ; +16: mp_ass_subscript

align 8
global memoryview_type
memoryview_type:
    dq 1                             ; ob_refcnt
    dq type_type                     ; ob_type
    dq mv_name_str                   ; tp_name
    dq PyMemoryViewObject_size       ; tp_basicsize
    dq memoryview_dealloc_proper     ; tp_dealloc
    dq memoryview_repr               ; tp_repr
    dq memoryview_repr               ; tp_str
    ; Unhashable while the buffer is writable, which is the only kind that
    ; reaches here in practice; a 0 would fall through to the object address.
    dq hash_not_implemented          ; tp_hash
    dq 0                             ; tp_call (set by add_builtin_type)
    dq memoryview_getattr            ; tp_getattr
    dq 0                             ; tp_setattr
    dq bytes_compare                 ; tp_richcompare (over the shared reader)
    dq memoryview_tp_iter            ; tp_iter
    dq 0                             ; tp_iternext
    dq 0                             ; tp_init
    dq 0                             ; tp_new
    dq 0                             ; tp_as_number
    dq memoryview_seq_methods        ; tp_as_sequence
    dq memoryview_mapping_methods    ; tp_as_mapping
    dq 0                             ; tp_base
    dq 0                             ; tp_dict
    dq 0                             ; tp_mro
    dq TYPE_FLAG_FINAL          ; tp_flags -- CPython gives this type no
                                ; Py_TPFLAGS_BASETYPE
    dq 0                             ; tp_bases
    dq 0                        ; tp_traverse
    dq 0                        ; tp_clear
    dq 0 ; tp_dictoffset

section .rodata
bas_msg_size: db "attempt to assign bytes of size ", 0
bas_msg_to:   db " to extended slice of size ", 0
