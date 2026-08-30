; bytearray.asm - bytearray type implementation (minimal, for test_int.py)
; Mutable byte sequence with inline storage (same layout as bytes)

%include "macros.inc"
%include "object.inc"
%include "types.inc"

extern ap_malloc
extern ap_free
extern ap_memcpy
extern gc_alloc
extern gc_track
extern type_type
extern raise_exception
extern exc_TypeError_type
extern bytes_type
extern bytearray_repr
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
    extern bytearray_range_msg
    lea rdx, [rel bytearray_range_msg]
    extern byteslike_source
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
