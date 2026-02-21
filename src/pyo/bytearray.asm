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

section .text

;; ============================================================================
;; bytearray_type_call(type, args, nargs) -> PyByteArrayObject*
;; Constructor: bytearray(bytes_obj)
;; ============================================================================
global bytearray_type_call
BA_TYPE  equ 8
BA_FRAME equ 8
DEF_FUNC bytearray_type_call, BA_FRAME
    ; rdi=type, rsi=args, rdx=nargs
    mov [rbp - BA_TYPE], rdi           ; save type
    cmp rdx, 1
    jne .ba_error
    mov rdi, [rsi]                     ; arg0 payload
    ; Must be a bytes object
    cmp qword [rsi + 8], TAG_SMALLINT
    je .ba_error                       ; SmallInt → error
    mov rax, [rdi + PyObject.ob_type]
    lea rcx, [rel bytes_type]
    cmp rax, rcx
    jne .ba_error

    ; Allocate bytearray: header + data
    mov rcx, [rdi + PyBytesObject.ob_size]
    push rdi                           ; save src bytes
    push rcx                           ; save size
    ; Check if type needs GC allocation (heap type subclass)
    mov rdx, [rbp - BA_TYPE]
    test qword [rdx + PyTypeObject.tp_flags], TYPE_FLAG_HAVE_GC
    lea rdi, [rcx + PyByteArrayObject.data]
    jz .ba_plain_alloc
    ; GC alloc for subclass
    mov rsi, rdx
    call gc_alloc
    jmp .ba_alloc_done
.ba_plain_alloc:
    call ap_malloc
    mov qword [rax + PyByteArrayObject.ob_refcnt], 1
    mov rdx, [rbp - BA_TYPE]
    mov [rax + PyByteArrayObject.ob_type], rdx
.ba_alloc_done:
    pop rcx                            ; size
    pop rsi                            ; src bytes

    ; Init header
    mov [rax + PyByteArrayObject.ob_size], rcx
    ; INCREF the type (needed for heap type subclasses)
    mov rdx, [rbp - BA_TYPE]
    inc qword [rdx + PyObject.ob_refcnt]

    ; Copy data
    push rax                           ; save result
    lea rdi, [rax + PyByteArrayObject.data]
    lea rsi, [rsi + PyBytesObject.data]
    mov rdx, rcx
    call ap_memcpy

    pop rax                            ; return bytearray obj
    ; gc_track if heap type subclass
    mov rdx, [rbp - BA_TYPE]
    test qword [rdx + PyTypeObject.tp_flags], TYPE_FLAG_HAVE_GC
    jz .ba_no_track
    push rax
    mov rdi, rax
    call gc_track
    pop rax
.ba_no_track:
    mov edx, TAG_PTR
    leave
    ret

.ba_error:
    lea rdi, [rel exc_TypeError_type]
    CSTRING rsi, "bytearray() argument must be a bytes object"
    call raise_exception
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
    dq 0                            ; tp_repr
    dq 0                            ; tp_str
    dq 0                            ; tp_hash
    dq 0                            ; tp_call (set by add_builtin_type)
    dq 0                            ; tp_getattr
    dq 0                            ; tp_setattr
    dq 0                            ; tp_richcompare
    dq 0                            ; tp_iter
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
