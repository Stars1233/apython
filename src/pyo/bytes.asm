; bytes_obj.asm - Bytes type implementation
; Immutable sequence of raw bytes with inline storage

%include "macros.inc"
%include "object.inc"
%include "types.inc"

section .note.GNU-stack noalloc noexec nowrite progbits

section .text

extern ap_malloc
extern ap_free
extern str_from_cstr
extern ap_memcpy

; bytes_new(int64_t size) -> PyBytesObject*
; Allocate a bytes object with room for 'size' bytes
global bytes_new
bytes_new:
    push rbp
    mov rbp, rsp
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
    pop rbp
    ret

; bytes_from_data(const void *data, int64_t size) -> PyBytesObject*
; Allocate a bytes object and copy data into it
global bytes_from_data
bytes_from_data:
    push rbp
    mov rbp, rsp
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
    pop rbp
    ret

; bytes_dealloc(PyObject *self)
; Data is inline, just free the object
global bytes_dealloc
bytes_dealloc:
    jmp ap_free

; bytes_repr(PyObject *self) -> PyStrObject*
; Stub: returns "b'...'"
global bytes_repr
bytes_repr:
    push rbp
    mov rbp, rsp
    lea rdi, [rel bytes_repr_str]
    call str_from_cstr
    pop rbp
    ret

section .data

bytes_name_str: db "bytes", 0
bytes_repr_str: db "b'...'", 0

; bytes type object
align 8
global bytes_type
bytes_type:
    dq 1                    ; ob_refcnt
    dq 0                    ; ob_type
    dq bytes_name_str       ; tp_name
    dq PyBytesObject.data   ; tp_basicsize (header, without data)
    dq bytes_dealloc        ; tp_dealloc
    dq bytes_repr           ; tp_repr
    dq bytes_repr           ; tp_str
    dq 0                    ; tp_hash
    dq 0                    ; tp_call
    dq 0                    ; tp_getattr
    dq 0                    ; tp_setattr
    dq 0                    ; tp_richcompare
    dq 0                    ; tp_iter
    dq 0                    ; tp_iternext
    dq 0                    ; tp_init
    dq 0                    ; tp_new
    dq 0                    ; tp_as_number
    dq 0                    ; tp_as_sequence
    dq 0                    ; tp_as_mapping
    dq 0                    ; tp_base
    dq 0                    ; tp_dict
    dq 0                    ; tp_mro
    dq 0                    ; tp_flags
    dq 0                    ; tp_bases
