; str_obj.asm - String type
; Phase 1: minimal stub providing str_from_cstr
; Full implementation in Phase 2/8

%include "macros.inc"
%include "object.inc"
%include "types.inc"

section .text

extern ap_malloc
extern strlen
extern memcpy

; str_from_cstr(const char *cstr) -> PyStrObject*
; Creates a new string object from a C string
global str_from_cstr
str_from_cstr:
    push rbp
    mov rbp, rsp
    push rbx
    push r12

    mov rbx, rdi            ; save cstr

    ; Get string length
    call strlen wrt ..plt
    mov r12, rax             ; r12 = length

    ; Allocate: PyStrObject header + length + 1 (null terminator)
    lea rdi, [rax + PyStrObject.data + 1]
    call ap_malloc
    ; rax = new PyStrObject*

    ; Fill header
    mov qword [rax + PyObject.ob_refcnt], 1
    lea rcx, [rel str_type]
    mov [rax + PyObject.ob_type], rcx
    mov [rax + PyStrObject.ob_size], r12
    mov qword [rax + PyStrObject.ob_hash], -1  ; not computed

    ; Copy string data
    push rax                 ; save obj ptr
    lea rdi, [rax + PyStrObject.data]
    mov rsi, rbx             ; source = cstr
    lea rdx, [r12 + 1]      ; length + null
    call memcpy wrt ..plt
    pop rax                  ; restore obj ptr

    pop r12
    pop rbx
    pop rbp
    ret

; str_new(const char *data, int64_t len) -> PyStrObject*
; Creates a new string object from data with given length (need not be null-terminated)
global str_new
str_new:
    push rbp
    mov rbp, rsp
    push rbx
    push r12
    push r13

    mov rbx, rdi            ; save data ptr
    mov r12, rsi            ; save length

    ; Allocate: header + length + 1
    lea rdi, [r12 + PyStrObject.data + 1]
    call ap_malloc
    mov r13, rax             ; r13 = new PyStrObject*

    ; Fill header
    mov qword [r13 + PyObject.ob_refcnt], 1
    lea rcx, [rel str_type]
    mov [r13 + PyObject.ob_type], rcx
    mov [r13 + PyStrObject.ob_size], r12
    mov qword [r13 + PyStrObject.ob_hash], -1

    ; Copy data
    lea rdi, [r13 + PyStrObject.data]
    mov rsi, rbx
    mov rdx, r12
    call memcpy wrt ..plt

    ; Null-terminate
    mov byte [r13 + PyStrObject.data + r12], 0

    mov rax, r13
    pop r13
    pop r12
    pop rbx
    pop rbp
    ret

; str_dealloc(PyObject *self)
global str_dealloc
str_dealloc:
    ; String data is inline, just free the object
    extern ap_free
    jmp ap_free

; str_repr(PyObject *self) -> PyObject*
; For now, returns self with incref (repr should add quotes, but phase 1 stub)
global str_repr
str_repr:
    inc qword [rdi + PyObject.ob_refcnt]
    mov rax, rdi
    ret

; str_hash(PyObject *self) -> int64
; FNV-1a hash
global str_hash
str_hash:
    push rbp
    mov rbp, rsp

    ; Check cached hash
    mov rax, [rdi + PyStrObject.ob_hash]
    cmp rax, -1
    jne .done

    ; Compute FNV-1a
    mov rcx, [rdi + PyStrObject.ob_size]
    lea rsi, [rdi + PyStrObject.data]
    mov rax, 0xcbf29ce484222325     ; FNV offset basis
    mov rdx, 0x100000001b3          ; FNV prime
.loop:
    test rcx, rcx
    jz .store
    movzx r8d, byte [rsi]
    xor rax, r8
    imul rax, rdx
    inc rsi
    dec rcx
    jmp .loop
.store:
    ; Ensure hash is never -1
    cmp rax, -1
    jne .cache
    mov rax, -2
.cache:
    mov [rdi + PyStrObject.ob_hash], rax
.done:
    pop rbp
    ret

section .data

str_name: db "str", 0

; str type object
align 8
global str_type
str_type:
    dq 1                ; ob_refcnt
    dq 0                ; ob_type
    dq str_name         ; tp_name
    dq PyStrObject.data ; tp_basicsize (minimum, without data)
    dq str_dealloc      ; tp_dealloc
    dq str_repr         ; tp_repr
    dq str_repr         ; tp_str (returns self for strings)
    dq str_hash         ; tp_hash
    dq 0                ; tp_call
    dq 0                ; tp_getattr
    dq 0                ; tp_setattr
    dq 0                ; tp_richcompare
    dq 0                ; tp_iter
    dq 0                ; tp_iternext
    dq 0                ; tp_init
    dq 0                ; tp_new
    dq 0                ; tp_as_number
    dq 0                ; tp_as_sequence
    dq 0                ; tp_as_mapping
    dq 0                ; tp_base
    dq 0                ; tp_dict
    dq 0                ; tp_mro
    dq 0                ; tp_flags
    dq 0                ; tp_bases
