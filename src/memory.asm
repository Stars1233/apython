; memory.asm - Memory allocation wrappers
; Wraps libc malloc/free/realloc with error checking

%include "macros.inc"

section .note.GNU-stack noalloc noexec nowrite progbits

section .text

extern malloc
extern free
extern realloc
extern fatal_error

; ap_malloc(size_t size) -> void*
; Allocates memory, fatal error on failure
global ap_malloc
ap_malloc:
    push rbp
    mov rbp, rsp
    push rbx
    mov rbx, rdi            ; save size
    call malloc wrt ..plt
    test rax, rax
    jz .oom
    pop rbx
    pop rbp
    ret
.oom:
    lea rdi, [rel mem_oom_msg]
    call fatal_error        ; never returns

; ap_free(void *ptr)
; Frees memory; NULL-safe
global ap_free
ap_free:
    test rdi, rdi
    jz .null
    jmp free wrt ..plt
.null:
    ret

; ap_realloc(void *ptr, size_t size) -> void*
; Reallocates memory, fatal error on failure
global ap_realloc
ap_realloc:
    push rbp
    mov rbp, rsp
    push rbx
    mov rbx, rsi            ; save size for error case
    call realloc wrt ..plt
    test rax, rax
    jz .oom
    pop rbx
    pop rbp
    ret
.oom:
    lea rdi, [rel mem_oom_msg]
    call fatal_error        ; never returns

section .rodata
mem_oom_msg: db "Fatal: out of memory", 0
