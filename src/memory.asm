; memory.asm - Memory allocation wrappers
; Wraps libc malloc/free/realloc with error checking

%include "macros.inc"


extern malloc
extern free
extern realloc
extern fatal_error

; ap_malloc(size_t size) -> void*
; Allocates memory, fatal error on failure
DEF_FUNC ap_malloc
    push rbx
    mov rbx, rdi            ; save size
    call malloc wrt ..plt
    test rax, rax
    jz .oom
    pop rbx
    leave
    ret
.oom:
    lea rdi, [rel mem_oom_msg]
    call fatal_error        ; never returns
END_FUNC ap_malloc

; ap_free(void *ptr)
; Frees memory; NULL-safe
DEF_FUNC_BARE ap_free
    test rdi, rdi
    jz .null
    jmp free wrt ..plt
.null:
    ret
END_FUNC ap_free

; ap_realloc(void *ptr, size_t size) -> void*
; Reallocates memory, fatal error on failure
DEF_FUNC ap_realloc
    push rbx
    mov rbx, rsi            ; save size for error case
    call realloc wrt ..plt
    test rax, rax
    jz .oom
    pop rbx
    leave
    ret
.oom:
    lea rdi, [rel mem_oom_msg]
    call fatal_error        ; never returns
END_FUNC ap_realloc

section .rodata
mem_oom_msg: db "Fatal: out of memory", 0
