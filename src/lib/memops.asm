; memory.asm - PLT-free memory operations
; Replaces libc memcpy and memset
; Uses rep movsb / rep stosb (optimal on ERMS-capable CPUs, Ivy Bridge+)

%include "macros.inc"


; ap_memcpy(void *dst, const void *src, size_t n) -> void *dst
DEF_FUNC_BARE ap_memcpy
    mov rax, rdi            ; save dst for return
    mov rcx, rdx            ; rcx = count
    rep movsb               ; rdi=dst, rsi=src already in place
    ret
END_FUNC ap_memcpy

; ap_memset(void *dst, int val, size_t n) -> void *dst
DEF_FUNC_BARE ap_memset
    mov r8, rdi             ; save dst for return
    mov al, sil             ; val (byte)
    mov rcx, rdx            ; rcx = count
    rep stosb               ; rdi=dst already in place
    mov rax, r8             ; return original dst
    ret
END_FUNC ap_memset
