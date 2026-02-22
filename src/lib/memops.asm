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

; ap_memmove(void *dst, const void *src, size_t n) -> void *dst
; Handles overlapping regions. n must be a multiple of 8.
; Forward: rep movsq (fast). Backward: manual qword loop (avoids std penalty).
DEF_FUNC_BARE ap_memmove
    mov rax, rdi            ; save dst for return
    mov rcx, rdx
    shr rcx, 3              ; qword count = n / 8
    jz .memmove_done
    cmp rdi, rsi
    je .memmove_done        ; dst == src, nop
    jb .memmove_fwd         ; dst < src: forward safe
.memmove_bk:
    ; dst > src: copy backward to avoid overlap corruption
    lea rsi, [rsi + rdx - 8]
    lea rdi, [rdi + rdx - 8]
.memmove_bk_loop:
    mov r8, [rsi]
    mov [rdi], r8
    sub rsi, 8
    sub rdi, 8
    dec rcx
    jnz .memmove_bk_loop
    ret
.memmove_fwd:
    rep movsq
.memmove_done:
    ret
END_FUNC ap_memmove

; ap_memcmp(const void *s1, const void *s2, size_t n) -> int
; Returns 0 if equal, <0 if s1<s2, >0 if s1>s2
DEF_FUNC_BARE ap_memcmp
    mov rcx, rdx            ; rcx = count
    repe cmpsb              ; rdi=s1, rsi=s2
    je .memcmp_equal
    movzx eax, byte [rdi - 1]
    movzx ecx, byte [rsi - 1]
    sub eax, ecx
    ret
.memcmp_equal:
    xor eax, eax
    ret
END_FUNC ap_memcmp
