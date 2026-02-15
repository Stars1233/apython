; string.asm - PLT-free string operations
; Replaces libc strlen, strcmp, strstr

%include "macros.inc"


; ap_strlen(const char *s) -> size_t
; Uses repne scasb (fast on modern x86-64 with FAST_SHORT_REP)
DEF_FUNC_BARE ap_strlen
    mov rdi, rdi            ; s already in rdi
    xor eax, eax            ; search for NUL byte
    mov rcx, -1             ; max search length
    repne scasb
    not rcx
    dec rcx                 ; rcx = length (not counting NUL)
    mov rax, rcx
    ret
END_FUNC ap_strlen

; ap_strcmp(const char *a, const char *b) -> int
; Byte-by-byte compare, returns <0 / 0 / >0
DEF_FUNC_BARE ap_strcmp
    ; rdi = a, rsi = b
.loop:
    movzx eax, byte [rdi]
    movzx ecx, byte [rsi]
    sub eax, ecx
    jnz .done               ; bytes differ
    test cl, cl
    jz .done                 ; both NUL
    inc rdi
    inc rsi
    jmp .loop
.done:
    ret
END_FUNC ap_strcmp

; ap_strstr(const char *haystack, const char *needle) -> char* or NULL
; Simple O(n*m) search. Returns pointer to first match or NULL.
DEF_FUNC_BARE ap_strstr
    ; rdi = haystack, rsi = needle
    ; If needle is empty, return haystack
    cmp byte [rsi], 0
    je .return_haystack

.outer:
    movzx eax, byte [rdi]
    test al, al
    jz .not_found               ; end of haystack
    ; Try to match needle starting here
    mov rcx, rdi                ; rcx = haystack cursor
    mov rdx, rsi                ; rdx = needle cursor
.inner:
    movzx eax, byte [rdx]
    test al, al
    jz .found                   ; end of needle = full match
    cmp al, [rcx]
    jne .advance                ; mismatch
    inc rcx
    inc rdx
    jmp .inner
.advance:
    inc rdi
    jmp .outer

.found:
    mov rax, rdi
    ret
.return_haystack:
    mov rax, rdi
    ret
.not_found:
    xor eax, eax
    ret
END_FUNC ap_strstr
