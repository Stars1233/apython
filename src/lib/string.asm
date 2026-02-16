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
; 8-byte fast path with byte-at-a-time fallback, returns <0 / 0 / >0
;
; Safety: reading 8 bytes at a time is safe because all callers compare
; PyStrObject.data which is inline after the header. Object allocation
; always provides >=8 bytes past .data even for 1-char strings, due to
; minimum object size and alignment.
DEF_FUNC_BARE ap_strcmp
    ; rdi = a, rsi = b
.fast8:
    mov rax, [rdi]          ; load 8 bytes from a
    mov rdx, [rsi]          ; load 8 bytes from b
    cmp rax, rdx
    jne .byte_loop          ; mismatch -> fall back

    ; Check if NUL within these 8 bytes (Mycroft's trick)
    mov rcx, rax
    mov r8, 0x0101010101010101
    sub rcx, r8
    not rax
    and rcx, rax
    mov r8, 0x8080808080808080
    and rcx, r8
    jnz .equal              ; NUL found -> strings equal

    add rdi, 8
    add rsi, 8
    jmp .fast8

.equal:
    xor eax, eax
    ret

.byte_loop:
    movzx eax, byte [rdi]
    movzx ecx, byte [rsi]
    sub eax, ecx
    jnz .done               ; bytes differ
    test cl, cl
    jz .done                 ; both NUL
    inc rdi
    inc rsi
    jmp .byte_loop
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
