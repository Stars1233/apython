; uniname.asm - resolve a Unicode character name for \N{...}
;
; The table lives in unicodename.asm, which is generated; this is the search
; over it.  Two shapes answer:
;
;   CJK UNIFIED IDEOGRAPH-XXXX   the codepoint is written in the name, so it is
;                                parsed rather than stored -- 97046 entries the
;                                table does not have to carry.  The ranges it
;                                is valid over do come from the table, so a
;                                bogus one is still rejected.
;   everything else              a linear scan of the NUL-separated blob.
;
; The scan is the whole index: `\N{...}` occurs a handful of times in a file at
; most, so a pass over a megabyte costs less -- in code and in generated source
; -- than an offset array and a binary search would.
;
; Matching is case-insensitive, as CPython's is: `\N{latin small letter a}`
; resolves.  Nothing else is normalised; underscores and stray spaces do not.

%include "macros.inc"
%include "object.inc"
%include "compiler.inc"

extern uniname_blob
extern uniname_blob_end
extern uniname_cjk_ranges
extern uniname_cjk_ranges_end
extern ap_memcmp

section .rodata
uniname_cjk_prefix: db "CJK UNIFIED IDEOGRAPH-", 0
UNINAME_CJK_PREFIX_LEN equ 22

section .text

;; ============================================================================
;; uniname_lookup(const char *name, int64_t len) -> rax = codepoint, or -1
;; ============================================================================
UN_BUFSZ equ 128         ; must match UNINAME_MAX in gen_unicodename.py
UN_NAME  equ 8
UN_LEN   equ 16
UN_BUF   equ 16 + UN_BUFSZ       ; the name being decoded, NUL-terminated
UN_QBUF  equ UN_BUF + UN_BUFSZ   ; the query, uppercased ONCE
UN_FRAME equ 280         ; UN_QBUF rounded up; + 3 pushes = 304, 16-aligned
DEF_FUNC uniname_lookup, UN_FRAME
    push rbx
    push r12
    push r13
    mov [rbp - UN_NAME], rdi
    mov [rbp - UN_LEN], rsi
    test rsi, rsi
    jle .miss

    ; --- the algorithmic family ---
    cmp rsi, UNINAME_CJK_PREFIX_LEN + 1
    jl .scan_table
    xor ecx, ecx
.cjk_prefix:
    cmp rcx, UNINAME_CJK_PREFIX_LEN
    jae .cjk_have_prefix
    movzx eax, byte [rdi + rcx]
    call .upper
    lea rdx, [rel uniname_cjk_prefix]
    movzx edx, byte [rdx + rcx]
    cmp al, dl
    jne .scan_table
    inc rcx
    jmp .cjk_prefix
.cjk_have_prefix:
    ; The rest must be hex digits, and there must be at least one.
    xor r8, r8                          ; the codepoint
    mov rcx, UNINAME_CJK_PREFIX_LEN
.cjk_hex:
    cmp rcx, [rbp - UN_LEN]
    jae .cjk_hex_done
    movzx eax, byte [rdi + rcx]
    call .upper
    sub eax, '0'
    cmp eax, 9
    jbe .cjk_digit
    sub eax, 'A' - '0'
    cmp eax, 5
    ja .miss
    add eax, 10
.cjk_digit:
    shl r8, 4
    or r8, rax
    cmp r8, 0x10ffff
    ja .miss
    inc rcx
    jmp .cjk_hex
.cjk_hex_done:
    ; ...and inside one of the ranges the name is defined over.
    lea rcx, [rel uniname_cjk_ranges]
    lea rdx, [rel uniname_cjk_ranges_end]
.cjk_range:
    cmp rcx, rdx
    jae .miss
    mov eax, [rcx]
    cmp r8, rax
    jb .cjk_range_next
    mov eax, [rcx + 4]
    cmp r8, rax
    jbe .cjk_hit
.cjk_range_next:
    add rcx, 8
    jmp .cjk_range
.cjk_hit:
    mov rax, r8
    jmp .done

    ; --- the table ---
    ;
    ; Entries are front-coded against their predecessor, so the scan decodes
    ; as it walks: `shared` bytes of the previous name stay in the buffer and
    ; only the suffix is copied in.  The scan was already strictly sequential,
    ; which is the whole reason the encoding is free here.
    ;
    ; The codepoint rides the same stream as a delta, with 255 escaping to an
    ; absolute for the thirty-one places the sequence jumps -- chiefly the
    ; aliases at the end, which are not in codepoint order at all.
.scan_table:
    ; Uppercase the query ONCE.  It used to be folded a character at a time,
    ; through a call, for every entry -- 46,100 times over a failing lookup.
    mov rsi, [rbp - UN_LEN]
    cmp rsi, UN_BUFSZ
    jae .miss                           ; longer than the buffer can hold with
                                        ; its NUL, and so longer than any name
                                        ; the generator will emit.  UN_BUFSZ-1
                                        ; here rejected a name of exactly
                                        ; UNINAME_MAX-1 bytes, which fits
    mov rdi, [rbp - UN_NAME]
    lea rdx, [rbp - UN_QBUF]
    xor ecx, ecx
.upcase:
    cmp rcx, rsi
    jae .upcased
    mov al, [rdi + rcx]
    cmp al, 'a'
    jb .up_store
    cmp al, 'z'
    ja .up_store
    sub al, 32
.up_store:
    mov [rdx + rcx], al
    inc rcx
    jmp .upcase
.upcased:

    lea rbx, [rel uniname_blob]
    lea r13, [rel uniname_blob_end]
    xor r12, r12                        ; the running codepoint
.entry:
    cmp rbx, r13
    jae .miss
    movzx ecx, byte [rbx]               ; bytes shared with the previous name
    movzx eax, byte [rbx + 1]           ; codepoint delta, or 255
    add rbx, 2
    cmp eax, 255
    jne .cp_delta
    mov eax, [rbx]                      ; the absolute that follows the escape
    add rbx, 4
    mov r12d, eax
    jmp .cp_done
.cp_delta:
    add r12, rax
.cp_done:

    ; Copy this entry's suffix in after the prefix already in the buffer.
    lea rdx, [rbp - UN_BUF]
    add rdx, rcx
.copy:
    mov al, [rbx]
    inc rbx
    mov [rdx], al
    inc rdx
    test al, al
    jnz .copy

    ; The buffer now holds the whole name.  Its length rejects almost every
    ; entry without looking at a byte of it.
    lea rax, [rbp - UN_BUF]
    sub rdx, rax
    dec rdx                             ; rdx = the decoded name's length
    cmp rdx, [rbp - UN_LEN]
    jne .entry
    lea rdi, [rbp - UN_BUF]
    lea rsi, [rbp - UN_QBUF]
    call ap_memcmp                      ; rdx is already the length
    test eax, eax
    jnz .entry
    mov rax, r12
    jmp .done

.miss:
    mov rax, -1
.done:
    pop r13
    pop r12
    pop rbx
    leave
    ret

; Local: uppercase the ASCII letter in al, leaving everything else alone.
.upper:
    cmp al, 'a'
    jb .upper_done
    cmp al, 'z'
    ja .upper_done
    sub al, 32
.upper_done:
    ret
END_FUNC uniname_lookup

ASM_INIT
