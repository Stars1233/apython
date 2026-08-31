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
extern uniname_cp
extern uniname_cjk_ranges
extern uniname_cjk_ranges_end

section .rodata
uniname_cjk_prefix: db "CJK UNIFIED IDEOGRAPH-", 0
UNINAME_CJK_PREFIX_LEN equ 22

section .text

;; ============================================================================
;; uniname_lookup(const char *name, int64_t len) -> rax = codepoint, or -1
;; ============================================================================
UN_NAME  equ 8
UN_LEN   equ 16
UN_FRAME equ 24          ; + 3 pushes = 48
global uniname_lookup
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
    cmp r8, 0x10FFFF
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
.scan_table:
    lea rbx, [rel uniname_blob]         ; the current entry
    lea r13, [rel uniname_blob_end]
    xor r12, r12                        ; its index
.entry:
    cmp rbx, r13
    jae .miss
    mov rdi, [rbp - UN_NAME]
    mov rsi, rbx
    xor ecx, ecx
.cmp_loop:
    cmp rcx, [rbp - UN_LEN]
    jae .cmp_name_end
    movzx eax, byte [rsi + rcx]
    test al, al
    jz .next                            ; the entry ended first
    movzx edx, byte [rdi + rcx]
    push rax
    mov eax, edx
    call .upper
    mov edx, eax
    pop rax
    cmp al, dl
    jne .next
    inc rcx
    jmp .cmp_loop
.cmp_name_end:
    ; The query ran out; it matches only if the entry did too.
    cmp byte [rsi + rcx], 0
    jne .next
    lea rax, [rel uniname_cp]
    mov eax, [rax + r12*4]
    jmp .done

.next:
    ; Step over this entry's NUL.
    inc r12
.skip:
    cmp rbx, r13
    jae .miss
    cmp byte [rbx], 0
    je .skipped
    inc rbx
    jmp .skip
.skipped:
    inc rbx
    jmp .entry

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
