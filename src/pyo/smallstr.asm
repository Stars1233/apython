; smallstr.asm - SmallStr inline string operations
; SmallStr: strings <= 14 bytes stored inline in 128-bit fat values
;   payload (qword 0): string bytes 0-7
;   tag (qword 1):     bits 0-7 = TAG_SMALLSTR (0x06),
;                       bits 8-55 = bytes 8-13 (6 bytes, zero-padded),
;                       bits 56-62 = length (0-14),
;                       bit 63 = 1 (SmallStr marker)

%include "macros.inc"
%include "object.inc"
%include "types.inc"

extern str_type
extern type_type
extern ap_memcpy

;; ============================================================================
;; smallstr_from_data(rdi=data_ptr, rsi=len) -> (rax=payload, rdx=tag)
;; Pack <= 14 bytes into SmallStr fat value.
;; Caller MUST ensure len <= 14. Undefined behavior otherwise.
;; ============================================================================
DEF_FUNC smallstr_from_data
    push rbx

    mov rcx, rsi               ; rcx = length

    ; Zero a 16-byte buffer on stack
    sub rsp, 16
    mov qword [rsp], 0
    mov qword [rsp + 8], 0

    ; Copy bytes into stack buffer (bytes 0..len-1)
    test rcx, rcx
    jz .pack

    ; Copy up to 14 bytes from [rdi] to [rsp]
    xor eax, eax
.copy:
    cmp rax, rcx
    jge .pack
    movzx edx, byte [rdi + rax]
    mov [rsp + rax], dl
    inc rax
    jmp .copy

.pack:
    ; Load two qwords: payload = [rsp], raw_tag_bytes = [rsp+8]
    mov rax, [rsp]             ; payload = bytes 0-7
    mov rdx, [rsp + 8]        ; raw bytes at position 8+ (will be shifted)

    ; Shift left 8 to make room for TAG_SMALLSTR in bits 0-7
    ; This moves byte 8 to bits 8-15, byte 9 to bits 16-23, etc.
    ; Byte 14 (bits 48-55) shifts to bits 56-63 and is lost (max=14 bytes)
    shl rdx, 8

    ; Insert TAG_SMALLSTR in bits 0-7
    or dl, TAG_SMALLSTR

    ; Set length in bits 56-62 and SmallStr marker in bit 63
    ; tag = tag_base | (len << 56) | (1 << 63)
    shl rcx, 56                ; length << 56
    or rdx, rcx
    bts rdx, 63               ; set bit 63 (SmallStr marker)

    add rsp, 16
    pop rbx
    leave
    ret
END_FUNC smallstr_from_data

;; ============================================================================
;; smallstr_to_obj(rdi=payload, rsi=tag) -> rax=PyStrObject*
;; Spill SmallStr to heap-allocated PyStrObject. Returns owned reference.
;; ============================================================================
extern str_new_heap

DEF_FUNC smallstr_to_obj
    push rbx
    push r12

    ; Extract length from tag bits 56-62
    mov rcx, rsi
    shr rcx, 56
    and ecx, 0x7F             ; ecx = length
    mov r12, rcx              ; save length

    ; Extract string bytes 8-13 from tag bits 8-55
    mov rax, rsi
    shr rax, 8                ; shift right to remove TAG_SMALLSTR
    mov rdx, 0x0000FFFFFFFFFFFF
    and rax, rdx              ; mask to 48 bits (6 bytes)

    ; Build contiguous buffer: [rsp] = bytes 0-7, [rsp+8] = bytes 8-13
    sub rsp, 16
    mov [rsp], rdi             ; bytes 0-7
    mov [rsp + 8], rax         ; bytes 8-13

    ; Call str_new_heap(data=rsp, len=r12)
    mov rdi, rsp
    mov rsi, r12
    call str_new_heap

    add rsp, 16
    pop r12
    pop rbx
    leave
    ret
END_FUNC smallstr_to_obj

;; ============================================================================
;; smallstr_hash(rdi=payload, rsi=tag) -> rax=hash
;; FNV-1a hash over inline SmallStr bytes. Same algorithm as str_hash.
;; ============================================================================
DEF_FUNC smallstr_hash

    ; Extract length from tag bits 56-62
    mov rcx, rsi
    shr rcx, 56
    and ecx, 0x7F             ; ecx = length

    ; Extract string bytes 8-13 from tag bits 8-55
    mov rax, rsi
    shr rax, 8
    mov rdx, 0x0000FFFFFFFFFFFF
    and rax, rdx

    ; Build contiguous buffer
    sub rsp, 16
    mov [rsp], rdi             ; bytes 0-7
    mov [rsp + 8], rax         ; bytes 8-13

    ; FNV-1a over rcx bytes starting at rsp
    mov rax, 0xcbf29ce484222325     ; FNV offset basis
    mov rdx, 0x100000001b3          ; FNV prime
    mov rsi, rsp               ; data pointer
    ; 4x unrolled FNV-1a loop (same as str_hash)
align 16
.loop4:
    cmp rcx, 4
    jb .tail
    movzx r8d, byte [rsi]
    xor rax, r8
    imul rax, rdx
    movzx r8d, byte [rsi+1]
    xor rax, r8
    imul rax, rdx
    movzx r8d, byte [rsi+2]
    xor rax, r8
    imul rax, rdx
    movzx r8d, byte [rsi+3]
    xor rax, r8
    imul rax, rdx
    add rsi, 4
    sub rcx, 4
    jmp .loop4
.tail:
    test rcx, rcx
    jz .done
    movzx r8d, byte [rsi]
    xor rax, r8
    imul rax, rdx
    inc rsi
    dec rcx
    jmp .tail
.done:
    ; Ensure hash is never -1
    cmp rax, -1
    jne .ret
    mov rax, -2
.ret:
    add rsp, 16
    leave
    ret
END_FUNC smallstr_hash

;; ============================================================================
;; smallstr_len(rdi=payload, rsi=tag) -> rax=length
;; Extract length from SmallStr tag.
;; ============================================================================
DEF_FUNC_BARE smallstr_len
    mov rax, rsi
    shr rax, 56
    and eax, 0x7F
    ret
END_FUNC smallstr_len

;; ============================================================================
;; smallstr_to_buf(rdi=payload, rsi=tag, rdx=buf_ptr)
;; Write SmallStr bytes to buffer at rdx. Writes len bytes (no null terminator).
;; Returns length in rax.
;; ============================================================================
DEF_FUNC_BARE smallstr_to_buf
    ; Extract length
    mov rax, rsi
    mov rcx, rsi
    shr rcx, 56
    and ecx, 0x7F             ; ecx = length

    ; Extract string bytes 8-13 from tag bits 8-55
    shr rax, 8
    mov r8, 0x0000FFFFFFFFFFFF
    and rax, r8

    ; Write payload (bytes 0-7) and extracted bytes (bytes 8-13) to buffer
    mov [rdx], rdi
    mov [rdx + 8], rax

    mov eax, ecx              ; return length
    ret
END_FUNC smallstr_to_buf
