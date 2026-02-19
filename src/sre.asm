; sre.asm - SRE match engine for apython
; Direct u32 interpreter implementing CPython's regex bytecode
;
; Main entry: sre_match(SRE_State*, u32* pattern) -> 0/1/-1
;             sre_search(SRE_State*) -> 0/1/-1
;
; Match engine registers (within sre_match):
;   rbx = u32 *pc (pattern code pointer)
;   r12 = SRE_State*
;   r13 = char index (position in string)
;   r14 = scratch / saved across calls
;   r15 = SRE_RepeatContext* chain head

%include "macros.inc"
%include "object.inc"
%include "types.inc"
%include "sre.inc"

extern ap_malloc
extern ap_free
extern ap_realloc
extern ap_memcpy
extern ap_memcmp
extern obj_incref
extern obj_decref
extern raise_exception
extern exc_RuntimeError_type

; ============================================================================
; sre_getchar(SRE_State* state, i64 index) -> u32 codepoint
; Get character at given index. ASCII fast path or codepoint_buf lookup.
; ============================================================================
DEF_FUNC_BARE sre_getchar
    ; rdi = state, rsi = index
    mov rax, [rdi + SRE_State.codepoint_buf]
    test rax, rax
    jnz .unicode
    ; ASCII: byte at str_begin[index]
    mov rax, [rdi + SRE_State.str_begin]
    movzx eax, byte [rax + rsi]
    ret
.unicode:
    ; Unicode: u32 at codepoint_buf[index]
    mov eax, [rax + rsi*4]
    ret
END_FUNC sre_getchar

; ============================================================================
; sre_category(u32 category_code, u32 ch) -> 0/1
; Check if character matches a category.
; rdi = category code, esi = character codepoint
; ============================================================================
DEF_FUNC sre_category
    cmp edi, SRE_CATEGORY_UNI_NOT_LINEBREAK
    ja .cat_false
    lea rax, [rel .cat_table]
    jmp [rax + rdi*8]

.cat_table:
    dq .cat_digit               ; 0 DIGIT
    dq .cat_not_digit           ; 1 NOT_DIGIT
    dq .cat_space               ; 2 SPACE
    dq .cat_not_space           ; 3 NOT_SPACE
    dq .cat_word                ; 4 WORD
    dq .cat_not_word            ; 5 NOT_WORD
    dq .cat_linebreak           ; 6 LINEBREAK
    dq .cat_not_linebreak       ; 7 NOT_LINEBREAK
    dq .cat_word                ; 8 LOC_WORD (same as WORD for us)
    dq .cat_not_word            ; 9 LOC_NOT_WORD
    dq .cat_uni_digit           ; 10 UNI_DIGIT
    dq .cat_uni_not_digit       ; 11 UNI_NOT_DIGIT
    dq .cat_uni_space           ; 12 UNI_SPACE
    dq .cat_uni_not_space       ; 13 UNI_NOT_SPACE
    dq .cat_uni_word            ; 14 UNI_WORD
    dq .cat_uni_not_word        ; 15 UNI_NOT_WORD
    dq .cat_uni_linebreak       ; 16 UNI_LINEBREAK
    dq .cat_uni_not_linebreak   ; 17 UNI_NOT_LINEBREAK

.cat_digit:
    cmp esi, '0'
    jb .cat_false
    cmp esi, '9'
    ja .cat_false
    jmp .cat_true

.cat_not_digit:
    cmp esi, '0'
    jb .cat_true
    cmp esi, '9'
    ja .cat_true
    jmp .cat_false

.cat_space:
    ; \t \n \v \f \r space
    cmp esi, ' '
    je .cat_true
    cmp esi, 0x09             ; \t
    jb .cat_false
    cmp esi, 0x0D             ; \r
    jbe .cat_true
    jmp .cat_false

.cat_not_space:
    cmp esi, ' '
    je .cat_false
    cmp esi, 0x09
    jb .cat_true
    cmp esi, 0x0D
    jbe .cat_false
    jmp .cat_true

.cat_word:
    ; [a-zA-Z0-9_]
    cmp esi, '_'
    je .cat_true
    cmp esi, '0'
    jb .cat_false
    cmp esi, '9'
    jbe .cat_true
    cmp esi, 'A'
    jb .cat_false
    cmp esi, 'Z'
    jbe .cat_true
    cmp esi, 'a'
    jb .cat_false
    cmp esi, 'z'
    jbe .cat_true
    jmp .cat_false

.cat_not_word:
    cmp esi, '_'
    je .cat_false
    cmp esi, '0'
    jb .cat_true
    cmp esi, '9'
    jbe .cat_false
    cmp esi, 'A'
    jb .cat_true
    cmp esi, 'Z'
    jbe .cat_false
    cmp esi, 'a'
    jb .cat_true
    cmp esi, 'z'
    jbe .cat_false
    jmp .cat_true

.cat_linebreak:
    cmp esi, 0x0A              ; \n
    je .cat_true
    cmp esi, 0x0D              ; \r
    je .cat_true
    jmp .cat_false

.cat_not_linebreak:
    cmp esi, 0x0A
    je .cat_false
    cmp esi, 0x0D
    je .cat_false
    jmp .cat_true

; Unicode categories — for ASCII range, same as ASCII versions
; For codepoints > 127, do basic checks
.cat_uni_digit:
    cmp esi, '0'
    jb .cat_false
    cmp esi, '9'
    jbe .cat_true
    jmp .cat_false              ; TODO: full Unicode digit ranges

.cat_uni_not_digit:
    cmp esi, '0'
    jb .cat_true
    cmp esi, '9'
    jbe .cat_false
    jmp .cat_true

.cat_uni_space:
    cmp esi, ' '
    je .cat_true
    cmp esi, 0x09
    jb .cat_false
    cmp esi, 0x0D
    jbe .cat_true
    ; Unicode whitespace: \u0085, \u00A0, \u2000-\u200A, etc.
    cmp esi, 0x85
    je .cat_true
    cmp esi, 0xA0
    je .cat_true
    cmp esi, 0x1680
    je .cat_true
    cmp esi, 0x2000
    jb .cat_false
    cmp esi, 0x200A
    jbe .cat_true
    cmp esi, 0x2028
    je .cat_true
    cmp esi, 0x2029
    je .cat_true
    cmp esi, 0x202F
    je .cat_true
    cmp esi, 0x205F
    je .cat_true
    cmp esi, 0x3000
    je .cat_true
    jmp .cat_false

.cat_uni_not_space:
    ; Invert of uni_space
    cmp esi, ' '
    je .cat_false
    cmp esi, 0x09
    jb .cat_true
    cmp esi, 0x0D
    jbe .cat_false
    cmp esi, 0x85
    je .cat_false
    cmp esi, 0xA0
    je .cat_false
    cmp esi, 0x1680
    je .cat_false
    cmp esi, 0x2000
    jb .cat_true
    cmp esi, 0x200A
    jbe .cat_false
    cmp esi, 0x2028
    je .cat_false
    cmp esi, 0x2029
    je .cat_false
    cmp esi, 0x202F
    je .cat_false
    cmp esi, 0x205F
    je .cat_false
    cmp esi, 0x3000
    je .cat_false
    jmp .cat_true

.cat_uni_word:
    ; ASCII word chars + basic Unicode letter ranges
    cmp esi, '_'
    je .cat_true
    cmp esi, '0'
    jb .cat_false
    cmp esi, '9'
    jbe .cat_true
    cmp esi, 'A'
    jb .cat_false
    cmp esi, 'Z'
    jbe .cat_true
    cmp esi, 'a'
    jb .cat_false
    cmp esi, 'z'
    jbe .cat_true
    ; Extended Latin, Cyrillic, etc. — simplified: consider >= 0x80 as word
    cmp esi, 0x80
    jae .cat_true
    jmp .cat_false

.cat_uni_not_word:
    cmp esi, '_'
    je .cat_false
    cmp esi, '0'
    jb .cat_true
    cmp esi, '9'
    jbe .cat_false
    cmp esi, 'A'
    jb .cat_true
    cmp esi, 'Z'
    jbe .cat_false
    cmp esi, 'a'
    jb .cat_true
    cmp esi, 'z'
    jbe .cat_false
    cmp esi, 0x80
    jae .cat_false
    jmp .cat_true

.cat_uni_linebreak:
    cmp esi, 0x0A
    je .cat_true
    cmp esi, 0x0D
    je .cat_true
    cmp esi, 0x0B              ; \v
    je .cat_true
    cmp esi, 0x0C              ; \f
    je .cat_true
    cmp esi, 0x85              ; NEL
    je .cat_true
    cmp esi, 0x2028            ; LINE SEPARATOR
    je .cat_true
    cmp esi, 0x2029            ; PARAGRAPH SEPARATOR
    je .cat_true
    jmp .cat_false

.cat_uni_not_linebreak:
    cmp esi, 0x0A
    je .cat_false
    cmp esi, 0x0D
    je .cat_false
    cmp esi, 0x0B
    je .cat_false
    cmp esi, 0x0C
    je .cat_false
    cmp esi, 0x85
    je .cat_false
    cmp esi, 0x2028
    je .cat_false
    cmp esi, 0x2029
    je .cat_false
    jmp .cat_true

.cat_true:
    mov eax, 1
    leave
    ret
.cat_false:
    xor eax, eax
    leave
    ret
END_FUNC sre_category

; ============================================================================
; sre_charset(u32* set, u32 ch) -> 0/1
; Check if ch is in a character set (IN opcode's set data).
; Set format: sequence of (opcode, args...) terminated by SRE_OP_FAILURE.
; ============================================================================
SM_CH     equ 8
SM_SET    equ 16
SM_FRAME  equ 16

DEF_FUNC sre_charset, SM_FRAME
    push rbx
    push r12

    mov rbx, rdi              ; rbx = u32* set pointer
    mov r12d, esi              ; r12d = character

.cs_loop:
    mov eax, [rbx]             ; opcode
    add rbx, 4

    cmp eax, SRE_OP_FAILURE
    je .cs_false

    cmp eax, SRE_OP_LITERAL
    je .cs_literal
    cmp eax, SRE_OP_RANGE
    je .cs_range
    cmp eax, SRE_OP_RANGE_UNI_IGNORE
    je .cs_range_uni_ignore
    cmp eax, SRE_OP_NEGATE
    je .cs_negate
    cmp eax, SRE_OP_CATEGORY
    je .cs_category
    cmp eax, SRE_OP_CHARSET
    je .cs_charset_block
    cmp eax, SRE_OP_BIGCHARSET
    je .cs_bigcharset
    ; Unknown opcode — skip (shouldn't happen)
    jmp .cs_false

.cs_literal:
    mov eax, [rbx]             ; literal value
    add rbx, 4
    cmp eax, r12d
    je .cs_true
    jmp .cs_loop

.cs_range:
    mov eax, [rbx]             ; low
    mov ecx, [rbx + 4]        ; high
    add rbx, 8
    cmp r12d, eax
    jb .cs_loop
    cmp r12d, ecx
    jbe .cs_true
    jmp .cs_loop

.cs_range_uni_ignore:
    ; Same as range but case-insensitive
    mov eax, [rbx]
    mov ecx, [rbx + 4]
    add rbx, 8
    ; Check original char
    cmp r12d, eax
    jb .cs_rui_try_lower
    cmp r12d, ecx
    jbe .cs_true
.cs_rui_try_lower:
    ; Try lowercased char
    mov edi, r12d
    call sre_ascii_tolower
    cmp eax, [rbx - 8]        ; low
    jb .cs_loop
    cmp eax, [rbx - 4]        ; high
    jbe .cs_true
    jmp .cs_loop

.cs_negate:
    ; Negate is handled by caller; in the set, it just flips result
    ; CPython handles this at the IN opcode level, not here
    ; We track it via a flag; for now, just continue
    jmp .cs_loop

.cs_category:
    mov edi, [rbx]             ; category code
    add rbx, 4
    mov esi, r12d              ; character
    call sre_category
    test eax, eax
    jnz .cs_true
    jmp .cs_loop

.cs_charset_block:
    ; 256-bit bitmap: 8 x u32 words
    ; Check if ch < 256
    cmp r12d, 256
    jae .cs_charset_skip
    ; Bit test: word = ch >> 5, bit = ch & 31
    mov eax, r12d
    shr eax, 5
    mov ecx, [rbx + rax*4]    ; get the u32 word
    mov eax, r12d
    and eax, 31
    bt ecx, eax
    jc .cs_true
.cs_charset_skip:
    add rbx, 32               ; skip 8 u32 words (256 bits)
    jmp .cs_loop

.cs_bigcharset:
    ; BIGCHARSET: u32 count, then count * 256 bytes of block data
    ; then 256 u8 block-index entries
    ; For simplicity, handle the common case
    mov eax, [rbx]             ; number of blocks
    add rbx, 4
    ; block_index is at rbx + eax*32
    ; For ch: block = block_index[ch >> 8], then check bitmap
    mov ecx, r12d
    shr ecx, 8                 ; high byte
    cmp ecx, 256
    jae .cs_big_skip
    lea rdx, [rbx]            ; blocks start
    imul rax, rax, 32         ; total block bytes (eax blocks * 32 bytes each)
    add rax, rdx              ; rax = blocks_start + total_block_bytes
    movzx ecx, byte [rax + rcx]  ; block_index[ch>>8]
    ; Now get bit from block[block_index]: 256-bit bitmap
    imul ecx, ecx, 32
    lea rdx, [rbx + rcx]      ; block data
    mov eax, r12d
    and eax, 0xFF              ; low byte of ch
    mov ecx, eax
    shr ecx, 5
    mov eax, [rdx + rcx*4]
    mov ecx, r12d
    and ecx, 31
    bt eax, ecx
    jc .cs_true
.cs_big_skip:
    ; Skip past bigcharset data: blocks + index
    mov eax, [rbx - 4]        ; block count
    imul eax, eax, 32
    add eax, 256               ; + index table
    lea rbx, [rbx + rax]
    jmp .cs_loop

.cs_true:
    mov eax, 1
    pop r12
    pop rbx
    leave
    ret
.cs_false:
    xor eax, eax
    pop r12
    pop rbx
    leave
    ret
END_FUNC sre_charset

; ============================================================================
; sre_at(SRE_State* state, i64 pos, u32 at_code) -> 0/1
; Check position assertion (^, $, \b, etc.)
; ============================================================================
DEF_FUNC sre_at
    ; rdi = state, rsi = pos, edx = at_code
    mov r8, rdi                ; r8 = state
    mov r9, rsi                ; r9 = pos
    mov ecx, edx               ; ecx = at_code

    cmp ecx, SRE_AT_BEGINNING
    je .at_beginning
    cmp ecx, SRE_AT_BEGINNING_STRING
    je .at_beginning_string
    cmp ecx, SRE_AT_BEGINNING_LINE
    je .at_beginning_line
    cmp ecx, SRE_AT_END
    je .at_end
    cmp ecx, SRE_AT_END_LINE
    je .at_end_line
    cmp ecx, SRE_AT_END_STRING
    je .at_end_string
    cmp ecx, SRE_AT_BOUNDARY
    je .at_word_boundary
    cmp ecx, SRE_AT_NON_BOUNDARY
    je .at_non_boundary
    cmp ecx, SRE_AT_UNI_BOUNDARY
    je .at_word_boundary        ; same as word boundary for now
    cmp ecx, SRE_AT_UNI_NON_BOUNDARY
    je .at_non_boundary
    cmp ecx, SRE_AT_LOC_BOUNDARY
    je .at_word_boundary
    cmp ecx, SRE_AT_LOC_NON_BOUNDARY
    je .at_non_boundary
    ; Unknown
    xor eax, eax
    leave
    ret

.at_beginning:
.at_beginning_string:
    ; pos == 0
    test r9, r9
    jz .at_true
    jmp .at_false

.at_beginning_line:
    ; pos == 0 or char[pos-1] == '\n'
    test r9, r9
    jz .at_true
    ; Get char at pos-1
    lea rsi, [r9 - 1]
    mov rdi, r8
    call sre_getchar
    cmp eax, 0x0A
    je .at_true
    jmp .at_false

.at_end:
    ; pos == len or (pos == len-1 and char[pos] == '\n')
    mov rax, [r8 + SRE_State.codepoint_buf]
    test rax, rax
    jnz .at_end_unicode
    ; ASCII
    mov rax, [r8 + SRE_State.str_end]
    sub rax, [r8 + SRE_State.str_begin]
    jmp .at_end_check
.at_end_unicode:
    mov rax, [r8 + SRE_State.codepoint_len]
.at_end_check:
    cmp r9, rax
    je .at_true
    lea rcx, [rax - 1]
    cmp r9, rcx
    jne .at_false
    ; Check if char[pos] == '\n'
    mov rdi, r8
    mov rsi, r9
    call sre_getchar
    cmp eax, 0x0A
    je .at_true
    jmp .at_false

.at_end_line:
    ; pos == len or char[pos] == '\n'
    mov rax, [r8 + SRE_State.codepoint_buf]
    test rax, rax
    jnz .at_endline_unicode
    mov rax, [r8 + SRE_State.str_end]
    sub rax, [r8 + SRE_State.str_begin]
    jmp .at_endline_check
.at_endline_unicode:
    mov rax, [r8 + SRE_State.codepoint_len]
.at_endline_check:
    cmp r9, rax
    je .at_true
    mov rdi, r8
    mov rsi, r9
    call sre_getchar
    cmp eax, 0x0A
    je .at_true
    jmp .at_false

.at_end_string:
    ; pos == len
    mov rax, [r8 + SRE_State.codepoint_buf]
    test rax, rax
    jnz .at_endstr_unicode
    mov rax, [r8 + SRE_State.str_end]
    sub rax, [r8 + SRE_State.str_begin]
    jmp .at_endstr_check
.at_endstr_unicode:
    mov rax, [r8 + SRE_State.codepoint_len]
.at_endstr_check:
    cmp r9, rax
    je .at_true
    jmp .at_false

.at_word_boundary:
    ; \b: word status differs at pos-1 and pos
    push r8
    push r9
    ; Get "is word" for pos-1
    xor r10d, r10d             ; left_is_word = 0
    test r9, r9
    jz .wb_check_right
    lea rsi, [r9 - 1]
    mov rdi, r8
    call sre_getchar
    mov edi, SRE_CATEGORY_WORD
    mov esi, eax
    call sre_category
    mov r10d, eax
.wb_check_right:
    pop r9
    pop r8
    ; Get "is word" for pos
    xor r11d, r11d             ; right_is_word = 0
    ; Get string length
    mov rax, [r8 + SRE_State.codepoint_buf]
    test rax, rax
    jnz .wb_right_unicode
    mov rax, [r8 + SRE_State.str_end]
    sub rax, [r8 + SRE_State.str_begin]
    jmp .wb_right_check
.wb_right_unicode:
    mov rax, [r8 + SRE_State.codepoint_len]
.wb_right_check:
    cmp r9, rax
    jge .wb_compare
    push r10
    mov rdi, r8
    mov rsi, r9
    call sre_getchar
    mov edi, SRE_CATEGORY_WORD
    mov esi, eax
    call sre_category
    mov r11d, eax
    pop r10
.wb_compare:
    cmp r10d, r11d
    jne .at_true
    jmp .at_false

.at_non_boundary:
    ; \B: word status same at pos-1 and pos
    push r8
    push r9
    xor r10d, r10d
    test r9, r9
    jz .nb_check_right
    lea rsi, [r9 - 1]
    mov rdi, r8
    call sre_getchar
    mov edi, SRE_CATEGORY_WORD
    mov esi, eax
    call sre_category
    mov r10d, eax
.nb_check_right:
    pop r9
    pop r8
    xor r11d, r11d
    mov rax, [r8 + SRE_State.codepoint_buf]
    test rax, rax
    jnz .nb_right_unicode
    mov rax, [r8 + SRE_State.str_end]
    sub rax, [r8 + SRE_State.str_begin]
    jmp .nb_right_check
.nb_right_unicode:
    mov rax, [r8 + SRE_State.codepoint_len]
.nb_right_check:
    cmp r9, rax
    jge .nb_compare
    push r10
    mov rdi, r8
    mov rsi, r9
    call sre_getchar
    mov edi, SRE_CATEGORY_WORD
    mov esi, eax
    call sre_category
    mov r11d, eax
    pop r10
.nb_compare:
    cmp r10d, r11d
    je .at_true
    jmp .at_false

.at_true:
    mov eax, 1
    leave
    ret
.at_false:
    xor eax, eax
    leave
    ret
END_FUNC sre_at

; ============================================================================
; sre_ascii_tolower(u32 ch) -> u32
; ============================================================================
DEF_FUNC_BARE sre_ascii_tolower
    mov eax, edi
    cmp eax, 'A'
    jb .no_lower
    cmp eax, 'Z'
    ja .no_lower
    add eax, 32
.no_lower:
    ret
END_FUNC sre_ascii_tolower

; ============================================================================
; sre_unicode_tolower(u32 ch) -> u32
; Basic Unicode case folding (ASCII + common Latin)
; ============================================================================
DEF_FUNC_BARE sre_unicode_tolower
    mov eax, edi
    cmp eax, 'A'
    jb .no_lower
    cmp eax, 'Z'
    jbe .do_lower
    ; Latin Extended: 0xC0-0xD6, 0xD8-0xDE
    cmp eax, 0xC0
    jb .no_lower
    cmp eax, 0xD6
    jbe .do_lower
    cmp eax, 0xD8
    jb .no_lower
    cmp eax, 0xDE
    jbe .do_lower
    jmp .no_lower
.do_lower:
    add eax, 32
.no_lower:
    ret
END_FUNC sre_unicode_tolower

; ============================================================================
; sre_state_init(SRE_State* state, SRE_PatternObject* pattern,
;                PyStrObject* string, i64 pos, i64 endpos)
; Initialize match state for a string.
; ============================================================================
DEF_FUNC sre_state_init, 48
    push rbx
    push r12
    push r13
    push r14

    mov rbx, rdi               ; state
    mov r12, rsi               ; pattern
    mov r13, rdx               ; string
    mov r14, rcx               ; pos
    mov [rbp - 8], r8          ; save endpos

    ; Zero out the state
    mov rdi, rbx
    xor eax, eax
    mov ecx, SRE_State_size / 8
.zero_loop:
    mov [rdi], rax
    add rdi, 8
    dec ecx
    jnz .zero_loop

    ; Set basic fields
    mov [rbx + SRE_State.pattern], r12
    mov [rbx + SRE_State.string_obj], r13

    ; Get string data
    lea rax, [r13 + PyStrObject.data]
    mov [rbx + SRE_State.str_begin], rax
    mov rcx, [r13 + PyStrObject.ob_size]
    lea rdx, [rax + rcx]
    mov [rbx + SRE_State.str_end], rdx

    ; Set flags from pattern
    mov eax, [r12 + SRE_PatternObject.flags]
    mov [rbx + SRE_State.flags], eax
    mov dword [rbx + SRE_State.match_all], 0
    mov dword [rbx + SRE_State.is_bytes], 0

    ; Determine if ASCII or needs Unicode codepoint decode
    ; Scan string bytes — if all < 0x80, ASCII fast path
    mov rdi, [rbx + SRE_State.str_begin]
    mov rcx, [r13 + PyStrObject.ob_size]
    mov dword [rbx + SRE_State.charsize], 1  ; assume ASCII
    mov qword [rbx + SRE_State.codepoint_buf], 0
    test rcx, rcx
    jz .ascii_done

    xor r8d, r8d               ; index
.scan_ascii:
    cmp r8, rcx
    jge .ascii_done
    movzx eax, byte [rdi + r8]
    cmp al, 0x80
    jae .need_unicode
    inc r8
    jmp .scan_ascii

.ascii_done:
    ; Pure ASCII: str_begin/str_end are byte ptrs, charsize=1
    ; String length in chars = byte length
    mov rax, [r13 + PyStrObject.ob_size]
    mov [rbx + SRE_State.codepoint_len], rax

    ; Clamp pos and endpos
    cmp r14, 0
    jge .pos_ok
    xor r14d, r14d
.pos_ok:
    cmp r14, rax
    jle .pos_ok2
    mov r14, rax
.pos_ok2:
    mov [rbx + SRE_State.str_pos], r14
    mov [rbx + SRE_State.str_start], r14

    jmp .init_marks

.need_unicode:
    mov dword [rbx + SRE_State.charsize], 4
    ; UTF-8 decode to u32 codepoint array
    mov rdi, [rbx + SRE_State.str_begin]
    mov rsi, [r13 + PyStrObject.ob_size]
    mov [rbp - 8], rbx         ; save state ptr
    mov [rbp - 16], rsi        ; save byte length

    ; Allocate codepoint buffer (worst case: byte_len codepoints)
    lea rdi, [rsi*4]           ; u32 per codepoint
    call ap_malloc
    mov rbx, [rbp - 8]        ; restore state
    mov [rbx + SRE_State.codepoint_buf], rax

    ; Decode UTF-8
    mov rdi, [rbx + SRE_State.str_begin]
    mov rsi, [rbp - 16]       ; byte length
    mov rdx, rax               ; output buffer
    xor ecx, ecx               ; byte index
    xor r8d, r8d               ; codepoint index

.utf8_decode:
    cmp rcx, rsi
    jge .utf8_done
    movzx eax, byte [rdi + rcx]
    cmp al, 0x80
    jb .utf8_1byte
    cmp al, 0xE0
    jb .utf8_2byte
    cmp al, 0xF0
    jb .utf8_3byte
    jmp .utf8_4byte

.utf8_1byte:
    mov [rdx + r8*4], eax
    inc rcx
    inc r8
    jmp .utf8_decode

.utf8_2byte:
    and eax, 0x1F
    shl eax, 6
    movzx r9d, byte [rdi + rcx + 1]
    and r9d, 0x3F
    or eax, r9d
    mov [rdx + r8*4], eax
    add rcx, 2
    inc r8
    jmp .utf8_decode

.utf8_3byte:
    and eax, 0x0F
    shl eax, 12
    movzx r9d, byte [rdi + rcx + 1]
    and r9d, 0x3F
    shl r9d, 6
    or eax, r9d
    movzx r9d, byte [rdi + rcx + 2]
    and r9d, 0x3F
    or eax, r9d
    mov [rdx + r8*4], eax
    add rcx, 3
    inc r8
    jmp .utf8_decode

.utf8_4byte:
    and eax, 0x07
    shl eax, 18
    movzx r9d, byte [rdi + rcx + 1]
    and r9d, 0x3F
    shl r9d, 12
    or eax, r9d
    movzx r9d, byte [rdi + rcx + 2]
    and r9d, 0x3F
    shl r9d, 6
    or eax, r9d
    movzx r9d, byte [rdi + rcx + 3]
    and r9d, 0x3F
    or eax, r9d
    mov [rdx + r8*4], eax
    add rcx, 4
    inc r8
    jmp .utf8_decode

.utf8_done:
    mov [rbx + SRE_State.codepoint_len], r8

    ; Clamp pos and endpos for Unicode
    cmp r14, 0
    jge .upos_ok
    xor r14d, r14d
.upos_ok:
    cmp r14, r8
    jle .upos_ok2
    mov r14, r8
.upos_ok2:
    mov [rbx + SRE_State.str_pos], r14
    mov [rbx + SRE_State.str_start], r14

.init_marks:
    ; Allocate marks array
    mov edi, SRE_MARKS_INITIAL * 8
    call ap_malloc
    mov [rbx + SRE_State.marks], rax
    mov qword [rbx + SRE_State.marks_count], SRE_MARKS_INITIAL
    mov qword [rbx + SRE_State.marks_size], 0
    mov qword [rbx + SRE_State.lastmark], -1
    mov qword [rbx + SRE_State.lastindex], -1
    mov qword [rbx + SRE_State.repeat_ctx], 0

    pop r14
    pop r13
    pop r12
    pop rbx
    leave
    ret
END_FUNC sre_state_init

; ============================================================================
; sre_state_fini(SRE_State* state)
; Clean up match state.
; ============================================================================
DEF_FUNC sre_state_fini
    push rbx
    mov rbx, rdi

    ; Free marks array
    mov rdi, [rbx + SRE_State.marks]
    test rdi, rdi
    jz .no_marks
    call ap_free
.no_marks:

    ; Free codepoint buffer
    mov rdi, [rbx + SRE_State.codepoint_buf]
    test rdi, rdi
    jz .no_cp
    call ap_free
.no_cp:

    pop rbx
    leave
    ret
END_FUNC sre_state_fini

; ============================================================================
; sre_state_set_mark(SRE_State* state, i64 mark_id, i64 pos)
; Set a mark (group boundary) in the state.
; ============================================================================
DEF_FUNC sre_state_set_mark
    ; rdi = state, rsi = mark_id, rdx = pos
    push rbx
    mov rbx, rdi

    ; Ensure marks array is large enough
    mov rcx, [rbx + SRE_State.marks_count]
    cmp rsi, rcx
    jb .mark_fits

    ; Grow marks array
    lea rdi, [rsi + 1]
    shl rdi, 1                 ; double requested size
    mov [rbx + SRE_State.marks_count], rdi
    shl rdi, 3                 ; * 8 bytes per mark
    push rsi
    push rdx
    mov rsi, rdi               ; new size
    mov rdi, [rbx + SRE_State.marks]
    call ap_realloc
    pop rdx
    pop rsi
    mov [rbx + SRE_State.marks], rax

.mark_fits:
    ; Set mark
    mov rax, [rbx + SRE_State.marks]
    mov [rax + rsi*8], rdx

    ; Update lastmark
    cmp rsi, [rbx + SRE_State.lastmark]
    jle .no_update_lastmark
    mov [rbx + SRE_State.lastmark], rsi
.no_update_lastmark:

    ; Update marks_size
    lea rcx, [rsi + 1]
    cmp rcx, [rbx + SRE_State.marks_size]
    jle .no_update_size
    mov [rbx + SRE_State.marks_size], rcx
.no_update_size:

    ; Update lastindex (group = mark_id / 2, but only for mark_id >= 2)
    cmp rsi, 2
    jb .done
    mov rax, rsi
    shr rax, 1                 ; group index
    mov [rbx + SRE_State.lastindex], rax

.done:
    pop rbx
    leave
    ret
END_FUNC sre_state_set_mark

; ============================================================================
; sre_state_get_mark(SRE_State* state, i64 mark_id) -> i64 pos (-1 if unset)
; ============================================================================
DEF_FUNC_BARE sre_state_get_mark
    ; rdi = state, rsi = mark_id
    cmp rsi, [rdi + SRE_State.marks_size]
    jge .unset
    mov rax, [rdi + SRE_State.marks]
    mov rax, [rax + rsi*8]
    ret
.unset:
    mov rax, -1
    ret
END_FUNC sre_state_get_mark

; ============================================================================
; sre_string_len(SRE_State* state) -> i64
; Get string length in characters.
; ============================================================================
DEF_FUNC_BARE sre_string_len
    mov rax, [rdi + SRE_State.codepoint_buf]
    test rax, rax
    jnz .unicode_len
    ; ASCII: byte length
    mov rax, [rdi + SRE_State.str_end]
    sub rax, [rdi + SRE_State.str_begin]
    ret
.unicode_len:
    mov rax, [rdi + SRE_State.codepoint_len]
    ret
END_FUNC sre_string_len

; ============================================================================
; sre_match(SRE_State* state, u32* pattern) -> 0/1
; Core recursive match engine.
;
; Frame layout constants
; ============================================================================
SM_STATE     equ 8
SM_PATTERN   equ 16
SM_SAVE_POS  equ 24
SM_SAVE_MARKS equ 32        ; ptr to saved marks snapshot
SM_SAVE_MARKS_SZ equ 40
SM_SAVE_LASTMARK equ 48
SM_SAVE_LASTINDEX equ 56
SM_RPT_CTX   equ 64          ; repeat context (on stack)
SM_RPT_END   equ 96          ; end of repeat context (32 bytes)
SM_MFRAME    equ 96

DEF_FUNC sre_match, SM_MFRAME
    push rbx
    push r12
    push r13
    push r14
    push r15

    mov [rbp - SM_STATE], rdi
    mov [rbp - SM_PATTERN], rsi
    mov r12, rdi               ; r12 = state
    mov rbx, rsi               ; rbx = pc (pattern pointer)
    mov r13, [rdi + SRE_State.str_pos]  ; r13 = current pos
    mov r15, [rdi + SRE_State.repeat_ctx]  ; r15 = repeat ctx

.dispatch:
    mov eax, [rbx]             ; opcode
    add rbx, 4                 ; advance past opcode

    ; Jump table dispatch
    cmp eax, SRE_OP_COUNT
    jae .op_failure

    lea rcx, [rel .op_table]
    jmp [rcx + rax*8]

    align 8
.op_table:
    dq .op_failure              ; 0  FAILURE
    dq .op_success              ; 1  SUCCESS
    dq .op_any                  ; 2  ANY
    dq .op_any_all              ; 3  ANY_ALL
    dq .op_assert               ; 4  ASSERT
    dq .op_assert_not           ; 5  ASSERT_NOT
    dq .op_at                   ; 6  AT
    dq .op_branch               ; 7  BRANCH
    dq .op_category             ; 8  CATEGORY
    dq .op_charset              ; 9  CHARSET
    dq .op_bigcharset           ; 10 BIGCHARSET
    dq .op_groupref             ; 11 GROUPREF
    dq .op_groupref_exists      ; 12 GROUPREF_EXISTS
    dq .op_in                   ; 13 IN
    dq .op_info                 ; 14 INFO
    dq .op_jump                 ; 15 JUMP
    dq .op_literal              ; 16 LITERAL
    dq .op_mark                 ; 17 MARK
    dq .op_max_until            ; 18 MAX_UNTIL
    dq .op_min_until            ; 19 MIN_UNTIL
    dq .op_not_literal           ; 20 NOT_LITERAL
    dq .op_negate               ; 21 NEGATE (only inside IN sets)
    dq .op_range                ; 22 RANGE (only inside IN sets)
    dq .op_repeat               ; 23 REPEAT
    dq .op_repeat_one           ; 24 REPEAT_ONE
    dq .op_subpattern           ; 25 SUBPATTERN
    dq .op_min_repeat_one       ; 26 MIN_REPEAT_ONE
    dq .op_atomic_group         ; 27 ATOMIC_GROUP
    dq .op_possessive_repeat    ; 28 POSSESSIVE_REPEAT
    dq .op_possessive_repeat_one ; 29 POSSESSIVE_REPEAT_ONE
    dq .op_groupref_ignore      ; 30 GROUPREF_IGNORE
    dq .op_in_ignore            ; 31 IN_IGNORE
    dq .op_literal_ignore       ; 32 LITERAL_IGNORE
    dq .op_not_literal_ignore   ; 33 NOT_LITERAL_IGNORE
    dq .op_groupref_ignore      ; 34 GROUPREF_LOC_IGNORE (same as ignore)
    dq .op_in_ignore            ; 35 IN_LOC_IGNORE
    dq .op_literal_ignore       ; 36 LITERAL_LOC_IGNORE
    dq .op_not_literal_ignore   ; 37 NOT_LITERAL_LOC_IGNORE
    dq .op_groupref_ignore      ; 38 GROUPREF_UNI_IGNORE
    dq .op_in_ignore            ; 39 IN_UNI_IGNORE
    dq .op_literal_uni_ignore   ; 40 LITERAL_UNI_IGNORE
    dq .op_not_literal_uni_ignore ; 41 NOT_LITERAL_UNI_IGNORE
    dq .op_range                ; 42 RANGE_UNI_IGNORE (handled in charset)

; --- Opcode implementations ---

.op_failure:
    xor eax, eax
    jmp .return

.op_success:
    ; Check fullmatch: if match_all, pos must == string_len
    cmp dword [r12 + SRE_State.match_all], 0
    jz .op_success_ok
    mov rdi, r12
    call sre_string_len
    cmp r13, rax
    jne .op_failure
.op_success_ok:
    ; Save final position in state
    mov [r12 + SRE_State.str_pos], r13
    mov eax, 1
    jmp .return

.op_any:
    ; Match any character except newline
    mov rdi, r12
    call sre_string_len
    cmp r13, rax
    jge .op_failure
    mov rdi, r12
    mov rsi, r13
    call sre_getchar
    cmp eax, 0x0A              ; '\n'
    je .op_failure
    inc r13
    jmp .dispatch

.op_any_all:
    ; Match any character including newline
    mov rdi, r12
    call sre_string_len
    cmp r13, rax
    jge .op_failure
    inc r13
    jmp .dispatch

.op_at:
    ; AT at_code
    mov edx, [rbx]             ; at_code
    add rbx, 4
    mov rdi, r12
    mov rsi, r13
    call sre_at
    test eax, eax
    jz .op_failure
    jmp .dispatch

.op_literal:
    ; LITERAL ch — match exact character
    mov rdi, r12
    call sre_string_len
    cmp r13, rax
    jge .op_failure
    mov r14d, [rbx]            ; expected char
    add rbx, 4
    mov rdi, r12
    mov rsi, r13
    call sre_getchar
    cmp eax, r14d
    jne .op_failure
    inc r13
    jmp .dispatch

.op_not_literal:
    ; NOT_LITERAL ch — match any character EXCEPT ch
    mov rdi, r12
    call sre_string_len
    cmp r13, rax
    jge .op_failure
    mov r14d, [rbx]
    add rbx, 4
    mov rdi, r12
    mov rsi, r13
    call sre_getchar
    cmp eax, r14d
    je .op_failure
    inc r13
    jmp .dispatch

.op_literal_ignore:
    ; LITERAL_IGNORE ch — case-insensitive literal match
    mov rdi, r12
    call sre_string_len
    cmp r13, rax
    jge .op_failure
    mov r14d, [rbx]
    add rbx, 4
    mov rdi, r12
    mov rsi, r13
    call sre_getchar
    ; Compare lowercase versions
    push rax
    mov edi, eax
    call sre_ascii_tolower
    mov r14d, eax
    pop rax
    mov edi, [rbx - 4]        ; re-read expected
    call sre_ascii_tolower
    cmp r14d, eax
    jne .op_failure
    inc r13
    jmp .dispatch

.op_not_literal_ignore:
    ; NOT_LITERAL_IGNORE ch — case-insensitive not-literal
    mov rdi, r12
    call sre_string_len
    cmp r13, rax
    jge .op_failure
    mov r14d, [rbx]
    add rbx, 4
    mov rdi, r12
    mov rsi, r13
    call sre_getchar
    push rax
    mov edi, eax
    call sre_ascii_tolower
    mov r14d, eax
    pop rax
    mov edi, [rbx - 4]
    call sre_ascii_tolower
    cmp r14d, eax
    je .op_failure
    inc r13
    jmp .dispatch

.op_literal_uni_ignore:
    ; Same as literal_ignore but uses unicode tolower
    mov rdi, r12
    call sre_string_len
    cmp r13, rax
    jge .op_failure
    mov r14d, [rbx]
    add rbx, 4
    mov rdi, r12
    mov rsi, r13
    call sre_getchar
    push rax
    mov edi, eax
    call sre_unicode_tolower
    mov r14d, eax
    pop rax
    mov edi, [rbx - 4]
    call sre_unicode_tolower
    cmp r14d, eax
    jne .op_failure
    inc r13
    jmp .dispatch

.op_not_literal_uni_ignore:
    mov rdi, r12
    call sre_string_len
    cmp r13, rax
    jge .op_failure
    mov r14d, [rbx]
    add rbx, 4
    mov rdi, r12
    mov rsi, r13
    call sre_getchar
    push rax
    mov edi, eax
    call sre_unicode_tolower
    mov r14d, eax
    pop rax
    mov edi, [rbx - 4]
    call sre_unicode_tolower
    cmp r14d, eax
    je .op_failure
    inc r13
    jmp .dispatch

.op_in:
    ; IN skip_offset [set] — match char against character set
    mov rdi, r12
    call sre_string_len
    cmp r13, rax
    jge .op_failure
    mov r14d, [rbx]            ; skip offset (u32 count from after IN)
    add rbx, 4                 ; now at set data
    ; Get current char
    push rbx                   ; save set pointer
    mov rdi, r12
    mov rsi, r13
    call sre_getchar
    pop rdi                    ; set pointer
    mov esi, eax               ; character
    ; Check for NEGATE
    xor r14d, r14d             ; negate flag
    cmp dword [rdi], SRE_OP_NEGATE
    jne .in_no_negate
    mov r14d, 1
    add rdi, 4                 ; skip NEGATE opcode
.in_no_negate:
    push r14                   ; save negate flag
    call sre_charset
    pop r14
    ; Apply negate
    xor eax, r14d
    test eax, eax
    jz .op_failure
    ; Advance pc past the set
    mov eax, [rbx - 4]        ; skip offset (from IN+4 position)
    ; IN format: IN skip [set data] — skip includes IN opcode word
    ; pc was advanced past IN and skip, now need to go to IN + skip*4
    mov rbx, [rbp - SM_PATTERN]
    ; Actually, the skip is relative to the start of the IN opcode
    ; Let me reconsider: pc = after IN skip, pointing at set data
    ; skip = number of u32 words from IN opcode to past the set
    ; So target = IN_opcode_addr + skip*4
    ; IN_opcode_addr = rbx - 8 (backed up past opcode + skip)
    ; Hmm, this is getting complex. Let me just advance past set using FAILURE terminator
    ; Actually CPython's format: IN skip <set_ops> FAILURE
    ; skip is relative offset from IN, in u32 words
    ; target pc = &IN + skip*4
    ; We need the original IN address. Let me recalculate.
    ; We entered .op_in with rbx pointing past IN opcode (at skip offset)
    ; So IN addr = rbx - 4
    ; After reading skip: rbx = IN addr + 8 (past IN + skip)
    ; Target = IN addr + skip*4 = (rbx - 8) + skip*4
    lea rbx, [rbx - 8]        ; back to IN addr
    mov eax, [rbx + 4]        ; re-read skip
    lea rbx, [rbx + rax*4]    ; advance past set
    inc r13
    jmp .dispatch

.op_in_ignore:
    ; Same as IN but case-insensitive
    mov rdi, r12
    call sre_string_len
    cmp r13, rax
    jge .op_failure
    mov r14d, [rbx]            ; skip offset
    add rbx, 4
    push rbx                   ; save set ptr
    mov rdi, r12
    mov rsi, r13
    call sre_getchar
    ; Lowercase the char for matching
    mov edi, eax
    call sre_ascii_tolower
    pop rdi                    ; set ptr
    mov esi, eax
    xor r14d, r14d
    cmp dword [rdi], SRE_OP_NEGATE
    jne .in_ign_no_neg
    mov r14d, 1
    add rdi, 4
.in_ign_no_neg:
    push r14
    call sre_charset
    pop r14
    xor eax, r14d
    test eax, eax
    jz .op_failure
    lea rbx, [rbx - 8]
    mov eax, [rbx + 4]
    lea rbx, [rbx + rax*4]
    inc r13
    jmp .dispatch

.op_info:
    ; INFO skip ... — skip past info block
    mov eax, [rbx]             ; skip (u32 words from INFO opcode)
    lea rbx, [rbx - 4]        ; back to INFO addr
    lea rbx, [rbx + rax*4]    ; advance past info
    jmp .dispatch

.op_jump:
    ; JUMP offset — relative jump
    mov eax, [rbx]             ; offset from JUMP opcode
    lea rbx, [rbx - 4]        ; back to JUMP addr
    lea rbx, [rbx + rax*4]    ; new pc
    jmp .dispatch

.op_mark:
    ; MARK mark_id — set mark at current position
    mov esi, [rbx]             ; mark_id
    add rbx, 4
    mov rdi, r12
    mov rdx, r13               ; current position
    push rbx
    call sre_state_set_mark
    pop rbx
    jmp .dispatch

.op_branch:
    ; BRANCH offset1 [pattern1] offset2 [pattern2] ... 0
    ; Try each branch in order
    mov r14, rbx               ; r14 = ptr to first offset

.branch_loop:
    mov eax, [r14]             ; offset (0 = end of branches)
    test eax, eax
    jz .op_failure             ; no more branches

    ; Save state for backtracking
    push r13                   ; save position
    push r14                   ; save branch pointer
    ; Save marks
    mov rdi, r12
    call sre_save_marks
    push rax                   ; saved marks ptr

    ; Try this branch
    mov [r12 + SRE_State.str_pos], r13
    mov rdi, r12
    lea rsi, [r14 + 4]        ; pattern after offset
    call sre_match

    test eax, eax
    jnz .branch_success

    ; Restore marks and try next branch
    pop rdi                    ; saved marks
    push rdi
    mov rsi, r12
    call sre_restore_marks
    pop rdi
    call ap_free               ; free saved marks

    pop r14                    ; restore branch ptr
    pop r13                    ; restore position

    ; Advance to next branch
    mov eax, [r14]
    lea r14, [r14 + rax*4]    ; next branch offset
    jmp .branch_loop

.branch_success:
    ; Clean up saved state
    pop rdi                    ; saved marks (already used by match)
    call ap_free
    pop r14
    pop r13
    mov r13, [r12 + SRE_State.str_pos]  ; update position from state
    mov eax, 1
    jmp .return

.op_category:
    ; CATEGORY code — match char against category
    mov rdi, r12
    call sre_string_len
    cmp r13, rax
    jge .op_failure
    mov r14d, [rbx]            ; category code
    add rbx, 4
    mov rdi, r12
    mov rsi, r13
    push rbx
    call sre_getchar
    mov edi, r14d
    mov esi, eax
    call sre_category
    pop rbx
    test eax, eax
    jz .op_failure
    inc r13
    jmp .dispatch

.op_charset:
.op_bigcharset:
    ; These should only appear inside IN sets, not at top level
    jmp .op_failure

.op_negate:
.op_range:
    ; These should only appear inside IN sets
    jmp .op_failure

.op_groupref:
    ; GROUPREF group — match same text as group
    mov eax, [rbx]             ; group id
    add rbx, 4
    ; Get group start and end marks
    mov ecx, eax
    shl ecx, 1                 ; mark_start = group * 2
    push rbx
    mov rdi, r12
    mov rsi, rcx
    call sre_state_get_mark
    mov r14, rax               ; group start pos
    cmp r14, -1
    je .groupref_fail
    mov rdi, r12
    lea rsi, [rcx + 1]        ; mark_end = group * 2 + 1
    call sre_state_get_mark
    mov r8, rax                ; group end pos
    cmp r8, -1
    je .groupref_fail

    ; Match characters from group_start..group_end at current pos
    mov rcx, r8
    sub rcx, r14               ; group length
    test rcx, rcx
    jz .groupref_done          ; empty group matches

    ; Check enough chars remain
    mov rdi, r12
    call sre_string_len
    sub rax, r13               ; remaining chars
    cmp rax, rcx
    jb .groupref_fail

    ; Compare char by char
    xor r9d, r9d               ; offset
.groupref_cmp:
    cmp r9, rcx
    jge .groupref_done
    mov rdi, r12
    lea rsi, [r14 + r9]       ; group char index
    push rcx
    push r9
    call sre_getchar
    mov r10d, eax              ; expected char
    pop r9
    pop rcx
    mov rdi, r12
    lea rsi, [r13 + r9]       ; current char index
    push rcx
    push r9
    push r10
    call sre_getchar
    pop r10
    pop r9
    pop rcx
    cmp eax, r10d
    jne .groupref_fail
    inc r9
    jmp .groupref_cmp

.groupref_done:
    pop rbx
    add r13, rcx               ; advance position
    jmp .dispatch
.groupref_fail:
    pop rbx
    jmp .op_failure

.op_groupref_exists:
    ; GROUPREF_EXISTS group jump_offset — conditional
    mov eax, [rbx]             ; group
    mov ecx, [rbx + 4]        ; jump offset (if group not matched)
    add rbx, 8
    ; Check if group is matched
    shl eax, 1
    push rbx
    push rcx
    mov rdi, r12
    mov esi, eax
    call sre_state_get_mark
    cmp rax, -1
    je .gre_not_matched
    ; Also check end mark
    pop rcx
    pop rbx
    push rbx
    push rcx
    mov rdi, r12
    lea esi, [eax + 1]
    call sre_state_get_mark
    cmp rax, -1
    je .gre_not_matched
    pop rcx
    pop rbx
    ; Group matched — continue with next instruction (true branch)
    jmp .dispatch
.gre_not_matched:
    pop rcx                    ; jump offset
    pop rbx
    ; Jump to else branch
    lea rbx, [rbx - 8]        ; back to GROUPREF_EXISTS opcode addr + 4
    lea rbx, [rbx - 4]        ; back to opcode addr
    lea rbx, [rbx + rcx*4]    ; jump
    jmp .dispatch

.op_subpattern:
    ; SUBPATTERN group_id dir ... — mark group boundaries
    ; Format: SUBPATTERN group_id 0 0 0 (4 args)
    mov eax, [rbx]             ; group_id
    add rbx, 12                ; skip group_id, 0, 0 (3 more u32s past group_id)
    ; The mark operations are handled by MARK opcodes that follow
    jmp .dispatch

.op_repeat_one:
    ; REPEAT_ONE skip min max [pattern] — greedy repeat with single-char body
    ; skip = u32 words from REPEAT_ONE to past the body
    ; min = minimum repeats
    ; max = maximum repeats (SRE_MAXREPEAT for unlimited)
    mov r14d, [rbx]            ; skip
    mov ecx, [rbx + 4]        ; min
    mov r8d, [rbx + 8]        ; max
    add rbx, 12                ; past skip/min/max, pointing at body pattern

    ; Save state
    push r13                   ; save start pos

    ; First, match minimum required repetitions
    mov r9d, ecx               ; remaining minimum
    test r9d, r9d
    jz .ro_min_done

.ro_min_loop:
    ; Save and try body match
    mov [r12 + SRE_State.str_pos], r13
    push r9
    push r8
    push r14
    mov rdi, r12
    mov rsi, rbx               ; body pattern
    call sre_match
    pop r14
    pop r8
    pop r9
    test eax, eax
    jz .ro_min_fail
    mov r13, [r12 + SRE_State.str_pos]
    dec r9d
    test r9d, r9d
    jnz .ro_min_loop
.ro_min_done:

    ; Now greedily match up to max
    ; Count successful body matches
    mov r9d, ecx               ; current count = min
    cmp r9d, r8d
    jge .ro_greedy_done

.ro_greedy_loop:
    push r13                   ; save pos before body try
    mov [r12 + SRE_State.str_pos], r13
    push r9
    push r8
    push r14
    mov rdi, r12
    mov rsi, rbx               ; body pattern
    call sre_match
    pop r14
    pop r8
    pop r9
    test eax, eax
    jz .ro_greedy_body_fail
    mov r13, [r12 + SRE_State.str_pos]
    add rsp, 8                 ; discard saved pos (body succeeded)
    inc r9d
    cmp r9d, r8d
    jb .ro_greedy_loop
    jmp .ro_greedy_done

.ro_greedy_body_fail:
    pop r13                    ; restore pos (body failed)
.ro_greedy_done:
    ; Now try tail (pattern after REPEAT_ONE body)
    ; Tail starts at REPEAT_ONE addr + skip*4
    ; REPEAT_ONE addr = rbx - 12 - 4 = rbx - 16
    ; Wait: we entered with rbx pointing past opcode, then read skip/min/max and advanced +12
    ; So body pattern = rbx (current). REPEAT_ONE addr = rbx - 16
    ; Tail = REPEAT_ONE_addr + skip*4

    lea rcx, [rbx - 16]       ; REPEAT_ONE opcode addr
    mov eax, r14d              ; skip
    lea r14, [rcx + rax*4]    ; tail pattern

    ; Try tail from current position, back off on failure (greedy backtracking)
.ro_tail_loop:
    mov [r12 + SRE_State.str_pos], r13
    push r13
    push r9
    mov rdi, r12
    mov rsi, r14               ; tail pattern
    call sre_match
    pop r9
    test eax, eax
    jnz .ro_tail_success

    pop r13                    ; restore pos
    ; Back off one body match
    dec r9d
    cmp r9d, ecx
    jb .ro_fail                ; below minimum, fail
    ; Need to recalculate position by undoing last body match
    ; This is tricky for multi-char bodies. For single-char body, just dec pos.
    dec r13
    jmp .ro_tail_loop

.ro_tail_success:
    pop r13                    ; discard saved pos
    mov r13, [r12 + SRE_State.str_pos]
    add rsp, 8                 ; discard initial saved pos
    mov eax, 1
    jmp .return

.ro_min_fail:
    pop r13                    ; restore start pos
    jmp .op_failure

.ro_fail:
    pop r13                    ; restore start pos
    jmp .op_failure

.op_min_repeat_one:
    ; MIN_REPEAT_ONE skip min max [pattern] — non-greedy repeat
    mov r14d, [rbx]            ; skip
    mov ecx, [rbx + 4]        ; min
    mov r8d, [rbx + 8]        ; max
    add rbx, 12

    push r13                   ; save start pos

    ; Match minimum
    mov r9d, ecx
    test r9d, r9d
    jz .mro_min_done
.mro_min_loop:
    mov [r12 + SRE_State.str_pos], r13
    push r9
    push r8
    push r14
    push rcx
    mov rdi, r12
    mov rsi, rbx
    call sre_match
    pop rcx
    pop r14
    pop r8
    pop r9
    test eax, eax
    jz .mro_fail
    mov r13, [r12 + SRE_State.str_pos]
    dec r9d
    test r9d, r9d
    jnz .mro_min_loop
.mro_min_done:

    ; Calculate tail
    mov eax, r14d
    lea r14, [rbx - 16]
    lea r14, [r14 + rax*4]

    ; Non-greedy: try tail first, then extend
    mov r9d, [rbx - 8]        ; re-read min (ecx may be clobbered)
.mro_tail_loop:
    mov [r12 + SRE_State.str_pos], r13
    push r13
    push r9
    push r14
    mov rdi, r12
    mov rsi, r14
    call sre_match
    pop r14
    pop r9
    test eax, eax
    jnz .mro_success

    pop r13
    ; Try one more body match
    cmp r9d, [rbx - 4]        ; max
    jge .mro_fail
    mov [r12 + SRE_State.str_pos], r13
    push r9
    push r14
    mov rdi, r12
    mov rsi, rbx               ; body pattern
    call sre_match
    pop r14
    pop r9
    test eax, eax
    jz .mro_fail
    mov r13, [r12 + SRE_State.str_pos]
    inc r9d
    jmp .mro_tail_loop

.mro_success:
    pop r13
    mov r13, [r12 + SRE_State.str_pos]
    add rsp, 8                 ; discard initial saved pos
    mov eax, 1
    jmp .return

.mro_fail:
    pop r13
    jmp .op_failure

.op_repeat:
    ; REPEAT skip min max — general repeat (body may be multi-char)
    ; Uses REPEAT_CTX for backtracking with MAX_UNTIL/MIN_UNTIL
    mov eax, [rbx]             ; skip
    mov ecx, [rbx + 4]        ; min
    mov r8d, [rbx + 8]        ; max
    add rbx, 12

    ; Create RepeatContext on stack
    ; We'll use a dynamically allocated one for simplicity
    push rbx
    push rcx
    push r8
    mov edi, SRE_RepeatContext_size
    call ap_malloc
    pop r8
    pop rcx
    pop rbx
    mov qword [rax + SRE_RepeatContext.count], 0
    mov [rax + SRE_RepeatContext.pattern], rbx  ; points to body (after skip/min/max)
    mov [rax + SRE_RepeatContext.prev], r15
    mov [rax + SRE_RepeatContext.last_pos], r13

    ; Push onto repeat context chain
    mov r15, rax
    mov [r12 + SRE_State.repeat_ctx], r15

    ; Pattern after REPEAT = REPEAT_addr + skip*4
    ; REPEAT addr = rbx - 16, skip at rbx - 12
    mov eax, [rbx - 12]
    lea rbx, [rbx - 16 + rax*4]

    ; Continue dispatch — MAX_UNTIL or MIN_UNTIL will handle the body
    jmp .dispatch

.op_max_until:
    ; MAX_UNTIL — greedy repeat body iteration
    ; r15 = current RepeatContext
    test r15, r15
    jz .op_failure

    mov rax, [r15 + SRE_RepeatContext.count]
    inc rax
    mov [r15 + SRE_RepeatContext.count], rax

    ; Get min/max from the REPEAT pattern
    ; rpt->pattern points to body, REPEAT's min is at pattern - 8, max at pattern - 4
    mov rcx, [r15 + SRE_RepeatContext.pattern]
    mov r8d, [rcx - 8]        ; min
    mov r9d, [rcx - 4]        ; max

    ; If count < min, must match body again
    cmp rax, r8
    jb .mu_try_body

    ; Save last_pos for zero-width check
    push qword [r15 + SRE_RepeatContext.last_pos]
    mov [r15 + SRE_RepeatContext.last_pos], r13

    ; If count <= max (or max == MAXREPEAT), try body first (greedy)
    cmp r9d, SRE_MAXREPEAT
    je .mu_try_body_greedy
    cmp rax, r9
    jbe .mu_try_body_greedy

    ; count > max — try tail only
    pop rax                    ; discard saved last_pos
    jmp .mu_try_tail

.mu_try_body_greedy:
    ; Zero-width check: if pos == last_pos, body matched empty, don't loop
    pop rax                    ; saved last_pos
    cmp r13, rax
    je .mu_try_tail

    ; Save marks for backtracking
    push r13
    mov rdi, r12
    call sre_save_marks
    push rax

    ; Try body
    mov [r12 + SRE_State.str_pos], r13
    mov rdi, r12
    mov rsi, [r15 + SRE_RepeatContext.pattern]
    call sre_match
    test eax, eax
    jnz .mu_body_success

    ; Body failed — restore marks, try tail
    pop rdi
    push rdi
    mov rsi, r12
    call sre_restore_marks
    pop rdi
    call ap_free
    pop r13

    ; Undo count increment
    dec qword [r15 + SRE_RepeatContext.count]
    jmp .mu_try_tail

.mu_body_success:
    pop rdi                    ; free saved marks
    call ap_free
    pop r13
    mov r13, [r12 + SRE_State.str_pos]
    mov eax, 1
    jmp .return

.mu_try_body:
    ; Must match body (count < min)
    mov [r12 + SRE_State.str_pos], r13
    mov rdi, r12
    mov rsi, [r15 + SRE_RepeatContext.pattern]
    call sre_match
    test eax, eax
    jz .mu_body_required_fail
    mov r13, [r12 + SRE_State.str_pos]
    mov eax, 1
    jmp .return

.mu_body_required_fail:
    dec qword [r15 + SRE_RepeatContext.count]
    jmp .op_failure

.mu_try_tail:
    ; Pop repeat context and try tail
    mov rax, [r15 + SRE_RepeatContext.prev]
    push r15                   ; save for potential restore
    mov r15, rax
    mov [r12 + SRE_State.repeat_ctx], r15

    mov [r12 + SRE_State.str_pos], r13
    mov rdi, r12
    mov rsi, rbx               ; tail = code after MAX_UNTIL
    call sre_match
    test eax, eax
    jnz .mu_tail_success

    ; Restore repeat context
    pop r15
    mov [r12 + SRE_State.repeat_ctx], r15
    dec qword [r15 + SRE_RepeatContext.count]
    jmp .op_failure

.mu_tail_success:
    ; Free repeat context
    pop rdi                    ; old r15
    call ap_free
    mov r13, [r12 + SRE_State.str_pos]
    mov eax, 1
    jmp .return

.op_min_until:
    ; MIN_UNTIL — non-greedy repeat body iteration
    test r15, r15
    jz .op_failure

    mov rax, [r15 + SRE_RepeatContext.count]
    inc rax
    mov [r15 + SRE_RepeatContext.count], rax

    mov rcx, [r15 + SRE_RepeatContext.pattern]
    mov r8d, [rcx - 8]        ; min
    mov r9d, [rcx - 4]        ; max

    ; If count < min, must match body
    cmp rax, r8
    jb .miu_try_body

    ; Try tail first (non-greedy)
    push r13
    mov rax, [r15 + SRE_RepeatContext.prev]
    push r15
    mov r15, rax
    mov [r12 + SRE_State.repeat_ctx], r15

    mov [r12 + SRE_State.str_pos], r13
    mov rdi, r12
    mov rsi, rbx
    call sre_match
    test eax, eax
    jnz .miu_tail_success

    ; Restore repeat context
    pop r15
    mov [r12 + SRE_State.repeat_ctx], r15
    pop r13

    ; Check max
    mov rax, [r15 + SRE_RepeatContext.count]
    mov rcx, [r15 + SRE_RepeatContext.pattern]
    mov r9d, [rcx - 4]        ; max
    cmp r9d, SRE_MAXREPEAT
    je .miu_try_body
    cmp rax, r9
    ja .miu_fail

.miu_try_body:
    ; Zero-width check
    cmp r13, [r15 + SRE_RepeatContext.last_pos]
    je .miu_fail
    mov [r15 + SRE_RepeatContext.last_pos], r13

    mov [r12 + SRE_State.str_pos], r13
    mov rdi, r12
    mov rsi, [r15 + SRE_RepeatContext.pattern]
    call sre_match
    test eax, eax
    jnz .miu_body_ok
    dec qword [r15 + SRE_RepeatContext.count]
    jmp .op_failure

.miu_body_ok:
    mov r13, [r12 + SRE_State.str_pos]
    mov eax, 1
    jmp .return

.miu_tail_success:
    pop rdi                    ; old r15
    call ap_free
    pop r13
    mov r13, [r12 + SRE_State.str_pos]
    mov eax, 1
    jmp .return

.miu_fail:
    dec qword [r15 + SRE_RepeatContext.count]
    jmp .op_failure

.op_assert:
    ; ASSERT direction width [pattern] — lookahead/lookbehind
    mov eax, [rbx]             ; skip (offset to past assert pattern)
    mov ecx, [rbx + 4]        ; direction (1=ahead, -1=behind)
    mov r8d, [rbx + 8]        ; width
    add rbx, 12

    ; For lookbehind (direction == -1), pos must >= width
    cmp ecx, -1
    jne .assert_ahead
    ; Lookbehind
    cmp r13, r8
    jb .op_failure
    push r13
    push rbx
    sub r13, r8                ; back up position
    mov [r12 + SRE_State.str_pos], r13
    mov rdi, r12
    mov rsi, rbx               ; assert body pattern
    call sre_match
    pop rbx
    pop r13
    test eax, eax
    jz .op_failure
    ; Advance past assert body
    mov eax, [rbx - 12]       ; skip
    lea rbx, [rbx - 16]       ; back to ASSERT addr
    lea rbx, [rbx + rax*4]
    jmp .dispatch

.assert_ahead:
    ; Lookahead
    push r13
    push rbx
    mov [r12 + SRE_State.str_pos], r13
    mov rdi, r12
    mov rsi, rbx               ; assert body
    call sre_match
    pop rbx
    pop r13
    test eax, eax
    jz .op_failure
    mov eax, [rbx - 12]
    lea rbx, [rbx - 16]
    lea rbx, [rbx + rax*4]
    jmp .dispatch

.op_assert_not:
    ; ASSERT_NOT direction width [pattern] — negative lookahead/lookbehind
    mov eax, [rbx]
    mov ecx, [rbx + 4]
    mov r8d, [rbx + 8]
    add rbx, 12

    cmp ecx, -1
    jne .assert_not_ahead
    ; Negative lookbehind
    cmp r13, r8
    jb .assert_not_pass        ; can't look behind — assertion passes
    push r13
    push rbx
    sub r13, r8
    mov [r12 + SRE_State.str_pos], r13
    mov rdi, r12
    mov rsi, rbx
    call sre_match
    pop rbx
    pop r13
    test eax, eax
    jnz .op_failure            ; pattern matched — assertion fails
    jmp .assert_not_pass

.assert_not_ahead:
    push r13
    push rbx
    mov [r12 + SRE_State.str_pos], r13
    mov rdi, r12
    mov rsi, rbx
    call sre_match
    pop rbx
    pop r13
    test eax, eax
    jnz .op_failure            ; matched — negative assertion fails

.assert_not_pass:
    mov eax, [rbx - 12]
    lea rbx, [rbx - 16]
    lea rbx, [rbx + rax*4]
    jmp .dispatch

.op_atomic_group:
    ; ATOMIC_GROUP skip [pattern] — match pattern atomically (no backtracking)
    mov r14d, [rbx]            ; skip
    add rbx, 4
    push r13
    mov [r12 + SRE_State.str_pos], r13
    mov rdi, r12
    mov rsi, rbx               ; body pattern
    call sre_match
    pop r14                    ; saved pos (discard on success, restore on failure)
    test eax, eax
    jz .atomic_fail
    mov r13, [r12 + SRE_State.str_pos]
    ; Advance past body
    lea rbx, [rbx - 8]        ; ATOMIC_GROUP addr
    mov eax, [rbx + 4]        ; skip
    lea rbx, [rbx + rax*4]
    jmp .dispatch
.atomic_fail:
    mov r13, r14               ; restore position
    jmp .op_failure

.op_possessive_repeat:
    ; POSSESSIVE_REPEAT skip min max [body] — like repeat but no backtracking
    mov r14d, [rbx]            ; skip
    mov ecx, [rbx + 4]        ; min
    mov r8d, [rbx + 8]        ; max
    add rbx, 12                ; body pattern

    push r13                   ; save initial pos

    ; Match minimum
    mov r9d, ecx
    test r9d, r9d
    jz .pr_min_done
.pr_min_loop:
    mov [r12 + SRE_State.str_pos], r13
    push r9
    push r8
    push r14
    push rcx
    mov rdi, r12
    mov rsi, rbx
    call sre_match
    pop rcx
    pop r14
    pop r8
    pop r9
    test eax, eax
    jz .pr_fail
    mov r13, [r12 + SRE_State.str_pos]
    dec r9d
    test r9d, r9d
    jnz .pr_min_loop
.pr_min_done:

    ; Greedily match up to max (no backtracking)
    mov r9d, ecx
.pr_greedy_loop:
    cmp r9d, r8d
    jge .pr_greedy_done
    push r13
    mov [r12 + SRE_State.str_pos], r13
    push r9
    push r8
    push r14
    push rcx
    mov rdi, r12
    mov rsi, rbx
    call sre_match
    pop rcx
    pop r14
    pop r8
    pop r9
    test eax, eax
    jz .pr_greedy_stop
    mov r13, [r12 + SRE_State.str_pos]
    add rsp, 8
    inc r9d
    jmp .pr_greedy_loop
.pr_greedy_stop:
    pop r13
.pr_greedy_done:
    ; Continue with tail (no backtracking into body)
    add rsp, 8                 ; discard initial saved pos
    lea rcx, [rbx - 16]
    mov eax, r14d
    lea rbx, [rcx + rax*4]
    jmp .dispatch

.pr_fail:
    pop r13
    jmp .op_failure

.op_possessive_repeat_one:
    ; Like REPEAT_ONE but possessive
    mov r14d, [rbx]
    mov ecx, [rbx + 4]
    mov r8d, [rbx + 8]
    add rbx, 12

    push r13

    ; Match minimum
    mov r9d, ecx
    test r9d, r9d
    jz .pro_min_done
.pro_min_loop:
    mov [r12 + SRE_State.str_pos], r13
    push r9
    push r8
    push r14
    push rcx
    mov rdi, r12
    mov rsi, rbx
    call sre_match
    pop rcx
    pop r14
    pop r8
    pop r9
    test eax, eax
    jz .pro_fail
    mov r13, [r12 + SRE_State.str_pos]
    dec r9d
    test r9d, r9d
    jnz .pro_min_loop
.pro_min_done:

    ; Greedy match
    mov r9d, ecx
.pro_greedy_loop:
    cmp r9d, r8d
    jge .pro_greedy_done
    push r13
    mov [r12 + SRE_State.str_pos], r13
    push r9
    push r8
    push r14
    push rcx
    mov rdi, r12
    mov rsi, rbx
    call sre_match
    pop rcx
    pop r14
    pop r8
    pop r9
    test eax, eax
    jz .pro_greedy_stop
    mov r13, [r12 + SRE_State.str_pos]
    add rsp, 8
    inc r9d
    jmp .pro_greedy_loop
.pro_greedy_stop:
    pop r13
.pro_greedy_done:
    add rsp, 8                 ; discard initial pos
    lea rcx, [rbx - 16]
    mov eax, r14d
    lea rbx, [rcx + rax*4]
    jmp .dispatch

.pro_fail:
    pop r13
    jmp .op_failure

.op_groupref_ignore:
    ; GROUPREF_IGNORE group — case-insensitive group reference
    mov eax, [rbx]
    add rbx, 4
    mov ecx, eax
    shl ecx, 1
    push rbx
    mov rdi, r12
    mov rsi, rcx
    call sre_state_get_mark
    mov r14, rax
    cmp r14, -1
    je .gri_fail
    mov rdi, r12
    lea rsi, [rcx + 1]
    call sre_state_get_mark
    mov r8, rax
    cmp r8, -1
    je .gri_fail

    mov rcx, r8
    sub rcx, r14
    test rcx, rcx
    jz .gri_done

    mov rdi, r12
    call sre_string_len
    sub rax, r13
    cmp rax, rcx
    jb .gri_fail

    xor r9d, r9d
.gri_cmp:
    cmp r9, rcx
    jge .gri_done
    push rcx
    push r9
    mov rdi, r12
    lea rsi, [r14 + r9]
    call sre_getchar
    mov edi, eax
    call sre_ascii_tolower
    mov r10d, eax
    pop r9
    push r9
    mov rdi, r12
    lea rsi, [r13 + r9]
    call sre_getchar
    mov edi, eax
    call sre_ascii_tolower
    pop r9
    pop rcx
    cmp eax, r10d
    jne .gri_fail
    inc r9
    jmp .gri_cmp

.gri_done:
    pop rbx
    add r13, rcx
    jmp .dispatch
.gri_fail:
    pop rbx
    jmp .op_failure

.return:
    pop r15
    pop r14
    pop r13
    pop r12
    pop rbx
    leave
    ret
END_FUNC sre_match

; ============================================================================
; sre_save_marks(SRE_State* state) -> void*
; Save marks snapshot for backtracking. Returns malloc'd buffer.
; ============================================================================
DEF_FUNC sre_save_marks
    push rbx
    mov rbx, rdi               ; state

    mov rcx, [rbx + SRE_State.marks_size]
    test rcx, rcx
    jz .save_empty

    lea rdi, [rcx*8 + 24]     ; marks data + lastmark + lastindex + marks_size
    call ap_malloc
    mov r8, rax

    ; Copy header: marks_size, lastmark, lastindex
    mov rcx, [rbx + SRE_State.marks_size]
    mov [r8], rcx
    mov rcx, [rbx + SRE_State.lastmark]
    mov [r8 + 8], rcx
    mov rcx, [rbx + SRE_State.lastindex]
    mov [r8 + 16], rcx

    ; Copy marks data
    mov rdi, r8
    add rdi, 24
    mov rsi, [rbx + SRE_State.marks]
    mov rdx, [rbx + SRE_State.marks_size]
    shl rdx, 3
    call ap_memcpy

    mov rax, r8
    pop rbx
    leave
    ret

.save_empty:
    mov edi, 24
    call ap_malloc
    mov qword [rax], 0
    mov qword [rax + 8], -1
    mov qword [rax + 16], -1
    pop rbx
    leave
    ret
END_FUNC sre_save_marks

; ============================================================================
; sre_restore_marks(void* saved, SRE_State* state)
; Restore marks from snapshot.
; ============================================================================
DEF_FUNC sre_restore_marks
    push rbx
    mov rbx, rsi               ; state
    mov r8, rdi                ; saved data

    ; Restore header
    mov rcx, [r8]              ; marks_size
    mov [rbx + SRE_State.marks_size], rcx
    mov rcx, [r8 + 8]         ; lastmark
    mov [rbx + SRE_State.lastmark], rcx
    mov rcx, [r8 + 16]        ; lastindex
    mov [rbx + SRE_State.lastindex], rcx

    ; Restore marks data
    mov rcx, [r8]
    test rcx, rcx
    jz .restore_done

    mov rdi, [rbx + SRE_State.marks]
    lea rsi, [r8 + 24]
    mov rdx, rcx
    shl rdx, 3
    call ap_memcpy

.restore_done:
    pop rbx
    leave
    ret
END_FUNC sre_restore_marks

; ============================================================================
; sre_search(SRE_State* state) -> 0/1
; Linear scan: try sre_match at each position from pos to endpos.
; ============================================================================
DEF_FUNC sre_search, 32
    push rbx
    push r12
    push r13

    mov r12, rdi               ; state
    mov r13, [rdi + SRE_State.str_pos]  ; starting pos

    ; Get string length
    mov rdi, r12
    call sre_string_len
    mov rbx, rax               ; rbx = string length (endpos)

.search_loop:
    cmp r13, rbx
    ja .search_fail

    ; Reset marks for each attempt
    mov qword [r12 + SRE_State.marks_size], 0
    mov qword [r12 + SRE_State.lastmark], -1
    mov qword [r12 + SRE_State.lastindex], -1
    mov qword [r12 + SRE_State.repeat_ctx], 0
    mov [r12 + SRE_State.str_pos], r13
    mov [r12 + SRE_State.str_start], r13

    ; Try match at current position
    mov rdi, r12
    mov rsi, [r12 + SRE_State.pattern]
    mov rsi, [rsi + SRE_PatternObject.code]
    call sre_match
    test eax, eax
    jnz .search_found

    inc r13
    jmp .search_loop

.search_found:
    mov eax, 1
    pop r13
    pop r12
    pop rbx
    leave
    ret

.search_fail:
    xor eax, eax
    pop r13
    pop r12
    pop rbx
    leave
    ret
END_FUNC sre_search

; ============================================================================
; sre_count(SRE_State* state, u32* pattern, i64 maxcount) -> i64
; Count successive matches of a single-char pattern.
; Used by REPEAT_ONE optimization.
; ============================================================================
DEF_FUNC sre_count
    push rbx
    push r12
    push r13
    push r14

    mov r12, rdi               ; state
    mov rbx, rsi               ; pattern
    mov r14, rdx               ; maxcount

    mov r13, [r12 + SRE_State.str_pos]
    xor ecx, ecx               ; count = 0

    ; Get string length
    mov rdi, r12
    call sre_string_len
    mov r8, rax                ; string length

.count_loop:
    cmp rcx, r14
    jge .count_done
    cmp r13, r8
    jge .count_done

    ; Try match at pos
    push rcx
    push r8
    mov [r12 + SRE_State.str_pos], r13
    mov rdi, r12
    mov rsi, rbx
    call sre_match
    pop r8
    pop rcx
    test eax, eax
    jz .count_done

    mov r13, [r12 + SRE_State.str_pos]
    inc rcx
    jmp .count_loop

.count_done:
    mov rax, rcx
    pop r14
    pop r13
    pop r12
    pop rbx
    leave
    ret
END_FUNC sre_count

section .rodata
; (category tables are inline in sre_category above)
