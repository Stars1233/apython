; sre_char.asm - the character layer of the SRE engine
;
; What the match engine asks about one character or one position: fetching a
; code point from the subject, the \d \s \w categories, a character set, the
; position assertions, and case folding.  Split out of sre.asm, which had
; reached the 100k cap for a hand-written file; the seam is the one the file
; already had, between "what is this character" and "how does the pattern
; walk the subject".

%include "macros.inc"
%include "object.inc"
%include "sre.inc"

;; ============================================================================
;; sre_getchar(SRE_State* state, i64 index) -> u32 codepoint
;; Get character at given index. ASCII fast path or codepoint_buf lookup.
;; ============================================================================
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

;; ============================================================================
;; sre_category(u32 category_code, u32 ch) -> 0/1
;; Check if character matches a category.
;; rdi = category code, esi = character codepoint
;; ============================================================================
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
    cmp esi, 0x0d             ; \r
    jbe .cat_true
    jmp .cat_false

.cat_not_space:
    cmp esi, ' '
    je .cat_false
    cmp esi, 0x09
    jb .cat_true
    cmp esi, 0x0d
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
    cmp esi, 0x0a              ; \n
    je .cat_true
    cmp esi, 0x0d              ; \r
    je .cat_true
    jmp .cat_false

.cat_not_linebreak:
    cmp esi, 0x0a
    je .cat_false
    cmp esi, 0x0d
    je .cat_false
    jmp .cat_true

; Unicode categories — for ASCII range, same as ASCII versions
; For codepoints > 127, do basic checks
.cat_uni_digit:
    cmp esi, '0'
    jb .cat_false
    cmp esi, '9'
    jbe .cat_true
    ; Arabic-Indic digits
    cmp esi, 0x0660
    jb .cat_false
    cmp esi, 0x0669
    jbe .cat_true
    ; Extended Arabic-Indic digits
    cmp esi, 0x06f0
    jb .cat_false
    cmp esi, 0x06f9
    jbe .cat_true
    ; Devanagari digits
    cmp esi, 0x0966
    jb .cat_false
    cmp esi, 0x096f
    jbe .cat_true
    ; Bengali digits
    cmp esi, 0x09e6
    jb .cat_false
    cmp esi, 0x09ef
    jbe .cat_true
    ; Fullwidth digits
    cmp esi, 0xff10
    jb .cat_false
    cmp esi, 0xff19
    jbe .cat_true
    jmp .cat_false

.cat_uni_not_digit:
    cmp esi, '0'
    jb .cat_true
    cmp esi, '9'
    jbe .cat_false
    ; Arabic-Indic digits
    cmp esi, 0x0660
    jb .cat_true
    cmp esi, 0x0669
    jbe .cat_false
    ; Extended Arabic-Indic digits
    cmp esi, 0x06f0
    jb .cat_true
    cmp esi, 0x06f9
    jbe .cat_false
    ; Devanagari digits
    cmp esi, 0x0966
    jb .cat_true
    cmp esi, 0x096f
    jbe .cat_false
    ; Bengali digits
    cmp esi, 0x09e6
    jb .cat_true
    cmp esi, 0x09ef
    jbe .cat_false
    ; Fullwidth digits
    cmp esi, 0xff10
    jb .cat_true
    cmp esi, 0xff19
    jbe .cat_false
    jmp .cat_true

.cat_uni_space:
    cmp esi, ' '
    je .cat_true
    cmp esi, 0x09
    jb .cat_false
    cmp esi, 0x0d
    jbe .cat_true
    ; Unicode whitespace: \u0085, \u00A0, \u2000-\u200A, etc.
    cmp esi, 0x85
    je .cat_true
    cmp esi, 0xa0
    je .cat_true
    cmp esi, 0x1680
    je .cat_true
    cmp esi, 0x2000
    jb .cat_false
    cmp esi, 0x200a
    jbe .cat_true
    cmp esi, 0x2028
    je .cat_true
    cmp esi, 0x2029
    je .cat_true
    cmp esi, 0x202f
    je .cat_true
    cmp esi, 0x205f
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
    cmp esi, 0x0d
    jbe .cat_false
    cmp esi, 0x85
    je .cat_false
    cmp esi, 0xa0
    je .cat_false
    cmp esi, 0x1680
    je .cat_false
    cmp esi, 0x2000
    jb .cat_true
    cmp esi, 0x200a
    jbe .cat_false
    cmp esi, 0x2028
    je .cat_false
    cmp esi, 0x2029
    je .cat_false
    cmp esi, 0x202f
    je .cat_false
    cmp esi, 0x205f
    je .cat_false
    cmp esi, 0x3000
    je .cat_false
    jmp .cat_true

.cat_uni_word:
    ; ASCII word chars first
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
    ; Check Unicode letter/digit ranges
    push rdi
    push rsi
    mov edi, esi
    call sre_uni_isword
    pop rsi
    pop rdi
    test eax, eax
    jnz .cat_true
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
    push rdi
    push rsi
    mov edi, esi
    call sre_uni_isword
    pop rsi
    pop rdi
    test eax, eax
    jnz .cat_false
    jmp .cat_true

.cat_uni_linebreak:
    cmp esi, 0x0a
    je .cat_true
    cmp esi, 0x0d
    je .cat_true
    cmp esi, 0x0b              ; \v
    je .cat_true
    cmp esi, 0x0c              ; \f
    je .cat_true
    cmp esi, 0x85              ; NEL
    je .cat_true
    cmp esi, 0x2028            ; LINE SEPARATOR
    je .cat_true
    cmp esi, 0x2029            ; PARAGRAPH SEPARATOR
    je .cat_true
    jmp .cat_false

.cat_uni_not_linebreak:
    cmp esi, 0x0a
    je .cat_false
    cmp esi, 0x0d
    je .cat_false
    cmp esi, 0x0b
    je .cat_false
    cmp esi, 0x0c
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

;; ============================================================================
;; sre_charset(u32* set, u32 ch) -> 0/1
;; Check if ch is in a character set (IN opcode's set data).
;; Set format: sequence of (opcode, args...) terminated by SRE_OP_FAILURE.
;; ============================================================================
SM_FRAME  equ 16            ; + 2 pushes = 32

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
    ; BIGCHARSET <count> <256-byte block map> <count * 32-byte bitmaps>
    ;
    ; The map comes FIRST.  _optimize_charset builds the operand as
    ; `[block] + mapping + data`, and this read the two the other way round:
    ; the block number came out of the middle of a bitmap and the bitmap out
    ; of the middle of the map.  For [a-z] under IGNORECASE the map is almost
    ; all 2s, so the "bitmap" it landed on was 0x02020202 and exactly the code
    ; points congruent to 1 mod 8 matched -- a, i, q, y and nothing else.
    mov eax, [rbx]             ; number of blocks
    add rbx, 4
    mov ecx, r12d
    shr ecx, 8                 ; the code point's high byte
    cmp ecx, 256
    jae .cs_big_skip           ; above U+FFFF, which the map does not reach
    movzx ecx, byte [rbx + rcx]     ; map[ch >> 8] = which bitmap
    imul ecx, ecx, 32               ; each is 256 bits
    lea rdx, [rbx + 256]            ; the bitmaps follow the map
    add rdx, rcx
    mov eax, r12d
    and eax, 0xff
    shr eax, 5
    mov eax, [rdx + rax*4]
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

;; ============================================================================
;; sre_at(SRE_State* state, i64 pos, u32 at_code) -> 0/1
;; Check position assertion (^, $, \b, etc.)
;; ============================================================================
SA_CAT   equ 8              ; which word category \b and \B ask about
SA_FRAME equ 16             ; + 0 pushes = 16, 16-aligned
DEF_FUNC sre_at, SA_FRAME
    ; rdi = state, rsi = pos, edx = at_code
    mov r8, rdi                ; r8 = state
    mov r9, rsi                ; r9 = pos
    mov ecx, edx               ; ecx = at_code
    ; \b and \B ask "is this a word character" on each side, and WHICH
    ; question that is depends on the opcode.  Both handlers used to
    ; hard-code the ASCII category, so a non-ASCII letter was not a word
    ; character to them and `\b\d+\b` matched inside "ééé42".  The default
    ; is the ASCII one: bytes patterns and re.ASCII both compile to the plain
    ; AT_BOUNDARY, and only a unicode str pattern emits the UNI_ form.
    mov qword [rbp - SA_CAT], SRE_CATEGORY_WORD

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
    je .at_uni_boundary
    cmp ecx, SRE_AT_UNI_NON_BOUNDARY
    je .at_uni_non_boundary
    cmp ecx, SRE_AT_LOC_BOUNDARY
    je .at_word_boundary
    cmp ecx, SRE_AT_LOC_NON_BOUNDARY
    je .at_non_boundary
    ; Unknown
    xor eax, eax
    leave
    ret

.at_beginning:
    ; AT_BEGINNING: pos == 0 (same as AT_BEGINNING_STRING per CPython)
    test r9, r9
    jz .at_true
    jmp .at_false

.at_beginning_string:
    ; AT_BEGINNING_STRING: pos == 0
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
    cmp eax, 0x0a
    je .at_true
    jmp .at_false

.at_end:
    ; pos == len or (pos == len-1 and char[pos] == '\n')
    SRE_SUBJECT_LEN r8
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
    cmp eax, 0x0a
    je .at_true
    jmp .at_false

.at_end_line:
    ; pos == len or char[pos] == '\n'
    SRE_SUBJECT_LEN r8
.at_endline_check:
    cmp r9, rax
    je .at_true
    mov rdi, r8
    mov rsi, r9
    call sre_getchar
    cmp eax, 0x0a
    je .at_true
    jmp .at_false

.at_end_string:
    ; pos == len
    SRE_SUBJECT_LEN r8
.at_endstr_check:
    cmp r9, rax
    je .at_true
    jmp .at_false

.at_uni_boundary:
    ; sre_uni_isword's category, which \w has always used and \b had not.
    mov qword [rbp - SA_CAT], SRE_CATEGORY_UNI_WORD
    jmp .at_word_boundary
.at_uni_non_boundary:
    mov qword [rbp - SA_CAT], SRE_CATEGORY_UNI_WORD
    jmp .at_non_boundary

.at_word_boundary:
    ; \b: word status differs at pos-1 and pos
    ; CPython returns 0 from both \b and \B on an EMPTY subject, before it
    ; looks at either side (sre_lib.h, SRE_AT_BOUNDARY).  Without the early
    ; out, \B saw "not a word on the left, not a word on the right, therefore
    ; the same" and matched.
    SRE_SUBJECT_LEN r8
.wb_empty_done:
    test rax, rax
    jz .at_false
    push r8
    push r9
    ; Get "is word" for pos-1
    xor r10d, r10d             ; left_is_word = 0
    test r9, r9
    jz .wb_check_right
    lea rsi, [r9 - 1]
    mov rdi, r8
    call sre_getchar
    mov rdi, [rbp - SA_CAT]
    mov esi, eax
    call sre_category
    mov r10d, eax
.wb_check_right:
    pop r9
    pop r8
    ; Get "is word" for pos
    xor r11d, r11d             ; right_is_word = 0
    ; Get string length
    SRE_SUBJECT_LEN r8
.wb_right_check:
    cmp r9, rax
    jge .wb_compare
    push r10
    mov rdi, r8
    mov rsi, r9
    call sre_getchar
    mov rdi, [rbp - SA_CAT]
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
    ; CPython returns 0 from both \b and \B on an EMPTY subject, before it
    ; looks at either side (sre_lib.h, SRE_AT_BOUNDARY).  Without the early
    ; out, \B saw "not a word on the left, not a word on the right, therefore
    ; the same" and matched.
    SRE_SUBJECT_LEN r8
.nb_empty_done:
    test rax, rax
    jz .at_false
    push r8
    push r9
    xor r10d, r10d
    test r9, r9
    jz .nb_check_right
    lea rsi, [r9 - 1]
    mov rdi, r8
    call sre_getchar
    mov rdi, [rbp - SA_CAT]
    mov esi, eax
    call sre_category
    mov r10d, eax
.nb_check_right:
    pop r9
    pop r8
    xor r11d, r11d
    SRE_SUBJECT_LEN r8
.nb_right_check:
    cmp r9, rax
    jge .nb_compare
    push r10
    mov rdi, r8
    mov rsi, r9
    call sre_getchar
    mov rdi, [rbp - SA_CAT]
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

;; ============================================================================
;; sre_ascii_tolower(u32 ch) -> u32
;; ============================================================================
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

;; ============================================================================
;; sre_unicode_tolower(u32 ch) -> u32
;; Basic Unicode case folding (ASCII + common Latin)
;; ============================================================================
DEF_FUNC_BARE sre_unicode_tolower
    mov eax, edi
    ; ASCII A-Z -> a-z (+32)
    cmp eax, 'A'
    jb .no_lower
    cmp eax, 'Z'
    jbe .add32
    ; Latin-1 Supplement: 0xc0-0xd6, 0xd8-0xde -> +32
    cmp eax, 0xc0
    jb .no_lower
    cmp eax, 0xd6
    jbe .add32
    cmp eax, 0xd8
    jb .no_lower
    cmp eax, 0xde
    jbe .add32
    ; Latin Extended-A: even codepoints 0x100-0x12e -> +1
    cmp eax, 0x100
    jb .no_lower
    cmp eax, 0x12e
    ja .latin_ext_a2
    test eax, 1
    jnz .no_lower              ; already lowercase (odd)
    inc eax
    ret
.latin_ext_a2:
    ; Latin Extended-A: 0x132-0x136 even -> +1
    cmp eax, 0x132
    jb .no_lower
    cmp eax, 0x136
    ja .latin_ext_a3
    test eax, 1
    jnz .no_lower
    inc eax
    ret
.latin_ext_a3:
    ; 0x139-0x148: odd codepoints -> +1
    cmp eax, 0x139
    jb .no_lower
    cmp eax, 0x148
    ja .latin_ext_a4
    test eax, 1
    jz .no_lower               ; even = already lowercase
    inc eax
    ret
.latin_ext_a4:
    ; 0x14a-0x177: even -> +1
    cmp eax, 0x14a
    jb .no_lower
    cmp eax, 0x177
    ja .latin_ext_a5
    test eax, 1
    jnz .no_lower
    inc eax
    ret
.latin_ext_a5:
    ; 0x179-0x17e: odd -> +1
    cmp eax, 0x179
    jb .no_lower
    cmp eax, 0x17e
    ja .check_greek
    test eax, 1
    jz .no_lower
    inc eax
    ret

.check_greek:
    ; Greek uppercase: 0x391-0x3a1 -> +32
    cmp eax, 0x391
    jb .no_lower
    cmp eax, 0x3a1
    jbe .add32
    ; Greek: 0x3a3-0x3a9 -> +32 (skip 0x3a2 which is final sigma)
    cmp eax, 0x3a3
    jb .no_lower
    cmp eax, 0x3a9
    jbe .add32

    ; Cyrillic uppercase: 0x410-0x42f -> +32 (А-Я -> а-я)
    cmp eax, 0x410
    jb .no_lower
    cmp eax, 0x42f
    jbe .add32
    ; Cyrillic extended: 0x400-0x40f -> +80 (Ѐ-Џ -> ѐ-џ)
    ; (These are before 0x410 numerically but checked after due to frequency)
    cmp eax, 0x400
    jb .no_lower
    cmp eax, 0x40f
    ja .cyrillic_ext
    add eax, 80
    ret
.cyrillic_ext:
    ; Cyrillic Extended: 0x460-0x481 even -> +1
    cmp eax, 0x460
    jb .no_lower
    cmp eax, 0x481
    ja .no_lower
    test eax, 1
    jnz .no_lower
    inc eax
    ret

.add32:
    add eax, 32
.no_lower:
    ret
END_FUNC sre_unicode_tolower

;; ============================================================================
;; sre_uni_isword(u32 codepoint) -> 0/1
;; Check if codepoint is a Unicode word character (letter, digit, underscore,
;; combining mark). Covers major scripts without full Unicode tables.
;; edi = codepoint (already checked not ASCII by caller)
;; ============================================================================
DEF_FUNC_BARE sre_uni_isword
    ; Underscore already handled by caller
    ; Latin Extended (letters): 0x00c0-0x00ff (excluding 0xd7 and 0xf7)
    cmp edi, 0xc0
    jb .uw_false
    cmp edi, 0xff
    ja .uw_check_latin_ext
    cmp edi, 0xd7              ; multiplication sign
    je .uw_false
    cmp edi, 0xf7              ; division sign
    je .uw_false
    jmp .uw_true
.uw_check_latin_ext:
    ; Latin Extended-A: 0x0100-0x017f
    cmp edi, 0x0100
    jb .uw_false
    cmp edi, 0x017f
    jbe .uw_true
    ; Latin Extended-B: 0x0180-0x024f
    cmp edi, 0x024f
    jbe .uw_true
    ; Combining Diacritical Marks: 0x0300-0x036f
    cmp edi, 0x0300
    jb .uw_check_ipa
    cmp edi, 0x036f
    jbe .uw_true
.uw_check_ipa:
    ; IPA Extensions: 0x0250-0x02af
    cmp edi, 0x0250
    jb .uw_false
    cmp edi, 0x02af
    jbe .uw_true
    ; Greek and Coptic: 0x0370-0x03ff
    cmp edi, 0x0370
    jb .uw_false
    cmp edi, 0x03ff
    jbe .uw_true
    ; Cyrillic: 0x0400-0x04ff
    cmp edi, 0x0400
    jb .uw_false
    cmp edi, 0x04ff
    jbe .uw_true
    ; Cyrillic Supplement: 0x0500-0x052f
    cmp edi, 0x052f
    jbe .uw_true
    ; Armenian: 0x0530-0x058f
    cmp edi, 0x058f
    jbe .uw_true
    ; Hebrew: 0x0590-0x05ff
    cmp edi, 0x05ff
    jbe .uw_true
    ; Arabic: 0x0600-0x06ff
    cmp edi, 0x06ff
    jbe .uw_true
    ; Devanagari: 0x0900-0x097f
    cmp edi, 0x0900
    jb .uw_check_thai
    cmp edi, 0x097f
    jbe .uw_true
    ; Bengali, Gurmukhi, Gujarati, etc.: 0x0980-0x0dff
    cmp edi, 0x0dff
    jbe .uw_true
.uw_check_thai:
    ; Thai: 0x0e00-0x0e7f
    cmp edi, 0x0e00
    jb .uw_check_georgian
    cmp edi, 0x0e7f
    jbe .uw_true
    ; Lao: 0x0e80-0x0eff
    cmp edi, 0x0eff
    jbe .uw_true
.uw_check_georgian:
    ; Georgian: 0x10a0-0x10ff
    cmp edi, 0x10a0
    jb .uw_check_hangul
    cmp edi, 0x10ff
    jbe .uw_true
.uw_check_hangul:
    ; Hangul Jamo: 0x1100-0x11ff
    cmp edi, 0x1100
    jb .uw_check_cjk
    cmp edi, 0x11ff
    jbe .uw_true
.uw_check_cjk:
    ; CJK Unified Ideographs: 0x4e00-0x9fff
    cmp edi, 0x4e00
    jb .uw_check_digits
    cmp edi, 0x9fff
    jbe .uw_true
    ; Hangul Syllables: 0xac00-0xd7af
    cmp edi, 0xac00
    jb .uw_check_digits
    cmp edi, 0xd7af
    jbe .uw_true
.uw_check_digits:
    ; Unicode digit ranges beyond ASCII
    ; Arabic-Indic: 0x0660-0x0669
    cmp edi, 0x0660
    jb .uw_check_digits2
    cmp edi, 0x0669
    jbe .uw_true
.uw_check_digits2:
    ; Extended Arabic-Indic: 0x06f0-0x06f9
    cmp edi, 0x06f0
    jb .uw_check_digits3
    cmp edi, 0x06f9
    jbe .uw_true
.uw_check_digits3:
    ; Devanagari digits: 0x0966-0x096f
    cmp edi, 0x0966
    jb .uw_check_digits4
    cmp edi, 0x096f
    jbe .uw_true
.uw_check_digits4:
    ; Thai digits: 0x0e50-0x0e59
    cmp edi, 0x0e50
    jb .uw_check_fullwidth
    cmp edi, 0x0e59
    jbe .uw_true
.uw_check_fullwidth:
    ; Fullwidth digits: 0xff10-0xff19, letters: 0xff21-0xff3a, 0xff41-0xff5a
    cmp edi, 0xff10
    jb .uw_check_connector
    cmp edi, 0xff19
    jbe .uw_true
    cmp edi, 0xff21
    jb .uw_false
    cmp edi, 0xff3a
    jbe .uw_true
    cmp edi, 0xff41
    jb .uw_false
    cmp edi, 0xff5a
    jbe .uw_true
.uw_check_connector:
    ; Connector punctuation (word chars in Python): 0x203f-0x2040, 0xfe33-0xfe34, 0xfe4d-0xfe4f, 0xff3f
    cmp edi, 0x203f
    je .uw_true
    cmp edi, 0x2040
    je .uw_true
.uw_false:
    xor eax, eax
    ret
.uw_true:
    mov eax, 1
    ret
END_FUNC sre_uni_isword

