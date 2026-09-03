; sre_template.asm - the replacement-template mini-language
;
; Match.expand(), and the string form of Pattern.sub()/subn().  Both were
; stubs: expand() handed the template straight back and sub() concatenated the
; replacement verbatim, so `\1`, `\g<name>` and every escape came out as their
; own source text.
;
; A parsed template is an ordinary list whose elements are either a str -- a
; literal run -- or an int, the number of a group to substitute.  That is the
; same shape re._parser.parse_template builds, and it lets sub() parse once
; and expand per match, as CPython's does.
;
; The syntax is _parser.parse_template's, which is NOT the pattern syntax:
;   \g<name>  \g<7>     a group, by name or by number
;   \1 .. \99            a group, by number
;   \0, \012             an octal character (three octal digits after a \0..\7
;                        are an octal escape, not a two-digit group)
;   \n \t \r \v \f \a \b \\   the fixed escapes; \b is a backspace here, not a
;                        word boundary
;   \<anything else>     an ASCII letter is an error, anything else keeps the
;                        backslash and the character both

%include "macros.inc"
%include "object.inc"
%include "sre.inc"

extern ap_malloc
extern ap_free
extern obj_decref
extern sre_new_slice
extern str_type
extern list_new
extern list_append
extern dict_get
extern str_method_join
extern sre_match_get_group_str
extern none_singleton
extern obj_as_index
extern raise_exception
extern exc_IndexError_type
extern exc_ValueError_type
extern exc_MemoryError_type

section .text

;; ============================================================================
;; sre_template_parse(rdi = the template, rsi = pattern) -> rax = a list
;;
;; Literal runs become str -- or bytes, for a bytes pattern -- elements, and
;; group references become int elements.  Which kind the pieces are is the
;; pattern's, and it is the only difference between the two: the escape
;; syntax is identical, and a bytes template is scanned as bytes because that
;; is what it already does.
;; Raises rather than answering NULL: every caller is a builtin method with a
;; live frame above it.
;; ============================================================================
TP_TMPL   equ 8
TP_PAT    equ 16
TP_DATA   equ 24
TP_LEN    equ 32
TP_BUF    equ 40
TP_BLEN   equ 48
TP_LIST   equ 56
TP_POS    equ 64
TP_NGROUP equ 72
TP_SCRATCH equ 80
TP_ISBYTES equ 88           ; the pattern's kind, which the pieces take
TP_FRAME  equ 96            ; + 0 pushes = 96

DEF_FUNC sre_template_parse, TP_FRAME
    mov [rbp - TP_TMPL], rdi
    mov [rbp - TP_PAT], rsi
    mov qword [rbp - TP_BUF], 0
    mov qword [rbp - TP_LIST], 0

    mov eax, [rsi + SRE_PatternObject.groups]
    mov [rbp - TP_NGROUP], rax

    ; bytes keeps its data at +24 and str at +40; the length is at +16 in
    ; both.
    mov rcx, [rsi + SRE_PatternObject.is_bytes]
    mov [rbp - TP_ISBYTES], rcx
    test rcx, rcx
    jz .tp_str_data
    lea rax, [rdi + PyBytesObject.data]
    jmp .tp_have_data
.tp_str_data:
    lea rax, [rdi + PyStrObject.data]
.tp_have_data:
    mov [rbp - TP_DATA], rax
    mov rax, [rdi + PyStrObject.ob_size]
    mov [rbp - TP_LEN], rax

    ; A literal is never longer than its source, except that an octal escape
    ; can name a code point above 127 and so take two UTF-8 bytes where it
    ; took four in the source.  Twice the length is comfortably enough.
    lea rdi, [rax + rax + 8]
    call ap_malloc
    test rax, rax
    jz .tp_oom
    mov [rbp - TP_BUF], rax
    mov qword [rbp - TP_BLEN], 0

    xor edi, edi
    call list_new
    test rax, rax
    jz .tp_oom
    mov [rbp - TP_LIST], rax

    mov qword [rbp - TP_POS], 0
.tp_loop:
    mov rcx, [rbp - TP_POS]
    cmp rcx, [rbp - TP_LEN]
    jge .tp_done
    mov rdx, [rbp - TP_DATA]
    movzx eax, byte [rdx + rcx]
    cmp al, '\'
    je .tp_escape
    call .tp_emit
    inc qword [rbp - TP_POS]
    jmp .tp_loop

.tp_done:
    call .tp_flush
    mov rax, [rbp - TP_LIST]
    mov [rbp - TP_SCRATCH], rax
    mov rdi, [rbp - TP_BUF]
    call ap_free
    mov rax, [rbp - TP_SCRATCH]
    leave
    ret

;; --- the escape sequences -------------------------------------------------
.tp_escape:
    ; rcx = the index of the backslash
    inc rcx
    cmp rcx, [rbp - TP_LEN]
    jge .tp_trailing
    movzx eax, byte [rdx + rcx]
    mov [rbp - TP_POS], rcx     ; the escape character's own index

    cmp al, 'g'
    je .tp_gref
    cmp al, '0'
    je .tp_octal
    cmp al, '1'
    jb .tp_fixed
    cmp al, '9'
    jbe .tp_digit

.tp_fixed:
    cmp al, 'n'
    je .tp_fix_n
    cmp al, 't'
    je .tp_fix_t
    cmp al, 'r'
    je .tp_fix_r
    cmp al, 'v'
    je .tp_fix_v
    cmp al, 'f'
    je .tp_fix_f
    cmp al, 'a'
    je .tp_fix_a
    cmp al, 'b'
    je .tp_fix_b
    cmp al, '\'
    je .tp_fix_bs

    ; An ASCII letter with no meaning is an error; anything else keeps both
    ; characters, so that r'\.' stays a backslash and a dot.
    mov cl, al
    or  cl, 0x20
    cmp cl, 'a'
    jb .tp_keep_both
    cmp cl, 'z'
    jbe .tp_bad_escape
.tp_keep_both:
    push rax
    mov al, '\'
    call .tp_emit
    pop rax
    call .tp_emit
    inc qword [rbp - TP_POS]
    jmp .tp_loop

.tp_fix_n:
    mov al, 10
    jmp .tp_fix_emit
.tp_fix_t:
    mov al, 9
    jmp .tp_fix_emit
.tp_fix_r:
    mov al, 13
    jmp .tp_fix_emit
.tp_fix_v:
    mov al, 11
    jmp .tp_fix_emit
.tp_fix_f:
    mov al, 12
    jmp .tp_fix_emit
.tp_fix_a:
    mov al, 7
    jmp .tp_fix_emit
.tp_fix_b:
    mov al, 8
    jmp .tp_fix_emit
.tp_fix_bs:
    mov al, '\'
.tp_fix_emit:
    call .tp_emit
    inc qword [rbp - TP_POS]
    jmp .tp_loop

;; \0 opens an octal escape of up to three digits in total.
.tp_octal:
    xor r8d, r8d                ; the value
    mov r9d, 1                  ; digits taken, counting the 0
.tp_oct_more:
    cmp r9d, 3
    jge .tp_oct_done
    mov rcx, [rbp - TP_POS]
    inc rcx
    cmp rcx, [rbp - TP_LEN]
    jge .tp_oct_done
    mov rdx, [rbp - TP_DATA]
    movzx eax, byte [rdx + rcx]
    cmp al, '0'
    jb .tp_oct_done
    cmp al, '7'
    ja .tp_oct_done
    sub al, '0'
    shl r8d, 3
    movzx eax, al
    or  r8d, eax
    mov [rbp - TP_POS], rcx
    inc r9d
    jmp .tp_oct_more
.tp_oct_done:
    mov eax, r8d
    call .tp_emit_cp
    inc qword [rbp - TP_POS]
    jmp .tp_loop

;; \1..\9 is a group number -- unless three octal digits follow the
;; backslash, which is an octal escape instead.
.tp_digit:
    movzx r8d, al
    sub r8d, '0'                ; the first digit
    mov r10d, 1                 ; how many digits taken

    mov rcx, [rbp - TP_POS]
    inc rcx
    cmp rcx, [rbp - TP_LEN]
    jge .tp_dig_have
    mov rdx, [rbp - TP_DATA]
    movzx eax, byte [rdx + rcx]
    cmp al, '0'
    jb .tp_dig_have
    cmp al, '9'
    ja .tp_dig_have
    ; A second digit.  Both it and the first must be octal, and a third octal
    ; digit must follow, for this to be an octal escape rather than a group.
    movzx r9d, al
    sub r9d, '0'
    mov [rbp - TP_POS], rcx
    mov r10d, 2
    cmp r8d, 7
    ja .tp_dig_two
    cmp r9d, 7
    ja .tp_dig_two
    inc rcx
    cmp rcx, [rbp - TP_LEN]
    jge .tp_dig_two
    movzx eax, byte [rdx + rcx]
    cmp al, '0'
    jb .tp_dig_two
    cmp al, '7'
    ja .tp_dig_two
    ; Three octal digits.
    movzx eax, al
    sub eax, '0'
    mov [rbp - TP_POS], rcx
    mov ecx, r8d
    shl ecx, 6
    mov edx, r9d
    shl edx, 3
    or  ecx, edx
    or  ecx, eax
    mov eax, ecx
    call .tp_emit_cp
    inc qword [rbp - TP_POS]
    jmp .tp_loop

.tp_dig_two:
    imul r8d, r8d, 10
    add r8d, r9d
.tp_dig_have:
    mov eax, r8d
    call .tp_addgroup
    inc qword [rbp - TP_POS]
    jmp .tp_loop

;; \g<name> or \g<number>
.tp_gref:
    mov rcx, [rbp - TP_POS]
    inc rcx
    cmp rcx, [rbp - TP_LEN]
    jge .tp_missing_lt
    mov rdx, [rbp - TP_DATA]
    cmp byte [rdx + rcx], '<'
    jne .tp_missing_lt
    inc rcx
    mov r9, rcx                 ; the name's first byte
.tp_gref_scan:
    cmp rcx, [rbp - TP_LEN]
    jge .tp_unterminated
    cmp byte [rdx + rcx], '>'
    je .tp_gref_end
    inc rcx
    jmp .tp_gref_scan
.tp_gref_end:
    mov [rbp - TP_POS], rcx     ; the '>' itself; the tail step passes it
    mov r10, rcx
    sub r10, r9                 ; the name's length
    test r10, r10
    jz .tp_bad_name

    ; All digits: a number, not a name.
    mov r8, r9
    xor r11d, r11d              ; the accumulated number
.tp_gref_digits:
    cmp r8, rcx
    jge .tp_gref_number
    movzx eax, byte [rdx + r8]
    cmp al, '0'
    jb .tp_gref_byname
    cmp al, '9'
    ja .tp_gref_byname
    imul r11d, r11d, 10
    movzx eax, al
    sub eax, '0'
    add r11d, eax
    inc r8
    jmp .tp_gref_digits
.tp_gref_number:
    mov eax, r11d
    call .tp_addgroup
    inc qword [rbp - TP_POS]
    jmp .tp_loop

.tp_gref_byname:
    ; Look the name up in the pattern's groupindex.
    mov rdi, [rbp - TP_PAT]
    mov rdi, [rdi + SRE_PatternObject.groupindex]
    test rdi, rdi
    jz .tp_unknown_name
    mov [rbp - TP_SCRATCH], rdi
    mov rdi, [rbp - TP_DATA]
    add rdi, r9
    mov rsi, r10
    mov edx, [rbp - TP_ISBYTES]
    call sre_new_slice
    test rax, rax
    jz .tp_oom
    mov rsi, rax                ; the name, ours
    mov rdi, [rbp - TP_SCRATCH]
    push rsi
    sub rsp, 8
    call dict_get
    add rsp, 8
    pop rdi
    push rax
    sub rsp, 8
    call obj_decref             ; the name string
    add rsp, 8
    pop rax
    test rax, rax
    jz .tp_unknown_name
    V_UNPACK rax, rdx
    mov rdi, rax
    call obj_as_index           ; the index may be a heap int
    call .tp_addgroup
    inc qword [rbp - TP_POS]
    jmp .tp_loop

;; --- the buffer, and the pieces it becomes --------------------------------

;; .tp_emit(al = one byte)
.tp_emit:
    mov rdx, [rbp - TP_BUF]
    mov rcx, [rbp - TP_BLEN]
    mov [rdx + rcx], al
    inc qword [rbp - TP_BLEN]
    ret

;; .tp_emit_cp(eax = a code point, 0..255) -- UTF-8, so an octal escape above
;; 127 is one character and not one byte.
.tp_emit_cp:
    cmp eax, 0x80
    jb .tp_emit
    push rax
    shr eax, 6
    or  al, 0xC0
    call .tp_emit
    pop rax
    and al, 0x3F
    or  al, 0x80
    jmp .tp_emit

;; .tp_flush -- the pending literal, if any, becomes the next list element.
;; Reached by `call`, so rsp arrives 8 below alignment; the sub/add pair is
;; what keeps every call below it on a 16-byte boundary.
.tp_flush:
    sub rsp, 8
    cmp qword [rbp - TP_BLEN], 0
    je .tp_flush_none
    mov rdi, [rbp - TP_BUF]
    mov rsi, [rbp - TP_BLEN]
    mov edx, [rbp - TP_ISBYTES]
    call sre_new_slice
    test rax, rax
    jz .tp_oom
    push rax
    sub rsp, 8
    mov rdi, [rbp - TP_LIST]
    mov rsi, rax
    call list_append
    add rsp, 8
    pop rdi
    call obj_decref
    mov qword [rbp - TP_BLEN], 0
.tp_flush_none:
    add rsp, 8
    ret

;; .tp_addgroup(eax = group number) -- flush the literal, then record it.
.tp_addgroup:
    cmp rax, [rbp - TP_NGROUP]
    ja .tp_bad_group
    push rax                    ; and this restores the 16-byte alignment
    call .tp_flush
    pop rax
    push rax
    V_PACK_I64 rax, rcx
    mov rsi, rax
    mov rdi, [rbp - TP_LIST]
    call list_append
    pop rax
    ret

;; --- the ways it can go wrong ---------------------------------------------
;; Every one of these abandons the frame, so the buffer and the list have to
;; go first or a loop that keeps catching the error keeps leaking them.
.tp_release:
    sub rsp, 8
    mov rdi, [rbp - TP_BUF]
    test rdi, rdi
    jz .tp_release_list
    mov qword [rbp - TP_BUF], 0
    call ap_free
.tp_release_list:
    mov rdi, [rbp - TP_LIST]
    test rdi, rdi
    jz .tp_released
    mov qword [rbp - TP_LIST], 0
    call obj_decref
.tp_released:
    add rsp, 8
    ret

.tp_bad_group:
    call .tp_release
    RAISE exc_IndexError_type, "invalid group reference"
.tp_unknown_name:
    call .tp_release
    RAISE exc_IndexError_type, "unknown group name"
.tp_bad_name:
    call .tp_release
    RAISE exc_ValueError_type, "missing group name"
.tp_missing_lt:
    call .tp_release
    RAISE exc_ValueError_type, "missing <"
.tp_unterminated:
    call .tp_release
    RAISE exc_ValueError_type, "missing >, unterminated name"
.tp_trailing:
    call .tp_release
    RAISE exc_ValueError_type, "bad escape (end of pattern)"
.tp_bad_escape:
    call .tp_release
    RAISE exc_ValueError_type, "bad escape"
.tp_oom:
    call .tp_release
    RAISE exc_MemoryError_type, "out of memory"
END_FUNC sre_template_parse


;; ============================================================================
;; sre_template_expand(rdi = a parsed template, rsi = a match) -> rax = str
;;
;; A group that did not match contributes the empty string, which is
;; expand_template's `g(group) or empty`.
;; ============================================================================
TE_TMPL   equ 8
TE_MATCH  equ 16
TE_OUT    equ 24
TE_SEP    equ 32
TE_IDX    equ 40
TE_ISBYTES equ 48           ; the match's pattern kind: the pieces take it
TE_FRAME  equ 64            ; + 0 pushes = 64

DEF_FUNC sre_template_expand, TE_FRAME
    mov [rbp - TE_TMPL], rdi
    mov [rbp - TE_MATCH], rsi
    mov qword [rbp - TE_OUT], 0
    mov qword [rbp - TE_SEP], 0
    ; Which kind the pieces are is the match's pattern's.
    mov rax, [rsi + SRE_MatchObject.pattern]
    mov rax, [rax + SRE_PatternObject.is_bytes]
    mov [rbp - TE_ISBYTES], rax

    xor edi, edi
    call list_new
    test rax, rax
    jz .te_oom
    mov [rbp - TE_OUT], rax

    mov qword [rbp - TE_IDX], 0
.te_loop:
    mov rdi, [rbp - TE_TMPL]
    mov rcx, [rbp - TE_IDX]
    cmp rcx, [rdi + PyListObject.ob_size]
    jge .te_join
    mov rdx, [rdi + PyListObject.ob_item]
    mov rax, [rdx + rcx*8]

    ; A str is a literal; anything else is the int naming a group.
    V_TEST_PTR rax, rcx
    ja .te_group
    ; A literal piece is a str -- or a bytes, for a bytes template.  Testing
    ; only for str sent every bytes literal down the group branch, where it
    ; was read as an index.
    mov rcx, [rax + PyObject.ob_type]
    lea rdx, [rel str_type]
    cmp rcx, rdx
    je .te_literal
    extern bytes_type
    lea rdx, [rel bytes_type]
    cmp rcx, rdx
    jne .te_group
.te_literal:

    mov rdi, [rbp - TE_OUT]
    mov rsi, rax
    call list_append            ; takes its own reference
    jmp .te_next

.te_group:
    V_UNPACK rax, rdx
    mov rdi, rax
    call obj_as_index           ; boxed whenever INT_STRESS=1 is on
    mov rsi, rax
    mov rdi, [rbp - TE_MATCH]
    call sre_match_get_group_str
    ; None is an ordinary singleton and so arrives with TAG_PTR like any other
    ; pointer -- the tag says nothing about which object it is.
    lea rcx, [rel none_singleton]
    cmp rax, rcx
    jne .te_group_have
    mov rdi, rax
    call obj_decref             ; the reference get_group_str just took
    ; Unmatched: the empty string.
    lea rdi, [rel te_nothing]
    xor esi, esi
    mov edx, [rbp - TE_ISBYTES]
    call sre_new_slice
    test rax, rax
    jz .te_oom
.te_group_have:
    mov [rbp - TE_SEP], rax     ; borrow the slot: it is free until the join
    mov rdi, [rbp - TE_OUT]
    mov rsi, rax
    call list_append
    mov rdi, [rbp - TE_SEP]
    call obj_decref
    mov qword [rbp - TE_SEP], 0

.te_next:
    inc qword [rbp - TE_IDX]
    jmp .te_loop

.te_join:
    ; "".join(pieces) -- or b"".join, for a bytes pattern.
    lea rdi, [rel te_nothing]
    xor esi, esi
    mov edx, [rbp - TE_ISBYTES]
    call sre_new_slice
    test rax, rax
    jz .te_oom
    mov [rbp - TE_SEP], rax

    sub rsp, 16
    mov [rsp], rax
    mov rax, [rbp - TE_OUT]
    mov [rsp + 8], rax
    mov rdi, rsp
    mov esi, 2
    cmp qword [rbp - TE_ISBYTES], 0
    jne .te_join_bytes
    call str_method_join
    jmp .te_joined
.te_join_bytes:
    extern bytes_method_join
    call bytes_method_join
.te_joined:
    add rsp, 16
    V_UNPACK rax, rdx
    mov [rbp - TE_IDX], rax

    mov rdi, [rbp - TE_SEP]
    call obj_decref
    mov rdi, [rbp - TE_OUT]
    call obj_decref
    mov rax, [rbp - TE_IDX]
    leave
    ret

.te_oom:
    mov rdi, [rbp - TE_OUT]
    test rdi, rdi
    jz .te_oom_raise
    mov qword [rbp - TE_OUT], 0
    call obj_decref
.te_oom_raise:
    RAISE exc_MemoryError_type, "out of memory"

section .rodata
te_nothing: db 0
section .text
END_FUNC sre_template_expand
