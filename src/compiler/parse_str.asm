; parse_str.asm - string literals, for the source parser
;
; A string literal is where the tokenizer stops and the parser starts doing
; character work: the escapes, the \N{...} names, the UTF-8 the escapes emit,
; the implicit concatenation of adjacent literals, and the decision that a run
; of them contains an f-string.  Split out of parse.asm, which had reached the
; 100k cap for a hand-written file; the seam is the one it already had.

%include "macros.inc"
%include "object.inc"
%include "value.inc"
%include "opcodes.inc"
%include "compiler.inc"

extern ast_end_at
extern ast_make
extern ast_mark
extern ast_obj
extern ast_push
extern buf_free
extern buf_init
extern buf_push_u8
extern bytes_from_data
extern comp_error_span
extern comp_intern
extern comp_msg_cstr
extern comp_msg_i64
extern comp_msg_start
extern exc_SyntaxError_type
extern par_advance
extern par_finish_list
extern par_fstring_pieces
extern par_kind
extern par_peek
extern par_syntax_error
extern uniname_lookup

section .text

;; ============================================================================
;; par_utf8_emit(Buf *b, uint32_t cp) -> void
;; Append one code point as UTF-8.  \x, \u, \U and \N all funnel through here,
;; so a string literal's bytes and its code-point count agree by construction.
;; ============================================================================
DEF_FUNC par_utf8_emit, 16
    push rbx
    push r12
    mov rbx, rdi
    mov r12, rsi

    cmp r12, 0x80
    jb .one
    cmp r12, 0x800
    jb .two
    cmp r12, 0x10000
    jb .three

    mov rsi, r12
    shr rsi, 18
    or esi, 0xf0
    mov rdi, rbx
    call buf_push_u8
    mov rsi, r12
    shr rsi, 12
    and esi, 0x3f
    or esi, 0x80
    mov rdi, rbx
    call buf_push_u8
    jmp .tail2
.three:
    mov rsi, r12
    shr rsi, 12
    or esi, 0xe0
    mov rdi, rbx
    call buf_push_u8
.tail2:
    mov rsi, r12
    shr rsi, 6
    and esi, 0x3f
    or esi, 0x80
    mov rdi, rbx
    call buf_push_u8
    jmp .tail1
.two:
    mov rsi, r12
    shr rsi, 6
    or esi, 0xc0
    mov rdi, rbx
    call buf_push_u8
.tail1:
    mov rsi, r12
    and esi, 0x3f
    or esi, 0x80
    mov rdi, rbx
    call buf_push_u8
    jmp .done
.one:
    mov rdi, rbx
    mov rsi, r12
    call buf_push_u8
.done:
    pop r12
    pop rbx
    leave
    ret
END_FUNC par_utf8_emit

;; ============================================================================
;; par_hexval(int ch) -> eax = 0..15, or -1
;; ============================================================================
DEF_FUNC_BARE par_hexval
    mov eax, edi
    sub eax, '0'
    cmp eax, 9
    jbe .done
    mov eax, edi
    or eax, 0x20
    sub eax, 'a'
    cmp eax, 5
    ja .bad
    add eax, 10
.done:
    ret
.bad:
    mov eax, -1
    ret
END_FUNC par_hexval

;; ============================================================================
;; par_escape_one(Comp *c, Buf *out, const char *p, const char *end, int bytes,
;;                 const char *content) -> rax = the position just past the
;;                 escape, or 0 on error
;;
;; `p` points at the character AFTER the backslash.  Factored out of
;; par_string_body so that the literal parts of an f-string get the same
;; escapes: they had a private decoder that knew only \n, \t and \r, so
;; f"\x41" printed "x41" while "\x41" printed "A".
;; ============================================================================
PSE_COMP  equ 8
PSE_OUT   equ 16
PSE_P     equ 24
PSE_END   equ 32
PSE_BYTES equ 40
PSE_ACC   equ 48
PSE_CONT  equ 56          ; where the literal's content starts, for the message
PSE_ESC   equ 64          ; and where THIS escape's backslash is
PSE_WANT  equ 72          ; how many hex digits it wanted, for the message
PSE_FRAME equ 88          ; + 3 pushes = 112, 16-aligned
DEF_FUNC par_escape_one, PSE_FRAME
    push rbx
    push r12
    push r13
    mov rbx, rdi
    mov [rbp - PSE_COMP], rdi
    mov [rbp - PSE_OUT], rsi
    mov [rbp - PSE_CONT], r9
    lea rax, [rdx - 1]                  ; the backslash itself
    mov [rbp - PSE_ESC], rax
    mov qword [rbp - PSE_WANT], 2
    mov [rbp - PSE_P], rdx
    mov [rbp - PSE_END], rcx
    mov [rbp - PSE_BYTES], r8

    mov r12, [rbp - PSE_P]
    cmp r12, [rbp - PSE_END]
    jae .pe_bad
    movzx eax, byte [r12]
    inc qword [rbp - PSE_P]

    cmp al, 10                          ; a backslash-newline vanishes
    je .pe_done
    cmp al, 'n'
    je .e_nl
    cmp al, 't'
    je .e_tab
    cmp al, 'r'
    je .e_cr
    cmp al, 92
    je .e_literal
    cmp al, 39
    je .e_literal
    cmp al, 34
    je .e_literal
    cmp al, '0'
    jb .e_unknown
    cmp al, '7'
    jbe .e_octal
    cmp al, 'a'
    je .e_bell
    cmp al, 'b'
    je .e_bs
    cmp al, 'f'
    je .e_ff
    cmp al, 'v'
    je .e_vt
    cmp al, 'x'
    je .e_hex2
    cmp al, 'u'
    je .e_hex4
    cmp al, 'U'
    je .e_hex8
    cmp al, 'N'
    je .e_named
.e_unknown:
    ; An unrecognised escape keeps the backslash, as Python does (with a
    ; SyntaxWarning it does not raise on).
    push rax
    mov rdi, [rbp - PSE_OUT]
    mov esi, 92
    call buf_push_u8
    pop rax
.e_literal:
    mov rdi, [rbp - PSE_OUT]
    mov esi, eax
    call buf_push_u8
    jmp .pe_done
.e_nl:   mov eax, 10
         jmp .e_literal
.e_tab:  mov eax, 9
         jmp .e_literal
.e_cr:   mov eax, 13
         jmp .e_literal
.e_bell: mov eax, 7
         jmp .e_literal
.e_bs:   mov eax, 8
         jmp .e_literal
.e_ff:   mov eax, 12
         jmp .e_literal
.e_vt:   mov eax, 11
         jmp .e_literal

.e_octal:
    ; Up to three octal digits, counting the one already consumed.
    sub eax, '0'
    mov [rbp - PSE_ACC], rax
    mov ecx, 2
.oct_loop:
    mov r12, [rbp - PSE_P]
    cmp r12, [rbp - PSE_END]
    jae .oct_done
    movzx eax, byte [r12]
    cmp al, '0'
    jb .oct_done
    cmp al, '7'
    ja .oct_done
    sub eax, '0'
    mov rdx, [rbp - PSE_ACC]
    shl rdx, 3
    or rdx, rax
    mov [rbp - PSE_ACC], rdx
    inc qword [rbp - PSE_P]
    dec ecx
    jnz .oct_loop
.oct_done:
    mov rdi, [rbp - PSE_OUT]
    mov rsi, [rbp - PSE_ACC]
    cmp qword [rbp - PSE_BYTES], 0
    jne .raw_byte
    call par_utf8_emit
    jmp .pe_done
.raw_byte:
    and esi, 0xff
    call buf_push_u8
    jmp .pe_done

.e_named:
    ; \N{NAME}.  In a bytes literal it is not an escape at all -- CPython
    ; leaves the backslash in place with a warning -- so only str takes it.
    cmp qword [rbp - PSE_BYTES], 0
    jne .e_unknown
    mov r12, [rbp - PSE_P]
    cmp r12, [rbp - PSE_END]
    jae .pe_bad
    cmp byte [r12], '{'
    jne .pe_bad
    inc r12
    mov r13, r12                        ; where the name starts
.named_scan:
    cmp r12, [rbp - PSE_END]
    jae .pe_bad
    cmp byte [r12], '}'
    je .named_close
    inc r12
    jmp .named_scan
.named_close:
    mov rdi, r13
    mov rsi, r12
    sub rsi, r13
    inc r12
    mov [rbp - PSE_P], r12
    extern uniname_lookup
    call uniname_lookup
    cmp rax, -1
    je .named_unknown
    mov rsi, rax
    mov rdi, [rbp - PSE_OUT]
    call par_utf8_emit
    jmp .pe_done
.named_unknown:
    ; r12 is one past the closing brace; the escape began two bytes before
    ; the `N`, which is what PSE_P pointed at on the way in.
    mov rdi, rbx
    mov rsi, [rbp - PSE_CONT]
    mov rdx, [rbp - PSE_ESC]
    mov rcx, r12
    sub rcx, rdx                        ; the escape's length
    CSTRING r8, "unknown Unicode character name"
    mov r9, [rbp - PSE_BYTES]
    call par_escape_error
    xor eax, eax
    pop r13
    pop r12
    pop rbx
    leave
    ret

.e_hex2:
    mov r13d, 2
    jmp .hex_common
.e_hex4:
    mov r13d, 4
    jmp .hex_common
.e_hex8:
    mov r13d, 8
.hex_common:
    mov [rbp - PSE_WANT], r13
    ; \u and \U have no meaning in a bytes literal; only \x does.
    cmp r13d, 2
    je .hex_go
    cmp qword [rbp - PSE_BYTES], 0
    jne .pe_bad
.hex_go:
    mov qword [rbp - PSE_ACC], 0
.hex_loop:
    test r13d, r13d
    jz .hex_done
    mov r12, [rbp - PSE_P]
    cmp r12, [rbp - PSE_END]
    jae .pe_bad
    movzx edi, byte [r12]
    call par_hexval
    cmp eax, -1
    je .pe_bad
    mov rdx, [rbp - PSE_ACC]
    shl rdx, 4
    or rdx, rax
    mov [rbp - PSE_ACC], rdx
    inc qword [rbp - PSE_P]
    dec r13d
    jmp .hex_loop
.hex_done:
    mov rsi, [rbp - PSE_ACC]
    cmp qword [rbp - PSE_BYTES], 0
    jne .hex_byte
    mov rdi, [rbp - PSE_OUT]
    call par_utf8_emit
    jmp .pe_done
.hex_byte:
    mov rdi, [rbp - PSE_OUT]
    and esi, 0xff
    call buf_push_u8
    jmp .pe_done

.pe_done:
    mov rax, [rbp - PSE_P]
    pop r13
    pop r12
    pop rbx
    leave
    ret
.pe_bad:
    ; CPython's wording is the codec's, wrapped, and it names the escape it
    ; could not finish: \x wants two hex digits, \u four, \U eight.  The
    ; span it reports is the escape as WRITTEN -- the backslash, the letter
    ; and whatever digits were there -- which PSE_P has already walked to.
    mov rdi, rbx
    mov rsi, [rbp - PSE_CONT]
    mov rdx, [rbp - PSE_ESC]
    mov rcx, [rbp - PSE_P]
    sub rcx, rdx
    CSTRING r8, "truncated \xXX escape"
    cmp qword [rbp - PSE_WANT], 4
    jne .pe_bad_not_u
    CSTRING r8, "truncated \uXXXX escape"
.pe_bad_not_u:
    cmp qword [rbp - PSE_WANT], 8
    jne .pe_bad_have_why
    CSTRING r8, "truncated \UXXXXXXXX escape"
.pe_bad_have_why:
    mov r9, [rbp - PSE_BYTES]
    call par_escape_error
    xor eax, eax
    pop r13
    pop r12
    pop rbx
    leave
    ret
END_FUNC par_escape_one

;; ============================================================================
;; par_string_body(Comp *c, Token *t, Buf *out) -> rax = 1 ok, 0 error
;;
;; Decodes one string token into `out`.  The token still carries its prefix and
;; quotes, so the span is found here rather than in the lexer -- the lexer's job
;; was to find where the literal ended, which is a different question from what
;; it means.
;;
;; In a raw literal a backslash stays in the output but still escapes a quote
;; for the purposes of finding the end, which is why r"\" is unterminated in
;; Python as well.
;; ============================================================================
PB_TOK   equ 16
PB_OUT   equ 24
PB_P     equ 32
PB_END   equ 40
PB_RAW   equ 48
PB_BYTES equ 56
PB_CONTENT equ 64        ; the first byte after the opening quotes
PB_FRAME equ 72          ; + 3 pushes = 96
DEF_FUNC par_string_body, PB_FRAME
    push rbx
    push r12
    push r13
    mov rbx, rdi
    mov [rbp - PB_TOK], rsi
    mov [rbp - PB_OUT], rdx

    movzx eax, word [rsi + Token.flags]
    xor ecx, ecx
    test eax, TF_STR_RAW
    setnz cl
    mov [rbp - PB_RAW], rcx
    xor ecx, ecx
    test eax, TF_STR_BYTES
    setnz cl
    mov [rbp - PB_BYTES], rcx

    mov r12, [rsi + Token.start]
    mov r13d, [rsi + Token.len]
    add r13, r12                        ; one past the literal

    ; Skip the prefix letters, then the opening quote run.
.skip_prefix:
    movzx eax, byte [r12]
    cmp al, 39                          ; '
    je .at_quote
    cmp al, 34                          ; "
    je .at_quote
    inc r12
    jmp .skip_prefix
.at_quote:
    movzx ecx, byte [r12]
    mov edx, 1
    lea rax, [r12 + 2]
    cmp rax, r13
    jae .have_quotes
    movzx eax, byte [r12 + 1]
    cmp eax, ecx
    jne .have_quotes
    movzx eax, byte [r12 + 2]
    cmp eax, ecx
    jne .have_quotes
    mov edx, 3
.have_quotes:
    add r12, rdx
    sub r13, rdx                        ; drop the closing quote run
    mov [rbp - PB_P], r12
    mov [rbp - PB_CONTENT], r12
    mov [rbp - PB_END], r13

.loop:
    mov r12, [rbp - PB_P]
    cmp r12, [rbp - PB_END]
    jae .ok
    movzx eax, byte [r12]
    cmp al, 92                          ; backslash
    je .escape
    mov rdi, [rbp - PB_OUT]
    mov esi, eax
    call buf_push_u8
    inc qword [rbp - PB_P]
    jmp .loop

.escape:
    cmp qword [rbp - PB_RAW], 0
    je .real_escape
    ; Raw: the backslash is data, and so is whatever follows it.
    mov rdi, [rbp - PB_OUT]
    mov esi, 92
    call buf_push_u8
    inc qword [rbp - PB_P]
    jmp .loop

.real_escape:
    ; The escape decoder is shared with the literal parts of f-strings.
    mov rdi, rbx
    mov rsi, [rbp - PB_OUT]
    mov rdx, [rbp - PB_P]
    inc rdx                             ; past the backslash
    mov rcx, [rbp - PB_END]
    mov r8, [rbp - PB_BYTES]
    mov r9, [rbp - PB_CONTENT]
    call par_escape_one
    test rax, rax
    jz .failed
    mov [rbp - PB_P], rax
    jmp .loop

.ok:
    mov eax, 1
    pop r13
    pop r12
    pop rbx
    leave
    ret
.failed:
    xor eax, eax
    pop r13
    pop r12
    pop rbx
    leave
    ret
.fstring_unsupported:
    mov rdi, rbx
    CSTRING rsi, "f-strings are not supported yet"
    call par_syntax_error
    xor eax, eax
    pop r13
    pop r12
    pop rbx
    leave
    ret
END_FUNC par_string_body

;; ============================================================================
;; pf_string(Comp *c) -> node
;;
;; Adjacent string literals concatenate: "a" "b" is one constant, not two.  The
;; whole run is consumed here, which is why it happens in the prefix handler
;; rather than as an infix operator -- there is no operator to speak of.
;; ============================================================================
PS2_LINE  equ 16
PS2_BYTES equ 24
PS2_BUF   equ 64         ; a Buf lives here
PS2_FRAME equ 72         ; + 1 push = 80
global pf_string
DEF_FUNC pf_string, PS2_FRAME
    push rbx
    mov rbx, rdi

    call par_peek
    TOK_POS rax
    mov [rbp - PS2_LINE], rcx
    movzx ecx, word [rax + Token.flags]
    and ecx, TF_STR_BYTES
    mov [rbp - PS2_BYTES], rcx

    ; Adjacent literals concatenate even when only some of them are f-strings,
    ; so the whole run is checked before deciding which shape to build.
    mov rdi, rbx
    call par_run_has_fstring
    test eax, eax
    jnz .fstring_run

    lea rdi, [rbp - PS2_BUF]
    mov esi, 1
    call buf_init

.piece:
    mov rdi, rbx
    call par_peek
    mov rsi, rax
    ; Mixing bytes and str in one concatenation is an error, not a coercion.
    movzx ecx, word [rsi + Token.flags]
    and ecx, TF_STR_BYTES
    cmp rcx, [rbp - PS2_BYTES]
    jne .mixed
    mov rdi, rbx
    lea rdx, [rbp - PS2_BUF]
    call par_string_body
    test eax, eax
    jz .fail
    mov rdi, rbx
    call par_advance
    mov rdi, rbx
    call par_kind
    cmp eax, TOK_STRING
    je .piece

    mov rdi, [rbp - PS2_BUF + Buf.data]
    mov rsi, [rbp - PS2_BUF + Buf.len]
    cmp qword [rbp - PS2_BYTES], 0
    jne .make_bytes
    ; comp_intern, not str_new_heap: this is where a plain string LITERAL
    ; becomes an object, and comp_intern is what decides whether it is shared.
    ; comp_intern's docblock claimed every string went through it; this call
    ; site was the counterexample, and an intern table it never reached is a
    ; table that does nothing for constants.
    call comp_intern
    jmp .have_object
.make_bytes:
    call bytes_from_data
.have_object:
    test rax, rax
    jz .fail
    mov rdi, rbx
    mov rsi, rax
    call ast_obj
    mov r8, rax

    push r8
    lea rdi, [rbp - PS2_BUF]
    call buf_free
    pop r8

    mov rdi, rbx
    mov esi, AST_CONST
    xor edx, edx
    mov rcx, [rbp - PS2_LINE]
    xor r9d, r9d
    call ast_make
    pop rbx
    leave
    ret

.mixed:
    mov rdi, rbx
    CSTRING rsi, "cannot mix bytes and str literals"
    call par_syntax_error
.fail:
    lea rdi, [rbp - PS2_BUF]
    call buf_free
    xor eax, eax
    pop rbx
    leave
    ret
    jmp .fstring_unreachable
.fstring_run:
    mov rdi, rbx
    call ast_mark
    mov [rbp - PS2_BYTES], rax
.frun_loop:
    mov rdi, rbx
    call par_peek
    mov rsi, rax
    mov rdi, rbx
    mov rdx, [rbp - PS2_BYTES]
    call par_fstring_piece_any
    test eax, eax
    jz .fail2
    mov rdi, rbx
    call par_advance
    mov rdi, rbx
    call par_kind
    cmp eax, TOK_STRING
    je .frun_loop
    mov rdi, rbx
    mov esi, AST_JOINEDSTR
    mov rdx, [rbp - PS2_LINE]
    mov rcx, [rbp - PS2_BYTES]
    call par_finish_list
    pop rbx
    leave
    ret
.fail2:
    xor eax, eax
    pop rbx
    leave
    ret
.fstring_unreachable:
END_FUNC pf_string

;; ============================================================================
;; par_run_has_fstring(Comp *c) -> rax = 1 when any literal in the adjacent run
;; carries the f prefix.
;; ============================================================================
DEF_FUNC_BARE par_run_has_fstring
    mov eax, [rdi + Comp.tok_idx]
    mov rdx, [rdi + Comp.tokens + Buf.data]
    mov rcx, [rdi + Comp.tokens + Buf.len]
.loop:
    cmp rax, rcx
    jae .no
    mov r8, rax
    shl r8, TOKEN_SHIFT
    movzx r9d, word [rdx + r8 + Token.kind]
    cmp r9d, TOK_STRING
    jne .no
    movzx r9d, word [rdx + r8 + Token.flags]
    test r9d, TF_STR_FMT
    jnz .yes
    inc rax
    jmp .loop
.yes:
    mov eax, 1
    ret
.no:
    xor eax, eax
    ret
END_FUNC par_run_has_fstring

;; ============================================================================
;; par_fstring_piece_any(Comp *c, Token *t, uint64_t mark) -> 1 ok, 0 error
;; One literal of a run, whether or not it is an f-string.
;; ============================================================================
PFA_TOK   equ 16
PFA_MARK  equ 24
PFA_NODE  equ 32
PFA_BUF   equ 64
PFA_FRAME equ 72          ; + 1 push = 80
DEF_FUNC par_fstring_piece_any, PFA_FRAME
    push rbx
    mov rbx, rdi
    mov [rbp - PFA_TOK], rsi
    mov [rbp - PFA_MARK], rdx
    movzx eax, word [rsi + Token.flags]
    test eax, TF_STR_BYTES
    jnz .mixed
    test eax, TF_STR_FMT
    jnz .fstring

    ; A plain literal inside an f-string run becomes one constant piece.
    lea rdi, [rbp - PFA_BUF]
    mov esi, 1
    call buf_init
    mov rdi, rbx
    mov rsi, [rbp - PFA_TOK]
    lea rdx, [rbp - PFA_BUF]
    call par_string_body
    test eax, eax
    jz .fail
    mov rdi, [rbp - PFA_BUF + Buf.data]
    mov rsi, [rbp - PFA_BUF + Buf.len]
    call comp_intern
    test rax, rax
    jz .fail
    mov rdi, rbx
    mov rsi, rax
    call ast_obj
    mov r8, rax
    mov rcx, [rbp - PFA_TOK]
    TOK_POS rcx
    mov rdi, rbx
    mov esi, AST_CONST
    xor edx, edx
    xor r9d, r9d
    call ast_make
    ; The piece ends at its OWN token, and the cursor is still on it -- this
    ; runs before par_advance, so the default end came from the token BEFORE
    ; it.  In `f'a {x} ' 'b'` that put the second piece's end back on the
    ; first piece's line.
    mov [rbp - PFA_NODE], rax
    mov rdi, rbx
    mov esi, eax
    mov edx, [rbx + Comp.tok_idx]
    inc edx
    extern ast_end_at
    call ast_end_at
    mov rdi, rbx
    mov rsi, [rbp - PFA_NODE]
    call ast_push
    lea rdi, [rbp - PFA_BUF]
    call buf_free
    mov eax, 1
    pop rbx
    leave
    ret

.fstring:
    mov rdi, rbx
    mov rsi, [rbp - PFA_TOK]
    mov rdx, [rbp - PFA_MARK]
    call par_fstring_pieces
    pop rbx
    leave
    ret
.mixed:
    ; This is reached before PFA_BUF has been initialised, so it must not fall
    ; into .fail -- buf_free on an uninitialised stack word freed whatever
    ; address happened to be there.
    mov rdi, rbx
    CSTRING rsi, "cannot mix bytes and f-string literals"
    call par_syntax_error
    xor eax, eax
    pop rbx
    leave
    ret
.fail:
    lea rdi, [rbp - PFA_BUF]
    call buf_free
    xor eax, eax
    pop rbx
    leave
    ret
END_FUNC par_fstring_piece_any


;; ============================================================================
;; par_escape_error(rdi = Comp*, rsi = the literal's content, rdx = the
;;                  backslash, rcx = the escape's length, r8 = the reason,
;;                  r9 = non-zero for a bytes literal) -> rax = 0, always
;;
;; CPython does not report a bad escape itself: it hands the literal to the
;; unicode_escape codec and wraps whatever that says, so the message names the
;; codec, the position WITHIN the literal, and the codec's own reason --
;;   (unicode error) 'unicodeescape' codec can't decode bytes in position 0-7:
;;   unknown Unicode character name
;; A bytes literal goes through a different path and gets a different shape:
;;   (value error) invalid \x escape at position 0
;; The span is the whole string token either way, which is what the parser is
;; looking at when this is called.
;; ============================================================================
PEE_COMP  equ 8
PEE_POS   equ 16
PEE_LEN   equ 24
PEE_WHY   equ 32
PEE_BYTES equ 40
PEE_FRAME equ 48            ; + 1 push = 56... one word more to land right
DEF_FUNC_LOCAL par_escape_error, 56     ; + 1 push = 64, 16-aligned
    push rbx
    mov rbx, rdi
    mov [rbp - PEE_COMP], rdi
    sub rdx, rsi
    mov [rbp - PEE_POS], rdx            ; the position within the content
    mov [rbp - PEE_LEN], rcx
    mov [rbp - PEE_WHY], r8
    mov [rbp - PEE_BYTES], r9

    call comp_msg_start
    push rax
    mov rdi, rax
    cmp qword [rbp - PEE_BYTES], 0
    jne .pee_bytes
    CSTRING rsi, "(unicode error) 'unicodeescape' codec can't decode bytes in position "
    call comp_msg_cstr
    mov rdi, rax
    mov rsi, [rbp - PEE_POS]
    call comp_msg_i64
    mov rdi, rax
    CSTRING rsi, "-"
    call comp_msg_cstr
    mov rdi, rax
    mov rsi, [rbp - PEE_POS]
    add rsi, [rbp - PEE_LEN]
    dec rsi
    call comp_msg_i64
    mov rdi, rax
    CSTRING rsi, ": "
    call comp_msg_cstr
    mov rdi, rax
    mov rsi, [rbp - PEE_WHY]
    call comp_msg_cstr
    jmp .pee_have_msg

.pee_bytes:
    CSTRING rsi, "(value error) invalid \x escape at position "
    call comp_msg_cstr
    mov rdi, rax
    mov rsi, [rbp - PEE_POS]
    call comp_msg_i64

.pee_have_msg:
    pop rdx                             ; the message

    ; The span is the whole string token, which is the one the parser is on.
    push rdx
    mov rdi, rbx
    call par_peek
    pop rdx
    mov ecx, [rax + Token.lineno]
    mov r8d, [rax + Token.col]
    mov r9d, ecx
    mov r10d, [rax + Token.len]
    add r10d, r8d
    mov rdi, rbx
    lea rsi, [rel exc_SyntaxError_type]
    call comp_error_span
    xor eax, eax
    pop rbx
    leave
    ret
END_FUNC par_escape_error
