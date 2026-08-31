; fstring.asm - f-string parsing
;
; An f-string is a sequence of literal pieces and replacement fields, and the
; fields hold ordinary Python expressions.  Rather than teach the tokenizer
; PEP 701's FSTRING_START/MIDDLE/END, each field's source span is tokenized on
; its own and APPENDED to the token array; the parser's cursor is then pointed
; at those tokens and the ordinary expression parser handles the rest.  The
; token array is what makes that cheap -- there is nowhere else to put them and
; nothing to restore afterwards but an index.
;
; The result is an AST_JOINEDSTR whose children alternate literal constants and
; AST_FORMATTEDVALUE nodes; codegen turns it into FORMAT_VALUE plus one
; BUILD_STRING.

%include "macros.inc"
%include "object.inc"
%include "value.inc"
%include "opcodes.inc"
%include "compiler.inc"

extern ast_commit
extern ast_make
extern ast_mark
extern ast_obj
extern ast_push
extern buf_free
extern buf_init
extern buf_push_u8
extern comp_error
extern comp_intern
extern comp_lex_span

extern par_expr
extern par_finish_list
extern par_syntax_error


BP_NONE equ 0

; --- Named frame-layout constants ---
FS2_TOK   equ 16
FS2_MARK  equ 24
FS2_P     equ 32
FS2_END   equ 40
FS2_LINE  equ 48
FS2_RAW   equ 56
FS2_BUF   equ 96          ; a Buf at [rbp - 96]
FS2_FRAME equ 136         ; + 3 pushes = 160

section .text

;; ============================================================================
;; par_fstring_pieces(Comp *c, Token *t, uint64_t mark) -> rax = 1 ok, 0 error
;;
;; Appends this f-string token's literal pieces and replacement fields to the
;; pending list that mark opened.
;; ============================================================================
DEF_FUNC par_fstring_pieces, FS2_FRAME
    push rbx
    push r12
    push r13
    mov rbx, rdi
    mov [rbp - FS2_TOK], rsi
    mov [rbp - FS2_MARK], rdx

    mov ecx, [rsi + Token.lineno]
    mov [rbp - FS2_LINE], rcx
    movzx eax, word [rsi + Token.flags]
    xor ecx, ecx
    test eax, TF_STR_RAW
    setnz cl
    mov [rbp - FS2_RAW], rcx

    ; Find the text between the quotes, exactly as par_string_body does.
    mov r12, [rsi + Token.start]
    mov r13d, [rsi + Token.len]
    add r13, r12
.skip_prefix:
    movzx eax, byte [r12]
    cmp al, 39
    je .at_quote
    cmp al, 34
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
    sub r13, rdx
    mov [rbp - FS2_P], r12
    mov [rbp - FS2_END], r13

    lea rdi, [rbp - FS2_BUF]
    mov esi, 1
    call buf_init

.scan:
    mov r12, [rbp - FS2_P]
    cmp r12, [rbp - FS2_END]
    jae .flush_last
    movzx eax, byte [r12]
    cmp al, '{'
    je .brace_open
    cmp al, '}'
    je .brace_close
    cmp al, 92                          ; backslash
    je .escape
    mov rdi, [rbp - FS2_BUF + 0]
    lea rdi, [rbp - FS2_BUF]
    mov esi, eax
    call buf_push_u8
    inc qword [rbp - FS2_P]
    jmp .scan

.escape:
    cmp qword [rbp - FS2_RAW], 0
    jne .raw_backslash
    ; The literal parts of an f-string take the ordinary escapes -- all of
    ; them.  A private decoder here knew only \n, \t and \r, so f"\x41"
    ; came out as "x41" while "\x41" came out as "A".
    mov rdi, rbx
    lea rsi, [rbp - FS2_BUF]
    mov rdx, [rbp - FS2_P]
    inc rdx                             ; past the backslash
    mov rcx, [rbp - FS2_END]
    xor r8d, r8d                        ; an f-string is never bytes
    extern par_escape_one
    call par_escape_one
    test rax, rax
    jz .bad
    mov [rbp - FS2_P], rax
    jmp .scan
.raw_backslash:
    lea rdi, [rbp - FS2_BUF]
    mov esi, 92
    call buf_push_u8
    inc qword [rbp - FS2_P]
    jmp .scan

.brace_close:
    ; `}}` is a literal brace; a lone `}` is an error, as in CPython.
    lea rax, [r12 + 1]
    cmp rax, [rbp - FS2_END]
    jae .bad_brace
    cmp byte [rax], '}'
    jne .bad_brace
    lea rdi, [rbp - FS2_BUF]
    mov esi, '}'
    call buf_push_u8
    add qword [rbp - FS2_P], 2
    jmp .scan

.brace_open:
    ; `{{` is a literal brace.
    lea rax, [r12 + 1]
    cmp rax, [rbp - FS2_END]
    jae .bad_brace
    cmp byte [rax], '{'
    jne .field
    lea rdi, [rbp - FS2_BUF]
    mov esi, '{'
    call buf_push_u8
    add qword [rbp - FS2_P], 2
    jmp .scan

.field:
    ; Flush whatever literal text came before it.
    call .flush
    test eax, eax
    jz .fail
    inc qword [rbp - FS2_P]             ; past the '{'
    mov rdi, rbx
    mov rsi, [rbp - FS2_TOK]
    lea rdx, [rbp - FS2_P]
    mov rcx, [rbp - FS2_END]
    mov r8, [rbp - FS2_LINE]
    call par_fstring_field
    test rax, rax
    jz .fail
    mov rdi, rbx
    mov rsi, rax
    call ast_push
    jmp .scan

.flush_last:
    call .flush
    test eax, eax
    jz .fail
    lea rdi, [rbp - FS2_BUF]
    call buf_free
    mov eax, 1
    jmp .ret

; Local: turn any pending literal bytes into a constant piece.
.flush:
    sub rsp, 8
    cmp qword [rbp - FS2_BUF + Buf.len], 0
    je .flush_none
    mov rdi, [rbp - FS2_BUF + Buf.data]
    mov rsi, [rbp - FS2_BUF + Buf.len]
    call comp_intern
    test rax, rax
    jz .flush_fail
    mov rdi, rbx
    mov rsi, rax
    call ast_obj
    mov r8, rax
    mov rdi, rbx
    mov esi, AST_CONST
    xor edx, edx
    mov rcx, [rbp - FS2_LINE]
    xor r9d, r9d
    call ast_make
    mov rdi, rbx
    mov rsi, rax
    call ast_push
    mov qword [rbp - FS2_BUF + Buf.len], 0
.flush_none:
    mov eax, 1
    add rsp, 8
    ret
.flush_fail:
    xor eax, eax
    add rsp, 8
    ret

.bad_brace:
    mov rdi, rbx
    CSTRING rsi, "single '}' is not allowed in an f-string"
    call par_syntax_error
    jmp .fail
.bad:
    mov rdi, rbx
    CSTRING rsi, "malformed f-string"
    call par_syntax_error
.fail:
    lea rdi, [rbp - FS2_BUF]
    call buf_free
    xor eax, eax
.ret:
    pop r13
    pop r12
    pop rbx
    leave
    ret
END_FUNC par_fstring_pieces

;; ============================================================================
;; par_fstring_field(Comp *c, Token *t, uint64_t *p, const char *end, int line)
;;   -> rax = an AST_FORMATTEDVALUE node, 0 on error
;;
;; One replacement field, with *p just past its opening brace.  Scans to the
;; matching close, splitting off `!conv` and `:spec` at brace depth zero, then
;; tokenizes the expression's own span and parses it with the ordinary
;; expression parser.
;; ============================================================================
FF_TOK   equ 16
FF_P     equ 24
FF_END   equ 32
FF_LINE  equ 40
FF_ESTART equ 48
FF_EEND  equ 56
FF_CONV  equ 64
FF_SPEC  equ 72
FF_SSTART equ 80
FF_SEND  equ 88
FF_EXPR  equ 96
FF_SAVE  equ 104
FF_DEBUG equ 112
FF_WSEND equ 120         ; end of the run of spaces after a `=`
FF_FRAME equ 136          ; + 3 pushes = 144
DEF_FUNC par_fstring_field, FF_FRAME
    push rbx
    push r12
    push r13
    mov rbx, rdi
    mov [rbp - FF_TOK], rsi
    mov [rbp - FF_P], rdx
    mov [rbp - FF_END], rcx
    mov [rbp - FF_LINE], r8
    mov qword [rbp - FF_CONV], 0
    mov qword [rbp - FF_SPEC], 0
    mov qword [rbp - FF_SSTART], 0
    mov qword [rbp - FF_DEBUG], 0

    mov rdx, [rbp - FF_P]
    mov r12, [rdx]
    mov [rbp - FF_ESTART], r12
    mov r13, [rbp - FF_END]
    xor ecx, ecx                        ; brace depth
    xor r8d, r8d                        ; quote character, 0 = outside a string

.scan:
    cmp r12, r13
    jae .unterminated
    movzx eax, byte [r12]
    test r8d, r8d
    jz .not_in_string
    cmp eax, r8d
    jne .advance
    xor r8d, r8d
    jmp .advance
.not_in_string:
    cmp al, 39
    je .open_quote
    cmp al, 34
    je .open_quote
    cmp al, '{'
    je .deeper
    cmp al, '['
    je .deeper
    cmp al, '('
    je .deeper
    cmp al, '}'
    je .maybe_close
    cmp al, ']'
    je .shallower
    cmp al, ')'
    je .shallower
    test ecx, ecx
    jnz .advance
    cmp al, '!'
    je .maybe_conv
    cmp al, ':'
    je .spec_start
    cmp al, '='
    je .maybe_debug
    jmp .advance
.open_quote:
    mov r8d, eax
    jmp .advance
.deeper:
    inc ecx
    jmp .advance
.shallower:
    dec ecx
    jmp .advance
.maybe_close:
    test ecx, ecx
    jz .close
    dec ecx
    jmp .advance
.advance:
    inc r12
    jmp .scan

.maybe_conv:
    ; `!=` is a comparison, not a conversion.
    lea rax, [r12 + 1]
    cmp rax, r13
    jae .advance
    cmp byte [rax], '='
    je .advance
    ; After a `=` the expression's end is already recorded; overwriting it
    ; here made `f"{x = !s}"` print "x = =5".
    cmp qword [rbp - FF_DEBUG], 0
    jne .conv_keep_end
    mov [rbp - FF_EEND], r12
.conv_keep_end:
    inc r12
    cmp r12, r13
    jae .unterminated
    movzx eax, byte [r12]
    mov edx, 1
    cmp al, 's'
    je .have_conv
    mov edx, 2
    cmp al, 'r'
    je .have_conv
    mov edx, 3
    cmp al, 'a'
    je .have_conv
    jmp .bad_conv
.have_conv:
    mov [rbp - FF_CONV], rdx
    inc r12
    cmp r12, r13
    jae .unterminated
    movzx eax, byte [r12]
    cmp al, ':'
    je .spec_after_conv
    cmp al, '}'
    je .close_with_end
    jmp .bad_conv

.maybe_debug:
    ; `{x=}` prints the expression text as well as its value; `==`, `<=`,
    ; `>=` and `!=` are comparisons and must not be mistaken for it.
    lea rax, [r12 + 1]
    cmp rax, r13
    jae .advance
    cmp byte [rax], '='
    je .skip_two
    mov rax, r12
    cmp rax, [rbp - FF_ESTART]
    jbe .debug_here
    movzx edx, byte [rax - 1]
    cmp dl, '='
    je .advance
    cmp dl, '!'
    je .advance
    cmp dl, '<'
    je .advance
    cmp dl, '>'
    je .advance
.debug_here:
    mov qword [rbp - FF_DEBUG], 1
    mov [rbp - FF_EEND], r12
    inc r12
    ; The spaces on either side of the `=` are part of the literal text
    ; CPython emits, so `f"{x = }"` is "x = 5".  Refusing to look past one
    ; made the space a conversion character and reported an invalid one.
    mov [rbp - FF_WSEND], r12
.debug_ws:
    cmp r12, r13
    jae .unterminated
    movzx eax, byte [r12]
    cmp al, ' '
    je .debug_ws_step
    cmp al, 9
    jne .debug_ws_done
.debug_ws_step:
    inc r12
    jmp .debug_ws
.debug_ws_done:
    mov [rbp - FF_WSEND], r12
    cmp r12, r13
    jae .unterminated
    movzx eax, byte [r12]
    cmp al, '}'
    je .close_with_end
    cmp al, ':'
    je .spec_after_conv
    cmp al, '!'
    je .maybe_conv
    jmp .bad_conv
.skip_two:
    add r12, 2
    jmp .scan

.spec_start:
    mov [rbp - FF_EEND], r12
.spec_after_conv:
    inc r12
    mov [rbp - FF_SSTART], r12
    xor ecx, ecx
.spec_scan:
    cmp r12, r13
    jae .unterminated
    movzx eax, byte [r12]
    cmp al, '{'
    je .spec_deeper
    cmp al, '}'
    je .spec_maybe_close
    inc r12
    jmp .spec_scan
.spec_deeper:
    inc ecx
    inc r12
    jmp .spec_scan
.spec_maybe_close:
    test ecx, ecx
    jz .spec_done
    dec ecx
    inc r12
    jmp .spec_scan
.spec_done:
    mov [rbp - FF_SEND], r12
    jmp .close_expr_done

.close:
    mov [rbp - FF_EEND], r12
.close_with_end:
.close_expr_done:
    inc r12
    mov rdx, [rbp - FF_P]
    mov [rdx], r12                      ; hand the cursor back past the '}'

    ; --- the expression ---
    mov rdi, rbx
    mov rsi, [rbp - FF_ESTART]
    mov rdx, [rbp - FF_EEND]
    mov rcx, [rbp - FF_LINE]
    call comp_lex_span
    cmp rax, -1
    je .fail
    mov r12, rax

    mov eax, [rbx + Comp.tok_idx]
    mov [rbp - FF_SAVE], rax
    mov [rbx + Comp.tok_idx], r12d
    mov rdi, rbx
    mov esi, BP_NONE
    call par_expr
    mov [rbp - FF_EXPR], rax
    mov rdx, [rbp - FF_SAVE]
    mov [rbx + Comp.tok_idx], edx
    cmp qword [rbp - FF_EXPR], 0
    je .fail

    ; --- `{x=}` prefixes the source text of the expression ---
    cmp qword [rbp - FF_DEBUG], 0
    je .no_debug
    mov rdi, rbx
    mov rsi, [rbp - FF_ESTART]
    mov rdx, [rbp - FF_EEND]
    mov rcx, [rbp - FF_LINE]
    mov r8, [rbp - FF_WSEND]
    call par_fstring_debug_text
    test rax, rax
    jz .fail
    mov rdi, rbx
    mov rsi, rax
    call ast_push
    cmp qword [rbp - FF_CONV], 0
    jne .no_debug
    ; `=` implies !r only when there is no format spec.  With one, CPython
    ; formats the value itself: f"{x=:.2f}" is x=1.50, where repr()ing first
    ; hands ".2f" a str and raises.
    cmp qword [rbp - FF_SSTART], 0
    jne .no_debug
    mov qword [rbp - FF_CONV], 2        ; `{x=}` implies !r
.no_debug:

    ; --- the format spec, itself an f-string ---
    cmp qword [rbp - FF_SSTART], 0
    je .build
    mov rdi, rbx
    call ast_mark
    mov r12, rax
    mov rdi, rbx
    mov rsi, [rbp - FF_SSTART]
    mov rdx, [rbp - FF_SEND]
    mov rcx, [rbp - FF_LINE]
    call par_fstring_spec
    test eax, eax
    jz .fail
    mov rdi, rbx
    mov esi, AST_JOINEDSTR
    mov rdx, [rbp - FF_LINE]
    mov rcx, r12
    call par_finish_list
    test rax, rax
    jz .fail
    mov [rbp - FF_SPEC], rax

.build:
    mov rdi, rbx
    mov esi, AST_FORMATTEDVALUE
    mov rdx, [rbp - FF_CONV]
    mov rcx, [rbp - FF_LINE]
    mov r8, [rbp - FF_EXPR]
    mov r9, [rbp - FF_SPEC]
    call ast_make
    jmp .ret

.unterminated:
    mov rdi, rbx
    CSTRING rsi, "f-string: expecting '}'"
    call par_syntax_error
    jmp .fail
.bad_conv:
    mov rdi, rbx
    CSTRING rsi, "f-string: invalid conversion, expected 's', 'r' or 'a'"
    call par_syntax_error
.fail:
    xor eax, eax
.ret:
    pop r13
    pop r12
    pop rbx
    leave
    ret
END_FUNC par_fstring_field

;; ============================================================================
;; par_fstring_spec(Comp *c, const char *start, const char *end, int line)
;;   -> rax = 1 ok, 0 error
;; A format spec is itself an f-string: `f"{x:{width}}"` is legal.  The pieces
;; go onto the pending list the caller opened.
;; ============================================================================
FSP_P     equ 16
FSP_END   equ 24
FSP_LINE  equ 32
FSP_BUF   equ 72
FSP_FRAME equ 72          ; + 3 pushes = 96
DEF_FUNC par_fstring_spec, FSP_FRAME
    push rbx
    push r12
    push r13
    mov rbx, rdi
    mov [rbp - FSP_P], rsi
    mov [rbp - FSP_END], rdx
    mov [rbp - FSP_LINE], rcx
    lea rdi, [rbp - FSP_BUF]
    mov esi, 1
    call buf_init
.scan:
    mov r12, [rbp - FSP_P]
    cmp r12, [rbp - FSP_END]
    jae .flush_last
    movzx eax, byte [r12]
    cmp al, '{'
    je .field
    lea rdi, [rbp - FSP_BUF]
    mov esi, eax
    call buf_push_u8
    inc qword [rbp - FSP_P]
    jmp .scan
.field:
    call .flush
    test eax, eax
    jz .fail
    inc qword [rbp - FSP_P]
    mov rdi, rbx
    xor esi, esi
    lea rdx, [rbp - FSP_P]
    mov rcx, [rbp - FSP_END]
    mov r8, [rbp - FSP_LINE]
    call par_fstring_field
    test rax, rax
    jz .fail
    mov rdi, rbx
    mov rsi, rax
    call ast_push
    jmp .scan
.flush_last:
    call .flush
    test eax, eax
    jz .fail
    lea rdi, [rbp - FSP_BUF]
    call buf_free
    mov eax, 1
    jmp .ret

.flush:
    sub rsp, 8
    cmp qword [rbp - FSP_BUF + Buf.len], 0
    je .flush_none
    mov rdi, [rbp - FSP_BUF + Buf.data]
    mov rsi, [rbp - FSP_BUF + Buf.len]
    call comp_intern
    test rax, rax
    jz .flush_fail
    mov rdi, rbx
    mov rsi, rax
    call ast_obj
    mov r8, rax
    mov rdi, rbx
    mov esi, AST_CONST
    xor edx, edx
    mov rcx, [rbp - FSP_LINE]
    xor r9d, r9d
    call ast_make
    mov rdi, rbx
    mov rsi, rax
    call ast_push
    mov qword [rbp - FSP_BUF + Buf.len], 0
.flush_none:
    mov eax, 1
    add rsp, 8
    ret
.flush_fail:
    xor eax, eax
    add rsp, 8
    ret

.fail:
    lea rdi, [rbp - FSP_BUF]
    call buf_free
    xor eax, eax
.ret:
    pop r13
    pop r12
    pop rbx
    leave
    ret
END_FUNC par_fstring_spec

;; ============================================================================
;; par_fstring_debug_text(Comp *c, const char *start, const char *end, int line)
;;   -> rax = an AST_CONST holding "<source>=", or 0
;; What `f"{x=}"` prints before the value.
;; ============================================================================
FD_START equ 16
FD_END   equ 24
FD_LINE  equ 32
FD_WSEND equ 40           ; end of the spaces after the `=`
FD_BUF   equ 88           ; a Buf lives here, so it comes last
DEF_FUNC par_fstring_debug_text, 96
    push rbx
    push r12
    mov rbx, rdi
    mov [rbp - FD_START], rsi
    mov [rbp - FD_END], rdx
    mov [rbp - FD_LINE], rcx
    mov [rbp - FD_WSEND], r8
    lea rdi, [rbp - FD_BUF]
    mov esi, 1
    call buf_init
    mov r12, [rbp - FD_START]
.copy:
    cmp r12, [rbp - FD_END]
    jae .done
    movzx esi, byte [r12]
    lea rdi, [rbp - FD_BUF]
    call buf_push_u8
    inc r12
    jmp .copy
.done:
    lea rdi, [rbp - FD_BUF]
    mov esi, '='
    call buf_push_u8
    ; ...and the spaces that followed it.
    mov r12, [rbp - FD_END]
    inc r12
.copy_ws:
    cmp r12, [rbp - FD_WSEND]
    jae .ws_done
    movzx esi, byte [r12]
    lea rdi, [rbp - FD_BUF]
    call buf_push_u8
    inc r12
    jmp .copy_ws
.ws_done:
    mov rdi, [rbp - FD_BUF + Buf.data]
    mov rsi, [rbp - FD_BUF + Buf.len]
    call comp_intern
    test rax, rax
    jz .fail
    mov rdi, rbx
    mov rsi, rax
    call ast_obj
    mov r8, rax
    mov rdi, rbx
    mov esi, AST_CONST
    xor edx, edx
    mov rcx, [rbp - FD_LINE]
    xor r9d, r9d
    call ast_make
    mov r12, rax
    lea rdi, [rbp - FD_BUF]
    call buf_free
    mov rax, r12
    pop r12
    pop rbx
    leave
    ret
.fail:
    lea rdi, [rbp - FD_BUF]
    call buf_free
    xor eax, eax
    pop r12
    pop rbx
    leave
    ret
END_FUNC par_fstring_debug_text

ASM_INIT
