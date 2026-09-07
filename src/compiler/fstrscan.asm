; fstrscan.asm - where a string literal ends, under PEP 701.
;
; This file is the only definition of that question for an f-string, and the
; three scanners that used to answer it separately -- the lexer's own
; .sub_string, par_fstring_field's expression scan, and par_fstring_spec's
; format-spec scan -- all come here now.
;
; PEP 701 made the answer recursive.  Before 3.12 an f-string's replacement
; field could not contain the quote character that opened the f-string, so the
; token ended at the first unescaped copy of it and a plain scan was enough.
; Now `f"{" ".join(cmd)!r}"` is legal, and so is a nested f-string, a comment,
; a backslash and a newline inside the field.  Finding the end of the token
; therefore means understanding the whole grammar of what is inside it.
;
; The two functions here are mutually recursive, and that is the shape of the
; problem rather than a choice: a nested literal may be an f-string, whose
; fields may hold literals of their own.  A single "skip one literal" helper
; cannot do it without containing the field machinery anyway.
;
; Three states, not two.  A format spec is a different language from an
; expression: `#` is an ordinary character in it (`f"{x:#x}"`), so are quotes
; (`f'{x:"}'` is a spec of one double quote), and there is no `{{` escape --
; `f"{x:{{w}}}"` is a field whose expression is the set `{w}`.  A single brace
; counter gets all three of those wrong.
;
; Two counters, not one.  The `:` that starts a spec only counts at bracket
; depth zero, which is why `f"{d[1:2]}"` and `f"{ {1:2} }"` mean what they say.
; The brackets are a STACK rather than a count so that `f"{(a}"` is rejected
; rather than mis-scanned: a bare counter would let the `}` cancel the `(` and
; run the scan past the real end of the field.
;
; What is deliberately NOT reported here: a lone `}` in the literal part, and
; an invalid conversion character.  Neither stops the scan from finding the
; closing quote, and src/compiler/fstring.asm already reports both at better
; positions than a scanner could.  Reporting them earlier would only make the
; message worse.

%include "macros.inc"
%include "object.inc"
%include "compiler.inc"

section .text


;; ============================================================================
;; fs_literal_at(rdi = p, rsi = end)
;;   -> rax = the opening quote of the literal starting at p, or 0 when p does
;;      not begin one; edx = the FS_LIT_* flags its prefix means
;;
;; A quote, or a run of one or two letters immediately followed by one.  Three
;; letters is an identifier, and so is a letter run that hits anything else --
;; which is why `format("x")` is not read as a literal at the f.
;;
;; It does not check that the letters spell a PREFIX Python accepts: an
;; unknown pair still delimits a string exactly the same way, and this only
;; has to find where that string ends.  lex_str_prefix is what refuses `qq""`,
;; and it runs over the same bytes afterwards.
;; ============================================================================
DEF_FUNC_BARE fs_literal_at
    xor edx, edx                ; the flags
    xor r8d, r8d                ; letters consumed
.fla_loop:
    lea rcx, [rdi + r8]
    cmp rcx, rsi
    jae .fla_no
    movzx eax, byte [rcx]
    cmp al, 34                  ; "
    je .fla_yes
    cmp al, 39                  ; '
    je .fla_yes
    cmp r8, 2
    jae .fla_no
    or al, 0x20                 ; fold, so F and R count as well
    cmp al, 'a'
    jb .fla_no
    cmp al, 'z'
    ja .fla_no
    cmp al, 'f'
    jne .fla_not_f
    or edx, FS_LIT_FMT
.fla_not_f:
    cmp al, 'r'
    jne .fla_not_r
    or edx, FS_LIT_RAW
.fla_not_r:
    inc r8
    jmp .fla_loop
.fla_yes:
    mov rax, rcx
    ret
.fla_no:
    xor eax, eax
    ret
END_FUNC fs_literal_at

;; ============================================================================
;; fs_skip_literal(rdi = p, at the OPENING QUOTE; rsi = end;
;;                 rdx = FsSkip *st; ecx = FS_LIT_* flags)
;;   -> rax = the byte just past the closing quote, or 0 with st->err set
;;
;; The caller has already recognised the literal and read its prefix, which is
;; where FS_LIT_FMT and FS_LIT_RAW come from; this starts at the quote.
;;
;; st->nl and st->lstart are advanced for every newline crossed, INCLUDING the
;; ones inside nested literals and fields and including on the failure paths,
;; because the lexer reports the line a literal gave up on and that line has to
;; be right whether the scan succeeded or not.
;; ============================================================================
LSL_FLAGS equ 8
LSL_FRAME equ 24            ; + 5 pushes = 64, 16-aligned
DEF_FUNC fs_skip_literal, LSL_FRAME
    push rbx
    push r12
    push r13
    push r14
    push r15
    mov rbx, rdi                ; p
    mov r12, rsi                ; end
    mov r13, rdx                ; st
    mov [rbp - LSL_FLAGS], rcx

    movzx r14d, byte [rbx]      ; the quote character
    xor r15d, r15d              ; quote run length - 1: 0 for one, 2 for three

    ; A run of three opens a triple-quoted literal; two is the empty short one.
    lea rax, [rbx + 2]
    cmp rax, r12
    ja .lsl_single
    movzx eax, byte [rbx + 1]
    cmp eax, r14d
    jne .lsl_single
    movzx eax, byte [rbx + 2]
    cmp eax, r14d
    jne .lsl_empty
    mov r15d, 2
.lsl_single:
    lea rbx, [rbx + r15 + 1]

.lsl_loop:
    cmp rbx, r12
    jae .lsl_unterminated
    movzx eax, byte [rbx]

    cmp al, 92                  ; a backslash escapes the next byte, raw or not
    je .lsl_escape
    cmp al, 10
    je .lsl_newline
    cmp eax, r14d
    je .lsl_maybe_close
    test qword [rbp - LSL_FLAGS], FS_LIT_FMT
    jz .lsl_advance
    cmp al, '{'
    je .lsl_brace_open
    cmp al, '}'
    je .lsl_brace_close
.lsl_advance:
    inc rbx
    jmp .lsl_loop

.lsl_escape:
    ; \N{NAME} is a named escape, not a replacement field: its brace must not
    ; open one.  Only when the literal is not raw -- rf"\N{x}" really is a
    ; field, and prints a backslash and an N in front of it.
    inc rbx
    cmp rbx, r12
    jae .lsl_unterminated
    movzx eax, byte [rbx]
    cmp al, 10
    jne .lsl_escape_not_nl
    inc qword [r13 + FsSkip.nl]
    lea rax, [rbx + 1]
    mov [r13 + FsSkip.lstart], rax
    jmp .lsl_advance
.lsl_escape_not_nl:
    cmp al, 'N'
    jne .lsl_advance
    mov rax, [rbp - LSL_FLAGS]
    test rax, FS_LIT_FMT
    jz .lsl_advance
    test rax, FS_LIT_RAW
    jnz .lsl_advance
    lea rax, [rbx + 1]
    cmp rax, r12
    jae .lsl_advance
    cmp byte [rax], '{'
    jne .lsl_advance
    ; Scan to the closing brace, bounded by the quote character and, for a
    ; literal that cannot span lines, by a newline.  Out of bounds, leave it
    ; alone and let the escape decoder report it.
    mov rcx, rax
.lsl_named:
    inc rcx
    cmp rcx, r12
    jae .lsl_advance
    movzx eax, byte [rcx]
    cmp al, '}'
    je .lsl_named_done
    cmp eax, r14d
    je .lsl_advance
    cmp al, 10
    jne .lsl_named
    test r15d, r15d
    jz .lsl_advance
    jmp .lsl_named
.lsl_named_done:
    mov rbx, rcx
    jmp .lsl_advance

.lsl_newline:
    ; Only a triple-quoted literal may span lines -- in its LITERAL part.  A
    ; newline inside a field is the field scanner's business and is allowed
    ; there whatever the quoting, which is the other half of PEP 701.
    test r15d, r15d
    jz .lsl_unterminated
    inc qword [r13 + FsSkip.nl]
    inc rbx
    mov [r13 + FsSkip.lstart], rbx
    jmp .lsl_loop

.lsl_brace_open:
    lea rax, [rbx + 1]
    cmp rax, r12
    jae .lsl_field
    movzx ecx, byte [rax]
    cmp ecx, '{'
    jne .lsl_field
    add rbx, 2                  ; {{ is one literal brace, never a field
    jmp .lsl_loop
.lsl_field:
    mov rdi, rax                ; just past the {
    mov rsi, r12
    mov rdx, r13
    mov ecx, r14d               ; the quote that closes the enclosing f-string
    mov r8d, r15d               ; and whether that takes three of them
    call fs_scan_field
    test rax, rax
    jz .lsl_failed
    mov rbx, rax
    jmp .lsl_loop

.lsl_brace_close:
    ; }} is one literal brace.  A lone } is left for fstring.asm to report,
    ; which it does at the brace rather than at the whole token.
    lea rax, [rbx + 1]
    cmp rax, r12
    jae .lsl_advance
    cmp byte [rax], '}'
    jne .lsl_advance
    add rbx, 2
    jmp .lsl_loop

.lsl_maybe_close:
    test r15d, r15d
    jz .lsl_close1
    lea rax, [rbx + 2]
    cmp rax, r12
    jae .lsl_advance
    movzx eax, byte [rbx + 1]
    cmp eax, r14d
    jne .lsl_advance
    movzx eax, byte [rbx + 2]
    cmp eax, r14d
    jne .lsl_advance
    lea rax, [rbx + 3]
    jmp .lsl_out
.lsl_close1:
    lea rax, [rbx + 1]
    jmp .lsl_out

.lsl_empty:
    lea rax, [rbx + 2]
    jmp .lsl_out

.lsl_unterminated:
    ; The quote it was looking for says which message the lexer prints; a
    ; nested literal opened with the OTHER quote is an unterminated string
    ; inside the field, which is what CPython reports for f"{ 'x".
    cmp qword [r13 + FsSkip.err], FSE_OK
    jne .lsl_failed
    mov qword [r13 + FsSkip.err], FSE_UNTERM
    mov [r13 + FsSkip.badp], rbx
.lsl_failed:
    xor eax, eax
.lsl_out:
    pop r15
    pop r14
    pop r13
    pop r12
    pop rbx
    leave
    ret
END_FUNC fs_skip_literal

;; ============================================================================
;; fs_scan_field(rdi = p, just past the field's '{'; rsi = end;
;;               rdx = FsSkip *st; ecx = the enclosing f-string's quote;
;;               r8d = 2 when that quote comes in threes, else 0)
;;   -> rax = the byte just past the matching '}', or 0 with st->err set
;;
;; The expression, then optionally `!conv` and `:spec`, then the closing brace.
;; What this has to get right is only where the field ENDS: the expression
;; itself is re-lexed and parsed later, by the ordinary tokenizer, over the
;; same bytes.
;; ============================================================================
FSF_SP     equ 8
FSF_SPEC   equ 16           ; 1 once a `:` has been seen at bracket depth 0
FSF_BSTACK equ 216          ; 200 bytes of open brackets, at [rbp-216, rbp-16)
FSF_FRAME  equ 216          ; + 5 pushes = 256, 16-aligned
FS_MAXBRACKET equ 200
DEF_FUNC fs_scan_field, FSF_FRAME
    push rbx
    push r12
    push r13
    push r14
    push r15
    mov rbx, rdi
    mov r12, rsi
    mov r13, rdx
    mov r14d, ecx               ; the enclosing quote
    mov r15d, r8d               ; and its run length - 1
    mov qword [rbp - FSF_SP], 0
    mov qword [rbp - FSF_SPEC], 0

    ; PEP 701 removed the expression-nesting limit but kept this one.
    inc qword [r13 + FsSkip.level]
    cmp qword [r13 + FsSkip.level], FS_MAXFSTRING
    jle .fsf_loop
    mov qword [r13 + FsSkip.err], FSE_TOO_NESTED
    mov [r13 + FsSkip.badp], rbx
    jmp .fsf_failed

.fsf_loop:
    cmp rbx, r12
    jae .fsf_ran_out
    movzx eax, byte [rbx]

    cmp al, 10
    je .fsf_newline
    cmp qword [rbp - FSF_SPEC], 0
    jne .fsf_spec_byte

    ; --- the expression ---
    cmp al, '#'
    je .fsf_comment
    cmp al, 34
    je .fsf_literal
    cmp al, 39
    je .fsf_literal
    cmp al, '('
    je .fsf_push
    cmp al, '['
    je .fsf_push
    cmp al, '{'
    je .fsf_push
    cmp al, ')'
    je .fsf_pop
    cmp al, ']'
    je .fsf_pop
    cmp al, '}'
    je .fsf_close_or_pop
    cmp qword [rbp - FSF_SP], 0
    jne .fsf_maybe_ident
    cmp al, ':'
    je .fsf_spec_start
.fsf_maybe_ident:
    ; A letter run of one or two immediately followed by a quote is a prefixed
    ; literal: rb'...', f"...".  Anything longer is an identifier, and the
    ; single advance below walks it a byte at a time.
    mov rdi, rbx
    mov rsi, r12
    call fs_literal_at
    test rax, rax
    jz .fsf_advance
    mov rbx, rax
    mov ecx, edx
    jmp .fsf_literal_here
.fsf_advance:
    inc rbx
    jmp .fsf_loop

.fsf_comment:
    ; A comment runs to the end of the line.  The newline itself is left for
    ; the handler above, so the line counter moves exactly once.
    inc rbx
    cmp rbx, r12
    jae .fsf_ran_out
    cmp byte [rbx], 10
    jne .fsf_comment
    jmp .fsf_loop

.fsf_newline:
    ; Allowed inside a field whatever quoted the f-string.  Before PEP 701 a
    ; single-quoted f-string could not contain one at all.
    inc qword [r13 + FsSkip.nl]
    inc rbx
    mov [r13 + FsSkip.lstart], rbx
    jmp .fsf_loop

.fsf_literal:
    xor ecx, ecx
.fsf_literal_here:
    mov rdi, rbx
    mov rsi, r12
    mov rdx, r13
    call fs_skip_literal
    test rax, rax
    jz .fsf_failed
    mov rbx, rax
    jmp .fsf_loop

.fsf_push:
    mov rcx, [rbp - FSF_SP]
    cmp rcx, FS_MAXBRACKET
    jae .fsf_too_deep
    lea rdx, [rbp - FSF_BSTACK]
    mov [rdx + rcx], al
    inc qword [rbp - FSF_SP]
    jmp .fsf_advance

.fsf_pop:
    ; A closer with nothing open belongs to whatever encloses the f-string,
    ; and the field cannot end on it; a mismatched one is an error either way.
    ; Both are reported by the parser, from the tokens; this only has to stop.
    cmp qword [rbp - FSF_SP], 0
    je .fsf_unmatched
    dec qword [rbp - FSF_SP]
    jmp .fsf_advance

.fsf_close_or_pop:
    cmp qword [rbp - FSF_SP], 0
    jne .fsf_pop_brace
    inc rbx                     ; the field ends here
    mov rax, rbx
    jmp .fsf_out
.fsf_pop_brace:
    dec qword [rbp - FSF_SP]
    jmp .fsf_advance

.fsf_spec_start:
    mov qword [rbp - FSF_SPEC], 1
    jmp .fsf_advance

    ; --- the format spec ---
    ; Its own language: no comments, no string literals, no {{ }} escape, and
    ; the first unnested } ends the field.
.fsf_spec_byte:
    cmp al, '{'
    je .fsf_spec_nested
    cmp al, '}'
    je .fsf_spec_close
    cmp eax, r14d
    je .fsf_spec_quote
    inc rbx
    jmp .fsf_loop

.fsf_spec_nested:
    lea rdi, [rbx + 1]
    mov rsi, r12
    mov rdx, r13
    mov ecx, r14d
    mov r8d, r15d
    call fs_scan_field
    test rax, rax
    jz .fsf_failed
    mov rbx, rax
    jmp .fsf_loop

.fsf_spec_close:
    inc rbx
    mov rax, rbx
    jmp .fsf_out

.fsf_spec_quote:
    ; The enclosing f-string ended inside the spec, so the field never closed.
    ; f"{x:"}" is `unterminated string literal` in CPython for the same reason.
    test r15d, r15d
    jz .fsf_ran_out_here
    lea rax, [rbx + 2]
    cmp rax, r12
    jae .fsf_spec_quote_plain
    movzx eax, byte [rbx + 1]
    cmp eax, r14d
    jne .fsf_spec_quote_plain
    movzx eax, byte [rbx + 2]
    cmp eax, r14d
    jne .fsf_spec_quote_plain
    jmp .fsf_ran_out_here
.fsf_spec_quote_plain:
    inc rbx
    jmp .fsf_loop

.fsf_ran_out:
    mov rbx, r12
.fsf_ran_out_here:
    cmp qword [r13 + FsSkip.err], FSE_OK
    jne .fsf_failed
    mov qword [r13 + FsSkip.err], FSE_EXPECT_RBRACE
    mov [r13 + FsSkip.badp], rbx
    jmp .fsf_failed

.fsf_unmatched:
    cmp qword [r13 + FsSkip.err], FSE_OK
    jne .fsf_failed
    mov qword [r13 + FsSkip.err], FSE_UNMATCHED
    mov [r13 + FsSkip.badp], rbx
    jmp .fsf_failed

.fsf_too_deep:
    cmp qword [r13 + FsSkip.err], FSE_OK
    jne .fsf_failed
    mov qword [r13 + FsSkip.err], FSE_TOO_NESTED
    mov [r13 + FsSkip.badp], rbx
.fsf_failed:
    xor eax, eax
.fsf_out:
    dec qword [r13 + FsSkip.level]
    pop r15
    pop r14
    pop r13
    pop r12
    pop rbx
    leave
    ret

END_FUNC fs_scan_field
