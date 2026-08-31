; lex.asm - Tokenizer for the Python source compiler
;
; One pass over the source produces the whole token array.  Materializing it up
; front rather than pulling tokens on demand is what makes the parser's
; backtracking cheap: restoring a parse position is `mov [comp+tok_idx], saved`
; instead of a snapshot of the indent stack, paren depth and pending dedents.
;
; Three tables in tables.asm carry the decisions:
;   cc_table  - 256 bytes of character-class flags; every scanning branch is a
;               load and a test against it.
;   kw_table  - keywords matched with one masked 8-byte compare, narrowed by
;               first byte through kw_index.
;   op_table  - operators sorted longest-first within each first byte, so the
;               first match found is already the maximal munch.
;
; Errors are recorded through comp_error and reported by returning 0.  Nothing
; here may call raise_exception: see the header of compiler.inc.

%include "macros.inc"
%include "object.inc"
%include "compiler.inc"

extern buf_reserve
extern comp_error

extern cc_table
extern kw_index
extern kw_masks
extern kw_table
extern op_index
extern op_masks
extern op_table

extern exc_IndentationError_type
extern exc_SyntaxError_type

; --- Named frame-layout constants ---
;
; Frame sizes are picked so (frame + 8 * register pushes) is a multiple of 16,
; leaving rsp 16-byte aligned at every call.  It matters here because the
; number scanner will hand token text to strtod, and libc's floating-point
; paths do use aligned SSE stores.
;
; The scan cursor stays in r12 throughout lex_run; everything that has to
; survive a call goes in a frame slot rather than a caller-saved register.
LR_COMP  equ 8
LR_TOKST equ 16          ; start of the token being scanned
LR_KIND  equ 24          ; its token kind
LR_FLAGS equ 32          ; its TF_* flags
LR_STRNL equ 40          ; lines a string literal spanned, not applied yet
LR_STRLS equ 48          ; where the last of those lines starts
LR_FRAME equ 56          ; + 5 pushes = 96

section .text

;; ============================================================================
;; lex_emit(Comp *c, int kind, const char *start, int64_t len, int flags)
;;   -> Token* (never NULL; ap_malloc is fatal on OOM)
;;
;; Appends one token, stamping the line and column from the lexer state.  The
;; column is computed in bytes from line_start, which is what a SyntaxError
;; caret wants; it is not a code-point count.
;; ============================================================================
LE_COMP  equ 8
LE_KIND  equ 16
LE_START equ 24
LE_LEN   equ 32
LE_FLAGS equ 40
LE_FRAME equ 40          ; + 1 push = 48
DEF_FUNC lex_emit, LE_FRAME
    push rbx
    mov [rbp - LE_COMP], rdi
    mov [rbp - LE_KIND], rsi
    mov [rbp - LE_START], rdx
    mov [rbp - LE_LEN], rcx
    mov [rbp - LE_FLAGS], r8
    mov rbx, rdi

    lea rdi, [rbx + Comp.tokens]
    mov esi, 1
    call buf_reserve                    ; rax = the new Token*

    mov rdx, [rbp - LE_KIND]
    mov [rax + Token.kind], dx
    mov rdx, [rbp - LE_FLAGS]
    mov [rax + Token.flags], dx
    mov rdx, [rbp - LE_START]
    mov [rax + Token.start], rdx
    mov rcx, [rbp - LE_LEN]
    mov [rax + Token.len], ecx
    mov qword [rax + Token.val], 0

    mov ecx, [rbx + Comp.lex + Lexer.lineno]
    mov [rax + Token.lineno], ecx
    sub rdx, [rbx + Comp.lex + Lexer.line_start]
    mov [rax + Token.col], edx

    pop rbx
    leave
    ret
END_FUNC lex_emit

;; ============================================================================
;; lex_keyword(const char *s, int64_t len) -> token kind, or TOK_NAME
;;
;; Every keyword is 2..8 bytes, so an identifier in that range is one unaligned
;; 8-byte load masked to its length and compared against a packed constant.
;; The read may cross the end of the identifier but never the end of the
;; buffer: comp.src carries a NUL-padded tail for exactly this.
;; ============================================================================
DEF_FUNC_BARE lex_keyword
    mov eax, TOK_NAME
    cmp rsi, 2
    jb .done
    cmp rsi, 8
    ja .done

    movzx ecx, byte [rdi]               ; first byte selects the bucket
    lea r8, [rel kw_index]
    movzx edx, byte [r8 + rcx*2 + 1]    ; count
    test edx, edx
    jz .done
    movzx r9d, byte [r8 + rcx*2]        ; first entry index

    mov r10, [rdi]                      ; the 8 bytes to match
    lea r8, [rel kw_masks]
    and r10, [r8 + rsi*8]               ; keep only the identifier's own bytes

    lea r8, [rel kw_table]
    shl r9, 4                           ; * KW_ENT_SIZE
    add r8, r9
.scan:
    cmp r10, [r8]
    jne .next
    cmp word [r8 + 10], si              ; the stored length must agree
    jne .next
    movzx eax, word [r8 + 8]
    ret
.next:
    add r8, 16
    dec edx
    jnz .scan
.done:
    ret
END_FUNC lex_keyword

;; ============================================================================
;; lex_operator(const char *s, const char *end) -> (rax = kind, rdx = length)
;;   rax = 0 (TOK_ENDMARKER) when nothing matches.
;;
;; op_table is sorted longest-first within each first byte, so the first hit is
;; the maximal munch.  The 4-byte load can read past `s`, which the NUL-padded
;; tail makes safe; the mask then discards whatever it picked up.
;; ============================================================================
DEF_FUNC_BARE lex_operator
    movzx ecx, byte [rdi]
    lea r8, [rel op_index]
    movzx r9d, byte [r8 + rcx*2 + 1]    ; count
    test r9d, r9d
    jz .none
    movzx r10d, byte [r8 + rcx*2]       ; first entry

    mov r11d, [rdi]                     ; up to 4 bytes at the cursor
    mov rcx, rsi
    sub rcx, rdi                        ; rcx = bytes remaining

    lea r8, [rel op_table]
    shl r10, 3                          ; * OP_ENT_SIZE
    add r8, r10
.scan:
    movzx edx, byte [r8 + 4]            ; candidate length
    cmp rdx, rcx
    ja .next                            ; would run past the end of the source
    mov eax, r11d
    lea r10, [rel op_masks]
    and eax, [r10 + rdx*4]
    cmp eax, [r8]
    jne .next
    movzx eax, byte [r8 + 5]            ; the token kind
    ret
.next:
    add r8, 8
    dec r9d
    jnz .scan
.none:
    xor eax, eax
    xor edx, edx
    ret
END_FUNC lex_operator

;; ============================================================================
;; lex_soft(const char *s, int64_t len) -> rax = TF_SOFT_* bit, or 0
;;
;; The soft keywords stay TOK_NAME -- `match` is an ordinary identifier almost
;; everywhere -- but carrying a flag lets the parser test a bit at each
;; statement boundary instead of doing a string compare.
;; ============================================================================
DEF_FUNC_BARE lex_soft
    xor eax, eax
    cmp rsi, 1
    je .one
    cmp rsi, 4
    je .four
    cmp rsi, 5
    je .five
    ret
.one:
    cmp byte [rdi], '_'
    jne .no
    mov eax, TF_SOFT_UNDER
    ret
.four:
    mov ecx, [rdi]
    cmp ecx, 'case'
    jne .four_type
    mov eax, TF_SOFT_CASE
    ret
.four_type:
    cmp ecx, 'type'
    jne .no
    mov eax, TF_SOFT_TYPE
    ret
.five:
    mov ecx, [rdi]
    cmp ecx, 'matc'
    jne .no
    cmp byte [rdi + 4], 'h'
    jne .no
    mov eax, TF_SOFT_MATCH
    ret
.no:
    xor eax, eax
    ret
END_FUNC lex_soft

;; ============================================================================
;; lex_str_prefix(const char *s, int64_t len) -> rax = TF_STR_* flags, or -1
;;
;; Called only when an identifier of one or two bytes sits directly against a
;; quote.  Returns -1 when the letters are not a legal prefix, so `rb'x'` is a
;; bytes literal while `xy'z'` stays a name followed by a string -- which is
;; what CPython does, and what makes `print'x'` a syntax error rather than a
;; mysterious literal.
;; ============================================================================
DEF_FUNC_BARE lex_str_prefix
    cmp rsi, 1
    je .one
    cmp rsi, 2
    je .two
    mov rax, -1
    ret

.one:
    movzx eax, byte [rdi]
    or eax, 0x20                        ; fold case
    cmp al, 'r'
    je .ret_raw
    cmp al, 'b'
    je .ret_bytes
    cmp al, 'f'
    je .ret_fmt
    cmp al, 'u'
    je .ret_none                        ; u'' is a plain str; the u means nothing
    mov rax, -1
    ret

.two:
    movzx eax, byte [rdi]
    or eax, 0x20
    movzx edx, byte [rdi + 1]
    or edx, 0x20
    ; Exactly four two-letter prefixes exist: br rb fr rf.  'u' never combines.
    cmp al, 'b'
    jne .two_not_b
    cmp dl, 'r'
    jne .bad
    mov eax, TF_STR_BYTES | TF_STR_RAW
    ret
.two_not_b:
    cmp al, 'f'
    jne .two_not_f
    cmp dl, 'r'
    jne .bad
    mov eax, TF_STR_FMT | TF_STR_RAW
    ret
.two_not_f:
    cmp al, 'r'
    jne .bad
    cmp dl, 'b'
    je .ret_rb
    cmp dl, 'f'
    je .ret_rf
.bad:
    mov rax, -1
    ret
.ret_rb:
    mov eax, TF_STR_RAW | TF_STR_BYTES
    ret
.ret_rf:
    mov eax, TF_STR_RAW | TF_STR_FMT
    ret
.ret_raw:
    mov eax, TF_STR_RAW
    ret
.ret_bytes:
    mov eax, TF_STR_BYTES
    ret
.ret_fmt:
    mov eax, TF_STR_FMT
    ret
.ret_none:
    xor eax, eax
    ret
END_FUNC lex_str_prefix

ASM_INIT

;; ============================================================================
;; lex_run(Comp *c, const char *start, const char *end, int lineno)
;;   -> rax = 1 on success, 0 with comp.err recorded
;;
;; With start == 0 it tokenizes the whole of comp.src.  With a span it appends
;; that span's tokens to the same array, which is how an f-string's replacement
;; fields are parsed: the tokens go on the end, the parser's cursor is pointed
;; at them, and the ordinary expression parser does the rest.
;;
;; Registers held across the whole loop:
;;   rbx = Comp*        r12 = read cursor        r13 = end of source
;;   r14 = &comp.lex    r15 = start of the token being scanned
;;
;; The cursor lives in r12 rather than in memory; it is written back to
;; lex.cur only on the paths that report an error, which is the only place
;; anything else reads it.
;; ============================================================================
DEF_FUNC lex_run, LR_FRAME
    push rbx
    push r12
    push r13
    push r14
    push r15

    mov rbx, rdi
    mov [rbp - LR_COMP], rdi
    lea r14, [rbx + Comp.lex]

    ; lex_run_range passes an explicit span; lex_run covers the whole source.
    test rsi, rsi
    jnz .have_span
    mov r12, [rbx + Comp.src]
    mov r13, r12
    add r13, [rbx + Comp.srclen]
    mov ecx, 1
    jmp .set_state
.have_span:
    mov r12, rsi
    mov r13, rdx
    mov ecx, r8d
.set_state:
    mov [r14 + Lexer.cur], r12
    mov [r14 + Lexer.end], r13
    mov [r14 + Lexer.line_start], r12
    mov [r14 + Lexer.lineno], ecx
    mov dword [r14 + Lexer.paren_depth], 0
    mov dword [r14 + Lexer.indent_top], 0
    mov dword [r14 + Lexer.indents], 0
    ; A span is a fragment in the middle of a line -- an f-string's replacement
    ; field -- so it does not begin a logical line.  Starting it at one would
    ; read `f'{ x }'`'s leading space as an indent.
    mov dword [r14 + Lexer.atbol], 1
    test rsi, rsi
    jz .whole_file
    mov dword [r14 + Lexer.atbol], 0
    ; A replacement field is scanned as its own span, and PEP 701 lets a
    ; newline inside one be a continuation rather than the end of a statement.
    ; At depth 0 the newline emitted NEWLINE and the next line's leading space
    ; became an INDENT, in the middle of an expression.  Depth 1 is what makes
    ; .newline suppress the token and .top skip the indent measurement;
    ; .op_close already refuses to go below zero, so an unbalanced `)` inside
    ; the span cannot re-enable it.
    mov dword [r14 + Lexer.paren_depth], 1
.whole_file:

.top:
    cmp dword [r14 + Lexer.atbol], 0
    je .scan
    cmp dword [r14 + Lexer.paren_depth], 0
    jne .clear_bol                      ; inside brackets, indentation is not a thing

    ;; --- measure the indentation of a new logical line --------------------
    ; Columns follow CPython: a space advances one, a tab advances to the next
    ; multiple of LEX_TABSIZE, a formfeed resets to zero.
    xor r15d, r15d                      ; r15d = column
.ind_loop:
    cmp r12, r13
    jae .ind_done
    movzx eax, byte [r12]
    cmp al, ' '
    je .ind_space
    cmp al, 9
    je .ind_tab
    cmp al, 12
    je .ind_ff
    jmp .ind_done
.ind_space:
    inc r15d
    inc r12
    jmp .ind_loop
.ind_tab:
    mov eax, r15d
    xor edx, edx
    mov ecx, LEX_TABSIZE
    div ecx
    inc eax
    imul eax, ecx
    mov r15d, eax
    inc r12
    jmp .ind_loop
.ind_ff:
    xor r15d, r15d
    inc r12
    jmp .ind_loop
.ind_done:
    ; A line holding only a comment, or nothing at all, carries no indentation
    ; information and does not end a logical line: it produces no INDENT, no
    ; DEDENT and no NEWLINE, and leaves the lexer still at a line start.
    cmp r12, r13
    jae .eof                            ; the epilogue emits the pending dedents
    movzx eax, byte [r12]
    cmp al, '#'
    je .blank_line
    cmp al, 10
    je .blank_line
    cmp al, 13
    je .blank_line

    ; Compare against the top of the indent stack.
    mov ecx, [r14 + Lexer.indent_top]
    mov edx, [r14 + rcx*4 + Lexer.indents]
    cmp r15d, edx
    je .clear_bol
    ja .do_indent

.do_dedent:
    ; Pop while the new column is below the top of the stack, then require an
    ; exact match.  Landing between two levels is an IndentationError, not a
    ; silent realignment -- which is the whole reason the stack is kept.
    mov ecx, [r14 + Lexer.indent_top]
    mov edx, [r14 + rcx*4 + Lexer.indents]
    cmp r15d, edx
    je .clear_bol                       ; back to a level we know
    ja .dedent_bad                      ; between two levels
    test ecx, ecx
    jz .dedent_bad                      ; below column zero is not reachable
    dec ecx
    mov [r14 + Lexer.indent_top], ecx
    mov rdi, rbx
    mov esi, TOK_DEDENT
    mov rdx, r12
    xor ecx, ecx
    xor r8d, r8d
    call lex_emit
    jmp .do_dedent
.dedent_bad:
    mov rdi, rbx
    lea rsi, [rel exc_IndentationError_type]
    CSTRING rdx, "unindent does not match any outer indentation level"
    mov ecx, [r14 + Lexer.lineno]
    mov r8d, r15d
    call comp_error
    jmp .fail

.do_indent:
    mov ecx, [r14 + Lexer.indent_top]
    cmp ecx, LEX_MAX_INDENT - 1
    jae .indent_deep
    inc ecx
    mov [r14 + Lexer.indent_top], ecx
    mov [r14 + rcx*4 + Lexer.indents], r15d
    mov rdi, rbx
    mov esi, TOK_INDENT
    mov rdx, r12
    xor ecx, ecx
    xor r8d, r8d
    call lex_emit
    jmp .clear_bol
.indent_deep:
    mov rdi, rbx
    lea rsi, [rel exc_IndentationError_type]
    CSTRING rdx, "too many levels of indentation"
    mov ecx, [r14 + Lexer.lineno]
    xor r8d, r8d
    call comp_error
    jmp .fail

.blank_line:
    ; Consume a comment-only or empty line whole, advancing the line counter
    ; but leaving atbol set, so the next real line is still measured for
    ; indentation against the same stack.
    cmp r12, r13
    jae .eof
    cmp byte [r12], '#'
    jne .bl_at_eol
.bl_comment:
    inc r12
    cmp r12, r13
    jae .eof
    movzx eax, byte [r12]
    cmp al, 10
    je .bl_at_eol
    cmp al, 13
    je .bl_at_eol
    jmp .bl_comment
.bl_at_eol:
    cmp r12, r13
    jae .eof
    movzx eax, byte [r12]
    cmp al, 13
    jne .bl_lf
    inc r12
    cmp r12, r13
    jae .bl_advance
    cmp byte [r12], 10
    jne .bl_advance
.bl_lf:
    inc r12
.bl_advance:
    inc dword [r14 + Lexer.lineno]
    mov [r14 + Lexer.line_start], r12
    jmp .top

.clear_bol:
    mov dword [r14 + Lexer.atbol], 0

    ;; --- the token scanner -------------------------------------------------
.scan:
    cmp r12, r13
    jae .eof
    movzx eax, byte [r12]

    ; horizontal whitespace
    cmp al, ' '
    je .skip_one
    cmp al, 9
    je .skip_one
    cmp al, 12
    je .skip_one

    cmp al, '#'
    je .comment
    cmp al, 10
    je .newline
    cmp al, 13
    je .newline
    cmp al, 92                          ; backslash: explicit line continuation
    je .continuation

    lea rcx, [rel cc_table]
    movzx edx, byte [rcx + rax]
    test dl, CC_IDSTART
    jnz .ident
    test dl, CC_DIGIT
    jnz .number
    test dl, CC_QUOTE
    jnz .string_plain
    cmp al, '.'
    je .maybe_number
    test dl, CC_OPSTART
    jnz .operator
    jmp .bad_char

.skip_one:
    inc r12
    jmp .scan

.comment:
    ; Run to the end of the line, leaving the newline itself for .newline.
.comment_loop:
    cmp r12, r13
    jae .scan
    movzx eax, byte [r12]
    cmp al, 10
    je .scan
    cmp al, 13
    je .scan
    inc r12
    jmp .comment_loop

.continuation:
    ; A backslash joins this line to the next.  It must be the last thing on
    ; the line; anything else is an error.
    inc r12
    cmp r12, r13
    jae .cont_bad
    movzx eax, byte [r12]
    cmp al, 13
    jne .cont_nl
    inc r12
    cmp r12, r13
    jae .cont_advance
    cmp byte [r12], 10
    jne .cont_advance
.cont_nl:
    cmp byte [r12], 10
    jne .cont_bad
    inc r12
.cont_advance:
    inc dword [r14 + Lexer.lineno]
    mov [r14 + Lexer.line_start], r12
    jmp .scan
.cont_bad:
    mov rdi, rbx
    lea rsi, [rel exc_SyntaxError_type]
    CSTRING rdx, "unexpected character after line continuation character"
    mov ecx, [r14 + Lexer.lineno]
    xor r8d, r8d
    call comp_error
    jmp .fail

.newline:
    mov r15, r12                        ; remember where the newline started
    cmp al, 13
    jne .nl_lf
    inc r12
    cmp r12, r13
    jae .nl_done
    cmp byte [r12], 10
    jne .nl_done
.nl_lf:
    inc r12
.nl_done:
    ; Inside brackets a newline is implicit continuation: no token, but the
    ; line counter still has to move or every later diagnostic is wrong.
    cmp dword [r14 + Lexer.paren_depth], 0
    jne .nl_no_token
    mov rdi, rbx
    mov esi, TOK_NEWLINE
    mov rdx, r15
    mov ecx, 1
    xor r8d, r8d
    call lex_emit
    mov dword [r14 + Lexer.atbol], 1
.nl_no_token:
    inc dword [r14 + Lexer.lineno]
    mov [r14 + Lexer.line_start], r12
    jmp .top

.maybe_number:
    ; '.' begins a number only when a digit follows; otherwise it is the
    ; attribute operator, and '...' is handled by the operator table.
    lea rax, [r12 + 1]
    cmp rax, r13
    jae .operator
    movzx edx, byte [rax]
    lea rcx, [rel cc_table]
    test byte [rcx + rdx], CC_DIGIT
    jz .operator
    jmp .number

.bad_char:
    mov r8, r12
    sub r8, [r14 + Lexer.line_start]        ; column, before rdx is claimed
    mov rdi, rbx
    lea rsi, [rel exc_SyntaxError_type]
    mov ecx, [r14 + Lexer.lineno]
    CSTRING rdx, "invalid character in source"
    call comp_error
    jmp .fail


;; --- identifiers, keywords, and prefixed string literals -------------------
.ident:
    mov r15, r12                        ; r15 = start of the identifier
    lea rcx, [rel cc_table]
.ident_loop:
    inc r12
    cmp r12, r13
    jae .ident_done
    movzx eax, byte [r12]
    test byte [rcx + rax], CC_IDCONT
    jnz .ident_loop
.ident_done:
    ; An identifier butted straight against a quote is a string prefix, not a
    ; name: rb"..", f'..'.  Only 1- and 2-character prefixes exist, so anything
    ; longer is unambiguously an identifier.
    cmp r12, r13
    jae .ident_word
    movzx eax, byte [r12]
    lea rcx, [rel cc_table]
    test byte [rcx + rax], CC_QUOTE
    jz .ident_word
    mov rsi, r12
    sub rsi, r15                        ; rsi = length of the candidate prefix
    cmp rsi, 2
    ja .ident_word
    mov rdi, r15
    call lex_str_prefix                 ; -> rax = TF_STR_* flags, or -1
    cmp rax, -1
    je .ident_word
    mov [rbp - LR_FLAGS], rax
    jmp .string_scan

.ident_word:
    mov rdi, r15
    mov rsi, r12
    sub rsi, r15
    call lex_keyword                    ; rax = keyword kind, or TOK_NAME
    mov [rbp - LR_KIND], rax
    mov qword [rbp - LR_FLAGS], 0
    cmp rax, TOK_NAME
    jne .ident_emit
    ; Soft keywords stay TOK_NAME but carry a flag, so the parser tests a bit
    ; rather than doing a string compare at every statement boundary.
    mov rdi, r15
    mov rsi, r12
    sub rsi, r15
    call lex_soft
    mov [rbp - LR_FLAGS], rax
.ident_emit:
    mov rdi, rbx
    mov rsi, [rbp - LR_KIND]
    mov rdx, r15
    mov rcx, r12
    sub rcx, r15
    mov r8, [rbp - LR_FLAGS]
    call lex_emit
    jmp .scan

;; --- numeric literals ------------------------------------------------------
; The extent is scanned here; the value is built later from the token text, so
; that a literal in a construct that fails to parse never allocates.
.number:
    mov r15, r12
    mov qword [rbp - LR_FLAGS], 0
    cmp byte [r12], '.'
    je .num_fraction

    cmp byte [r12], '0'
    jne .num_decimal
    lea rax, [r12 + 1]
    cmp rax, r13
    jae .num_decimal
    movzx eax, byte [r12 + 1]
    or eax, 0x20                        ; fold case
    cmp al, 'x'
    je .num_radix
    cmp al, 'o'
    je .num_radix
    cmp al, 'b'
    je .num_radix
    jmp .num_decimal

.num_radix:
    ; Scan permissively over hex digits and underscores; whether the digits
    ; actually suit the radix is the converter's problem, and it gives a better
    ; message than the lexer could.
    add r12, 2
    lea rcx, [rel cc_table]
.num_radix_loop:
    cmp r12, r13
    jae .num_done
    movzx eax, byte [r12]
    cmp al, '_'
    je .num_radix_next
    test byte [rcx + rax], CC_HEX
    jz .num_done
.num_radix_next:
    inc r12
    jmp .num_radix_loop

.num_decimal:
    call .sub_digits
    cmp r12, r13
    jae .num_done
    cmp byte [r12], '.'
    jne .num_exponent
.num_fraction:
    or qword [rbp - LR_FLAGS], TF_NUM_FLOAT
    inc r12
    call .sub_digits
.num_exponent:
    cmp r12, r13
    jae .num_imag
    movzx eax, byte [r12]
    or eax, 0x20
    cmp al, 'e'
    jne .num_imag
    ; Only an exponent if a digit follows, optionally signed -- otherwise the
    ; 'e' begins an identifier, as in `1if x else 2` or `0e` as a name.
    lea rax, [r12 + 1]
    cmp rax, r13
    jae .num_imag
    movzx edx, byte [rax]
    cmp dl, '+'
    je .num_exp_signed
    cmp dl, '-'
    je .num_exp_signed
    jmp .num_exp_check
.num_exp_signed:
    inc rax
    cmp rax, r13
    jae .num_imag
    movzx edx, byte [rax]
.num_exp_check:
    lea rcx, [rel cc_table]
    test byte [rcx + rdx], CC_DIGIT
    jz .num_imag
    or qword [rbp - LR_FLAGS], TF_NUM_FLOAT
    mov r12, rax
    call .sub_digits

.num_imag:
    cmp r12, r13
    jae .num_done
    movzx eax, byte [r12]
    or eax, 0x20
    cmp al, 'j'
    jne .num_done
    or qword [rbp - LR_FLAGS], TF_NUM_IMAG
    inc r12

.num_done:
    mov rdi, rbx
    mov esi, TOK_NUMBER
    mov rdx, r15
    mov rcx, r12
    sub rcx, r15
    mov r8, [rbp - LR_FLAGS]
    call lex_emit
    jmp .scan

;; --- string literals -------------------------------------------------------
.string_plain:
    mov r15, r12                        ; no prefix; the literal starts here
    mov qword [rbp - LR_FLAGS], 0
.string_scan:
    ; r15 is the start of the whole token (prefix included), r12 points at the
    ; opening quote, and LR_FLAGS holds the prefix bits.
    ;
    ; A triple-quoted literal spans lines, but the token belongs to the line it
    ; STARTED on: lex_emit stamps Token.lineno from the counter and computes
    ; Token.col as start - line_start, which goes NEGATIVE once line_start has
    ; moved past the token.  As a u32 that is about 4.29e9, and the traceback's
    ; caret pads to it one space at a time.  So .sub_string counts the lines
    ; here and they are applied below, after the token is stamped.
    mov qword [rbp - LR_STRNL], 0
    call .sub_string                    ; advances r12 past the closing quote
    test eax, eax
    jnz .string_ok
    mov r8, r15
    sub r8, [r14 + Lexer.line_start]    ; the column, before rdx is claimed
    mov rdi, rbx
    lea rsi, [rel exc_SyntaxError_type]
    CSTRING rdx, "unterminated string literal"
    mov ecx, [r14 + Lexer.lineno]
    call comp_error
    jmp .fail
.string_ok:
    mov rdi, rbx
    mov esi, TOK_STRING
    mov rdx, r15
    mov rcx, r12
    sub rcx, r15
    mov r8, [rbp - LR_FLAGS]
    call lex_emit
    ; Only now do the lines the literal spanned move the counter.
    mov rax, [rbp - LR_STRNL]
    test rax, rax
    jz .scan
    add [r14 + Lexer.lineno], eax
    mov rax, [rbp - LR_STRLS]
    mov [r14 + Lexer.line_start], rax
    jmp .scan

;; --- operators and delimiters ---------------------------------------------
.operator:
    mov r15, r12
    mov rdi, r12
    mov rsi, r13
    call lex_operator                   ; rax = kind, rdx = length
    test eax, eax
    jz .bad_char
    add r12, rdx
    mov [rbp - LR_KIND], rax

    ; Bracket depth drives implicit line joining, so it is maintained here
    ; rather than in the parser: the lexer is what has to know.
    cmp eax, TOK_LPAR
    je .op_open
    cmp eax, TOK_LSQB
    je .op_open
    cmp eax, TOK_LBRACE
    je .op_open
    cmp eax, TOK_RPAR
    je .op_close
    cmp eax, TOK_RSQB
    je .op_close
    cmp eax, TOK_RBRACE
    je .op_close
    jmp .op_emit
.op_open:
    inc dword [r14 + Lexer.paren_depth]
    jmp .op_emit
.op_close:
    cmp dword [r14 + Lexer.paren_depth], 0
    je .op_emit                         ; unbalanced; the parser reports it
    dec dword [r14 + Lexer.paren_depth]
.op_emit:
    mov rdi, rbx
    mov rsi, [rbp - LR_KIND]
    mov rdx, r15
    mov rcx, r12
    sub rcx, r15
    xor r8d, r8d
    call lex_emit
    jmp .scan

;; --- end of input ----------------------------------------------------------
; CPython's shape: a NEWLINE if the last logical line was not terminated, then
; one DEDENT per open level, then ENDMARKER.  The parser relies on every suite
; being closed before it sees ENDMARKER.
.eof:
    mov rax, [rbx + Comp.tokens + Buf.len]
    test rax, rax
    jz .eof_dedents
    dec rax
    shl rax, TOKEN_SHIFT
    add rax, [rbx + Comp.tokens + Buf.data]
    movzx eax, word [rax + Token.kind]
    cmp eax, TOK_NEWLINE
    je .eof_dedents
    cmp eax, TOK_INDENT
    je .eof_dedents
    cmp eax, TOK_DEDENT
    je .eof_dedents
    mov rdi, rbx
    mov esi, TOK_NEWLINE
    mov rdx, r13
    mov ecx, 0
    xor r8d, r8d
    call lex_emit

.eof_dedents:
    mov ecx, [r14 + Lexer.indent_top]
    test ecx, ecx
    jz .eof_end
    dec ecx
    mov [r14 + Lexer.indent_top], ecx
    mov rdi, rbx
    mov esi, TOK_DEDENT
    mov rdx, r13
    xor ecx, ecx
    xor r8d, r8d
    call lex_emit
    jmp .eof_dedents

.eof_end:
    mov rdi, rbx
    mov esi, TOK_ENDMARKER
    mov rdx, r13
    xor ecx, ecx
    xor r8d, r8d
    call lex_emit
    mov eax, 1
    jmp .ret


;; --- local subroutines -----------------------------------------------------
; These run inside lex_run's register world: r12 is the cursor, r13 the end,
; r14 the lexer state.  They advance r12 and touch nothing else.

; .sub_digits - advance over decimal digits and the underscores between them.
.sub_digits:
    lea rcx, [rel cc_table]
.sd_loop:
    cmp r12, r13
    jae .sd_done
    movzx eax, byte [r12]
    cmp al, '_'
    je .sd_next
    test byte [rcx + rax], CC_DIGIT
    jz .sd_done
.sd_next:
    inc r12
    jmp .sd_loop
.sd_done:
    ret

; .sub_string - advance past a string literal, r12 pointing at its open quote.
;   -> eax = 1, or 0 for an unterminated literal (the caller reports it).
; A backslash escapes the next byte even in a raw string: r"\" is unterminated
; in Python too, because rawness affects the *value*, not where the token ends.
.sub_string:
    movzx r10d, byte [r12]              ; the quote character
    mov r11d, 1                         ; quote run length
    lea rax, [r12 + 2]
    cmp rax, r13
    ja .ss_single
    movzx edx, byte [r12 + 1]
    cmp edx, r10d
    jne .ss_single
    movzx edx, byte [r12 + 2]
    cmp edx, r10d
    jne .ss_empty                       ; '' or "" -- an empty short string
    mov r11d, 3
.ss_single:
    add r12, r11
.ss_loop:
    cmp r12, r13
    jae .ss_unterminated
    movzx eax, byte [r12]
    cmp al, 92                          ; backslash
    je .ss_escape
    cmp al, 10
    je .ss_newline
    cmp eax, r10d
    je .ss_maybe_close
    inc r12
    jmp .ss_loop

.ss_escape:
    ; Skip the escaped byte.  A backslash-newline inside a literal still ends
    ; a source line, so the line counter has to move with it.
    inc r12
    cmp r12, r13
    jae .ss_unterminated
    cmp byte [r12], 10
    jne .ss_escape_done
    inc qword [rbp - LR_STRNL]
    lea rax, [r12 + 1]
    mov [rbp - LR_STRLS], rax
.ss_escape_done:
    inc r12
    jmp .ss_loop

.ss_newline:
    ; Only a triple-quoted literal may span lines.
    cmp r11d, 3
    jne .ss_unterminated
    inc qword [rbp - LR_STRNL]
    inc r12
    mov [rbp - LR_STRLS], r12
    jmp .ss_loop

.ss_maybe_close:
    cmp r11d, 3
    je .ss_close3
    inc r12
    mov eax, 1
    ret
.ss_close3:
    lea rax, [r12 + 2]
    cmp rax, r13
    jae .ss_close3_short
    movzx edx, byte [r12 + 1]
    cmp edx, r10d
    jne .ss_close3_no
    movzx edx, byte [r12 + 2]
    cmp edx, r10d
    jne .ss_close3_no
    add r12, 3
    mov eax, 1
    ret
.ss_close3_short:
.ss_close3_no:
    inc r12
    jmp .ss_loop

.ss_empty:
    add r12, 2
    mov eax, 1
    ret

.ss_unterminated:
    xor eax, eax
    ret

.fail:
    mov [r14 + Lexer.cur], r12
    xor eax, eax
    jmp .ret

.ret:
    pop r15
    pop r14
    pop r13
    pop r12
    pop rbx
    leave
    ret
END_FUNC lex_run
