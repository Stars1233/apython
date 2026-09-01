; pattern.asm - the `match` statement: its patterns, parsed
;
; A pattern is not an expression.  `case x` BINDS x rather than reading it,
; `case C(a)` calls nothing, and `case {"k": v}` neither builds a dict nor
; subscripts one.  So patterns get their own node kinds; reusing AST_NAME and
; AST_CALL would leave every consumer asking which of the two meanings it was
; looking at.
;
; `match` and `case` are soft keywords -- ordinary names that the grammar only
; treats specially in one position.  CPython settles that by backtracking; we
; settle it by looking ahead to the end of the logical line, because the one
; thing that distinguishes `match x:` from `match(x)` or `match = 1` is that a
; match statement's header line ends with a colon.  The lexer only emits
; NEWLINE at bracket depth zero, so the scan cannot run off into a subscript.

%include "macros.inc"
%include "object.inc"
%include "value.inc"
%include "compiler.inc"

extern ast_at
extern ast_make
extern ast_mark
extern ast_push
extern comp_error
extern par_advance
extern par_expect
extern par_expr
extern par_finish_list
extern par_kind
extern par_peek
extern par_suite_into
extern par_syntax_error
extern ap_memcmp
extern par_name_obj
extern par_exprlist_stmt

global ps_match
global par_looks_like_match
global par_soft_keyword_is

section .text

;; ============================================================================
;; par_soft_keyword_is(Comp *c, const char *word, uint64_t len) -> rax = 1/0
;; Is the current token a NAME spelled exactly `word`?
;; ============================================================================
SK_WORD  equ 8
SK_LEN   equ 16
SK_FRAME equ 24           ; + 1 push = 32
DEF_FUNC par_soft_keyword_is, SK_FRAME
    push rbx
    mov rbx, rdi
    mov [rbp - SK_WORD], rsi
    mov [rbp - SK_LEN], rdx
    call par_kind
    cmp eax, TOK_NAME
    jne .no
    mov rdi, rbx
    call par_peek
    mov ecx, [rax + Token.len]
    cmp rcx, [rbp - SK_LEN]
    jne .no
    mov rdi, [rax + Token.start]
    mov rsi, [rbp - SK_WORD]
    mov rdx, [rbp - SK_LEN]
    call ap_memcmp
    test eax, eax
    jne .no
    mov eax, 1
    pop rbx
    leave
    ret
.no:
    xor eax, eax
    pop rbx
    leave
    ret
END_FUNC par_soft_keyword_is

;; ============================================================================
;; par_looks_like_match(Comp *c) -> rax = 1 if the current line is a `match`
;; header, else 0.
;;
;; True when the token is the name `match`, the next token can begin an
;; expression, and the logical line ends with a colon.  That last test is what
;; separates `match x:` from `match(x)`, `match = 1`, `match[i] = v` and the
;; annotation `match: int`, all of which end in something else.
;; ============================================================================
LM_I     equ 8
LM_FRAME equ 8            ; + 1 push = 16
DEF_FUNC par_looks_like_match, LM_FRAME
    push rbx
    mov rbx, rdi
    lea rsi, [rel pm_match_kw]
    mov edx, 5
    call par_soft_keyword_is
    test eax, eax
    jz .no

    ; The token after `match` must be able to start an expression: a bare
    ; `match:` on its own is an annotation, not a statement.
    mov eax, [rbx + Comp.tok_idx]
    inc eax
    mov [rbp - LM_I], rax
    mov rdx, [rbx + Comp.tokens + Buf.len]
    cmp rax, rdx
    jae .no
    mov rcx, [rbx + Comp.tokens + Buf.data]
    imul rax, rax, Token_size
    movzx eax, word [rcx + rax + Token.kind]
    cmp eax, TOK_COLON
    je .no
    cmp eax, TOK_NEWLINE
    je .no
    cmp eax, TOK_EQUAL
    je .no

    ; Walk to the end of the logical line.  NEWLINE is only emitted at bracket
    ; depth zero, so this stops at the header's own end.
    mov rax, [rbp - LM_I]
.scan:
    mov rdx, [rbx + Comp.tokens + Buf.len]
    cmp rax, rdx
    jae .no
    mov rcx, [rbx + Comp.tokens + Buf.data]
    mov rsi, rax
    imul rsi, rsi, Token_size
    movzx esi, word [rcx + rsi + Token.kind]
    cmp esi, TOK_NEWLINE
    je .at_end
    cmp esi, TOK_ENDMARKER
    je .no
    mov [rbp - LM_I], rax
    inc rax
    jmp .scan
.at_end:
    ; LM_I is the last token before the NEWLINE.
    mov rax, [rbp - LM_I]
    mov rcx, [rbx + Comp.tokens + Buf.data]
    imul rax, rax, Token_size
    movzx eax, word [rcx + rax + Token.kind]
    cmp eax, TOK_COLON
    jne .no
    mov eax, 1
    pop rbx
    leave
    ret
.no:
    xor eax, eax
    pop rbx
    leave
    ret
END_FUNC par_looks_like_match

;; ============================================================================
;; ps_match(Comp *c) -> node   -- `match subject:` NEWLINE INDENT case+ DEDENT
;; ============================================================================
PM_LINE  equ 8
PM_SUBJ  equ 16
PM_MARK  equ 24
PM_NODE  equ 32
PM_FRAME equ 40           ; + 1 push = 48
DEF_FUNC ps_match, PM_FRAME
    push rbx
    mov rbx, rdi
    call par_peek
    mov ecx, [rax + Token.lineno]
    mov [rbp - PM_LINE], rcx
    mov rdi, rbx
    call par_advance                    ; the soft keyword `match`

    ; The subject is an expression list: `match a, b:` matches the tuple.
    mov rdi, rbx
    call par_exprlist_stmt
    test rax, rax
    jz .fail
    mov [rbp - PM_SUBJ], rax

    mov rdi, rbx
    mov esi, TOK_COLON
    CSTRING rdx, "expected ':' after the match subject"
    call par_expect
    test eax, eax
    jz .fail
    mov rdi, rbx
    mov esi, TOK_NEWLINE
    CSTRING rdx, "expected a newline after 'match'"
    call par_expect
    test eax, eax
    jz .fail
    mov rdi, rbx
    mov esi, TOK_INDENT
    CSTRING rdx, "expected an indented block of case clauses"
    call par_expect
    test eax, eax
    jz .fail

    mov rdi, rbx
    call ast_mark
    mov [rbp - PM_MARK], rax
.case_loop:
    mov rdi, rbx
    call par_kind
    cmp eax, TOK_DEDENT
    je .close
    cmp eax, TOK_ENDMARKER
    je .close
    mov rdi, rbx
    call par_case
    test rax, rax
    jz .fail
    mov rdi, rbx
    mov rsi, rax
    call ast_push
    jmp .case_loop

.close:
    mov rdi, rbx
    call par_kind
    cmp eax, TOK_DEDENT
    jne .no_dedent
    mov rdi, rbx
    call par_advance
.no_dedent:
    mov rdi, rbx
    mov esi, AST_MATCH
    mov rdx, [rbp - PM_LINE]
    mov rcx, [rbp - PM_MARK]
    call par_finish_list
    test rax, rax
    jz .fail
    mov [rbp - PM_NODE], rax
    mov rdi, rbx
    mov rsi, rax
    call ast_at
    mov rdx, [rbp - PM_SUBJ]
    mov [rax + AstNode.a], edx
    mov rax, [rbp - PM_NODE]
    pop rbx
    leave
    ret
.fail:
    xor eax, eax
    pop rbx
    leave
    ret
END_FUNC ps_match

;; ============================================================================
;; par_case(Comp *c) -> node   -- `case pattern [if guard]:` suite
;; ============================================================================
PC2_LINE  equ 8
PC2_PAT   equ 16
PC2_GUARD equ 24
PC2_MARK  equ 32
PC2_NODE  equ 40
PC2_FRAME equ 40          ; + 1 push = 48
DEF_FUNC_LOCAL par_case, PC2_FRAME
    push rbx
    mov rbx, rdi
    lea rsi, [rel pm_case_kw]
    mov edx, 4
    call par_soft_keyword_is
    test eax, eax
    jz .not_case
    mov rdi, rbx
    call par_peek
    mov ecx, [rax + Token.lineno]
    mov [rbp - PC2_LINE], rcx
    mov rdi, rbx
    call par_advance                    ; `case`

    mov rdi, rbx
    call par_patterns
    test rax, rax
    jz .fail
    mov [rbp - PC2_PAT], rax

    mov qword [rbp - PC2_GUARD], 0
    mov rdi, rbx
    call par_kind
    cmp eax, TOK_IF
    jne .colon
    mov rdi, rbx
    call par_advance
    mov rdi, rbx
    mov esi, BP_NONE
    call par_expr
    test rax, rax
    jz .fail
    mov [rbp - PC2_GUARD], rax
.colon:
    mov rdi, rbx
    call ast_mark
    mov [rbp - PC2_MARK], rax
    mov rdi, rbx
    call par_suite_into
    test eax, eax
    jz .fail
    mov rdi, rbx
    mov esi, AST_CASE
    mov rdx, [rbp - PC2_LINE]
    mov rcx, [rbp - PC2_MARK]
    call par_finish_list
    test rax, rax
    jz .fail
    mov [rbp - PC2_NODE], rax
    mov rdi, rbx
    mov rsi, rax
    call ast_at
    mov rdx, [rbp - PC2_PAT]
    mov [rax + AstNode.a], edx
    mov rdx, [rbp - PC2_GUARD]
    mov [rax + AstNode.b], edx
    mov rax, [rbp - PC2_NODE]
    pop rbx
    leave
    ret
.not_case:
    mov rdi, rbx
    CSTRING rsi, "expected 'case' in a match block"
    call par_syntax_error
.fail:
    xor eax, eax
    pop rbx
    leave
    ret
END_FUNC par_case

;; ============================================================================
;; par_patterns(Comp *c) -> node
;; The whole `case` pattern.  A top-level comma makes it a sequence pattern,
;; exactly as it makes an expression a tuple -- `case 1, 2:` matches a
;; two-element sequence.
;; ============================================================================
PP_LINE  equ 8
PP_FIRST equ 16
PP_MARK  equ 24
PP_STAR  equ 32
PP_I     equ 40
PP_NODE  equ 48
PP_FRAME equ 56           ; + 1 push = 64
DEF_FUNC_LOCAL par_patterns, PP_FRAME
    push rbx
    mov rbx, rdi
    call par_peek
    mov ecx, [rax + Token.lineno]
    mov [rbp - PP_LINE], rcx

    mov rdi, rbx
    call par_or_pattern
    test rax, rax
    jz .fail
    mov [rbp - PP_FIRST], rax

    mov rdi, rbx
    call par_kind
    cmp eax, TOK_COMMA
    je .sequence
    mov rax, [rbp - PP_FIRST]
    pop rbx
    leave
    ret

.sequence:
    mov rdi, rbx
    call ast_mark
    mov [rbp - PP_MARK], rax
    mov qword [rbp - PP_STAR], 0
    mov qword [rbp - PP_I], 0
    mov rdi, rbx
    mov rsi, [rbp - PP_FIRST]
    call ast_push
    call .note_star
.more:
    inc qword [rbp - PP_I]
    mov rdi, rbx
    call par_kind
    cmp eax, TOK_COMMA
    jne .close
    mov rdi, rbx
    call par_advance
    mov rdi, rbx
    call par_kind
    cmp eax, TOK_COLON
    je .close
    cmp eax, TOK_IF
    je .close
    mov rdi, rbx
    call par_or_pattern
    test rax, rax
    jz .fail
    mov [rbp - PP_FIRST], rax
    mov rdi, rbx
    mov rsi, rax
    call ast_push
    call .note_star
    jmp .more

.close:
    mov rdi, rbx
    mov esi, AST_PAT_SEQUENCE
    mov rdx, [rbp - PP_LINE]
    mov rcx, [rbp - PP_MARK]
    call par_finish_list
    test rax, rax
    jz .fail
    mov [rbp - PP_NODE], rax
    mov rdi, rbx
    mov rsi, rax
    call ast_at
    mov rdx, [rbp - PP_STAR]
    mov [rax + AstNode.b], edx
    mov rax, [rbp - PP_NODE]
    pop rbx
    leave
    ret

; Local: remember where the starred element is, one-based so 0 means none.
.note_star:
    sub rsp, 8
    mov rdi, rbx
    mov rsi, [rbp - PP_FIRST]
    call ast_at
    movzx ecx, byte [rax + AstNode.kind]
    cmp ecx, AST_PAT_CAPTURE
    jne .ns_done
    cmp byte [rax + AstNode.subkind], 1     ; a starred capture
    jne .ns_done
    mov rax, [rbp - PP_I]
    inc rax
    mov [rbp - PP_STAR], rax
.ns_done:
    add rsp, 8
    ret

.fail:
    xor eax, eax
    pop rbx
    leave
    ret
END_FUNC par_patterns

;; ============================================================================
;; par_or_pattern(Comp *c) -> node   -- `p1 | p2 | ...` and a trailing `as`
;; ============================================================================
PO_LINE  equ 8
PO_MARK  equ 16
PO_FIRST equ 24
PO_FRAME equ 40           ; + 1 push = 48
DEF_FUNC_LOCAL par_or_pattern, PO_FRAME
    push rbx
    mov rbx, rdi
    call par_peek
    mov ecx, [rax + Token.lineno]
    mov [rbp - PO_LINE], rcx

    mov rdi, rbx
    call par_closed_pattern
    test rax, rax
    jz .fail
    mov [rbp - PO_FIRST], rax

    mov rdi, rbx
    call par_kind
    cmp eax, TOK_VBAR
    jne .as_clause

    mov rdi, rbx
    call ast_mark
    mov [rbp - PO_MARK], rax
    mov rdi, rbx
    mov rsi, [rbp - PO_FIRST]
    call ast_push
.alt_loop:
    mov rdi, rbx
    call par_kind
    cmp eax, TOK_VBAR
    jne .close_or
    mov rdi, rbx
    call par_advance
    mov rdi, rbx
    call par_closed_pattern
    test rax, rax
    jz .fail
    mov rdi, rbx
    mov rsi, rax
    call ast_push
    jmp .alt_loop
.close_or:
    mov rdi, rbx
    mov esi, AST_PAT_OR
    mov rdx, [rbp - PO_LINE]
    mov rcx, [rbp - PO_MARK]
    call par_finish_list
    test rax, rax
    jz .fail
    mov [rbp - PO_FIRST], rax

.as_clause:
    mov rdi, rbx
    call par_kind
    cmp eax, TOK_AS
    jne .done
    mov rdi, rbx
    call par_advance
    mov rdi, rbx
    call par_name_obj
    test rax, rax
    jz .fail
    mov r9, rax
    mov rdi, rbx
    mov esi, AST_PAT_AS
    xor edx, edx
    mov rcx, [rbp - PO_LINE]
    mov r8, [rbp - PO_FIRST]
    call ast_make
    test rax, rax
    jz .fail
    mov [rbp - PO_FIRST], rax
.done:
    mov rax, [rbp - PO_FIRST]
    pop rbx
    leave
    ret
.fail:
    xor eax, eax
    pop rbx
    leave
    ret
END_FUNC par_or_pattern

;; ============================================================================
;; par_closed_pattern(Comp *c) -> node
;; One pattern with no top-level `|` or `as`: a literal, a name, a group, a
;; sequence, a mapping, or a class pattern.
;; ============================================================================
CP_LINE  equ 8
CP_FRAME equ 24           ; + 1 push = 32
DEF_FUNC_LOCAL par_closed_pattern, CP_FRAME
    push rbx
    mov rbx, rdi
    call par_peek
    mov ecx, [rax + Token.lineno]
    mov [rbp - CP_LINE], rcx
    mov rdi, rbx
    call par_kind

    cmp eax, TOK_LSQB
    je .seq_bracket
    cmp eax, TOK_LPAR
    je .group
    cmp eax, TOK_LBRACE
    je .mapping
    cmp eax, TOK_STAR
    je .star
    cmp eax, TOK_NAME
    je .name
    ; Everything else is a value to compare against: a number, a string, or
    ; one of the three singletons.
    jmp .value

;; `*rest` inside a sequence.  It is a capture with a flag rather than a kind
;; of its own, because only the enclosing sequence cares.
.star:
    mov rdi, rbx
    call par_advance
    mov rdi, rbx
    call par_kind
    cmp eax, TOK_NAME
    jne .bad_star
    mov rdi, rbx
    call par_wildcard_or_name
    test rax, rax
    js .fail
    mov r8, rax
    mov rdi, rbx
    mov esi, AST_PAT_CAPTURE
    mov edx, 1                          ; starred
    mov rcx, [rbp - CP_LINE]
    xor r9d, r9d
    call ast_make
    pop rbx
    leave
    ret
.bad_star:
    mov rdi, rbx
    CSTRING rsi, "expected a name after '*' in a pattern"
    call par_syntax_error
    jmp .fail

;; A bare name captures; a dotted name is a value to compare against; a name
;; followed by '(' is a class pattern.
.name:
    mov rdi, rbx
    call par_name_is_dotted_or_call
    test eax, eax
    jnz .value
    mov rdi, rbx
    call par_wildcard_or_name
    test rax, rax
    js .fail
    mov r8, rax
    mov rdi, rbx
    mov esi, AST_PAT_CAPTURE
    xor edx, edx
    mov rcx, [rbp - CP_LINE]
    xor r9d, r9d
    call ast_make
    pop rbx
    leave
    ret

.seq_bracket:
    mov rdi, rbx
    mov esi, TOK_RSQB
    call par_sequence_pattern
    pop rbx
    leave
    ret

.group:
    ; `(p)` is a group; `(p, q)` and `()` are sequence patterns.  Both are
    ; handled by the same reader, which collapses a single element with no
    ; trailing comma back to the element itself.
    mov rdi, rbx
    mov esi, TOK_RPAR
    call par_sequence_pattern
    pop rbx
    leave
    ret

.mapping:
    mov rdi, rbx
    call par_mapping_pattern
    pop rbx
    leave
    ret

.value:
    mov rdi, rbx
    call par_value_pattern
    pop rbx
    leave
    ret
.fail:
    xor eax, eax
    pop rbx
    leave
    ret
END_FUNC par_closed_pattern

;; ============================================================================
;; par_wildcard_or_name(Comp *c) -> rax = obj index, 0 for `_`, -1 on error
;; `_` is the wildcard: it matches anything and binds nothing, so it is the one
;; name a capture pattern does not capture.
;; ============================================================================
DEF_FUNC_LOCAL par_wildcard_or_name, 8
    push rbx
    mov rbx, rdi
    call par_peek
    mov ecx, [rax + Token.len]
    cmp ecx, 1
    jne .real_name
    mov rcx, [rax + Token.start]
    cmp byte [rcx], '_'
    jne .real_name
    mov rdi, rbx
    call par_advance
    xor eax, eax
    pop rbx
    leave
    ret
.real_name:
    mov rdi, rbx
    call par_name_obj
    test rax, rax
    jnz .ok
    mov rax, -1
.ok:
    pop rbx
    leave
    ret
END_FUNC par_wildcard_or_name

;; ============================================================================
;; par_name_is_dotted_or_call(Comp *c) -> rax = 1 if the NAME at the cursor is
;; followed by '.' or '(' -- i.e. a value or class pattern rather than a
;; capture.
;; ============================================================================
DEF_FUNC_BARE par_name_is_dotted_or_call
    mov eax, [rdi + Comp.tok_idx]
    inc eax
    mov rdx, [rdi + Comp.tokens + Buf.len]
    cmp rax, rdx
    jae .no
    mov rcx, [rdi + Comp.tokens + Buf.data]
    imul rax, rax, Token_size
    movzx eax, word [rcx + rax + Token.kind]
    cmp eax, TOK_DOT
    je .yes
    cmp eax, TOK_LPAR
    je .yes
.no:
    xor eax, eax
    ret
.yes:
    mov eax, 1
    ret
END_FUNC par_name_is_dotted_or_call

;; ============================================================================
;; par_sequence_pattern(Comp *c, int close) -> node
;; `[...]` or `(...)`.  A parenthesised single element with no trailing comma
;; is a group rather than a sequence, which is the one place the two bracket
;; forms differ.
;; ============================================================================
SP_LINE  equ 8
SP_CLOSE equ 16
SP_MARK  equ 24
SP_STAR  equ 32
SP_I     equ 40
SP_ELT   equ 48
SP_COMMA equ 56
SP_NODE  equ 64
SP_FRAME equ 72           ; + 1 push = 80
DEF_FUNC_LOCAL par_sequence_pattern, SP_FRAME
    push rbx
    mov rbx, rdi
    mov [rbp - SP_CLOSE], rsi
    call par_peek
    mov ecx, [rax + Token.lineno]
    mov [rbp - SP_LINE], rcx
    mov rdi, rbx
    call par_advance                    ; the opening bracket

    mov rdi, rbx
    call ast_mark
    mov [rbp - SP_MARK], rax
    mov qword [rbp - SP_STAR], 0
    mov qword [rbp - SP_I], 0
    mov qword [rbp - SP_COMMA], 0
.loop:
    mov rdi, rbx
    call par_kind
    cmp rax, [rbp - SP_CLOSE]
    je .close
    mov rdi, rbx
    call par_or_pattern
    test rax, rax
    jz .fail
    mov [rbp - SP_ELT], rax
    mov rdi, rbx
    mov rsi, rax
    call ast_push

    ; A starred element: remember where, one-based so 0 means none.
    mov rdi, rbx
    mov rsi, [rbp - SP_ELT]
    call ast_at
    movzx ecx, byte [rax + AstNode.kind]
    cmp ecx, AST_PAT_CAPTURE
    jne .not_star
    cmp byte [rax + AstNode.subkind], 1
    jne .not_star
    mov rax, [rbp - SP_I]
    inc rax
    mov [rbp - SP_STAR], rax
.not_star:
    inc qword [rbp - SP_I]

    mov rdi, rbx
    call par_kind
    cmp eax, TOK_COMMA
    jne .close
    mov qword [rbp - SP_COMMA], 1
    mov rdi, rbx
    call par_advance
    jmp .loop

.close:
    mov rdi, rbx
    mov esi, [rbp - SP_CLOSE]
    CSTRING rdx, "the pattern was never closed"
    call par_expect
    test eax, eax
    jz .fail

    ; `(p)` is a group, not a one-element sequence.
    cmp qword [rbp - SP_CLOSE], TOK_RPAR
    jne .build
    cmp qword [rbp - SP_COMMA], 0
    jne .build
    cmp qword [rbp - SP_I], 1
    jne .build
    mov rdi, rbx
    mov rsi, [rbp - SP_MARK]
    call pat_drop_to
    mov rax, [rbp - SP_ELT]
    pop rbx
    leave
    ret

.build:
    mov rdi, rbx
    mov esi, AST_PAT_SEQUENCE
    mov rdx, [rbp - SP_LINE]
    mov rcx, [rbp - SP_MARK]
    call par_finish_list
    test rax, rax
    jz .fail
    mov [rbp - SP_NODE], rax
    mov rdi, rbx
    mov rsi, rax
    call ast_at
    mov rdx, [rbp - SP_STAR]
    mov [rax + AstNode.b], edx
    mov rax, [rbp - SP_NODE]
    pop rbx
    leave
    ret
.fail:
    xor eax, eax
    pop rbx
    leave
    ret
END_FUNC par_sequence_pattern

;; ============================================================================
;; par_mapping_pattern(Comp *c) -> node   -- `{key: pat, ..., **rest}`
;; The child list alternates key expression and value pattern.
;; ============================================================================
MP_LINE  equ 8
MP_MARK  equ 16
MP_REST  equ 24
MP_NODE  equ 32
MP_FRAME equ 40           ; + 1 push = 48
DEF_FUNC_LOCAL par_mapping_pattern, MP_FRAME
    push rbx
    mov rbx, rdi
    call par_peek
    mov ecx, [rax + Token.lineno]
    mov [rbp - MP_LINE], rcx
    mov rdi, rbx
    call par_advance                    ; '{'

    mov rdi, rbx
    call ast_mark
    mov [rbp - MP_MARK], rax
    mov qword [rbp - MP_REST], 0
.loop:
    mov rdi, rbx
    call par_kind
    cmp eax, TOK_RBRACE
    je .close
    cmp eax, TOK_DOUBLESTAR
    je .rest

    ; The key is an ordinary expression -- a literal or a dotted name -- and
    ; never a pattern: `{"k": v}` looks up "k" rather than matching against it.
    mov rdi, rbx
    mov esi, BP_TERNARY
    call par_expr
    test rax, rax
    jz .fail
    mov rdi, rbx
    mov rsi, rax
    call ast_push
    mov rdi, rbx
    mov esi, TOK_COLON
    CSTRING rdx, "expected ':' in a mapping pattern"
    call par_expect
    test eax, eax
    jz .fail
    mov rdi, rbx
    call par_or_pattern
    test rax, rax
    jz .fail
    mov rdi, rbx
    mov rsi, rax
    call ast_push
    jmp .comma

.rest:
    mov rdi, rbx
    call par_advance
    mov rdi, rbx
    call par_name_obj
    test rax, rax
    jz .fail
    mov [rbp - MP_REST], rax

.comma:
    mov rdi, rbx
    call par_kind
    cmp eax, TOK_COMMA
    jne .close
    mov rdi, rbx
    call par_advance
    jmp .loop

.close:
    mov rdi, rbx
    mov esi, TOK_RBRACE
    CSTRING rdx, "'{' was never closed"
    call par_expect
    test eax, eax
    jz .fail
    mov rdi, rbx
    mov esi, AST_PAT_MAPPING
    mov rdx, [rbp - MP_LINE]
    mov rcx, [rbp - MP_MARK]
    call par_finish_list
    test rax, rax
    jz .fail
    mov [rbp - MP_NODE], rax
    mov rdi, rbx
    mov rsi, rax
    call ast_at
    mov rdx, [rbp - MP_REST]
    mov [rax + AstNode.b], edx
    mov rax, [rbp - MP_NODE]
    pop rbx
    leave
    ret
.fail:
    xor eax, eax
    pop rbx
    leave
    ret
END_FUNC par_mapping_pattern

;; ============================================================================
;; par_value_pattern(Comp *c) -> node
;; A literal, one of the three singletons, or a dotted name -- and, when a
;; dotted-or-plain name is followed by '(', a class pattern instead.
;;
;; None, True and False compare by identity; everything else by equality.
;; That is not a shortcut: `case 1:` must match True, because 1 == True, while
;; `case True:` must not match 1.
;; ============================================================================
VP_LINE  equ 8
VP_EXPR  equ 16
VP_KIND  equ 24
VP_FRAME equ 24           ; + 1 push = 32
DEF_FUNC_LOCAL par_value_pattern, VP_FRAME
    push rbx
    mov rbx, rdi
    call par_peek
    mov ecx, [rax + Token.lineno]
    mov [rbp - VP_LINE], rcx
    movzx eax, word [rax + Token.kind]
    mov [rbp - VP_KIND], rax

    ; A pattern's value is a primary: a name with dots, a literal, or a signed
    ; number.  Parsing at BP_POSTFIX keeps `|` and `as` out of it, and stops
    ; before the '(' of a class pattern is mistaken for a call.
    mov rdi, rbx
    call par_pattern_value_expr
    test rax, rax
    jz .fail
    mov [rbp - VP_EXPR], rax

    ; A '(' here makes it a class pattern rather than a value.
    mov rdi, rbx
    call par_kind
    cmp eax, TOK_LPAR
    je .class_pattern

    mov edx, 0
    mov rax, [rbp - VP_KIND]
    cmp eax, TOK_NONE
    je .identity
    cmp eax, TOK_TRUE
    je .identity
    cmp eax, TOK_FALSE
    jne .build
.identity:
    mov edx, 1
.build:
    mov rdi, rbx
    mov esi, AST_PAT_VALUE
    mov rcx, [rbp - VP_LINE]
    mov r8, [rbp - VP_EXPR]
    xor r9d, r9d
    call ast_make
    pop rbx
    leave
    ret

.class_pattern:
    mov rdi, rbx
    mov rsi, [rbp - VP_EXPR]
    mov rdx, [rbp - VP_LINE]
    call par_class_pattern
    pop rbx
    leave
    ret
.fail:
    xor eax, eax
    pop rbx
    leave
    ret
END_FUNC par_value_pattern

;; ============================================================================
;; par_class_pattern(Comp *c, uint32_t cls, int line) -> node
;;   `C(p1, p2, kw=p3)`
;; Positional sub-patterns come first in the child list, then AST_PAT_KEYWORD
;; nodes; .b records how many were positional, which is MATCH_CLASS's oparg.
;; ============================================================================
KP_CLS   equ 8
KP_LINE  equ 16
KP_MARK  equ 24
KP_NPOS  equ 32
KP_SEEN  equ 40
KP_NODE  equ 48
KP_NAME  equ 56
KP_FRAME equ 56           ; + 1 push = 64
DEF_FUNC_LOCAL par_class_pattern, KP_FRAME
    push rbx
    mov rbx, rdi
    mov [rbp - KP_CLS], rsi
    mov [rbp - KP_LINE], rdx
    mov rdi, rbx
    call par_advance                    ; '('

    mov rdi, rbx
    call ast_mark
    mov [rbp - KP_MARK], rax
    mov qword [rbp - KP_NPOS], 0
    mov qword [rbp - KP_SEEN], 0        ; a keyword has been seen
.loop:
    mov rdi, rbx
    call par_kind
    cmp eax, TOK_RPAR
    je .close

    ; `name=pattern` is a keyword sub-pattern; anything else is positional.
    mov rdi, rbx
    call par_kind
    cmp eax, TOK_NAME
    jne .positional
    mov rdi, rbx
    call par_name_is_kwarg
    test eax, eax
    jz .positional

    mov rdi, rbx
    call par_name_obj
    test rax, rax
    jz .fail
    mov [rbp - KP_NAME], rax
    mov rdi, rbx
    call par_advance                    ; '='
    mov rdi, rbx
    call par_or_pattern
    test rax, rax
    jz .fail
    mov r8, [rbp - KP_NAME]
    mov r9, rax
    mov rdi, rbx
    mov esi, AST_PAT_KEYWORD
    xor edx, edx
    mov rcx, [rbp - KP_LINE]
    call ast_make
    test rax, rax
    jz .fail
    mov rdi, rbx
    mov rsi, rax
    call ast_push
    mov qword [rbp - KP_SEEN], 1
    jmp .comma

.positional:
    cmp qword [rbp - KP_SEEN], 0
    jne .pos_after_kw
    mov rdi, rbx
    call par_or_pattern
    test rax, rax
    jz .fail
    mov rdi, rbx
    mov rsi, rax
    call ast_push
    inc qword [rbp - KP_NPOS]

.comma:
    mov rdi, rbx
    call par_kind
    cmp eax, TOK_COMMA
    jne .close
    mov rdi, rbx
    call par_advance
    jmp .loop

.pos_after_kw:
    mov rdi, rbx
    CSTRING rsi, "positional patterns follow keyword patterns"
    call par_syntax_error
    jmp .fail

.close:
    mov rdi, rbx
    mov esi, TOK_RPAR
    CSTRING rdx, "'(' was never closed"
    call par_expect
    test eax, eax
    jz .fail
    mov rdi, rbx
    mov esi, AST_PAT_CLASS
    mov rdx, [rbp - KP_LINE]
    mov rcx, [rbp - KP_MARK]
    call par_finish_list
    test rax, rax
    jz .fail
    mov [rbp - KP_NODE], rax
    mov rdi, rbx
    mov rsi, rax
    call ast_at
    mov rdx, [rbp - KP_CLS]
    mov [rax + AstNode.a], edx
    mov rdx, [rbp - KP_NPOS]
    mov [rax + AstNode.b], edx
    mov rax, [rbp - KP_NODE]
    pop rbx
    leave
    ret
.fail:
    xor eax, eax
    pop rbx
    leave
    ret
END_FUNC par_class_pattern

;; ============================================================================
;; par_name_is_kwarg(Comp *c) -> rax = 1 if NAME '=' follows (and not '==').
;; ============================================================================
DEF_FUNC_BARE par_name_is_kwarg
    mov eax, [rdi + Comp.tok_idx]
    inc eax
    mov rdx, [rdi + Comp.tokens + Buf.len]
    cmp rax, rdx
    jae .no
    mov rcx, [rdi + Comp.tokens + Buf.data]
    imul rax, rax, Token_size
    movzx eax, word [rcx + rax + Token.kind]
    cmp eax, TOK_EQUAL
    jne .no
    mov eax, 1
    ret
.no:
    xor eax, eax
    ret
END_FUNC par_name_is_kwarg

;; ============================================================================
;; pat_drop_to(Comp *c, uint64_t mark) - unstage everything pushed since mark.
;; Only `(p)` needs it: the element is read as if it were the first of a
;; sequence, and only the closing bracket says it was a group after all.
;; ============================================================================
DEF_FUNC_BARE pat_drop_to
    mov [rdi + Comp.pending + Buf.len], rsi
    ret
END_FUNC pat_drop_to

;; ============================================================================
;; par_pattern_value_expr(Comp *c) -> node
;; The expression a value pattern compares against: a dotted name, or a
;; literal.  A dotted name is read here rather than through par_expr because
;; par_expr would take a following '(' as a call, and in a pattern it opens a
;; class pattern instead.
;;
;; A literal goes through par_expr at BP_ARITH, which admits `-1` and `1 + 2j`
;; and stops before `|`, so an or-pattern's bar is never read as a bitwise or.
;; ============================================================================
PV_LINE  equ 8
PV_NODE  equ 16
PV_FRAME equ 24           ; + 1 push = 32
DEF_FUNC_LOCAL par_pattern_value_expr, PV_FRAME
    push rbx
    mov rbx, rdi
    call par_peek
    mov ecx, [rax + Token.lineno]
    mov [rbp - PV_LINE], rcx
    mov rdi, rbx
    call par_kind
    cmp eax, TOK_NAME
    je .dotted
    mov rdi, rbx
    mov esi, BP_ARITH
    call par_expr
    pop rbx
    leave
    ret

.dotted:
    mov rdi, rbx
    call par_name_obj
    test rax, rax
    jz .fail
    mov r8, rax
    mov rdi, rbx
    mov esi, AST_NAME
    mov edx, CTX_LOAD
    mov rcx, [rbp - PV_LINE]
    xor r9d, r9d
    call ast_make
    test rax, rax
    jz .fail
    mov [rbp - PV_NODE], rax
.dot_loop:
    mov rdi, rbx
    call par_kind
    cmp eax, TOK_DOT
    jne .done
    mov rdi, rbx
    call par_advance
    mov rdi, rbx
    call par_name_obj
    test rax, rax
    jz .fail
    mov r9, rax
    mov rdi, rbx
    mov esi, AST_ATTRIBUTE
    mov edx, CTX_LOAD
    mov rcx, [rbp - PV_LINE]
    mov r8, [rbp - PV_NODE]
    call ast_make
    test rax, rax
    jz .fail
    mov [rbp - PV_NODE], rax
    jmp .dot_loop
.done:
    mov rax, [rbp - PV_NODE]
    pop rbx
    leave
    ret
.fail:
    xor eax, eax
    pop rbx
    leave
    ret
END_FUNC par_pattern_value_expr

ASM_INIT

section .rodata
pm_match_kw: db "match", 0
pm_case_kw:  db "case", 0
