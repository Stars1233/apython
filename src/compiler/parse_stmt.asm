; parse_stmt.asm - Statement parsing
;
; Statements dispatch on their leading keyword through stmt_table; anything
; that is not a keyword falls through to the expression-or-assignment parser.
;
; Assignment is not decided by lookahead.  A target is parsed as an ordinary
; expression -- `a, b` and `a, b = t` are the same production until the `=`
; appears -- and then re-marked with ast_set_ctx, which is also where "cannot
; assign to a literal" is discovered: a node kind with no store form fails
; there.  Backtracking would work too, and CPython's PEG parser does exactly
; that, but this is smaller and gives a better message.

%include "macros.inc"
%include "object.inc"
%include "value.inc"
%include "opcodes.inc"
%include "compiler.inc"

extern ast_at
extern ast_commit
extern ast_make
extern ast_end_at
extern ast_end_here
extern ast_mark
extern ast_obj
extern ast_obj_at
extern ast_push
extern ast_set_ctx
extern comp_error
extern comp_intern
extern buf_free
extern buf_init
extern buf_push_u8

extern par_advance
extern par_expect
extern par_expr
extern par_finish_list
extern par_kind
extern par_peek
extern par_peek_next
extern par_syntax_error
extern par_looks_like_match
extern par_soft_keyword_is
extern ps_match
extern in_call_public


BP_NONE equ 0
BP_COMPARE equ 12

; --- Named frame-layout constants ---
PM_MARK  equ 16
PM_LINE  equ 24
PM_FRAME equ 24          ; + 1 push = 32

section .text

;; ============================================================================
;; par_module(Comp *c) -> rax = the AST_MODULE node, 0 on error
;; ============================================================================
DEF_FUNC par_module, PM_FRAME
    push rbx
    mov rbx, rdi
    call par_peek
    TOK_POS rax
    mov [rbp - PM_LINE], rcx
    mov rdi, rbx
    call ast_mark
    mov [rbp - PM_MARK], rax

.loop:
    mov rdi, rbx
    call par_kind
    cmp eax, TOK_ENDMARKER
    je .done
    cmp eax, TOK_NEWLINE
    jne .stmt
    mov rdi, rbx
    call par_advance
    jmp .loop
.stmt:
    ; par_statement_any, not par_simple_stmts: a compound statement consumes
    ; its own suite and the DEDENT that ends it, so there is no NEWLINE left
    ; for the simple-statement path to demand.
    mov rdi, rbx
    call par_statement_any
    test eax, eax
    jz .fail
    jmp .loop

.done:
    mov rdi, rbx
    mov esi, AST_MODULE
    mov rdx, [rbp - PM_LINE]
    mov rcx, [rbp - PM_MARK]
    call par_finish_list
    pop rbx
    leave
    ret
.fail:
    xor eax, eax
    pop rbx
    leave
    ret
END_FUNC par_module

;; ============================================================================
;; par_simple_stmts(Comp *c) -> rax = 1 ok, 0 error
;; One logical line: `stmt (';' stmt)* NEWLINE`, each pushed onto the pending
;; stack for whatever list is being built.
;; ============================================================================
DEF_FUNC par_simple_stmts, 8
    push rbx
    mov rbx, rdi
.loop:
    mov rdi, rbx
    call par_statement
    test eax, eax
    jz .fail
    mov rdi, rbx
    mov rsi, rax
    call ast_push

    mov rdi, rbx
    call par_kind
    cmp eax, TOK_SEMI
    jne .end_of_line
    mov rdi, rbx
    call par_advance
    ; A trailing semicolon is allowed: `x = 1;` ends the line.
    mov rdi, rbx
    call par_kind
    cmp eax, TOK_NEWLINE
    je .end_of_line
    cmp eax, TOK_ENDMARKER
    je .ok
    jmp .loop

.end_of_line:
    mov rdi, rbx
    call par_kind
    cmp eax, TOK_ENDMARKER
    je .ok
    mov rdi, rbx
    mov esi, TOK_NEWLINE
    CSTRING rdx, "invalid syntax"
    call par_expect
    test eax, eax
    jz .fail
.ok:
    mov eax, 1
    pop rbx
    leave
    ret
.fail:
    xor eax, eax
    pop rbx
    leave
    ret
END_FUNC par_simple_stmts

;; ============================================================================
;; par_statement(Comp *c) -> rax = a statement node, 0 on error
;; ============================================================================
DEF_FUNC par_statement, 8
    push rbx
    mov rbx, rdi
    call par_kind
    ; `match` is a soft keyword, so it arrives as a NAME and cannot be in
    ; stmt_table: only its context tells it from a variable of the same name.
    cmp eax, TOK_NAME
    jne .not_soft
    mov rdi, rbx
    call par_looks_like_match
    test eax, eax
    jz .maybe_alias
    mov rdi, rbx
    call ps_match
    pop rbx
    leave
    ret
.maybe_alias:
    ; `type` is a soft keyword as well as a builtin, so only the statement
    ; shape -- `type X =` or `type X[` -- is taken as one.
    mov rdi, rbx
    lea rsi, [rel ps_type_kw]
    mov edx, 4
    call par_soft_keyword_is
    test eax, eax
    jz .as_expr
    mov rdi, rbx
    call par_looks_like_type_alias
    test eax, eax
    jz .as_expr
    mov rdi, rbx
    call ps_type_alias
    pop rbx
    leave
    ret
.not_soft:
    cmp eax, TOK_COUNT
    jae .as_expr
    lea rcx, [rel stmt_table]
    mov rax, [rcx + rax*8]
    test rax, rax
    jz .as_expr
    mov rdi, rbx
    call rax
    pop rbx
    leave
    ret
.as_expr:
    mov rdi, rbx
    call par_expr_stmt
    pop rbx
    leave
    ret
END_FUNC par_statement

;; ============================================================================
;; par_expr_stmt(Comp *c) -> node
;;
;; An expression, an assignment, an augmented assignment or an annotated one --
;; decided by what follows the first expression rather than by lookahead.
;; ============================================================================
PE2_FIRST equ 16
PE2_LINE  equ 24
PE2_MARK  equ 32
PE2_OP    equ 40
PE2_NODE  equ 48
PE2_FRAME equ 56         ; + 1 push = 64
DEF_FUNC par_expr_stmt, PE2_FRAME
    push rbx
    mov rbx, rdi
    call par_peek
    TOK_POS rax
    mov [rbp - PE2_LINE], rcx

    mov rdi, rbx
    call par_exprlist_stmt              ; an expression, or a bare tuple
    test rax, rax
    jz .fail
    mov [rbp - PE2_FIRST], rax

    mov rdi, rbx
    call par_kind
    cmp eax, TOK_EQUAL
    je .assign
    cmp eax, TOK_COLON
    je .annassign
    call par_augop
    cmp eax, -1
    jne .augassign

    ; A bare expression statement.
    mov rdi, rbx
    mov esi, AST_EXPR_STMT
    xor edx, edx
    mov rcx, [rbp - PE2_LINE]
    mov r8, [rbp - PE2_FIRST]
    xor r9d, r9d
    call ast_make
    pop rbx
    leave
    ret

.assign:
    ; Every `=`-separated item but the last is a target; the last is the value.
    mov rdi, rbx
    call ast_mark
    mov [rbp - PE2_MARK], rax
    mov rdi, rbx
    mov rsi, [rbp - PE2_FIRST]
    call ast_push
.assign_loop:
    mov rdi, rbx
    call par_advance                    ; consume '='
    mov rdi, rbx
    call par_exprlist_stmt
    test rax, rax
    jz .fail
    mov [rbp - PE2_FIRST], rax
    mov rdi, rbx
    call par_kind
    cmp eax, TOK_EQUAL
    jne .assign_done
    mov rdi, rbx
    mov rsi, [rbp - PE2_FIRST]
    call ast_push
    jmp .assign_loop

.assign_done:
    ; Mark everything staged as a store target.
    mov rdi, rbx
    call ast_mark
    mov rcx, rax
    mov rdx, [rbp - PE2_MARK]
.mark_loop:
    cmp rdx, rcx
    jae .marked
    push rcx
    push rdx
    mov rax, [rbx + Comp.pending + Buf.data]
    mov esi, [rax + rdx*4]
    mov rdi, rbx
    mov edx, CTX_STORE
    call ast_set_ctx
    pop rdx
    pop rcx
    test eax, eax
    jz .bad_target
    inc rdx
    jmp .mark_loop
.marked:
    mov rdi, rbx
    mov esi, AST_ASSIGN
    mov rdx, [rbp - PE2_LINE]
    mov rcx, [rbp - PE2_MARK]
    call par_finish_list
    test rax, rax
    jz .fail
    mov [rbp - PE2_NODE], rax
    mov rdi, rbx
    mov rsi, rax
    call ast_at
    mov rdx, [rbp - PE2_FIRST]
    mov [rax + AstNode.b], edx          ; the value
    mov rax, [rbp - PE2_NODE]
    pop rbx
    leave
    ret

.augassign:
    mov [rbp - PE2_OP], rax
    mov rdi, rbx
    call par_advance                    ; consume the operator
    mov rdi, rbx
    mov rsi, [rbp - PE2_FIRST]
    mov edx, CTX_STORE
    call ast_set_ctx
    test eax, eax
    jz .bad_target
    ; Only a single, simple target may be augmented: `a, b += 1` is illegal.
    mov rdi, rbx
    mov rsi, [rbp - PE2_FIRST]
    call ast_at
    movzx eax, byte [rax + AstNode.kind]
    cmp eax, AST_NAME
    je .aug_ok
    cmp eax, AST_ATTRIBUTE
    je .aug_ok
    cmp eax, AST_SUBSCRIPT
    jne .bad_aug_target
.aug_ok:
    mov rdi, rbx
    call par_exprlist_stmt
    test rax, rax
    jz .fail
    mov r9, rax
    mov rdi, rbx
    mov esi, AST_AUGASSIGN
    mov rdx, [rbp - PE2_OP]
    mov rcx, [rbp - PE2_LINE]
    mov r8, [rbp - PE2_FIRST]
    call ast_make
    pop rbx
    leave
    ret

.annassign:
    mov rdi, rbx
    call par_advance                    ; consume ':'
    mov rdi, rbx
    mov rsi, [rbp - PE2_FIRST]
    mov edx, CTX_STORE
    call ast_set_ctx
    test eax, eax
    jz .bad_target
    mov rdi, rbx
    mov esi, BP_NONE
    call par_expr                       ; the annotation
    test rax, rax
    jz .fail
    mov [rbp - PE2_OP], rax

    mov rdi, rbx
    mov esi, AST_ANNASSIGN
    xor edx, edx
    mov rcx, [rbp - PE2_LINE]
    mov r8, [rbp - PE2_FIRST]
    mov r9, [rbp - PE2_OP]
    call ast_make
    mov [rbp - PE2_NODE], rax

    mov rdi, rbx
    call par_kind
    cmp eax, TOK_EQUAL
    jne .ann_done
    mov rdi, rbx
    call par_advance
    mov rdi, rbx
    call par_exprlist_stmt
    test rax, rax
    jz .fail
    mov rdx, rax
    mov rdi, rbx
    mov rsi, [rbp - PE2_NODE]
    push rdx
    call ast_at
    pop rdx
    mov [rax + AstNode.c], edx
    ; The node was made before the value: its end is here, not at the
    ; annotation.
    mov rdi, rbx
    mov esi, [rbp - PE2_NODE]
    call ast_end_here
.ann_done:
    mov rax, [rbp - PE2_NODE]
    pop rbx
    leave
    ret

.bad_target:
    mov rdi, rbx
    CSTRING rsi, "cannot assign to that expression"
    call par_syntax_error
    jmp .fail
.bad_aug_target:
    mov rdi, rbx
    CSTRING rsi, "augmented assignment requires a single simple target"
    call par_syntax_error
.fail:
    xor eax, eax
    pop rbx
    leave
    ret
END_FUNC par_expr_stmt

;; ============================================================================
;; par_exprlist_stmt(Comp *c) -> node
;; An expression, or a bare comma-separated tuple: `a, b` and `x = 1, 2` both
;; need one, and neither has brackets to delimit it.
;; ============================================================================
PX2_FIRST equ 16
PX2_LINE  equ 24
PX2_MARK  equ 32
PX2_FRAME equ 40         ; + 1 push = 48
DEF_FUNC par_exprlist_stmt, PX2_FRAME
    push rbx
    mov rbx, rdi
    call par_peek
    TOK_POS rax
    mov [rbp - PX2_LINE], rcx

    mov rdi, rbx
    mov esi, BP_NONE
    call par_expr
    test rax, rax
    jz .fail
    mov [rbp - PX2_FIRST], rax

    mov rdi, rbx
    call par_kind
    cmp eax, TOK_COMMA
    jne .single

    mov rdi, rbx
    call ast_mark
    mov [rbp - PX2_MARK], rax
    mov rdi, rbx
    mov rsi, [rbp - PX2_FIRST]
    call ast_push
.tuple_loop:
    mov rdi, rbx
    call par_advance                    ; consume ','
    mov rdi, rbx
    call par_kind
    ; A trailing comma ends the tuple: `x = 1,` is a one-tuple.
    cmp eax, TOK_NEWLINE
    je .tuple_done
    cmp eax, TOK_ENDMARKER
    je .tuple_done
    cmp eax, TOK_EQUAL
    je .tuple_done
    cmp eax, TOK_SEMI
    je .tuple_done
    cmp eax, TOK_COLON
    je .tuple_done
    cmp eax, TOK_RPAR
    je .tuple_done
    mov rdi, rbx
    mov esi, BP_NONE
    call par_expr
    test rax, rax
    jz .fail
    mov rdi, rbx
    mov rsi, rax
    call ast_push
    mov rdi, rbx
    call par_kind
    cmp eax, TOK_COMMA
    je .tuple_loop
.tuple_done:
    mov rdi, rbx
    mov esi, AST_TUPLE
    mov rdx, [rbp - PX2_LINE]
    mov rcx, [rbp - PX2_MARK]
    call par_finish_list
    pop rbx
    leave
    ret

.single:
    mov rax, [rbp - PX2_FIRST]
    pop rbx
    leave
    ret
.fail:
    xor eax, eax
    pop rbx
    leave
    ret
END_FUNC par_exprlist_stmt

;; ============================================================================
;; par_augop(Comp *c) -> eax = the NB_INPLACE_* code, or -1
;; Does not consume the token.
;; ============================================================================
DEF_FUNC par_augop, 8
    push rbx
    mov rbx, rdi
    call par_kind
    lea rcx, [rel augop_table]
    xor edx, edx
.scan:
    mov r8d, [rcx + rdx*8]
    cmp r8d, -1
    je .none
    cmp r8d, eax
    je .found
    inc rdx
    jmp .scan
.found:
    mov eax, [rcx + rdx*8 + 4]
    pop rbx
    leave
    ret
.none:
    mov eax, -1
    pop rbx
    leave
    ret
END_FUNC par_augop

;; ============================================================================
;; Keyword statement parsers.  Each is entered with its keyword current and
;; returns a node, or 0 with an error recorded.
;; ============================================================================

PK_LINE  equ 16
PK_A     equ 24
PK_B     equ 32
PK_MARK  equ 40
PK_NODE  equ 48
PK_ALINE equ 56          ; where one alias starts, which is not where the
                         ; statement does
PK_FRAME equ 72          ; + 1 push = 80, 16-byte aligned
PK2_FRAME equ 64         ; + 2 pushes = 80, 16-byte aligned; for the handlers
                         ; that save r12

;; ps_simple - pass, break and continue: a keyword and nothing else.
DEF_FUNC_LOCAL ps_simple, PK_FRAME
    push rbx
    mov rbx, rdi
    call par_peek
    TOK_POS rax
    mov [rbp - PK_LINE], rcx
    movzx eax, word [rax + Token.kind]
    mov esi, AST_PASS
    cmp eax, TOK_BREAK
    jne .not_break
    mov esi, AST_BREAK
    jmp .have
.not_break:
    cmp eax, TOK_CONTINUE
    jne .have
    mov esi, AST_CONTINUE
.have:
    push rsi
    mov rdi, rbx
    call par_advance
    pop rsi
    mov rdi, rbx
    xor edx, edx
    mov rcx, [rbp - PK_LINE]
    xor r8d, r8d
    xor r9d, r9d
    call ast_make
    pop rbx
    leave
    ret
END_FUNC ps_simple

;; ps_del - `del a, b[0], c.d`
DEF_FUNC_LOCAL ps_del, PK_FRAME
    push rbx
    mov rbx, rdi
    call par_peek
    TOK_POS rax
    mov [rbp - PK_LINE], rcx
    mov rdi, rbx
    call par_advance
    mov rdi, rbx
    call ast_mark
    mov [rbp - PK_MARK], rax
.loop:
    mov rdi, rbx
    mov esi, BP_NONE
    call par_expr
    test rax, rax
    jz .fail
    mov rsi, rax
    mov rdi, rbx
    mov edx, CTX_DEL
    push rsi
    call ast_set_ctx
    pop rsi
    test eax, eax
    jz .bad_target
    mov rdi, rbx
    call ast_push
    mov rdi, rbx
    call par_kind
    cmp eax, TOK_COMMA
    jne .done
    mov rdi, rbx
    call par_advance
    jmp .loop
.done:
    mov rdi, rbx
    mov esi, AST_DELETE
    mov rdx, [rbp - PK_LINE]
    mov rcx, [rbp - PK_MARK]
    call par_finish_list
    pop rbx
    leave
    ret
.bad_target:
    mov rdi, rbx
    CSTRING rsi, "cannot delete that expression"
    call par_syntax_error
.fail:
    xor eax, eax
    pop rbx
    leave
    ret
END_FUNC ps_del

;; ps_scope - `global a, b` and `nonlocal a, b`
DEF_FUNC_LOCAL ps_scope, PK2_FRAME
    push rbx
    push r12
    mov rbx, rdi
    call par_peek
    TOK_POS rax
    mov [rbp - PK_LINE], rcx
    movzx eax, word [rax + Token.kind]
    mov r12d, AST_GLOBAL
    cmp eax, TOK_GLOBAL
    je .have
    mov r12d, AST_NONLOCAL
.have:
    mov rdi, rbx
    call par_advance
    mov rdi, rbx
    call ast_mark
    mov [rbp - PK_MARK], rax
.loop:
    mov rdi, rbx
    call par_kind
    cmp eax, TOK_NAME
    jne .need_name
    mov rdi, rbx
    call par_peek
    mov rsi, [rax + Token.start]
    mov edx, [rax + Token.len]
    ; Mangled, like every other identifier.  CPython mangles the DECLARATION
    ; too -- `global __v` inside a method of C is a declaration about _C__v --
    ; and interning it raw here bound a different name from the one every use
    ; of it resolved to, silently.
    mov rdi, rbx
    call comp_intern_name
    test rax, rax
    jz .fail
    mov rdi, rbx
    mov rsi, rax
    call ast_obj
    mov rdi, rbx
    mov rsi, rax
    call ast_push
    mov rdi, rbx
    call par_advance
    mov rdi, rbx
    call par_kind
    cmp eax, TOK_COMMA
    jne .done
    mov rdi, rbx
    call par_advance
    jmp .loop
.done:
    mov rdi, rbx
    mov rsi, r12
    mov rdx, [rbp - PK_LINE]
    mov rcx, [rbp - PK_MARK]
    call par_finish_list
    pop r12
    pop rbx
    leave
    ret
.need_name:
    mov rdi, rbx
    CSTRING rsi, "expected a name"
    call par_syntax_error
.fail:
    xor eax, eax
    pop r12
    pop rbx
    leave
    ret
END_FUNC ps_scope

;; ps_assert - `assert test` or `assert test, message`
DEF_FUNC_LOCAL ps_assert, PK_FRAME
    push rbx
    mov rbx, rdi
    call par_peek
    TOK_POS rax
    mov [rbp - PK_LINE], rcx
    mov rdi, rbx
    call par_advance
    mov rdi, rbx
    mov esi, BP_NONE
    call par_expr
    test rax, rax
    jz .fail
    mov [rbp - PK_A], rax
    mov qword [rbp - PK_B], 0

    mov rdi, rbx
    call par_kind
    cmp eax, TOK_COMMA
    jne .build
    mov rdi, rbx
    call par_advance
    mov rdi, rbx
    mov esi, BP_NONE
    call par_expr
    test rax, rax
    jz .fail
    mov [rbp - PK_B], rax
.build:
    mov rdi, rbx
    mov esi, AST_ASSERT
    xor edx, edx
    mov rcx, [rbp - PK_LINE]
    mov r8, [rbp - PK_A]
    mov r9, [rbp - PK_B]
    call ast_make
    pop rbx
    leave
    ret
.fail:
    xor eax, eax
    pop rbx
    leave
    ret
END_FUNC ps_assert

;; ps_raise - `raise`, `raise E`, `raise E from F`
DEF_FUNC_LOCAL ps_raise, PK_FRAME
    push rbx
    mov rbx, rdi
    call par_peek
    TOK_POS rax
    mov [rbp - PK_LINE], rcx
    mov rdi, rbx
    call par_advance
    mov qword [rbp - PK_A], 0
    mov qword [rbp - PK_B], 0

    mov rdi, rbx
    call par_kind
    cmp eax, TOK_NEWLINE
    je .build
    cmp eax, TOK_SEMI
    je .build
    cmp eax, TOK_ENDMARKER
    je .build

    mov rdi, rbx
    mov esi, BP_NONE
    call par_expr
    test rax, rax
    jz .fail
    mov [rbp - PK_A], rax
    mov rdi, rbx
    call par_kind
    cmp eax, TOK_FROM
    jne .build
    mov rdi, rbx
    call par_advance
    mov rdi, rbx
    mov esi, BP_NONE
    call par_expr
    test rax, rax
    jz .fail
    mov [rbp - PK_B], rax
.build:
    mov rdi, rbx
    mov esi, AST_RAISE
    xor edx, edx
    mov rcx, [rbp - PK_LINE]
    mov r8, [rbp - PK_A]
    mov r9, [rbp - PK_B]
    call ast_make
    pop rbx
    leave
    ret
.fail:
    xor eax, eax
    pop rbx
    leave
    ret
END_FUNC ps_raise

;; ============================================================================
;; par_bound_name(Comp *c, uint32_t dotted) -> rax = obj index of the name the
;;   import binds, or 0 on error.
;;
;; `import a.b` binds `a`, not `a.b`: the submodule is reached through the
;; attribute.  The head is mangled the way every other identifier in a class
;; body is -- CPython puts the raw dotted name in IMPORT_NAME and the mangled
;; head in STORE_NAME -- and here, with Comp.private still live, is the only
;; place that can do it.  By the time the symbol table runs, the class the
;; mangling depends on is gone.
;; ============================================================================
PBN_FRAME equ 24          ; + 1 push = 32
DEF_FUNC_LOCAL par_bound_name, PBN_FRAME
    push rbx
    mov rbx, rdi
    call ast_obj_at                     ; rax = the dotted PyStrObject*
    test rax, rax
    jz .fail
    mov rcx, [rax + PyStrObject.ob_size]
    lea rsi, [rax + PyStrObject.data]
    xor edx, edx
.scan:
    cmp rdx, rcx
    jae .have
    cmp byte [rsi + rdx], '.'
    je .have
    inc rdx
    jmp .scan
.have:
    mov rdi, rbx
    call comp_intern_name               ; an owned, mangled PyStrObject*
    test rax, rax
    jz .fail
    mov rdi, rbx
    mov rsi, rax
    call ast_obj                        ; the arena takes ownership
    pop rbx
    leave
    ret
.fail:
    xor eax, eax
    pop rbx
    leave
    ret
END_FUNC par_bound_name

;; ============================================================================
;; ============================================================================
;; par_dotted_name(Comp *c) -> rax = obj index of the joined name, 0 on error
;; `a.b.c` becomes the single string "a.b.c", which is what IMPORT_NAME wants.
;; ============================================================================
PDN_BUF   equ 48         ; a Buf lives here
PDN_FRAME equ 56         ; + 3 pushes = 80
DEF_FUNC par_dotted_name, PDN_FRAME
    push rbx
    push r12
    push r13
    mov rbx, rdi
    lea rdi, [rbp - PDN_BUF]
    mov esi, 1
    call buf_init
.loop:
    mov rdi, rbx
    call par_kind
    cmp eax, TOK_NAME
    jne .need_name
    mov rdi, rbx
    call par_peek
    mov r12, [rax + Token.start]        ; the text; callee-saved, so it survives
    mov r13d, [rax + Token.len]         ; and so does its length
.copy:
    test r13, r13
    jz .copied
    movzx esi, byte [r12]
    lea rdi, [rbp - PDN_BUF]
    call buf_push_u8
    inc r12
    dec r13
    jmp .copy
.copied:
    mov rdi, rbx
    call par_advance
    mov rdi, rbx
    call par_kind
    cmp eax, TOK_DOT
    jne .done
    mov rdi, rbx
    call par_advance
    lea rdi, [rbp - PDN_BUF]
    mov esi, '.'
    call buf_push_u8
    jmp .loop
.done:
    mov rdi, [rbp - PDN_BUF + Buf.data]
    mov rsi, [rbp - PDN_BUF + Buf.len]
    call comp_intern
    test rax, rax
    jz .fail
    mov rdi, rbx
    mov rsi, rax
    call ast_obj
    mov r12, rax
    lea rdi, [rbp - PDN_BUF]
    call buf_free
    mov rax, r12
    pop r13
    pop r12
    pop rbx
    leave
    ret
.need_name:
    mov rdi, rbx
    CSTRING rsi, "expected a module name"
    call par_syntax_error
.fail:
    lea rdi, [rbp - PDN_BUF]
    call buf_free
    xor eax, eax
    pop r13
    pop r12
    pop rbx
    leave
    ret
END_FUNC par_dotted_name

;; ============================================================================
;; par_name_obj(Comp *c) -> rax = obj index for the current NAME, 0 on error
;; ============================================================================
DEF_FUNC par_name_obj, 8
    push rbx
    mov rbx, rdi
    call par_kind
    cmp eax, TOK_NAME
    jne .bad
    mov rdi, rbx
    call par_peek
    mov rdi, rbx
    mov rsi, [rax + Token.start]
    mov edx, [rax + Token.len]
    extern comp_intern_name
    call comp_intern_name
    test rax, rax
    jz .fail
    mov rdi, rbx
    mov rsi, rax
    call ast_obj
    push rax
    mov rdi, rbx
    call par_advance
    pop rax
    pop rbx
    leave
    ret
.bad:
    mov rdi, rbx
    CSTRING rsi, "expected a name"
    call par_syntax_error
.fail:
    xor eax, eax
    pop rbx
    leave
    ret
END_FUNC par_name_obj

;; ps_import - `import a.b, c as d`
DEF_FUNC_LOCAL ps_import, PK2_FRAME
    push rbx
    push r12
    mov rbx, rdi
    call par_peek
    TOK_POS rax
    mov [rbp - PK_LINE], rcx
    mov rdi, rbx
    call par_advance
    mov rdi, rbx
    call ast_mark
    mov [rbp - PK_MARK], rax
.loop:
    ; Each alias starts at its own name.  PK_LINE is the statement's, which
    ; is what `import` is at, and every alias in `import a, b` would take it.
    mov rdi, rbx
    call par_peek
    TOK_POS rax
    mov [rbp - PK_ALINE], rcx
    mov rdi, rbx
    call par_dotted_name
    test rax, rax
    jz .fail
    mov r12, rax
    mov qword [rbp - PK_B], 0
    mov rdi, rbx
    call par_kind
    cmp eax, TOK_AS
    jne .make_alias
    mov rdi, rbx
    call par_advance
    mov rdi, rbx
    call par_name_obj
    test rax, rax
    jz .fail
    mov [rbp - PK_B], rax
.make_alias:
    mov rdi, rbx
    mov esi, AST_ALIAS
    xor edx, edx
    mov rcx, [rbp - PK_ALINE]
    mov r8, r12
    mov r9, [rbp - PK_B]
    call ast_make
    mov [rbp - PK_NODE], rax
    ; The name this alias binds, decided here rather than in codegen: only the
    ; parser still knows the enclosing class, and mangling needs it.
    mov rdx, [rbp - PK_B]
    test rdx, rdx
    jnz .bound_is_as
    mov rdi, rbx
    mov rsi, r12
    call par_bound_name
    test rax, rax
    jz .fail
    mov rdx, rax
.bound_is_as:
    mov rdi, rbx
    mov rsi, [rbp - PK_NODE]
    push rdx
    call ast_at
    pop rdx
    mov [rax + AstNode.c], edx
    mov rdi, rbx
    mov rsi, [rbp - PK_NODE]
    call ast_push
    mov rdi, rbx
    call par_kind
    cmp eax, TOK_COMMA
    jne .done
    mov rdi, rbx
    call par_advance
    jmp .loop
.done:
    mov rdi, rbx
    mov esi, AST_IMPORT
    mov rdx, [rbp - PK_LINE]
    mov rcx, [rbp - PK_MARK]
    call par_finish_list
    pop r12
    pop rbx
    leave
    ret
.fail:
    xor eax, eax
    pop r12
    pop rbx
    leave
    ret
END_FUNC ps_import

;; ps_from - `from a.b import c, d as e` and `from . import x` and `from m import *`
PFR_LEVEL equ 64
PFR_MOD   equ 72
PFR_FRAME equ 80         ; + 2 pushes = 96
DEF_FUNC_LOCAL ps_from, PFR_FRAME
    push rbx
    push r12
    mov rbx, rdi
    call par_peek
    TOK_POS rax
    mov [rbp - PK_LINE], rcx
    mov rdi, rbx
    call par_advance
    mov qword [rbp - PFR_LEVEL], 0
    mov qword [rbp - PFR_MOD], 0

    ; Leading dots are the relative-import level; `...` counts as three.
.dots:
    mov rdi, rbx
    call par_kind
    cmp eax, TOK_DOT
    je .one_dot
    cmp eax, TOK_ELLIPSIS
    je .three_dots
    jmp .after_dots
.one_dot:
    inc qword [rbp - PFR_LEVEL]
    mov rdi, rbx
    call par_advance
    jmp .dots
.three_dots:
    add qword [rbp - PFR_LEVEL], 3
    mov rdi, rbx
    call par_advance
    jmp .dots
.after_dots:
    mov rdi, rbx
    call par_kind
    cmp eax, TOK_IMPORT
    je .no_module
    mov rdi, rbx
    call par_dotted_name
    test rax, rax
    jz .fail
    mov [rbp - PFR_MOD], rax
.no_module:
    mov rdi, rbx
    mov esi, TOK_IMPORT
    CSTRING rdx, "expected 'import'"
    call par_expect
    test eax, eax
    jz .fail

    mov rdi, rbx
    call ast_mark
    mov [rbp - PK_MARK], rax

    mov rdi, rbx
    call par_kind
    cmp eax, TOK_STAR
    je .star_import

    ; An optional parenthesised list: from m import (a, b)
    xor r12d, r12d
    mov rdi, rbx
    call par_kind
    cmp eax, TOK_LPAR
    jne .names
    mov r12d, 1
    mov rdi, rbx
    call par_advance
.names:
    ; As in ps_import: the alias starts at its own name, not at `from`.
    mov rdi, rbx
    call par_peek
    TOK_POS rax
    mov [rbp - PK_ALINE], rcx
    mov rdi, rbx
    call par_name_obj
    test rax, rax
    jz .fail
    mov [rbp - PK_A], rax
    mov qword [rbp - PK_B], 0
    mov rdi, rbx
    call par_kind
    cmp eax, TOK_AS
    jne .make_alias
    mov rdi, rbx
    call par_advance
    mov rdi, rbx
    call par_name_obj
    test rax, rax
    jz .fail
    mov [rbp - PK_B], rax
.make_alias:
    mov rdi, rbx
    mov esi, AST_ALIAS
    xor edx, edx
    mov rcx, [rbp - PK_ALINE]
    mov r8, [rbp - PK_A]
    mov r9, [rbp - PK_B]
    call ast_make
    ; Both halves are already mangled by par_name_obj, so the bound name is
    ; just the asname when there is one and the imported name otherwise.
    push rax
    mov rdi, rbx
    mov rsi, rax
    call ast_at
    mov rdx, [rbp - PK_B]
    test rdx, rdx
    jnz .from_bound_as
    mov rdx, [rbp - PK_A]
.from_bound_as:
    mov [rax + AstNode.c], edx
    pop rax
    mov rdi, rbx
    mov rsi, rax
    call ast_push
    mov rdi, rbx
    call par_kind
    cmp eax, TOK_COMMA
    jne .close_names
    mov rdi, rbx
    call par_advance
    mov rdi, rbx
    call par_kind
    cmp eax, TOK_RPAR
    je .close_names                     ; a trailing comma
    jmp .names
.close_names:
    test r12d, r12d
    jz .build
    mov rdi, rbx
    mov esi, TOK_RPAR
    CSTRING rdx, "'(' was never closed"
    call par_expect
    test eax, eax
    jz .fail
    jmp .build

.star_import:
    ; The alias is at the '*', not at `from` -- the same rule as a named one.
    mov rdi, rbx
    call par_peek
    TOK_POS rax
    mov [rbp - PK_ALINE], rcx
    mov rdi, rbx
    call par_advance
    ; A star import is an alias with no name at all; codegen recognises it by
    ; the empty child list plus the star flag.
    mov rdi, rbx
    mov esi, AST_ALIAS
    mov edx, 1                          ; subkind 1 marks the star form
    mov rcx, [rbp - PK_ALINE]
    xor r8d, r8d
    xor r9d, r9d
    call ast_make
    mov rdi, rbx
    mov rsi, rax
    call ast_push

.build:
    mov rdi, rbx
    mov esi, AST_IMPORTFROM
    mov rdx, [rbp - PK_LINE]
    mov rcx, [rbp - PK_MARK]
    call par_finish_list
    test rax, rax
    jz .fail
    mov [rbp - PK_NODE], rax
    mov rdi, rbx
    mov rsi, rax
    call ast_at
    mov rdx, [rbp - PFR_MOD]
    mov [rax + AstNode.a], edx
    mov rdx, [rbp - PFR_LEVEL]
    mov [rax + AstNode.subkind], dl
    mov rax, [rbp - PK_NODE]
    pop r12
    pop rbx
    leave
    ret
.fail:
    xor eax, eax
    pop r12
    pop rbx
    leave
    ret
END_FUNC ps_from

;; ============================================================================
;; par_suite(Comp *c) -> rax = an AST_BLOCK node, 0 on error
;;
;; Either `: stmt; stmt` on one line, or `: NEWLINE INDENT stmts DEDENT`.  The
;; block node exists so an empty `else` and a missing one stay distinguishable:
;; node 0 means there was no clause at all.
;; ============================================================================
PSU_MARK  equ 16
PSU_LINE  equ 24
PSU_FRAME equ 24          ; + 1 push = 32
DEF_FUNC par_suite, PSU_FRAME
    push rbx
    mov rbx, rdi
    call par_peek
    TOK_POS rax
    mov [rbp - PSU_LINE], rcx

    mov rdi, rbx
    mov esi, TOK_COLON
    CSTRING rdx, "expected ':'"
    call par_expect
    test eax, eax
    jz .fail

    mov rdi, rbx
    call ast_mark
    mov [rbp - PSU_MARK], rax

    mov rdi, rbx
    call par_kind
    cmp eax, TOK_NEWLINE
    je .block

    ; A one-line suite: `if x: a; b`
    mov rdi, rbx
    call par_simple_stmts
    test eax, eax
    jz .fail
    jmp .done

.block:
    mov rdi, rbx
    call par_advance                    ; the NEWLINE
    mov rdi, rbx
    mov esi, TOK_INDENT
    CSTRING rdx, "expected an indented block"
    call par_expect
    test eax, eax
    jz .fail
.stmts:
    mov rdi, rbx
    call par_kind
    cmp eax, TOK_DEDENT
    je .end_block
    cmp eax, TOK_ENDMARKER
    je .end_block
    cmp eax, TOK_NEWLINE
    jne .one
    mov rdi, rbx
    call par_advance
    jmp .stmts
.one:
    mov rdi, rbx
    call par_statement_any
    test eax, eax
    jz .fail
    jmp .stmts
.end_block:
    mov rdi, rbx
    call par_kind
    cmp eax, TOK_ENDMARKER
    je .done
    mov rdi, rbx
    call par_advance                    ; the DEDENT

.done:
    mov rdi, rbx
    mov esi, AST_BLOCK
    mov rdx, [rbp - PSU_LINE]
    mov rcx, [rbp - PSU_MARK]
    call par_finish_list
    pop rbx
    leave
    ret
.fail:
    xor eax, eax
    pop rbx
    leave
    ret
END_FUNC par_suite

;; ============================================================================
;; par_statement_any(Comp *c) -> rax = 1 ok, 0 error
;; One statement, compound or simple, pushed onto the pending stack.  A
;; compound statement is a whole logical unit and consumes its own NEWLINEs; a
;; simple one is part of a `;`-separated line.
;; ============================================================================
DEF_FUNC par_statement_any, 8
    push rbx
    mov rbx, rdi
    call par_kind
    cmp eax, TOK_IF
    je .compound
    cmp eax, TOK_WHILE
    je .compound
    cmp eax, TOK_FOR
    je .compound
    cmp eax, TOK_DEF
    je .compound
    cmp eax, TOK_TRY
    je .compound
    cmp eax, TOK_WITH
    je .compound
    cmp eax, TOK_CLASS
    je .compound
    cmp eax, TOK_AT
    je .compound
    cmp eax, TOK_ASYNC
    je .compound
    ; `match` is a soft keyword: an ordinary name everywhere but here.
    cmp eax, TOK_NAME
    jne .simple
    mov rdi, rbx
    call par_looks_like_match
    test eax, eax
    jnz .compound
.simple:
    mov rdi, rbx
    call par_simple_stmts
    pop rbx
    leave
    ret
.compound:
    mov rdi, rbx
    call par_statement
    test rax, rax
    jz .fail
    mov rdi, rbx
    mov rsi, rax
    call ast_push
    mov eax, 1
    pop rbx
    leave
    ret
.fail:
    xor eax, eax
    pop rbx
    leave
    ret
END_FUNC par_statement_any

;; ============================================================================
;; ps_if - `if`, `elif` chains and `else`
;; An `elif` is parsed as an `if` nested inside the outer one's else block,
;; which is what makes an arbitrarily long chain need no special handling in
;; either the parser or the code generator.
;; ============================================================================
PIF_LINE  equ 16
PIF_TEST  equ 24
PIF_BODY  equ 32
PIF_ELSE  equ 40
PIF_NODE  equ 48
PIF_MARK  equ 56
PIF_FRAME equ 56          ; + 1 push = 64
DEF_FUNC_LOCAL ps_if, PIF_FRAME
    push rbx
    mov rbx, rdi
    call par_peek
    TOK_POS rax
    mov [rbp - PIF_LINE], rcx
    mov rdi, rbx
    call par_advance                    ; `if` or `elif`

    mov rdi, rbx
    mov esi, BP_NONE
    call par_expr
    test rax, rax
    jz .fail
    mov [rbp - PIF_TEST], rax

    mov rdi, rbx
    call par_suite
    test rax, rax
    jz .fail
    mov [rbp - PIF_BODY], rax
    mov qword [rbp - PIF_ELSE], 0

    mov rdi, rbx
    call par_kind
    cmp eax, TOK_ELIF
    je .elif
    cmp eax, TOK_ELSE
    jne .build

    mov rdi, rbx
    call par_advance
    mov rdi, rbx
    call par_suite
    test rax, rax
    jz .fail
    mov [rbp - PIF_ELSE], rax
    jmp .build

.elif:
    ; Wrap the nested `if` in a block so the else branch has a uniform shape.
    mov rdi, rbx
    call ast_mark
    mov [rbp - PIF_MARK], rax
    mov rdi, rbx
    call ps_if
    test rax, rax
    jz .fail
    mov rdi, rbx
    mov rsi, rax
    call ast_push
    mov rdi, rbx
    mov esi, AST_BLOCK
    mov rdx, [rbp - PIF_LINE]
    mov rcx, [rbp - PIF_MARK]
    call par_finish_list
    test rax, rax
    jz .fail
    mov [rbp - PIF_ELSE], rax

.build:
    mov rdi, rbx
    mov esi, AST_IF
    xor edx, edx
    mov rcx, [rbp - PIF_LINE]
    mov r8, [rbp - PIF_TEST]
    mov r9, [rbp - PIF_ELSE]
    call ast_make
    mov [rbp - PIF_NODE], rax
    mov rdi, rbx
    mov rsi, rax
    call ast_at
    mov rdx, [rbp - PIF_BODY]
    mov [rax + AstNode.c], edx
    mov rax, [rbp - PIF_NODE]
    pop rbx
    leave
    ret
.fail:
    xor eax, eax
    pop rbx
    leave
    ret
END_FUNC ps_if

;; ============================================================================
;; ps_while - `while test: body [else: body]`
;; ============================================================================
DEF_FUNC_LOCAL ps_while, PIF_FRAME
    push rbx
    mov rbx, rdi
    call par_peek
    TOK_POS rax
    mov [rbp - PIF_LINE], rcx
    mov rdi, rbx
    call par_advance

    mov rdi, rbx
    mov esi, BP_NONE
    call par_expr
    test rax, rax
    jz .fail
    mov [rbp - PIF_TEST], rax

    mov rdi, rbx
    call par_suite
    test rax, rax
    jz .fail
    mov [rbp - PIF_BODY], rax
    mov qword [rbp - PIF_ELSE], 0

    mov rdi, rbx
    call par_kind
    cmp eax, TOK_ELSE
    jne .build
    mov rdi, rbx
    call par_advance
    mov rdi, rbx
    call par_suite
    test rax, rax
    jz .fail
    mov [rbp - PIF_ELSE], rax

.build:
    mov rdi, rbx
    mov esi, AST_WHILE
    xor edx, edx
    mov rcx, [rbp - PIF_LINE]
    mov r8, [rbp - PIF_TEST]
    mov r9, [rbp - PIF_ELSE]
    call ast_make
    mov [rbp - PIF_NODE], rax
    mov rdi, rbx
    mov rsi, rax
    call ast_at
    mov rdx, [rbp - PIF_BODY]
    mov [rax + AstNode.c], edx
    mov rax, [rbp - PIF_NODE]
    pop rbx
    leave
    ret
.fail:
    xor eax, eax
    pop rbx
    leave
    ret
END_FUNC ps_while

;; ============================================================================
;; ps_for - `for target in iter: body [else: body]`
;; The target is parsed as an expression and re-marked, exactly as an
;; assignment target is -- `for a, b in pairs` unpacks for the same reason
;; `a, b = pair` does.
;; ============================================================================
DEF_FUNC_LOCAL ps_for, PIF_FRAME
    push rbx
    mov rbx, rdi
    call par_peek
    TOK_POS rax
    mov [rbp - PIF_LINE], rcx
    mov rdi, rbx
    call par_advance

    mov rdi, rbx
    call par_for_target
    test rax, rax
    jz .fail
    mov [rbp - PIF_TEST], rax           ; the target

    mov rdi, rbx
    mov esi, TOK_IN
    CSTRING rdx, "expected 'in' after the loop target"
    call par_expect
    test eax, eax
    jz .fail

    mov rdi, rbx
    call par_exprlist_stmt              ; `for x in a, b` iterates the tuple
    test rax, rax
    jz .fail
    mov [rbp - PIF_NODE], rax           ; the iterable

    mov rdi, rbx
    call par_suite
    test rax, rax
    jz .fail
    mov [rbp - PIF_BODY], rax
    mov qword [rbp - PIF_ELSE], 0

    mov rdi, rbx
    call par_kind
    cmp eax, TOK_ELSE
    jne .build
    mov rdi, rbx
    call par_advance
    mov rdi, rbx
    call par_suite
    test rax, rax
    jz .fail
    mov [rbp - PIF_ELSE], rax

.build:
    mov rdi, rbx
    mov esi, AST_FOR
    xor edx, edx
    mov rcx, [rbp - PIF_LINE]
    mov r8, [rbp - PIF_TEST]            ; a = target
    mov r9, [rbp - PIF_NODE]            ; b = iterable
    call ast_make
    mov [rbp - PIF_MARK], rax
    mov rdi, rbx
    mov rsi, rax
    call ast_at
    mov rdx, [rbp - PIF_BODY]
    mov [rax + AstNode.c], edx          ; c = body block
    mov rdx, [rbp - PIF_ELSE]
    mov [rax + AstNode.clist], edx      ; clist doubles as the else block here
    mov rax, [rbp - PIF_MARK]
    pop rbx
    leave
    ret
.fail:
    xor eax, eax
    pop rbx
    leave
    ret
END_FUNC ps_for

;; ============================================================================
;; par_for_target(Comp *c) -> node, marked for storing
;; Stops at `in`, which par_expr would otherwise take as a comparison.
;; ============================================================================
PFT_FIRST equ 16
PFT_LINE  equ 24
PFT_MARK  equ 32
PFT_FRAME equ 40          ; + 1 push = 48
DEF_FUNC par_for_target, PFT_FRAME
    push rbx
    mov rbx, rdi
    call par_peek
    TOK_POS rax
    mov [rbp - PFT_LINE], rcx

    mov rdi, rbx
    mov esi, BP_COMPARE                 ; above `in`, so the loop keyword ends it
    call par_expr
    test rax, rax
    jz .fail
    mov [rbp - PFT_FIRST], rax

    mov rdi, rbx
    call par_kind
    cmp eax, TOK_COMMA
    jne .single

    mov rdi, rbx
    call ast_mark
    mov [rbp - PFT_MARK], rax
    mov rdi, rbx
    mov rsi, [rbp - PFT_FIRST]
    call ast_push
.loop:
    mov rdi, rbx
    call par_advance                    ; the comma
    mov rdi, rbx
    call par_kind
    cmp eax, TOK_IN
    je .close
    mov rdi, rbx
    mov esi, BP_COMPARE
    call par_expr
    test rax, rax
    jz .fail
    mov rdi, rbx
    mov rsi, rax
    call ast_push
    mov rdi, rbx
    call par_kind
    cmp eax, TOK_COMMA
    je .loop
.close:
    mov rdi, rbx
    mov esi, AST_TUPLE
    mov rdx, [rbp - PFT_LINE]
    mov rcx, [rbp - PFT_MARK]
    call par_finish_list
    test rax, rax
    jz .fail
    mov [rbp - PFT_FIRST], rax
.single:
    mov rdi, rbx
    mov rsi, [rbp - PFT_FIRST]
    mov edx, CTX_STORE
    call ast_set_ctx
    test eax, eax
    jz .bad
    mov rax, [rbp - PFT_FIRST]
    pop rbx
    leave
    ret
.bad:
    mov rdi, rbx
    CSTRING rsi, "cannot assign to that loop target"
    call par_syntax_error
.fail:
    xor eax, eax
    pop rbx
    leave
    ret
END_FUNC par_for_target

;; ============================================================================
;; par_params(Comp *c, int close) -> rax = an AST_ARGUMENTS node, 0 on error
;;
;; The child list holds the positional parameters followed by the keyword-only
;; ones -- the order co_varnames needs -- while `*args` and `**kwargs` hang off
;; .b and .c.  They cannot go in the list: localsplus puts them after the
;; keyword-only slots, and func_call finds them by arithmetic on co_argcount
;; and co_kwonlyargcount rather than by searching.
;;
;;   .a      an AST_EXTRA node carrying argcount / posonly / kwonly
;;   .b      the *args parameter, or 0
;;   .c      the **kwargs parameter, or 0
;;   .clist  positional parameters, then keyword-only ones
;; ============================================================================
PP_CLOSE  equ 16
PP_LINE   equ 24
PP_MARK   equ 32
PP_NPOS   equ 40
PP_NKW    equ 48
PP_POSONLY equ 56
PP_STAR   equ 64
PP_VARARG equ 72
PP_VARKW  equ 80
PP_NODE   equ 88
PP_FRAME  equ 88          ; + 1 push = 96
DEF_FUNC par_params, PP_FRAME
    push rbx
    mov rbx, rdi
    mov [rbp - PP_CLOSE], rsi
    call par_peek
    TOK_POS rax
    mov [rbp - PP_LINE], rcx
    mov rdi, rbx
    call ast_mark
    mov [rbp - PP_MARK], rax
    mov qword [rbp - PP_NPOS], 0
    mov qword [rbp - PP_NKW], 0
    mov qword [rbp - PP_POSONLY], 0
    mov qword [rbp - PP_STAR], 0        ; have we passed the * marker?
    mov qword [rbp - PP_VARARG], 0
    mov qword [rbp - PP_VARKW], 0

.loop:
    mov rdi, rbx
    call par_kind
    cmp rax, [rbp - PP_CLOSE]
    je .build
    cmp eax, TOK_ENDMARKER
    je .build

    cmp eax, TOK_SLASH
    je .posonly_marker
    cmp eax, TOK_STAR
    je .star_marker
    cmp eax, TOK_DOUBLESTAR
    je .varkw

    ; An ordinary parameter.
    mov rdi, rbx
    mov rsi, 1
    cmp qword [rbp - PP_CLOSE], TOK_COLON
    jne .ann_ok1
    xor esi, esi
.ann_ok1:
    call par_param_here
    test rax, rax
    jz .fail
    mov rdi, rbx
    mov rsi, rax
    call ast_push
    cmp qword [rbp - PP_STAR], 0
    je .count_pos
    inc qword [rbp - PP_NKW]
    jmp .comma
.count_pos:
    inc qword [rbp - PP_NPOS]
    jmp .comma

.posonly_marker:
    ; Everything so far was positional-only.
    mov rax, [rbp - PP_NPOS]
    mov [rbp - PP_POSONLY], rax
    mov rdi, rbx
    call par_advance
    jmp .comma

.star_marker:
    mov rdi, rbx
    call par_advance
    mov qword [rbp - PP_STAR], 1
    ; A bare `*` only separates; `*args` also collects.
    mov rdi, rbx
    call par_kind
    cmp eax, TOK_COMMA
    je .comma
    cmp rax, [rbp - PP_CLOSE]
    je .build
    mov rdi, rbx
    mov rsi, 1
    cmp qword [rbp - PP_CLOSE], TOK_COLON
    jne .ann_ok2
    xor esi, esi
.ann_ok2:
    call par_param_here
    test rax, rax
    jz .fail
    mov [rbp - PP_VARARG], rax
    jmp .comma

.varkw:
    mov rdi, rbx
    call par_advance
    mov rdi, rbx
    mov rsi, 1
    cmp qword [rbp - PP_CLOSE], TOK_COLON
    jne .ann_ok3
    xor esi, esi
.ann_ok3:
    call par_param_here
    test rax, rax
    jz .fail
    mov [rbp - PP_VARKW], rax
    jmp .comma

.comma:
    mov rdi, rbx
    call par_kind
    cmp eax, TOK_COMMA
    jne .build
    mov rdi, rbx
    call par_advance
    jmp .loop

.build:
    mov rdi, rbx
    mov esi, AST_ARGUMENTS
    mov rdx, [rbp - PP_LINE]
    mov rcx, [rbp - PP_MARK]
    call par_finish_list
    test rax, rax
    jz .fail
    mov [rbp - PP_NODE], rax

    ; The counts live on an auxiliary node: five fields are not enough.
    mov rdi, rbx
    mov esi, AST_EXTRA
    xor edx, edx
    mov rcx, [rbp - PP_LINE]
    mov r8, [rbp - PP_NPOS]
    mov r9, [rbp - PP_POSONLY]
    call ast_make
    mov rsi, rax
    push rsi
    mov rdi, rbx
    call ast_at
    pop rsi
    mov rdx, [rbp - PP_NKW]
    mov [rax + AstNode.c], edx

    mov rdi, rbx
    push rsi
    mov rsi, [rbp - PP_NODE]
    call ast_at
    pop rsi
    mov [rax + AstNode.a], esi
    mov rdx, [rbp - PP_VARARG]
    mov [rax + AstNode.b], edx
    mov rdx, [rbp - PP_VARKW]
    mov [rax + AstNode.c], edx

    mov rax, [rbp - PP_NODE]
    pop rbx
    leave
    ret
.fail:
    xor eax, eax
    pop rbx
    leave
    ret
END_FUNC par_params

;; ============================================================================
;; par_one_param(Comp *c) -> rax = an AST_ARG node, 0 on error
;;   .a = the name, .b = its annotation node, .c = its default expression
;; ============================================================================
POP_LINE  equ 16
POP_NAME  equ 24
POP_ANN   equ 32
POP_DEF   equ 40
POP_NODE  equ 48
POP_END   equ 56          ; the token cursor where the parameter itself ends
POP_FRAME equ 64          ; + 2 pushes = 80
DEF_FUNC par_one_param, POP_FRAME
    push rbx
    push r12
    mov rbx, rdi
    mov r12, rsi                        ; annotations allowed?
    call par_peek
    TOK_POS rax
    mov [rbp - POP_LINE], rcx
    mov qword [rbp - POP_ANN], 0
    mov qword [rbp - POP_DEF], 0

    mov rdi, rbx
    call par_name_obj
    test rax, rax
    jz .fail
    mov [rbp - POP_NAME], rax

    ; A lambda's parameter list is terminated by a colon, so a colon there is
    ; the end of the list rather than an annotation -- and lambda parameters
    ; cannot be annotated at all.
    test r12, r12
    jz .no_ann
    mov rdi, rbx
    call par_kind
    cmp eax, TOK_COLON
    jne .no_ann
    mov rdi, rbx
    call par_advance
    mov rdi, rbx
    mov esi, BP_NONE
    call par_expr
    test rax, rax
    jz .fail
    mov [rbp - POP_ANN], rax
.no_ann:
    ; A parameter ends at its name, or at its annotation; the default that
    ; may follow is not part of it, and this node is not made until after it
    ; has been parsed.
    mov eax, [rbx + Comp.tok_idx]
    mov [rbp - POP_END], rax
    mov rdi, rbx
    call par_kind
    cmp eax, TOK_EQUAL
    jne .build
    mov rdi, rbx
    call par_advance
    mov rdi, rbx
    mov esi, BP_NONE
    call par_expr
    test rax, rax
    jz .fail
    mov [rbp - POP_DEF], rax

.build:
    mov rdi, rbx
    mov esi, AST_ARG
    xor edx, edx
    mov rcx, [rbp - POP_LINE]
    mov r8, [rbp - POP_NAME]
    mov r9, [rbp - POP_ANN]
    call ast_make
    mov [rbp - POP_NODE], rax
    mov rdi, rbx
    mov rsi, rax
    call ast_at
    mov rdx, [rbp - POP_DEF]
    mov [rax + AstNode.c], edx
    mov rdi, rbx
    mov rsi, [rbp - POP_NODE]
    mov edx, [rbp - POP_END]
    call ast_end_at
    mov rax, [rbp - POP_NODE]
    pop r12
    pop rbx
    leave
    ret
.fail:
    xor eax, eax
    pop r12
    pop rbx
    leave
    ret
END_FUNC par_one_param

;; ============================================================================
;; par_param_here(Comp *c) -> one parameter, annotations allowed only when the
;; list is not a lambda's.  par_params keeps the terminator in PP_CLOSE, and a
;; colon terminator means a lambda.
;; ============================================================================
DEF_FUNC_BARE par_param_here
    ; PP_CLOSE lives in the caller's frame; par_params passes it through rsi.
    jmp par_one_param
END_FUNC par_param_here

;; ============================================================================
;; ps_def - `def name(params) [-> ann]: body`
;;   .a = the name, .b = the AST_ARGUMENTS node, .clist = the body statements
;; ============================================================================
PDF_LINE  equ 16
PDF_NAME  equ 24
PDF_ARGS  equ 32
PDF_MARK  equ 40
PDF_NODE  equ 48
PDF_RET   equ 56          ; the return annotation, kept but never generated
PDF_FRAME equ 72          ; + 1 push = 80
DEF_FUNC_LOCAL ps_def, PDF_FRAME
    push rbx
    mov rbx, rdi
    call par_peek
    TOK_POS rax
    mov [rbp - PDF_LINE], rcx
    mov rdi, rbx
    call par_advance                    ; `def`

    mov rdi, rbx
    call par_name_obj
    test rax, rax
    jz .fail
    mov [rbp - PDF_NAME], rax

    mov rdi, rbx
    call par_skip_type_params
    test eax, eax
    jz .fail

    mov rdi, rbx
    mov esi, TOK_LPAR
    CSTRING rdx, "expected '(' after the function name"
    call par_expect
    test eax, eax
    jz .fail
    mov rdi, rbx
    mov esi, TOK_RPAR
    call par_params
    test rax, rax
    jz .fail
    mov [rbp - PDF_ARGS], rax
    mov rdi, rbx
    mov esi, TOK_RPAR
    CSTRING rdx, "'(' was never closed"
    call par_expect
    test eax, eax
    jz .fail

    ; A return annotation is parsed and kept on the node, but never generated:
    ; apython's MAKE_FUNCTION drops annotations anyway, so evaluating one would
    ; only add a side effect that CPython has and we cannot honour.  Neither
    ; the symbol table nor the code generator looks at `.c`; `_ast` does, and
    ; reports it as `returns`.
    mov qword [rbp - PDF_RET], 0
    mov rdi, rbx
    call par_kind
    cmp eax, TOK_RARROW
    jne .suite
    mov rdi, rbx
    call par_advance
    mov rdi, rbx
    mov esi, BP_NONE
    call par_expr
    test rax, rax
    jz .fail
    mov [rbp - PDF_RET], rax

.suite:
    ; The body is collected directly into the def's own child list.
    mov rdi, rbx
    call ast_mark
    mov [rbp - PDF_MARK], rax
    mov rdi, rbx
    call par_suite_into
    test eax, eax
    jz .fail

    mov rdi, rbx
    mov esi, AST_FUNCTIONDEF
    mov rdx, [rbp - PDF_LINE]
    mov rcx, [rbp - PDF_MARK]
    call par_finish_list
    test rax, rax
    jz .fail
    mov [rbp - PDF_NODE], rax
    mov rdi, rbx
    mov rsi, rax
    call ast_at
    mov rdx, [rbp - PDF_NAME]
    mov [rax + AstNode.a], edx
    mov rdx, [rbp - PDF_ARGS]
    mov [rax + AstNode.b], edx
    mov rdx, [rbp - PDF_RET]
    mov [rax + AstNode.c], edx
    mov rax, [rbp - PDF_NODE]
    pop rbx
    leave
    ret
.fail:
    xor eax, eax
    pop rbx
    leave
    ret
END_FUNC ps_def

;; ============================================================================
;; par_suite_into(Comp *c) -> rax = 1 ok, 0 error
;; Like par_suite, but pushes the statements onto the caller's pending list
;; instead of wrapping them in a block node.
;; ============================================================================
DEF_FUNC par_suite_into, 8
    push rbx
    mov rbx, rdi
    mov esi, TOK_COLON
    CSTRING rdx, "expected ':'"
    call par_expect
    test eax, eax
    jz .fail
    mov rdi, rbx
    call par_kind
    cmp eax, TOK_NEWLINE
    je .block
    mov rdi, rbx
    call par_simple_stmts
    pop rbx
    leave
    ret
.block:
    mov rdi, rbx
    call par_advance
    mov rdi, rbx
    mov esi, TOK_INDENT
    CSTRING rdx, "expected an indented block"
    call par_expect
    test eax, eax
    jz .fail
.stmts:
    mov rdi, rbx
    call par_kind
    cmp eax, TOK_DEDENT
    je .close
    cmp eax, TOK_ENDMARKER
    je .close
    cmp eax, TOK_NEWLINE
    jne .one
    mov rdi, rbx
    call par_advance
    jmp .stmts
.one:
    mov rdi, rbx
    call par_statement_any
    test eax, eax
    jz .fail
    jmp .stmts
.close:
    mov rdi, rbx
    call par_kind
    cmp eax, TOK_ENDMARKER
    je .ok
    mov rdi, rbx
    call par_advance
.ok:
    mov eax, 1
    pop rbx
    leave
    ret
.fail:
    xor eax, eax
    pop rbx
    leave
    ret
END_FUNC par_suite_into

;; ============================================================================
;; ps_async(Comp *c) -> node   -- `async def`, `async for`, `async with`
;;
;; `async` only ever prefixes one of three statements, and each of the three
;; already has a parser.  So consume the keyword, run the ordinary parser, and
;; stamp subkind=1 on what comes back; every consumer of AST_FUNCTIONDEF,
;; AST_FOR and AST_WITH reads that one bit rather than a parallel node kind.
;; ============================================================================
PAS_LINE  equ 8           ; the `async` keyword's own position
PAS_FRAME equ 24          ; + 1 push = 32
DEF_FUNC_LOCAL ps_async, PAS_FRAME
    push rbx
    mov rbx, rdi
    mov rdi, rbx
    call par_peek
    TOK_POS rax
    mov [rbp - PAS_LINE], rcx
    mov rdi, rbx
    call par_advance                    ; `async`

    mov rdi, rbx
    call par_kind
    mov rdi, rbx
    cmp eax, TOK_DEF
    je .def
    cmp eax, TOK_FOR
    je .for
    cmp eax, TOK_WITH
    je .with

    mov rdi, rbx
    CSTRING rsi, "expected 'def', 'for' or 'with' after 'async'"
    call par_syntax_error
    xor eax, eax
    pop rbx
    leave
    ret

.def:
    call ps_def
    jmp .stamp
.for:
    call ps_for
    jmp .stamp
.with:
    call ps_with
.stamp:
    test rax, rax
    jz .done
    push rax
    mov rdi, rbx
    mov rsi, rax
    call ast_at
    mov byte [rax + AstNode.subkind], 1
    ; The statement begins at `async`, not at the `def`/`for`/`with` that the
    ; inner parser saw first.
    mov rcx, [rbp - PAS_LINE]
    mov [rax + AstNode.lineno], ecx
    shr rcx, 32
    mov [rax + AstNode.col], ecx
    pop rax
.done:
    pop rbx
    leave
    ret
END_FUNC ps_async

;; ============================================================================
;; par_skip_type_params(Comp *c) -> rax = 1 ok, 0 error
;;
;; PEP 695's `def f[T](...)`, `class C[T]:` and `type X[T] = ...`.
;;
;; The parameters are read and discarded.  A type parameter is visible only to
;; annotations, and annotations are not evaluated here at all -- op_make_function
;; pops and discards them -- so there is nothing for the names to be bound for.
;; Accepting the syntax is what matters: without this the whole definition is a
;; syntax error rather than a function that ignores its type parameters.
;; ============================================================================
STP_DEPTH equ 8
STP_FRAME equ 24          ; + 1 push = 32
DEF_FUNC par_skip_type_params, STP_FRAME
    push rbx
    mov rbx, rdi
    call par_kind
    cmp eax, TOK_LSQB
    jne .none
    mov qword [rbp - STP_DEPTH], 0
.loop:
    mov rdi, rbx
    call par_kind
    cmp eax, TOK_ENDMARKER
    je .unterminated
    cmp eax, TOK_NEWLINE
    je .unterminated
    cmp eax, TOK_LSQB
    jne .not_open
    inc qword [rbp - STP_DEPTH]
.not_open:
    cmp eax, TOK_RSQB
    jne .step
    dec qword [rbp - STP_DEPTH]
    cmp qword [rbp - STP_DEPTH], 0
    jne .step
    mov rdi, rbx
    call par_advance
    mov eax, 1
    pop rbx
    leave
    ret
.step:
    mov rdi, rbx
    call par_advance
    jmp .loop
.unterminated:
    mov rdi, rbx
    CSTRING rsi, "'[' was never closed"
    call par_syntax_error
    xor eax, eax
    pop rbx
    leave
    ret
.none:
    mov eax, 1
    pop rbx
    leave
    ret
END_FUNC par_skip_type_params

;; ============================================================================
;; par_looks_like_type_alias(Comp *c) -> rax = 1 for a PEP 695 `type X = ...`
;; `type` is a soft keyword and also a builtin, so `type(x)`, `type = f` and
;; `type.mro` all have to keep working: the statement form is NAME NAME, or
;; NAME NAME '[', and nothing else is.
;; ============================================================================
DEF_FUNC_BARE par_looks_like_type_alias
    mov eax, [rdi + Comp.tok_idx]
    inc eax
    mov rdx, [rdi + Comp.tokens + Buf.len]
    cmp rax, rdx
    jae .no
    mov rcx, [rdi + Comp.tokens + Buf.data]
    mov rsi, rax
    imul rsi, rsi, Token_size
    movzx esi, word [rcx + rsi + Token.kind]
    cmp esi, TOK_NAME
    jne .no
    ; The token after the alias name settles it: '=' or a parameter list.
    inc rax
    cmp rax, rdx
    jae .no
    imul rax, rax, Token_size
    movzx eax, word [rcx + rax + Token.kind]
    cmp eax, TOK_EQUAL
    je .yes
    cmp eax, TOK_LSQB
    je .yes
.no:
    xor eax, eax
    ret
.yes:
    mov eax, 1
    ret
END_FUNC par_looks_like_type_alias

;; ============================================================================
;; ps_type_alias(Comp *c) -> node   -- `type X = V` and `type X[T] = V`
;;
;; Compiled as the assignment `X = V`.  CPython makes X a TypeAliasType whose
;; value is evaluated lazily; there is no such type here, and nothing that
;; would observe the difference, since annotations are never evaluated.  The
;; alias is the value, which is what code that reads it back expects.
;; ============================================================================
PTA_LINE  equ 8
PTA_NAME  equ 16
PTA_MARK  equ 24
PTA_VALUE equ 32
PTA_NODE  equ 40
PTA_FRAME equ 40          ; + 1 push = 48
DEF_FUNC_LOCAL ps_type_alias, PTA_FRAME
    push rbx
    mov rbx, rdi
    call par_peek
    TOK_POS rax
    mov [rbp - PTA_LINE], rcx
    mov rdi, rbx
    call par_advance                    ; the soft keyword `type`

    call par_peek
    TOK_POS rax
    mov rdi, rbx
    call par_name_obj
    test rax, rax
    jz .fail
    mov r8, rax
    mov rdi, rbx
    mov esi, AST_NAME
    mov edx, CTX_STORE
    mov rcx, [rbp - PTA_LINE]
    xor r9d, r9d
    call ast_make
    test rax, rax
    jz .fail
    mov [rbp - PTA_NAME], rax

    mov rdi, rbx
    call par_skip_type_params
    test eax, eax
    jz .fail

    mov rdi, rbx
    mov esi, TOK_EQUAL
    CSTRING rdx, "expected '=' in a type alias"
    call par_expect
    test eax, eax
    jz .fail

    mov rdi, rbx
    mov esi, BP_NONE
    call par_expr
    test rax, rax
    jz .fail
    mov [rbp - PTA_VALUE], rax

    mov rdi, rbx
    call ast_mark
    mov [rbp - PTA_MARK], rax
    mov rdi, rbx
    mov rsi, [rbp - PTA_NAME]
    call ast_push
    mov rdi, rbx
    mov esi, AST_ASSIGN
    mov rdx, [rbp - PTA_LINE]
    mov rcx, [rbp - PTA_MARK]
    call par_finish_list
    test rax, rax
    jz .fail
    mov [rbp - PTA_NODE], rax
    mov rdi, rbx
    mov rsi, rax
    call ast_at
    mov rdx, [rbp - PTA_VALUE]
    mov [rax + AstNode.b], edx
    mov rax, [rbp - PTA_NODE]
    pop rbx
    leave
    ret
.fail:
    xor eax, eax
    pop rbx
    leave
    ret
END_FUNC ps_type_alias

;; ============================================================================
;; ps_return - `return` and `return value`
;; ============================================================================
DEF_FUNC_LOCAL ps_return, PK_FRAME
    push rbx
    mov rbx, rdi
    call par_peek
    TOK_POS rax
    mov [rbp - PK_LINE], rcx
    mov rdi, rbx
    call par_advance
    mov qword [rbp - PK_A], 0
    mov rdi, rbx
    call par_kind
    cmp eax, TOK_NEWLINE
    je .build
    cmp eax, TOK_SEMI
    je .build
    cmp eax, TOK_ENDMARKER
    je .build
    cmp eax, TOK_DEDENT
    je .build
    mov rdi, rbx
    call par_exprlist_stmt
    test rax, rax
    jz .fail
    mov [rbp - PK_A], rax
.build:
    mov rdi, rbx
    mov esi, AST_RETURN
    xor edx, edx
    mov rcx, [rbp - PK_LINE]
    mov r8, [rbp - PK_A]
    xor r9d, r9d
    call ast_make
    pop rbx
    leave
    ret
.fail:
    xor eax, eax
    pop rbx
    leave
    ret
END_FUNC ps_return

;; ============================================================================
;; ps_try - try / except / else / finally
;;   .clist = body statements
;;   .a     = a block of AST_HANDLER clauses
;;   .b     = the else block
;;   .c     = the finally block
;; ============================================================================
PT2_LINE  equ 8
PT2_MARK  equ 16
PT2_HAND  equ 24
PT2_ELSE  equ 32
PT2_FIN   equ 40
PT2_NODE  equ 48
PT2_HMARK equ 56
PT2_TYPE  equ 64
PT2_NAME  equ 72
PT2_BODY  equ 80
PT2_SAVET equ 88          ; token index, for the parenthesised with-items try
PT2_SAVEP equ 96          ; pending-stack height
PT2_SAVEE equ 104         ; whether an error was already recorded
PT2_PAREN equ 112         ; 1 while inside a parenthesised item list
PT2_STAR  equ 88
PT2_HLINE equ 120         ; where THIS handler's `except` is, not the `try`
PT2_FRAME equ 152         ; + 1 push = 160, 16-byte aligned
DEF_FUNC_LOCAL ps_try, PT2_FRAME
    push rbx
    mov rbx, rdi
    call par_peek
    TOK_POS rax
    mov [rbp - PT2_LINE], rcx
    mov rdi, rbx
    call par_advance                    ; `try`

    mov rdi, rbx
    call ast_mark
    mov [rbp - PT2_MARK], rax
    mov rdi, rbx
    call par_suite_into
    test eax, eax
    jz .fail
    mov qword [rbp - PT2_HAND], 0
    mov qword [rbp - PT2_ELSE], 0
    mov qword [rbp - PT2_FIN], 0
    mov qword [rbp - PT2_STAR], 0

    ; --- except clauses ---
    mov rdi, rbx
    call par_kind
    cmp eax, TOK_EXCEPT
    jne .else_clause

    mov rdi, rbx
    call ast_mark
    mov [rbp - PT2_HMARK], rax
.except_loop:
    mov rdi, rbx
    call par_kind
    cmp eax, TOK_EXCEPT
    jne .close_handlers
    ; A handler is at its own `except`, not at the `try` above it.
    mov rdi, rbx
    call par_peek
    TOK_POS rax
    mov [rbp - PT2_HLINE], rcx
    mov rdi, rbx
    call par_advance
    mov qword [rbp - PT2_TYPE], 0
    mov qword [rbp - PT2_NAME], 0

    ; `except*` is a different statement, not a variant of one clause: all of
    ; a try's handlers are star handlers or none are, and the flag belongs to
    ; the try.  The mixed form is a syntax error CPython also rejects.
    mov rdi, rbx
    call par_kind
    cmp eax, TOK_STAR
    jne .not_star
    mov rdi, rbx
    call par_advance
    mov qword [rbp - PT2_STAR], 1
.not_star:

    mov rdi, rbx
    call par_kind
    cmp eax, TOK_COLON
    je .handler_suite
    mov rdi, rbx
    mov esi, BP_NONE
    call par_expr
    test rax, rax
    jz .fail
    mov [rbp - PT2_TYPE], rax
    mov rdi, rbx
    call par_kind
    cmp eax, TOK_AS
    jne .handler_suite
    mov rdi, rbx
    call par_advance
    mov rdi, rbx
    call par_name_obj
    test rax, rax
    jz .fail
    mov [rbp - PT2_NAME], rax

.handler_suite:
    mov rdi, rbx
    call ast_mark
    mov [rbp - PT2_BODY], rax
    mov rdi, rbx
    call par_suite_into
    test eax, eax
    jz .fail
    mov rdi, rbx
    mov esi, AST_HANDLER
    mov rdx, [rbp - PT2_HLINE]
    mov rcx, [rbp - PT2_BODY]
    call par_finish_list
    test rax, rax
    jz .fail
    mov [rbp - PT2_BODY], rax
    mov rdi, rbx
    mov rsi, rax
    call ast_at
    mov rdx, [rbp - PT2_TYPE]
    mov [rax + AstNode.a], edx
    mov rdx, [rbp - PT2_NAME]
    mov [rax + AstNode.b], edx
    mov rdi, rbx
    mov rsi, [rbp - PT2_BODY]
    call ast_push
    jmp .except_loop

.close_handlers:
    mov rdi, rbx
    mov esi, AST_BLOCK
    mov rdx, [rbp - PT2_LINE]
    mov rcx, [rbp - PT2_HMARK]
    call par_finish_list
    test rax, rax
    jz .fail
    mov [rbp - PT2_HAND], rax

.else_clause:
    mov rdi, rbx
    call par_kind
    cmp eax, TOK_ELSE
    jne .finally_clause
    mov rdi, rbx
    call par_advance
    mov rdi, rbx
    call par_suite
    test rax, rax
    jz .fail
    mov [rbp - PT2_ELSE], rax

.finally_clause:
    mov rdi, rbx
    call par_kind
    cmp eax, TOK_FINALLY
    jne .check
    mov rdi, rbx
    call par_advance
    mov rdi, rbx
    call par_suite
    test rax, rax
    jz .fail
    mov [rbp - PT2_FIN], rax

.check:
    cmp qword [rbp - PT2_HAND], 0
    jne .build
    cmp qword [rbp - PT2_FIN], 0
    jne .build
    mov rdi, rbx
    CSTRING rsi, "try statement must have except or finally"
    call par_syntax_error
    jmp .fail

.build:
    mov rdi, rbx
    mov esi, AST_TRY
    mov rdx, [rbp - PT2_LINE]
    mov rcx, [rbp - PT2_MARK]
    call par_finish_list
    test rax, rax
    jz .fail
    mov [rbp - PT2_NODE], rax
    mov rdi, rbx
    mov rsi, rax
    call ast_at
    mov rdx, [rbp - PT2_HAND]
    mov [rax + AstNode.a], edx
    mov rdx, [rbp - PT2_ELSE]
    mov [rax + AstNode.b], edx
    mov rdx, [rbp - PT2_FIN]
    mov [rax + AstNode.c], edx
    mov rdx, [rbp - PT2_STAR]
    mov [rax + AstNode.subkind], dl
    mov rax, [rbp - PT2_NODE]
    pop rbx
    leave
    ret
.fail:
    xor eax, eax
    pop rbx
    leave
    ret
END_FUNC ps_try

;; ============================================================================
;; ps_with - `with a as x, b: body`
;;   .clist = AST_WITHITEM nodes, .a = the body block
;; ============================================================================
DEF_FUNC_LOCAL ps_with, PT2_FRAME
    push rbx
    mov rbx, rdi
    call par_peek
    TOK_POS rax
    mov [rbp - PT2_LINE], rcx
    mov rdi, rbx
    call par_advance                    ; `with`

    mov rdi, rbx
    call ast_mark
    mov [rbp - PT2_MARK], rax
    mov qword [rbp - PT2_PAREN], 0

    ; A `(` here may open a parenthesised item list -- `with (a as x, b):` --
    ; or an ordinary parenthesised expression, and only trying it tells them
    ; apart.  CPython's PEG parser backtracks here too; the state to restore
    ; is the token index, the pending-node height and the recorded error.
    mov rdi, rbx
    call par_kind
    cmp eax, TOK_LPAR
    jne .item_loop
    mov eax, [rbx + Comp.tok_idx]
    mov [rbp - PT2_SAVET], rax
    mov rax, [rbx + Comp.pending + Buf.len]
    mov [rbp - PT2_SAVEP], rax
    mov eax, [rbx + Comp.err + CompErr.set]
    mov [rbp - PT2_SAVEE], rax
    mov qword [rbp - PT2_PAREN], 1
    mov rdi, rbx
    call par_advance                    ; `(`
    jmp .item_loop

.restore_plain:
    ; Not an item list after all: put everything back and parse the `(` as
    ; the start of an ordinary expression.
    mov rax, [rbp - PT2_SAVET]
    mov [rbx + Comp.tok_idx], eax
    mov rax, [rbp - PT2_SAVEP]
    mov [rbx + Comp.pending + Buf.len], rax
    mov rax, [rbp - PT2_SAVEE]
    mov [rbx + Comp.err + CompErr.set], eax
    mov qword [rbp - PT2_PAREN], 0

.item_loop:
    mov rdi, rbx
    mov esi, BP_NONE
    call par_expr
    test rax, rax
    jz .item_failed
    mov [rbp - PT2_TYPE], rax
    mov qword [rbp - PT2_NAME], 0
    mov rdi, rbx
    call par_kind
    cmp eax, TOK_AS
    jne .make_item
    mov rdi, rbx
    call par_advance
    mov rdi, rbx
    mov esi, BP_COMPARE
    call par_expr
    test rax, rax
    jz .fail
    mov rsi, rax
    mov [rbp - PT2_NAME], rax
    mov rdi, rbx
    mov edx, CTX_STORE
    call ast_set_ctx
    test eax, eax
    jz .bad_target
.make_item:
    mov rdi, rbx
    mov esi, AST_WITHITEM
    xor edx, edx
    mov rcx, [rbp - PT2_LINE]
    mov r8, [rbp - PT2_TYPE]
    mov r9, [rbp - PT2_NAME]
    call ast_make
    mov rdi, rbx
    mov rsi, rax
    call ast_push
    mov rdi, rbx
    call par_kind
    cmp eax, TOK_COMMA
    jne .items_end
    mov rdi, rbx
    call par_advance
    ; A trailing comma before the `)` ends the list.
    cmp qword [rbp - PT2_PAREN], 0
    je .item_loop
    mov rdi, rbx
    call par_kind
    cmp eax, TOK_RPAR
    jne .item_loop

.items_end:
    cmp qword [rbp - PT2_PAREN], 0
    je .with_body
    ; The list has to close and be followed by the suite's colon; anything
    ; else means this was a parenthesised expression all along.
    mov rdi, rbx
    call par_kind
    cmp eax, TOK_RPAR
    jne .restore_plain
    mov rdi, rbx
    call par_advance
    mov rdi, rbx
    call par_kind
    cmp eax, TOK_COLON
    jne .restore_plain
    mov qword [rbp - PT2_PAREN], 0

.with_body:
    mov rdi, rbx
    call par_suite
    test rax, rax
    jz .fail
    mov [rbp - PT2_BODY], rax

    mov rdi, rbx
    mov esi, AST_WITH
    mov rdx, [rbp - PT2_LINE]
    mov rcx, [rbp - PT2_MARK]
    call par_finish_list
    test rax, rax
    jz .fail
    mov [rbp - PT2_NODE], rax
    mov rdi, rbx
    mov rsi, rax
    call ast_at
    mov rdx, [rbp - PT2_BODY]
    mov [rax + AstNode.a], edx
    mov rax, [rbp - PT2_NODE]
    pop rbx
    leave
    ret
.item_failed:
    ; Inside the parenthesised attempt a failure is not fatal -- it just means
    ; the `(` opened an expression.
    cmp qword [rbp - PT2_PAREN], 0
    jne .restore_plain
    jmp .fail
.bad_target:
    mov rdi, rbx
    CSTRING rsi, "cannot assign to that with-target"
    call par_syntax_error
.fail:
    xor eax, eax
    pop rbx
    leave
    ret
END_FUNC ps_with

;; ============================================================================
;; ps_class - `class C(bases, **kw): body`
;;   .a = the name, .b = an AST_CALL holding the bases and keywords,
;;   .clist = the body statements
;; ============================================================================
PC_LINE   equ 8
PC_NAME   equ 16
PC_BASES  equ 24
PC_MARK   equ 32
PC_NODE   equ 40
PC_PRIV   equ 48          ; Comp.private, saved across the body
PC_FRAME  equ 56          ; + 1 push = 64
DEF_FUNC_LOCAL ps_class, PC_FRAME
    push rbx
    mov rbx, rdi
    call par_peek
    TOK_POS rax
    mov [rbp - PC_LINE], rcx
    mov rdi, rbx
    call par_advance                    ; `class`

    mov rdi, rbx
    call par_name_obj
    test rax, rax
    jz .fail
    mov [rbp - PC_NAME], rax
    mov qword [rbp - PC_BASES], 0

    mov rdi, rbx
    call par_skip_type_params
    test eax, eax
    jz .fail

    ; The base list is an argument list, keywords and all -- `class C(B,
    ; metaclass=M)` -- so it is parsed by the same code that parses a call.
    mov rdi, rbx
    call par_kind
    cmp eax, TOK_LPAR
    jne .body
    mov rdi, rbx
    mov esi, 0                          ; a placeholder callee
    call in_call_public
    test rax, rax
    jz .fail
    mov [rbp - PC_BASES], rax

.body:
    mov rdi, rbx
    call ast_mark
    mov [rbp - PC_MARK], rax

    ; Private names inside the body mangle against this class.  The bases and
    ; keywords above are evaluated in the enclosing scope and do not.  A
    ; nested class replaces the name for its own body, which is why this is
    ; saved and restored rather than set once.
    mov rax, [rbx + Comp.private]
    mov [rbp - PC_PRIV], rax
    mov rdi, rbx
    mov rsi, [rbp - PC_NAME]
    call ast_obj_at
    mov [rbx + Comp.private], rax

    mov rdi, rbx
    call par_suite_into
    push rax
    mov rax, [rbp - PC_PRIV]
    mov [rbx + Comp.private], rax
    pop rax
    test eax, eax
    jz .fail

    mov rdi, rbx
    mov esi, AST_CLASSDEF
    mov rdx, [rbp - PC_LINE]
    mov rcx, [rbp - PC_MARK]
    call par_finish_list
    test rax, rax
    jz .fail
    mov [rbp - PC_NODE], rax
    mov rdi, rbx
    mov rsi, rax
    call ast_at
    mov rdx, [rbp - PC_NAME]
    mov [rax + AstNode.a], edx
    mov rdx, [rbp - PC_BASES]
    mov [rax + AstNode.b], edx
    mov rax, [rbp - PC_NODE]
    pop rbx
    leave
    ret
.fail:
    xor eax, eax
    pop rbx
    leave
    ret
END_FUNC ps_class

;; ============================================================================
;; ps_decorated - one or more `@decorator` lines, then a def or class
;;
;; The decorators are evaluated top to bottom and applied bottom to top, which
;; is why they are collected in order and the calls emitted in reverse.
;; ============================================================================
PDC_LINE  equ 8
PDC_MARK  equ 16
PDC_TARGET equ 24
PDC_NODE  equ 32
PDC_FRAME equ 40          ; + 1 push = 48
DEF_FUNC_LOCAL ps_decorated, PDC_FRAME
    push rbx
    mov rbx, rdi
    call par_peek
    TOK_POS rax
    mov [rbp - PDC_LINE], rcx

    mov rdi, rbx
    call ast_mark
    mov [rbp - PDC_MARK], rax
.loop:
    mov rdi, rbx
    call par_kind
    cmp eax, TOK_AT
    jne .target
    mov rdi, rbx
    call par_advance
    mov rdi, rbx
    mov esi, BP_NONE
    call par_expr
    test rax, rax
    jz .fail
    mov rdi, rbx
    mov rsi, rax
    call ast_push
    mov rdi, rbx
    mov esi, TOK_NEWLINE
    CSTRING rdx, "expected a newline after the decorator"
    call par_expect
    test eax, eax
    jz .fail
.skip_blank:
    mov rdi, rbx
    call par_kind
    cmp eax, TOK_NEWLINE
    jne .loop
    mov rdi, rbx
    call par_advance
    jmp .skip_blank

.target:
    mov rdi, rbx
    call par_kind
    cmp eax, TOK_DEF
    je .have_target
    cmp eax, TOK_CLASS
    je .have_target
    ; `async def` is decoratable and `async for` / `async with` are not, so the
    ; one token of lookahead decides it here rather than after the statement is
    ; parsed -- the error then points at the `async`, not at the end of a loop.
    ; @wraps over an async def is how contextlib.py and a hundred other Lib/
    ; sites are written, and this rejected every one of them.
    cmp eax, TOK_ASYNC
    jne .bad_target
    mov rdi, rbx
    call par_peek_next
    cmp eax, TOK_DEF
    je .have_target
.bad_target:
    mov rdi, rbx
    CSTRING rsi, "expected 'def', 'class' or 'async def' after a decorator"
    call par_syntax_error
    jmp .fail
.have_target:
    mov rdi, rbx
    call par_statement
    test rax, rax
    jz .fail
    mov [rbp - PDC_TARGET], rax

    ; The decorated node carries the decorator list; the emitter applies them.
    mov rdi, rbx
    mov esi, AST_DECORATED
    mov rdx, [rbp - PDC_LINE]
    mov rcx, [rbp - PDC_MARK]
    call par_finish_list
    test rax, rax
    jz .fail
    mov [rbp - PDC_NODE], rax
    mov rdi, rbx
    mov rsi, rax
    call ast_at
    mov rdx, [rbp - PDC_TARGET]
    mov [rax + AstNode.a], edx
    mov rax, [rbp - PDC_NODE]
    pop rbx
    leave
    ret
.fail:
    xor eax, eax
    pop rbx
    leave
    ret
END_FUNC ps_decorated

section .rodata

;; ============================================================================
;; stmt_table - leading token -> statement parser.  A zero entry means the
;; statement does not begin with a keyword, and falls through to the
;; expression-or-assignment parser.
;; ============================================================================
align 8
stmt_table:
    dq 0            ;  0 TOK_ENDMARKER
    dq 0            ;  1 TOK_NEWLINE
    dq 0            ;  2 TOK_INDENT
    dq 0            ;  3 TOK_DEDENT
    dq 0            ;  4 TOK_NAME
    dq 0            ;  5 TOK_NUMBER
    dq 0            ;  6 TOK_STRING
    dq 0            ;  7 TOK_FSTRING
    dq 0            ;  8 TOK_LPAR
    dq 0            ;  9 TOK_RPAR
    dq 0            ; 10 TOK_LSQB
    dq 0            ; 11 TOK_RSQB
    dq 0            ; 12 TOK_LBRACE
    dq 0            ; 13 TOK_RBRACE
    dq 0            ; 14 TOK_COLON
    dq 0            ; 15 TOK_COMMA
    dq 0            ; 16 TOK_SEMI
    dq 0            ; 17 TOK_DOT
    dq 0            ; 18 TOK_ELLIPSIS
    dq 0            ; 19 TOK_PLUS
    dq 0            ; 20 TOK_MINUS
    dq 0            ; 21 TOK_STAR
    dq 0            ; 22 TOK_DOUBLESTAR
    dq 0            ; 23 TOK_SLASH
    dq 0            ; 24 TOK_DOUBLESLASH
    dq 0            ; 25 TOK_PERCENT
    dq ps_decorated            ; 26 TOK_AT
    dq 0            ; 27 TOK_VBAR
    dq 0            ; 28 TOK_AMPER
    dq 0            ; 29 TOK_CIRCUMFLEX
    dq 0            ; 30 TOK_TILDE
    dq 0            ; 31 TOK_LEFTSHIFT
    dq 0            ; 32 TOK_RIGHTSHIFT
    dq 0            ; 33 TOK_LESS
    dq 0            ; 34 TOK_GREATER
    dq 0            ; 35 TOK_LESSEQUAL
    dq 0            ; 36 TOK_GREATEREQUAL
    dq 0            ; 37 TOK_EQEQUAL
    dq 0            ; 38 TOK_NOTEQUAL
    dq 0            ; 39 TOK_EQUAL
    dq 0            ; 40 TOK_COLONEQUAL
    dq 0            ; 41 TOK_RARROW
    dq 0            ; 42 TOK_PLUSEQUAL
    dq 0            ; 43 TOK_MINEQUAL
    dq 0            ; 44 TOK_STAREQUAL
    dq 0            ; 45 TOK_DOUBLESTAREQUAL
    dq 0            ; 46 TOK_SLASHEQUAL
    dq 0            ; 47 TOK_DOUBLESLASHEQUAL
    dq 0            ; 48 TOK_PERCENTEQUAL
    dq 0            ; 49 TOK_ATEQUAL
    dq 0            ; 50 TOK_VBAREQUAL
    dq 0            ; 51 TOK_AMPEREQUAL
    dq 0            ; 52 TOK_CIRCUMFLEXEQUAL
    dq 0            ; 53 TOK_LEFTSHIFTEQUAL
    dq 0            ; 54 TOK_RIGHTSHIFTEQUAL
    dq 0            ; 55 TOK_FALSE
    dq 0            ; 56 TOK_NONE
    dq 0            ; 57 TOK_TRUE
    dq 0            ; 58 TOK_AND
    dq 0            ; 59 TOK_AS
    dq ps_assert    ; 60 TOK_ASSERT
    dq ps_async                ; 61 TOK_ASYNC
    dq 0            ; 62 TOK_AWAIT
    dq ps_simple    ; 63 TOK_BREAK
    dq ps_class                ; 64 TOK_CLASS
    dq ps_simple    ; 65 TOK_CONTINUE
    dq ps_def                  ; 66 TOK_DEF
    dq ps_del       ; 67 TOK_DEL
    dq 0            ; 68 TOK_ELIF
    dq 0            ; 69 TOK_ELSE
    dq 0            ; 70 TOK_EXCEPT
    dq 0            ; 71 TOK_FINALLY
    dq ps_for                  ; 72 TOK_FOR
    dq ps_from      ; 73 TOK_FROM
    dq ps_scope     ; 74 TOK_GLOBAL
    dq ps_if                   ; 75 TOK_IF
    dq ps_import    ; 76 TOK_IMPORT
    dq 0            ; 77 TOK_IN
    dq 0            ; 78 TOK_IS
    dq 0            ; 79 TOK_LAMBDA
    dq ps_scope     ; 80 TOK_NONLOCAL
    dq 0            ; 81 TOK_NOT
    dq 0            ; 82 TOK_OR
    dq ps_simple    ; 83 TOK_PASS
    dq ps_raise     ; 84 TOK_RAISE
    dq ps_return               ; 85 TOK_RETURN
    dq ps_try                  ; 86 TOK_TRY
    dq ps_while                ; 87 TOK_WHILE
    dq ps_with                 ; 88 TOK_WITH
    dq 0            ; 89 TOK_YIELD

section .rodata

;; Augmented-assignment operators, paired with their BINARY_OP in-place codes.
align 8
augop_table:
    dd TOK_PLUSEQUAL,        NB_INPLACE_ADD
    dd TOK_MINEQUAL,         NB_INPLACE_SUBTRACT
    dd TOK_STAREQUAL,        NB_INPLACE_MULTIPLY
    dd TOK_SLASHEQUAL,       NB_INPLACE_TRUE_DIVIDE
    dd TOK_DOUBLESLASHEQUAL, NB_INPLACE_FLOOR_DIVIDE
    dd TOK_PERCENTEQUAL,     NB_INPLACE_REMAINDER
    dd TOK_DOUBLESTAREQUAL,  NB_INPLACE_POWER
    dd TOK_LEFTSHIFTEQUAL,   NB_INPLACE_LSHIFT
    dd TOK_RIGHTSHIFTEQUAL,  NB_INPLACE_RSHIFT
    dd TOK_AMPEREQUAL,       NB_INPLACE_AND
    dd TOK_VBAREQUAL,        NB_INPLACE_OR
    dd TOK_CIRCUMFLEXEQUAL,  NB_INPLACE_XOR
    dd TOK_ATEQUAL,          NB_INPLACE_MATRIX_MULTIPLY
    dd -1, 0

ASM_INIT

section .rodata
ps_type_kw: db "type", 0
