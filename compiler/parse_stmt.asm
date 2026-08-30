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
extern ast_mark
extern ast_obj
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
extern par_syntax_error

extern exc_SyntaxError_type

BP_NONE equ 0

; --- Named frame-layout constants ---
PM_COMP  equ 8
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
    mov ecx, [rax + Token.lineno]
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
    mov rdi, rbx
    call par_simple_stmts
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
PE2_COMP  equ 8
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
    mov ecx, [rax + Token.lineno]
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
PX2_COMP  equ 8
PX2_FIRST equ 16
PX2_LINE  equ 24
PX2_MARK  equ 32
PX2_FRAME equ 40         ; + 1 push = 48
DEF_FUNC par_exprlist_stmt, PX2_FRAME
    push rbx
    mov rbx, rdi
    call par_peek
    mov ecx, [rax + Token.lineno]
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

PK_COMP  equ 8
PK_LINE  equ 16
PK_A     equ 24
PK_B     equ 32
PK_MARK  equ 40
PK_NODE  equ 48
PK_FRAME equ 56          ; + 1 push = 64
PK2_FRAME equ 64         ; + 2 pushes = 80, for the handlers that save r12

;; ps_simple - pass, break and continue: a keyword and nothing else.
DEF_FUNC_LOCAL ps_simple, PK_FRAME
    push rbx
    mov rbx, rdi
    call par_peek
    mov ecx, [rax + Token.lineno]
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
    mov ecx, [rax + Token.lineno]
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
    mov ecx, [rax + Token.lineno]
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
    mov rdi, [rax + Token.start]
    mov esi, [rax + Token.len]
    call comp_intern
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
    mov ecx, [rax + Token.lineno]
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
    mov ecx, [rax + Token.lineno]
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
;; par_dotted_name(Comp *c) -> rax = obj index of the joined name, 0 on error
;; `a.b.c` becomes the single string "a.b.c", which is what IMPORT_NAME wants.
;; ============================================================================
PDN_SELF  equ 8
PDN_BUF   equ 48         ; a Buf at [rbp - 48]
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
    mov rdi, [rax + Token.start]
    mov esi, [rax + Token.len]
    call comp_intern
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
    mov ecx, [rax + Token.lineno]
    mov [rbp - PK_LINE], rcx
    mov rdi, rbx
    call par_advance
    mov rdi, rbx
    call ast_mark
    mov [rbp - PK_MARK], rax
.loop:
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
    mov rcx, [rbp - PK_LINE]
    mov r8, r12
    mov r9, [rbp - PK_B]
    call ast_make
    mov rdi, rbx
    mov rsi, rax
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
    mov ecx, [rax + Token.lineno]
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
    mov rcx, [rbp - PK_LINE]
    mov r8, [rbp - PK_A]
    mov r9, [rbp - PK_B]
    call ast_make
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
    mov rdi, rbx
    call par_advance
    ; A star import is an alias with no name at all; codegen recognises it by
    ; the empty child list plus the star flag.
    mov rdi, rbx
    mov esi, AST_ALIAS
    mov edx, 1                          ; subkind 1 marks the star form
    mov rcx, [rbp - PK_LINE]
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

section .rodata

;; ---------------------------------------------------------------------------
;; stmt_table - leading token -> statement parser.  A zero entry means the
;; statement does not begin with a keyword, and falls through to the
;; expression-or-assignment parser.
;; ---------------------------------------------------------------------------
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
    dq 0            ; 26 TOK_AT
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
    dq 0            ; 61 TOK_ASYNC
    dq 0            ; 62 TOK_AWAIT
    dq ps_simple    ; 63 TOK_BREAK
    dq 0            ; 64 TOK_CLASS
    dq ps_simple    ; 65 TOK_CONTINUE
    dq 0            ; 66 TOK_DEF
    dq ps_del       ; 67 TOK_DEL
    dq 0            ; 68 TOK_ELIF
    dq 0            ; 69 TOK_ELSE
    dq 0            ; 70 TOK_EXCEPT
    dq 0            ; 71 TOK_FINALLY
    dq 0            ; 72 TOK_FOR
    dq ps_from      ; 73 TOK_FROM
    dq ps_scope     ; 74 TOK_GLOBAL
    dq 0            ; 75 TOK_IF
    dq ps_import    ; 76 TOK_IMPORT
    dq 0            ; 77 TOK_IN
    dq 0            ; 78 TOK_IS
    dq 0            ; 79 TOK_LAMBDA
    dq ps_scope     ; 80 TOK_NONLOCAL
    dq 0            ; 81 TOK_NOT
    dq 0            ; 82 TOK_OR
    dq ps_simple    ; 83 TOK_PASS
    dq ps_raise     ; 84 TOK_RAISE
    dq 0            ; 85 TOK_RETURN
    dq 0            ; 86 TOK_TRY
    dq 0            ; 87 TOK_WHILE
    dq 0            ; 88 TOK_WITH
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
