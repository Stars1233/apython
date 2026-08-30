; parse.asm - Recursive-descent parser with a table-driven expression core
;
; Expressions use precedence climbing over one table indexed by token kind:
; each row gives a prefix handler, an infix handler, and the binding powers.
; Python's awkward corners then fall out of the numbers rather than needing
; special cases:
;
;   **   lbp 28, but its handler recurses at BP_UNARY (26).  That single
;        asymmetry gives right-associativity (2**3**2 == 512), lets the right
;        side take a unary operator (2**-1), and makes -2**2 == -4, because
;        unary minus parses its operand at 26 and ** at 28 binds tighter.
;   not  prefix only, parsing its operand at BP_NOT (10), which is below
;        BP_COMPARE -- so `not a == b` is `not (a == b)`.
;   ,    deliberately NOT in the table.  A comma is handled by the callers
;        that actually permit a tuple; putting it in the table is the classic
;        way to break call arguments, subscripts and target lists.
;
; Contexts that must exclude a ternary or a walrus just pass a higher minimum
; binding power.  That is what the parameter is for, and it means the
; comprehension `for y in z if c` needs no lookahead to leave the `if` alone.
;
; Nothing here raises: an error is recorded through comp_error and reported by
; returning node 0.  After the first error the token cursor reports ENDMARKER
; forever, so every parse loop terminates on its own and most call sites need
; no explicit check.

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
extern comp_error

extern ap_free
extern ap_malloc
extern ap_memcpy
extern comp_intern
extern int_from_cstr_base
extern strtod

extern bool_false
extern bool_true
extern none_singleton

extern exc_SyntaxError_type

;; ---------------------------------------------------------------------------
;; Binding powers, in steps of two so that a right-associative operator is
;; simply "recurse at lbp - 1" and there is still room between levels.
;; ---------------------------------------------------------------------------
BP_NONE     equ 0
BP_LAMBDA   equ 2
BP_WALRUS   equ 3
BP_TERNARY  equ 4
BP_OR       equ 6
BP_AND      equ 8
BP_NOT      equ 10
BP_COMPARE  equ 12
BP_BITOR    equ 14
BP_BITXOR   equ 16
BP_BITAND   equ 18
BP_SHIFT    equ 20
BP_ARITH    equ 22
BP_TERM     equ 24
BP_UNARY    equ 26
BP_POWER    equ 28
BP_AWAIT    equ 30
BP_POSTFIX  equ 32

; PRule.flags
PR_CHAIN    equ 0x01     ; a comparison operator: folds into one n-ary node

struc PRule
    .prefix: resq 1      ; fn(Comp*) -> node index, or 0 if not a prefix
    .infix:  resq 1      ; fn(Comp*, node left) -> node index, or 0
    .lbp:    resb 1      ; left binding power; 0 means "not an infix operator"
    .rbp:    resb 1      ; the power its handler recurses at
    .aux:    resb 1      ; NB_* / CMP_* / UOP_* payload
    .flags:  resb 1      ; PR_*
    .pad:    resd 1
endstruc                 ; 24

; --- Named frame-layout constants ---
PE_COMP  equ 8
PE_MINBP equ 16
PE_LEFT  equ 24
PE_RULE  equ 32
PE_FRAME equ 40          ; + 1 push = 48

section .text

;; ============================================================================
;; par_peek(Comp *c) -> rax = Token*
;; Once an error has been recorded the cursor reports the final ENDMARKER
;; forever.  That is what lets the parser run to a stop after a syntax error
;; without every loop in the file testing for it.
;; ============================================================================
DEF_FUNC_BARE par_peek
    mov rax, [rdi + Comp.tokens + Buf.data]
    mov ecx, [rdi + Comp.tok_idx]
    cmp dword [rdi + Comp.err + CompErr.set], 0
    jne .at_end
    shl rcx, TOKEN_SHIFT
    add rax, rcx
    ret
.at_end:
    mov rcx, [rdi + Comp.tokens + Buf.len]
    dec rcx
    shl rcx, TOKEN_SHIFT
    add rax, rcx
    ret
END_FUNC par_peek

;; ============================================================================
;; par_kind(Comp *c) -> eax = the current token's kind
;; ============================================================================
DEF_FUNC par_kind
    call par_peek
    movzx eax, word [rax + Token.kind]
    leave
    ret
END_FUNC par_kind

;; ============================================================================
;; par_advance(Comp *c)
;; ============================================================================
DEF_FUNC_BARE par_advance
    mov eax, [rdi + Comp.tok_idx]
    mov rcx, [rdi + Comp.tokens + Buf.len]
    dec rcx
    cmp rax, rcx
    jae .at_end                          ; never step past the ENDMARKER
    inc eax
    mov [rdi + Comp.tok_idx], eax
.at_end:
    ret
END_FUNC par_advance

;; ============================================================================
;; par_syntax_error(Comp *c, const char *msg) -> rax = 0
;; Stamps the message with the current token's position.
;; ============================================================================
PS_MSG   equ 8
PS_FRAME equ 8           ; + 1 push = 16
DEF_FUNC par_syntax_error, PS_FRAME
    push rbx
    mov rbx, rdi
    mov [rbp - PS_MSG], rsi
    call par_peek
    mov ecx, [rax + Token.lineno]
    mov r8d, [rax + Token.col]
    mov rdi, rbx
    lea rsi, [rel exc_SyntaxError_type]
    mov rdx, [rbp - PS_MSG]
    call comp_error
    xor eax, eax
    pop rbx
    leave
    ret
END_FUNC par_syntax_error

;; ============================================================================
;; par_expect(Comp *c, int kind, const char *msg) -> rax = 1 ok, 0 error
;; ============================================================================
PX_KIND  equ 8
PX_MSG   equ 16
PX_FRAME equ 24          ; + 1 push = 32
DEF_FUNC par_expect, PX_FRAME
    push rbx
    mov rbx, rdi
    mov [rbp - PX_KIND], rsi
    mov [rbp - PX_MSG], rdx
    call par_kind
    cmp rax, [rbp - PX_KIND]
    jne .bad
    mov rdi, rbx
    call par_advance
    mov eax, 1
    pop rbx
    leave
    ret
.bad:
    mov rdi, rbx
    mov rsi, [rbp - PX_MSG]
    call par_syntax_error
    xor eax, eax
    pop rbx
    leave
    ret
END_FUNC par_expect

;; ============================================================================
;; par_expr(Comp *c, int min_bp) -> rax = node index, 0 on error
;;
;; The precedence-climbing driver.  Everything about Python's expression
;; precedence lives in prule_table, not here.
;; ============================================================================
DEF_FUNC par_expr, PE_FRAME
    push rbx
    mov rbx, rdi
    mov [rbp - PE_MINBP], rsi

    ; Guard the machine stack.  apython's recursion_depth counts Python frames
    ; only, so without this a pathological "((((..." walks off the stack long
    ; before anything notices.
    inc dword [rbx + Comp.depth]
    cmp dword [rbx + Comp.depth], COMP_MAX_DEPTH
    jae .too_deep

    mov rdi, rbx
    call par_kind
    lea rcx, [rel prule_table]
    imul rax, rax, PRule_size
    add rcx, rax
    mov rax, [rcx + PRule.prefix]
    test rax, rax
    jz .no_prefix

    mov rdi, rbx
    call rax                            ; the prefix handler advances the cursor
    mov [rbp - PE_LEFT], rax
    test rax, rax
    jz .fail

.loop:
    mov rdi, rbx
    call par_kind
    lea rcx, [rel prule_table]
    imul rax, rax, PRule_size
    add rcx, rax
    mov [rbp - PE_RULE], rcx
    movzx eax, byte [rcx + PRule.lbp]
    cmp rax, [rbp - PE_MINBP]
    jbe .done
    mov rax, [rcx + PRule.infix]
    test rax, rax
    jz .done

    mov rdi, rbx
    mov rsi, [rbp - PE_LEFT]
    call rax
    mov [rbp - PE_LEFT], rax
    test rax, rax
    jz .fail
    jmp .loop

.done:
    dec dword [rbx + Comp.depth]
    mov rax, [rbp - PE_LEFT]
    pop rbx
    leave
    ret

.no_prefix:
    mov rdi, rbx
    CSTRING rsi, "invalid syntax"
    call par_syntax_error
    jmp .fail

.too_deep:
    mov rdi, rbx
    CSTRING rsi, "too many nested parentheses"
    call par_syntax_error
.fail:
    dec dword [rbx + Comp.depth]
    xor eax, eax
    pop rbx
    leave
    ret
END_FUNC par_expr

;; ============================================================================
;; par_number(Comp *c, Token *t) -> rax = Value, or 0 with an error recorded
;;
;; The literal's text is a slice of the source, so it has to be copied out and
;; NUL-terminated before the converters will look at it.  Both converters take
;; the whole job from there: int_from_cstr_base with base 0 handles 0x/0o/0b
;; and underscores, and strtod handles every float spelling.
;; ============================================================================
PN_COMP  equ 8
PN_TOK   equ 16
PN_BUF   equ 24
PN_HEAP  equ 32
PN_END   equ 40
PN_SMALL equ 176         ; 128 bytes of inline scratch at [rbp - PN_SMALL]
PN_FRAME equ 184         ; + 1 push = 192
DEF_FUNC par_number, PN_FRAME
    push rbx
    mov rbx, rdi
    mov [rbp - PN_TOK], rsi

    mov ecx, [rsi + Token.len]
    mov qword [rbp - PN_HEAP], 0
    cmp rcx, 127
    jbe .use_stack
    ; A literal longer than the inline buffer is rare but perfectly legal --
    ; a thousand-digit integer, say -- so fall back to the heap rather than
    ; truncating or refusing.
    lea rdi, [rcx + 1]
    call ap_malloc
    mov [rbp - PN_HEAP], rax
    mov [rbp - PN_BUF], rax
    jmp .have_buf
.use_stack:
    lea rax, [rbp - PN_SMALL]
    mov [rbp - PN_BUF], rax
.have_buf:
    mov rdi, [rbp - PN_BUF]
    mov rsi, [rbp - PN_TOK]
    mov edx, [rsi + Token.len]          ; a dword field: a 64-bit read would pick up Token.col
    mov rsi, [rsi + Token.start]
    call ap_memcpy
    mov rax, [rbp - PN_TOK]
    mov ecx, [rax + Token.len]
    mov rdx, [rbp - PN_BUF]
    mov byte [rdx + rcx], 0

    mov rax, [rbp - PN_TOK]
    movzx eax, word [rax + Token.flags]
    test eax, TF_NUM_IMAG
    jnz .imaginary
    test eax, TF_NUM_FLOAT
    jnz .float

    mov rdi, [rbp - PN_BUF]
    xor esi, esi                        ; base 0: detect the prefix
    call int_from_cstr_base
    test edx, edx                       ; TAG_NULL means the parse failed
    jz .bad
    V_PACK rax, rdx
    jmp .done

.float:
    mov rdi, [rbp - PN_BUF]
    lea rsi, [rbp - PN_END]
    call strtod wrt ..plt
    mov rax, [rbp - PN_END]
    cmp byte [rax], 0                   ; strtod must have consumed all of it
    jne .bad
    movq rax, xmm0
    V_FROM_F64 rax, rcx
    jmp .done

.imaginary:
    mov rdi, rbx
    CSTRING rsi, "complex literals are not supported"
    call par_syntax_error
    xor eax, eax
    jmp .cleanup

.bad:
    mov rdi, rbx
    CSTRING rsi, "invalid numeric literal"
    call par_syntax_error
    xor eax, eax
    jmp .cleanup

.done:
.cleanup:
    mov rdi, [rbp - PN_HEAP]
    test rdi, rdi
    jz .no_heap
    mov [rbp - PN_BUF], rax             ; park the result across the free
    call ap_free
    mov rax, [rbp - PN_BUF]
.no_heap:
    pop rbx
    leave
    ret
END_FUNC par_number

;; ============================================================================
;; Prefix handlers.  Each consumes its own token and returns a node index.
;; ============================================================================

;; pf_number(Comp *c) -> node
PFN_COMP  equ 8
PFN_LINE  equ 16
PFN_FRAME equ 24         ; + 1 push = 32
DEF_FUNC_LOCAL pf_number, PFN_FRAME
    push rbx
    mov rbx, rdi
    call par_peek
    mov ecx, [rax + Token.lineno]
    mov [rbp - PFN_LINE], rcx
    mov rdi, rbx
    mov rsi, rax
    call par_number
    test rax, rax
    jz .fail
    mov rdi, rbx
    mov rsi, rax
    call ast_obj                        ; takes ownership of the Value
    mov r8, rax
    mov rdi, rbx
    call par_advance
    mov rdi, rbx
    mov esi, AST_CONST
    xor edx, edx
    mov rcx, [rbp - PFN_LINE]
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
END_FUNC pf_number

;; pf_const(Comp *c) -> node   -- True, False, None share one handler
DEF_FUNC_LOCAL pf_const, PFN_FRAME
    push rbx
    mov rbx, rdi
    call par_peek
    mov ecx, [rax + Token.lineno]
    mov [rbp - PFN_LINE], rcx
    movzx edx, word [rax + Token.kind]

    lea rsi, [rel none_singleton]
    cmp edx, TOK_TRUE
    jne .not_true
    lea rsi, [rel bool_true]
    jmp .have
.not_true:
    cmp edx, TOK_FALSE
    jne .have
    lea rsi, [rel bool_false]
.have:
    ; These are ordinary heap singletons, and a pointer is its own Value.  They
    ; are immortal, but comp_free will DECREF the table entry, so take a
    ; reference to keep the accounting uniform.
    inc qword [rsi + PyObject.ob_refcnt]
    mov rdi, rbx
    call ast_obj
    mov r8, rax
    mov rdi, rbx
    call par_advance
    mov rdi, rbx
    mov esi, AST_CONST
    xor edx, edx
    mov rcx, [rbp - PFN_LINE]
    xor r9d, r9d
    call ast_make
    pop rbx
    leave
    ret
END_FUNC pf_const

;; pf_name(Comp *c) -> node
PFM_COMP  equ 8
PFM_LINE  equ 16
PFM_TOK   equ 24
PFM_FRAME equ 24         ; + 1 push = 32
DEF_FUNC_LOCAL pf_name, PFM_FRAME
    push rbx
    mov rbx, rdi
    call par_peek
    mov [rbp - PFM_TOK], rax
    mov ecx, [rax + Token.lineno]
    mov [rbp - PFM_LINE], rcx

    mov rdi, [rax + Token.start]
    mov esi, [rax + Token.len]
    call comp_intern                    ; -> an owned PyStrObject*
    test rax, rax
    jz .fail
    mov rdi, rbx
    mov rsi, rax
    call ast_obj
    mov r8, rax
    mov rdi, rbx
    call par_advance
    mov rdi, rbx
    mov esi, AST_NAME
    mov edx, CTX_LOAD
    mov rcx, [rbp - PFM_LINE]
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
END_FUNC pf_name

;; pf_group(Comp *c) -> node   -- a parenthesized expression
DEF_FUNC_LOCAL pf_group, PFN_FRAME
    push rbx
    mov rbx, rdi
    call par_advance                    ; consume '('
    mov rdi, rbx
    mov esi, BP_NONE
    call par_expr
    test rax, rax
    jz .fail
    mov [rbp - PFN_LINE], rax
    mov rdi, rbx
    mov esi, TOK_RPAR
    CSTRING rdx, "'(' was never closed"
    call par_expect
    test eax, eax
    jz .fail
    mov rax, [rbp - PFN_LINE]
    pop rbx
    leave
    ret
.fail:
    xor eax, eax
    pop rbx
    leave
    ret
END_FUNC pf_group

;; pf_unary(Comp *c) -> node   -- +x, -x, ~x, not x
PFU_COMP  equ 8
PFU_LINE  equ 16
PFU_OP    equ 24
PFU_RBP   equ 32
PFU_FRAME equ 40         ; + 1 push = 48
DEF_FUNC_LOCAL pf_unary, PFU_FRAME
    push rbx
    mov rbx, rdi
    call par_peek
    mov ecx, [rax + Token.lineno]
    mov [rbp - PFU_LINE], rcx
    movzx eax, word [rax + Token.kind]
    lea rcx, [rel prule_table]
    imul rdx, rax, PRule_size
    movzx edx, byte [rcx + rdx + PRule.rbp]
    mov [rbp - PFU_RBP], rdx

    ; The unary operator comes from the token, NOT from PRule.aux: `+` and `-`
    ; share one row with their binary selves, and that row's aux holds the
    ; BINARY_OP code.  Reading it here turned -5 into `not 5`.
    mov edx, UOP_NEG
    cmp eax, TOK_MINUS
    je .have_op
    mov edx, UOP_POS
    cmp eax, TOK_PLUS
    je .have_op
    mov edx, UOP_INVERT
    cmp eax, TOK_TILDE
    je .have_op
    mov edx, UOP_NOT
.have_op:
    mov [rbp - PFU_OP], rdx

    mov rdi, rbx
    call par_advance
    mov rdi, rbx
    mov rsi, [rbp - PFU_RBP]
    call par_expr
    test rax, rax
    jz .fail
    mov r8, rax
    mov rdi, rbx
    mov esi, AST_UNARYOP
    mov rdx, [rbp - PFU_OP]
    mov rcx, [rbp - PFU_LINE]
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
END_FUNC pf_unary

;; ============================================================================
;; Infix handlers.  Each is entered with the operator token still current.
;; ============================================================================

;; in_binop(Comp *c, node left) -> node
IB_COMP  equ 8
IB_LEFT  equ 16
IB_LINE  equ 24
IB_OP    equ 32
IB_RBP   equ 40
IB_FRAME equ 40          ; + 1 push = 48
DEF_FUNC_LOCAL in_binop, IB_FRAME
    push rbx
    mov rbx, rdi
    mov [rbp - IB_LEFT], rsi
    call par_peek
    mov ecx, [rax + Token.lineno]
    mov [rbp - IB_LINE], rcx
    movzx eax, word [rax + Token.kind]
    lea rcx, [rel prule_table]
    imul rax, rax, PRule_size
    movzx edx, byte [rcx + rax + PRule.aux]
    mov [rbp - IB_OP], rdx
    movzx edx, byte [rcx + rax + PRule.rbp]
    mov [rbp - IB_RBP], rdx

    mov rdi, rbx
    call par_advance
    mov rdi, rbx
    mov rsi, [rbp - IB_RBP]
    call par_expr
    test rax, rax
    jz .fail
    mov r9, rax
    mov rdi, rbx
    mov esi, AST_BINOP
    mov rdx, [rbp - IB_OP]
    mov rcx, [rbp - IB_LINE]
    mov r8, [rbp - IB_LEFT]
    call ast_make
    pop rbx
    leave
    ret
.fail:
    xor eax, eax
    pop rbx
    leave
    ret
END_FUNC in_binop

;; in_boolop(Comp *c, node left) -> node   -- and / or, folded n-ary
;;
;; `a and b and c` becomes one node with three operands rather than a nest of
;; two, so codegen emits a single exit label and one jump per operand.
IO_COMP  equ 8
IO_LEFT  equ 16
IO_LINE  equ 24
IO_OP    equ 32
IO_MARK  equ 40
IO_NODE  equ 48
IO_FRAME equ 56          ; + 1 push = 64
DEF_FUNC_LOCAL in_boolop, IO_FRAME
    push rbx
    mov rbx, rdi
    mov [rbp - IO_LEFT], rsi
    call par_peek
    mov ecx, [rax + Token.lineno]
    mov [rbp - IO_LINE], rcx
    movzx eax, word [rax + Token.kind]
    mov edx, BOOL_AND
    cmp eax, TOK_OR
    jne .have_op
    mov edx, BOOL_OR
.have_op:
    mov [rbp - IO_OP], rdx

    mov rdi, rbx
    call ast_mark
    mov [rbp - IO_MARK], rax
    mov rdi, rbx
    mov rsi, [rbp - IO_LEFT]
    call ast_push

.more:
    mov rdi, rbx
    call par_advance                    ; consume `and` / `or`
    mov rdi, rbx
    mov rsi, BP_AND
    cmp qword [rbp - IO_OP], BOOL_OR
    jne .have_bp
    mov rsi, BP_OR
.have_bp:
    call par_expr
    test rax, rax
    jz .fail
    mov rdi, rbx
    mov rsi, rax
    call ast_push

    ; Absorb a run of the same operator into this one node.
    mov rdi, rbx
    call par_kind
    cmp qword [rbp - IO_OP], BOOL_OR
    je .check_or
    cmp eax, TOK_AND
    je .more
    jmp .finish
.check_or:
    cmp eax, TOK_OR
    je .more

.finish:
    mov rdi, rbx
    mov esi, AST_BOOLOP
    mov rdx, [rbp - IO_OP]
    mov rcx, [rbp - IO_LINE]
    xor r8d, r8d
    xor r9d, r9d
    call ast_make
    mov [rbp - IO_NODE], rax

    mov rdi, rbx
    mov rsi, [rbp - IO_MARK]
    call ast_commit                     ; rax = offset, rdx = count
    mov r8, rax
    mov r9, rdx
    mov rdi, rbx
    mov rsi, [rbp - IO_NODE]
    call ast_at
    mov [rax + AstNode.clist], r8d
    mov [rax + AstNode.nchild], r9d
    mov rax, [rbp - IO_NODE]
    pop rbx
    leave
    ret
.fail:
    xor eax, eax
    pop rbx
    leave
    ret
END_FUNC in_boolop

;; in_ternary(Comp *c, node body) -> node   -- body if test else orelse
;;
;; The condition is parsed at BP_OR so it cannot itself be a ternary, and the
;; alternative at BP_TERNARY so that a if b else c if d else e nests to the
;; right.  Both facts are just the minimum binding power, no lookahead.
IT_COMP  equ 8
IT_BODY  equ 16
IT_LINE  equ 24
IT_TEST  equ 32
IT_ELSE  equ 40
IT_NODE  equ 48
IT_FRAME equ 56          ; + 1 push = 64
DEF_FUNC_LOCAL in_ternary, IT_FRAME
    push rbx
    mov rbx, rdi
    mov [rbp - IT_BODY], rsi
    call par_peek
    mov ecx, [rax + Token.lineno]
    mov [rbp - IT_LINE], rcx

    mov rdi, rbx
    call par_advance                    ; consume `if`
    mov rdi, rbx
    mov esi, BP_OR
    call par_expr
    test rax, rax
    jz .fail
    mov [rbp - IT_TEST], rax

    mov rdi, rbx
    mov esi, TOK_ELSE
    CSTRING rdx, "expected 'else' after conditional expression"
    call par_expect
    test eax, eax
    jz .fail

    mov rdi, rbx
    mov esi, BP_TERNARY
    call par_expr
    test rax, rax
    jz .fail
    mov [rbp - IT_ELSE], rax

    mov rdi, rbx
    mov esi, AST_IFEXP
    xor edx, edx
    mov rcx, [rbp - IT_LINE]
    mov r8, [rbp - IT_TEST]
    mov r9, [rbp - IT_BODY]
    call ast_make
    mov [rbp - IT_NODE], rax
    mov rdi, rbx
    mov rsi, rax
    call ast_at
    mov rdx, [rbp - IT_ELSE]
    mov [rax + AstNode.c], edx
    mov rax, [rbp - IT_NODE]
    pop rbx
    leave
    ret
.fail:
    xor eax, eax
    pop rbx
    leave
    ret
END_FUNC in_ternary

;; in_compare(Comp *c, node left) -> node
;;
;; A comparison chain is one node, not a nest: `a < b < c` evaluates b once and
;; short-circuits, which a left-associative fold cannot express.  The child
;; list holds (operator, operand) pairs, so .nchild counts pairs.
;;
;; `not in` and `is not` are two tokens each and are recognised here rather
;; than in the lexer, because `not` and `is` are ordinary operators elsewhere.
IC_COMP  equ 8
IC_LEFT  equ 16
IC_LINE  equ 24
IC_MARK  equ 32
IC_NODE  equ 40
IC_FRAME equ 48          ; + 2 pushes = 64
DEF_FUNC_LOCAL in_compare, IC_FRAME
    push rbx
    push r12
    mov rbx, rdi
    mov [rbp - IC_LEFT], rsi
    call par_peek
    mov ecx, [rax + Token.lineno]
    mov [rbp - IC_LINE], rcx

    mov rdi, rbx
    call ast_mark
    mov [rbp - IC_MARK], rax

.chain:
    mov rdi, rbx
    call par_cmpop                      ; -> eax = CMPOP_*, or -1
    cmp eax, -1
    je .finish
    mov r12d, eax                       ; r12 survives the recursive parse

    mov rdi, rbx
    mov esi, BP_COMPARE
    call par_expr
    test rax, rax
    jz .fail

    mov rdi, rbx
    mov rsi, r12
    push rax
    call ast_push                       ; the operator
    pop rsi
    mov rdi, rbx
    call ast_push                       ; then its right operand
    jmp .chain

.finish:
    mov rdi, rbx
    mov esi, AST_COMPARE
    xor edx, edx
    mov rcx, [rbp - IC_LINE]
    mov r8, [rbp - IC_LEFT]
    xor r9d, r9d
    call ast_make
    mov [rbp - IC_NODE], rax

    mov rdi, rbx
    mov rsi, [rbp - IC_MARK]
    call ast_commit
    mov r8, rax
    shr rdx, 1                          ; two u32 per comparator
    mov r9, rdx
    mov rdi, rbx
    mov rsi, [rbp - IC_NODE]
    call ast_at
    mov [rax + AstNode.clist], r8d
    mov [rax + AstNode.nchild], r9d
    mov rax, [rbp - IC_NODE]
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
END_FUNC in_compare

;; ============================================================================
;; par_cmpop(Comp *c) -> eax = CMPOP_*, or -1 when the next token is not one
;; Consumes the operator, including the second word of `not in` and `is not`.
;; ============================================================================
DEF_FUNC par_cmpop, 8           ; + 1 push = 16
    push rbx
    mov rbx, rdi
    call par_kind
    mov ecx, eax

    cmp ecx, TOK_LESS
    je .lt
    cmp ecx, TOK_GREATER
    je .gt
    cmp ecx, TOK_LESSEQUAL
    je .le
    cmp ecx, TOK_GREATEREQUAL
    je .ge
    cmp ecx, TOK_EQEQUAL
    je .eq
    cmp ecx, TOK_NOTEQUAL
    je .ne
    cmp ecx, TOK_IN
    je .in
    cmp ecx, TOK_IS
    je .is
    cmp ecx, TOK_NOT
    je .notin
    mov eax, -1
    pop rbx
    leave
    ret

.lt: mov r8d, CMPOP_LT
     jmp .one
.gt: mov r8d, CMPOP_GT
     jmp .one
.le: mov r8d, CMPOP_LE
     jmp .one
.ge: mov r8d, CMPOP_GE
     jmp .one
.eq: mov r8d, CMPOP_EQ
     jmp .one
.ne: mov r8d, CMPOP_NE
     jmp .one
.in: mov r8d, CMPOP_IN
.one:
    push r8
    mov rdi, rbx
    call par_advance
    pop rax
    pop rbx
    leave
    ret

.is:
    mov rdi, rbx
    call par_advance
    mov rdi, rbx
    call par_kind
    cmp eax, TOK_NOT
    jne .is_plain
    mov rdi, rbx
    call par_advance
    mov eax, CMPOP_ISNOT
    pop rbx
    leave
    ret
.is_plain:
    mov eax, CMPOP_IS
    pop rbx
    leave
    ret

.notin:
    ; A bare `not` here can only be the start of `not in`; anything else ends
    ; the chain, and the caller will fail on it in its own right.
    mov rdi, rbx
    call par_advance
    mov rdi, rbx
    mov esi, TOK_IN
    CSTRING rdx, "expected 'in' after 'not'"
    call par_expect
    test eax, eax
    jz .bad
    mov eax, CMPOP_NOTIN
    pop rbx
    leave
    ret
.bad:
    mov eax, -1
    pop rbx
    leave
    ret
END_FUNC par_cmpop

section .rodata

;; ---------------------------------------------------------------------------
;; prule_table - the expression grammar, one row per token kind.
;;
;; Reading a row: `prefix` runs when the token starts an expression, `infix`
;; when it follows one.  lbp is how tightly the token binds to what is already
;; parsed -- 0 means it is not an infix operator and therefore ends the
;; expression.  rbp is the minimum the handler recurses at, which is what
;; encodes associativity: equal to lbp is left-associative, one below is
;; right-associative.
;;
;; A comma is deliberately absent.  Tuples are built by the callers that
;; actually permit them, because a comma in this table would silently swallow
;; call arguments, subscripts and assignment targets.
;; ---------------------------------------------------------------------------
align 8
prule_table:
    dq 0           , 0           
    db BP_NONE    , BP_NONE    , 0                 , 0           ; TOK_ENDMARKER
    dd 0
    dq 0           , 0           
    db BP_NONE    , BP_NONE    , 0                 , 0           ; TOK_NEWLINE
    dd 0
    dq 0           , 0           
    db BP_NONE    , BP_NONE    , 0                 , 0           ; TOK_INDENT
    dd 0
    dq 0           , 0           
    db BP_NONE    , BP_NONE    , 0                 , 0           ; TOK_DEDENT
    dd 0
    dq pf_name     , 0           
    db BP_NONE    , BP_NONE    , 0                 , 0           ; TOK_NAME
    dd 0
    dq pf_number   , 0           
    db BP_NONE    , BP_NONE    , 0                 , 0           ; TOK_NUMBER
    dd 0
    dq 0           , 0           
    db BP_NONE    , BP_NONE    , 0                 , 0           ; TOK_STRING
    dd 0
    dq 0           , 0           
    db BP_NONE    , BP_NONE    , 0                 , 0           ; TOK_FSTRING
    dd 0
    dq pf_group    , 0           
    db BP_NONE    , BP_NONE    , 0                 , 0           ; TOK_LPAR
    dd 0
    dq 0           , 0           
    db BP_NONE    , BP_NONE    , 0                 , 0           ; TOK_RPAR
    dd 0
    dq 0           , 0           
    db BP_NONE    , BP_NONE    , 0                 , 0           ; TOK_LSQB
    dd 0
    dq 0           , 0           
    db BP_NONE    , BP_NONE    , 0                 , 0           ; TOK_RSQB
    dd 0
    dq 0           , 0           
    db BP_NONE    , BP_NONE    , 0                 , 0           ; TOK_LBRACE
    dd 0
    dq 0           , 0           
    db BP_NONE    , BP_NONE    , 0                 , 0           ; TOK_RBRACE
    dd 0
    dq 0           , 0           
    db BP_NONE    , BP_NONE    , 0                 , 0           ; TOK_COLON
    dd 0
    dq 0           , 0           
    db BP_NONE    , BP_NONE    , 0                 , 0           ; TOK_COMMA
    dd 0
    dq 0           , 0           
    db BP_NONE    , BP_NONE    , 0                 , 0           ; TOK_SEMI
    dd 0
    dq 0           , 0           
    db BP_NONE    , BP_NONE    , 0                 , 0           ; TOK_DOT
    dd 0
    dq 0           , 0           
    db BP_NONE    , BP_NONE    , 0                 , 0           ; TOK_ELLIPSIS
    dd 0
    dq pf_unary    , in_binop    
    db BP_ARITH   , BP_ARITH   , NB_ADD            , 0           ; TOK_PLUS -- aux is the BINARY op; pf_unary reads the token, not aux
    dd 0
    dq pf_unary    , in_binop    
    db BP_ARITH   , BP_ARITH   , NB_SUBTRACT       , 0           ; TOK_MINUS
    dd 0
    dq 0           , in_binop    
    db BP_TERM    , BP_TERM    , NB_MULTIPLY       , 0           ; TOK_STAR
    dd 0
    dq 0           , in_binop    
    db BP_POWER   , BP_UNARY   , NB_POWER          , 0           ; TOK_DOUBLESTAR -- rbp one level BELOW lbp: right-associative, and its RHS accepts unary
    dd 0
    dq 0           , in_binop    
    db BP_TERM    , BP_TERM    , NB_TRUE_DIVIDE    , 0           ; TOK_SLASH
    dd 0
    dq 0           , in_binop    
    db BP_TERM    , BP_TERM    , NB_FLOOR_DIVIDE   , 0           ; TOK_DOUBLESLASH
    dd 0
    dq 0           , in_binop    
    db BP_TERM    , BP_TERM    , NB_REMAINDER      , 0           ; TOK_PERCENT
    dd 0
    dq 0           , in_binop    
    db BP_TERM    , BP_TERM    , NB_MATRIX_MULTIPLY, 0           ; TOK_AT
    dd 0
    dq 0           , in_binop    
    db BP_BITOR   , BP_BITOR   , NB_OR             , 0           ; TOK_VBAR
    dd 0
    dq 0           , in_binop    
    db BP_BITAND  , BP_BITAND  , NB_AND            , 0           ; TOK_AMPER
    dd 0
    dq 0           , in_binop    
    db BP_BITXOR  , BP_BITXOR  , NB_XOR            , 0           ; TOK_CIRCUMFLEX
    dd 0
    dq pf_unary    , 0           
    db BP_NONE    , BP_UNARY   , UOP_INVERT        , 0           ; TOK_TILDE
    dd 0
    dq 0           , in_binop    
    db BP_SHIFT   , BP_SHIFT   , NB_LSHIFT         , 0           ; TOK_LEFTSHIFT
    dd 0
    dq 0           , in_binop    
    db BP_SHIFT   , BP_SHIFT   , NB_RSHIFT         , 0           ; TOK_RIGHTSHIFT
    dd 0
    dq 0           , in_compare  
    db BP_COMPARE , BP_COMPARE , 0                 , PR_CHAIN    ; TOK_LESS
    dd 0
    dq 0           , in_compare  
    db BP_COMPARE , BP_COMPARE , 0                 , PR_CHAIN    ; TOK_GREATER
    dd 0
    dq 0           , in_compare  
    db BP_COMPARE , BP_COMPARE , 0                 , PR_CHAIN    ; TOK_LESSEQUAL
    dd 0
    dq 0           , in_compare  
    db BP_COMPARE , BP_COMPARE , 0                 , PR_CHAIN    ; TOK_GREATEREQUAL
    dd 0
    dq 0           , in_compare  
    db BP_COMPARE , BP_COMPARE , 0                 , PR_CHAIN    ; TOK_EQEQUAL
    dd 0
    dq 0           , in_compare  
    db BP_COMPARE , BP_COMPARE , 0                 , PR_CHAIN    ; TOK_NOTEQUAL
    dd 0
    dq 0           , 0           
    db BP_NONE    , BP_NONE    , 0                 , 0           ; TOK_EQUAL
    dd 0
    dq 0           , 0           
    db BP_NONE    , BP_NONE    , 0                 , 0           ; TOK_COLONEQUAL
    dd 0
    dq 0           , 0           
    db BP_NONE    , BP_NONE    , 0                 , 0           ; TOK_RARROW
    dd 0
    dq 0           , 0           
    db BP_NONE    , BP_NONE    , 0                 , 0           ; TOK_PLUSEQUAL
    dd 0
    dq 0           , 0           
    db BP_NONE    , BP_NONE    , 0                 , 0           ; TOK_MINEQUAL
    dd 0
    dq 0           , 0           
    db BP_NONE    , BP_NONE    , 0                 , 0           ; TOK_STAREQUAL
    dd 0
    dq 0           , 0           
    db BP_NONE    , BP_NONE    , 0                 , 0           ; TOK_DOUBLESTAREQUAL
    dd 0
    dq 0           , 0           
    db BP_NONE    , BP_NONE    , 0                 , 0           ; TOK_SLASHEQUAL
    dd 0
    dq 0           , 0           
    db BP_NONE    , BP_NONE    , 0                 , 0           ; TOK_DOUBLESLASHEQUAL
    dd 0
    dq 0           , 0           
    db BP_NONE    , BP_NONE    , 0                 , 0           ; TOK_PERCENTEQUAL
    dd 0
    dq 0           , 0           
    db BP_NONE    , BP_NONE    , 0                 , 0           ; TOK_ATEQUAL
    dd 0
    dq 0           , 0           
    db BP_NONE    , BP_NONE    , 0                 , 0           ; TOK_VBAREQUAL
    dd 0
    dq 0           , 0           
    db BP_NONE    , BP_NONE    , 0                 , 0           ; TOK_AMPEREQUAL
    dd 0
    dq 0           , 0           
    db BP_NONE    , BP_NONE    , 0                 , 0           ; TOK_CIRCUMFLEXEQUAL
    dd 0
    dq 0           , 0           
    db BP_NONE    , BP_NONE    , 0                 , 0           ; TOK_LEFTSHIFTEQUAL
    dd 0
    dq 0           , 0           
    db BP_NONE    , BP_NONE    , 0                 , 0           ; TOK_RIGHTSHIFTEQUAL
    dd 0
    dq pf_const    , 0           
    db BP_NONE    , BP_NONE    , 0                 , 0           ; TOK_FALSE
    dd 0
    dq pf_const    , 0           
    db BP_NONE    , BP_NONE    , 0                 , 0           ; TOK_NONE
    dd 0
    dq pf_const    , 0           
    db BP_NONE    , BP_NONE    , 0                 , 0           ; TOK_TRUE
    dd 0
    dq 0           , in_boolop   
    db BP_AND     , BP_AND     , 0                 , 0           ; TOK_AND
    dd 0
    dq 0           , 0           
    db BP_NONE    , BP_NONE    , 0                 , 0           ; TOK_AS
    dd 0
    dq 0           , 0           
    db BP_NONE    , BP_NONE    , 0                 , 0           ; TOK_ASSERT
    dd 0
    dq 0           , 0           
    db BP_NONE    , BP_NONE    , 0                 , 0           ; TOK_ASYNC
    dd 0
    dq 0           , 0           
    db BP_NONE    , BP_NONE    , 0                 , 0           ; TOK_AWAIT
    dd 0
    dq 0           , 0           
    db BP_NONE    , BP_NONE    , 0                 , 0           ; TOK_BREAK
    dd 0
    dq 0           , 0           
    db BP_NONE    , BP_NONE    , 0                 , 0           ; TOK_CLASS
    dd 0
    dq 0           , 0           
    db BP_NONE    , BP_NONE    , 0                 , 0           ; TOK_CONTINUE
    dd 0
    dq 0           , 0           
    db BP_NONE    , BP_NONE    , 0                 , 0           ; TOK_DEF
    dd 0
    dq 0           , 0           
    db BP_NONE    , BP_NONE    , 0                 , 0           ; TOK_DEL
    dd 0
    dq 0           , 0           
    db BP_NONE    , BP_NONE    , 0                 , 0           ; TOK_ELIF
    dd 0
    dq 0           , 0           
    db BP_NONE    , BP_NONE    , 0                 , 0           ; TOK_ELSE
    dd 0
    dq 0           , 0           
    db BP_NONE    , BP_NONE    , 0                 , 0           ; TOK_EXCEPT
    dd 0
    dq 0           , 0           
    db BP_NONE    , BP_NONE    , 0                 , 0           ; TOK_FINALLY
    dd 0
    dq 0           , 0           
    db BP_NONE    , BP_NONE    , 0                 , 0           ; TOK_FOR
    dd 0
    dq 0           , 0           
    db BP_NONE    , BP_NONE    , 0                 , 0           ; TOK_FROM
    dd 0
    dq 0           , 0           
    db BP_NONE    , BP_NONE    , 0                 , 0           ; TOK_GLOBAL
    dd 0
    dq 0           , in_ternary  
    db BP_TERNARY , BP_TERNARY , 0                 , 0           ; TOK_IF
    dd 0
    dq 0           , 0           
    db BP_NONE    , BP_NONE    , 0                 , 0           ; TOK_IMPORT
    dd 0
    dq 0           , in_compare  
    db BP_COMPARE , BP_COMPARE , 0                 , PR_CHAIN    ; TOK_IN
    dd 0
    dq 0           , in_compare  
    db BP_COMPARE , BP_COMPARE , 0                 , PR_CHAIN    ; TOK_IS
    dd 0
    dq 0           , 0           
    db BP_NONE    , BP_NONE    , 0                 , 0           ; TOK_LAMBDA
    dd 0
    dq 0           , 0           
    db BP_NONE    , BP_NONE    , 0                 , 0           ; TOK_NONLOCAL
    dd 0
    dq pf_unary    , in_compare  
    db BP_COMPARE , BP_NOT     , UOP_NOT           , PR_CHAIN    ; TOK_NOT -- prefix `not x`; as an infix it can only start `not in`
    dd 0
    dq 0           , in_boolop   
    db BP_OR      , BP_OR      , 0                 , 0           ; TOK_OR
    dd 0
    dq 0           , 0           
    db BP_NONE    , BP_NONE    , 0                 , 0           ; TOK_PASS
    dd 0
    dq 0           , 0           
    db BP_NONE    , BP_NONE    , 0                 , 0           ; TOK_RAISE
    dd 0
    dq 0           , 0           
    db BP_NONE    , BP_NONE    , 0                 , 0           ; TOK_RETURN
    dd 0
    dq 0           , 0           
    db BP_NONE    , BP_NONE    , 0                 , 0           ; TOK_TRY
    dd 0
    dq 0           , 0           
    db BP_NONE    , BP_NONE    , 0                 , 0           ; TOK_WHILE
    dd 0
    dq 0           , 0           
    db BP_NONE    , BP_NONE    , 0                 , 0           ; TOK_WITH
    dd 0
    dq 0           , 0           
    db BP_NONE    , BP_NONE    , 0                 , 0           ; TOK_YIELD
    dd 0

ASM_INIT
