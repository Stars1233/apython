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
extern buf_free
extern buf_init
extern buf_push_u8
extern bytes_from_data
extern str_new_heap
extern comp_intern
extern par_for_target
extern par_fstring_pieces
extern par_params
extern int_from_cstr_base
extern strtod

extern bool_false
extern ellipsis_singleton
extern bool_true
extern none_singleton

extern exc_SyntaxError_type

;; ---------------------------------------------------------------------------
;; Binding powers, in steps of two so that a right-associative operator is
;; simply "recurse at lbp - 1" and there is still room between levels.
;; ---------------------------------------------------------------------------
; The binding powers live in compiler.inc: pattern.asm needs them too.

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
    je .arith_unary
    mov edx, UOP_POS
    cmp eax, TOK_PLUS
    je .arith_unary
    mov edx, UOP_INVERT
    cmp eax, TOK_TILDE
    je .have_op
    mov edx, UOP_NOT
    jmp .have_op
.arith_unary:
    ; The same row's rbp is the BINARY power, which is below `*`, `/`, `//` and
    ; `%` -- so taking it here parsed `-7 // 2` as `-(7 // 2)`, which is -3
    ; rather than -4.  Unary minus binds tighter than a term: the grammar is
    ; `factor: ('+'|'-'|'~') factor`.  BP_UNARY is still below BP_POWER, which
    ; is what keeps `-2**2` equal to -4.
    mov qword [rbp - PFU_RBP], BP_UNARY
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
;; alternative one level BELOW the ternary so that `a if b else c if d else e`
;; nests to the right.  Parsing it AT BP_TERNARY does not do that: the driver
;; continues only while lbp > min_bp, so an equal power stops -- and the
;; expression came out left-nested, which for `"pos" if x > 0 else "zero" if
;; x == 0 else "neg"` answers "neg" for a positive x.
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
    mov esi, BP_WALRUS
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
    jne .not_false
    lea rsi, [rel bool_false]
    jmp .have
.not_false:
    cmp edx, TOK_ELLIPSIS
    jne .have
    lea rsi, [rel ellipsis_singleton]
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

;; ============================================================================
;; par_utf8_emit(Buf *b, uint32_t cp)
;; Append one code point as UTF-8.  \x, \u, \U and \N all funnel through here,
;; so a string literal's bytes and its code-point count agree by construction.
;; ============================================================================
DEF_FUNC par_utf8_emit, 8
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
    or esi, 0xF0
    mov rdi, rbx
    call buf_push_u8
    mov rsi, r12
    shr rsi, 12
    and esi, 0x3F
    or esi, 0x80
    mov rdi, rbx
    call buf_push_u8
    jmp .tail2
.three:
    mov rsi, r12
    shr rsi, 12
    or esi, 0xE0
    mov rdi, rbx
    call buf_push_u8
.tail2:
    mov rsi, r12
    shr rsi, 6
    and esi, 0x3F
    or esi, 0x80
    mov rdi, rbx
    call buf_push_u8
    jmp .tail1
.two:
    mov rsi, r12
    shr rsi, 6
    or esi, 0xC0
    mov rdi, rbx
    call buf_push_u8
.tail1:
    mov rsi, r12
    and esi, 0x3F
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
PB_COMP  equ 8
PB_TOK   equ 16
PB_OUT   equ 24
PB_P     equ 32
PB_END   equ 40
PB_RAW   equ 48
PB_BYTES equ 56
PB_ACC   equ 64
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
    inc qword [rbp - PB_P]
    mov r12, [rbp - PB_P]
    cmp r12, [rbp - PB_END]
    jae .bad_escape
    movzx eax, byte [r12]
    inc qword [rbp - PB_P]

    cmp al, 10                          ; a backslash-newline vanishes
    je .loop
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
.e_unknown:
    ; An unrecognised escape keeps the backslash, as Python does (with a
    ; SyntaxWarning it does not raise on).
    push rax
    mov rdi, [rbp - PB_OUT]
    mov esi, 92
    call buf_push_u8
    pop rax
.e_literal:
    mov rdi, [rbp - PB_OUT]
    mov esi, eax
    call buf_push_u8
    jmp .loop
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
    mov [rbp - PB_ACC], rax
    mov ecx, 2
.oct_loop:
    mov r12, [rbp - PB_P]
    cmp r12, [rbp - PB_END]
    jae .oct_done
    movzx eax, byte [r12]
    cmp al, '0'
    jb .oct_done
    cmp al, '7'
    ja .oct_done
    sub eax, '0'
    mov rdx, [rbp - PB_ACC]
    shl rdx, 3
    or rdx, rax
    mov [rbp - PB_ACC], rdx
    inc qword [rbp - PB_P]
    dec ecx
    jnz .oct_loop
.oct_done:
    mov rdi, [rbp - PB_OUT]
    mov rsi, [rbp - PB_ACC]
    cmp qword [rbp - PB_BYTES], 0
    jne .raw_byte
    call par_utf8_emit
    jmp .loop
.raw_byte:
    and esi, 0xff
    call buf_push_u8
    jmp .loop

.e_hex2:
    mov r13d, 2
    jmp .hex_common
.e_hex4:
    mov r13d, 4
    jmp .hex_common
.e_hex8:
    mov r13d, 8
.hex_common:
    ; \u and \U have no meaning in a bytes literal; only \x does.
    cmp r13d, 2
    je .hex_go
    cmp qword [rbp - PB_BYTES], 0
    jne .bad_escape
.hex_go:
    mov qword [rbp - PB_ACC], 0
.hex_loop:
    test r13d, r13d
    jz .hex_done
    mov r12, [rbp - PB_P]
    cmp r12, [rbp - PB_END]
    jae .bad_escape
    movzx edi, byte [r12]
    call par_hexval
    cmp eax, -1
    je .bad_escape
    mov rdx, [rbp - PB_ACC]
    shl rdx, 4
    or rdx, rax
    mov [rbp - PB_ACC], rdx
    inc qword [rbp - PB_P]
    dec r13d
    jmp .hex_loop
.hex_done:
    mov rsi, [rbp - PB_ACC]
    cmp qword [rbp - PB_BYTES], 0
    jne .hex_byte
    mov rdi, [rbp - PB_OUT]
    call par_utf8_emit
    jmp .loop
.hex_byte:
    mov rdi, [rbp - PB_OUT]
    and esi, 0xff
    call buf_push_u8
    jmp .loop

.ok:
    mov eax, 1
    pop r13
    pop r12
    pop rbx
    leave
    ret
.bad_escape:
    mov rdi, rbx
    CSTRING rsi, "invalid escape sequence in string literal"
    call par_syntax_error
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
PS2_COMP  equ 8
PS2_LINE  equ 16
PS2_BYTES equ 24
PS2_BUF   equ 64         ; a Buf at [rbp - 64]
PS2_FRAME equ 72         ; + 1 push = 80
DEF_FUNC_LOCAL pf_string, PS2_FRAME
    push rbx
    mov rbx, rdi

    call par_peek
    mov ecx, [rax + Token.lineno]
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
    call str_new_heap
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
PFA_COMP  equ 8
PFA_TOK   equ 16
PFA_MARK  equ 24
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
    mov ecx, [rcx + Token.lineno]
    mov rdi, rbx
    mov esi, AST_CONST
    xor edx, edx
    xor r9d, r9d
    call ast_make
    mov rdi, rbx
    mov rsi, rax
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
    mov rdi, rbx
    CSTRING rsi, "cannot mix bytes and f-string literals"
    call par_syntax_error
.fail:
    lea rdi, [rbp - PFA_BUF]
    call buf_free
    xor eax, eax
    pop rbx
    leave
    ret
END_FUNC par_fstring_piece_any

;; ============================================================================
;; par_exprlist(Comp *c, int close, int *saw_comma) -> rax = child-list mark
;;
;; Parses a comma-separated run up to `close`, pushing each element onto the
;; pending stack.  A trailing comma is allowed and recorded, because for a
;; parenthesised run it is the difference between (a) and (a,).
;;
;; Returns the mark to hand to ast_commit; rax = -1 on error.
;; ============================================================================
PL_COMP  equ 8
PL_CLOSE equ 16
PL_FLAG  equ 24
PL_MARK  equ 32
PL_FRAME equ 40          ; + 1 push = 48
DEF_FUNC par_exprlist, PL_FRAME
    push rbx
    mov rbx, rdi
    mov [rbp - PL_CLOSE], rsi
    mov [rbp - PL_FLAG], rdx
    mov qword [rdx], 0

    mov rdi, rbx
    call ast_mark
    mov [rbp - PL_MARK], rax

.loop:
    mov rdi, rbx
    call par_kind
    cmp rax, [rbp - PL_CLOSE]
    je .done
    cmp eax, TOK_ENDMARKER
    je .done

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
    jne .done
    mov rdx, [rbp - PL_FLAG]
    mov qword [rdx], 1
    mov rdi, rbx
    call par_advance
    jmp .loop

.done:
    mov rax, [rbp - PL_MARK]
    pop rbx
    leave
    ret
.fail:
    mov rax, -1
    pop rbx
    leave
    ret
END_FUNC par_exprlist

;; ============================================================================
;; par_finish_list(Comp *c, int kind, int line, uint64_t mark) -> node
;; Commits the staged children onto a fresh node.
;; ============================================================================
PF_COMP  equ 8
PF_MARK  equ 16
PF_NODE  equ 24
PF_FRAME equ 24          ; + 1 push = 32
DEF_FUNC par_finish_list, PF_FRAME
    push rbx
    mov rbx, rdi
    mov [rbp - PF_MARK], rcx
    xor r8d, r8d
    xor r9d, r9d
    mov rcx, rdx                        ; lineno
    mov rdx, 0                          ; subkind
    call ast_make
    mov [rbp - PF_NODE], rax
    mov rdi, rbx
    mov rsi, [rbp - PF_MARK]
    call ast_commit
    mov r8, rax
    mov r9, rdx
    mov rdi, rbx
    mov rsi, [rbp - PF_NODE]
    call ast_at
    mov [rax + AstNode.clist], r8d
    mov [rax + AstNode.nchild], r9d
    mov rax, [rbp - PF_NODE]
    pop rbx
    leave
    ret
END_FUNC par_finish_list

;; ============================================================================
;; pf_group(Comp *c) -> node   -- (), (a), (a, b), (a,)
;;
;; A parenthesised single expression is just that expression; it becomes a
;; tuple only when a comma appears.  That is why the trailing-comma flag is
;; tracked: (a) is a, and (a,) is a one-tuple.
;; ============================================================================
PG_COMP  equ 8
PG_LINE  equ 16
PG_MARK  equ 24
PG_COMMA equ 32
PG_FRAME equ 40          ; + 1 push = 48
DEF_FUNC_LOCAL pf_group, PG_FRAME
    push rbx
    mov rbx, rdi
    call par_peek
    mov ecx, [rax + Token.lineno]
    mov [rbp - PG_LINE], rcx
    mov rdi, rbx
    call par_advance                    ; consume '('

    ; `(x for ...)` is a generator expression rather than a group; as with a
    ; list, the two are the same production until the token after the element.
    mov rdi, rbx
    call par_kind
    cmp eax, TOK_RPAR
    je .as_list
    mov rdi, rbx
    call par_peek_second_is_for
    test eax, eax
    jz .as_list
    mov rdi, rbx
    mov esi, BP_NONE
    call par_expr
    test rax, rax
    jz .fail
    mov rdi, rbx
    mov esi, AST_GENEXP
    mov rdx, rax
    xor ecx, ecx
    mov r8, [rbp - PG_LINE]
    mov r9, TOK_RPAR
    call par_comprehension
    pop rbx
    leave
    ret

.as_list:
    mov rdi, rbx
    mov esi, TOK_RPAR
    lea rdx, [rbp - PG_COMMA]
    call par_exprlist
    cmp rax, -1
    je .fail
    mov [rbp - PG_MARK], rax

    mov rdi, rbx
    mov esi, TOK_RPAR
    CSTRING rdx, "'(' was never closed"
    call par_expect
    test eax, eax
    jz .fail

    ; One element and no comma: the parentheses were only grouping.
    mov rax, [rbx + Comp.pending + Buf.len]
    sub rax, [rbp - PG_MARK]
    cmp rax, 1
    jne .make_tuple
    cmp qword [rbp - PG_COMMA], 0
    jne .make_tuple
    mov rax, [rbx + Comp.pending + Buf.data]
    mov rcx, [rbx + Comp.pending + Buf.len]
    dec rcx
    mov eax, [rax + rcx*4]
    mov [rbx + Comp.pending + Buf.len], rcx
    pop rbx
    leave
    ret

.make_tuple:
    mov rdi, rbx
    mov esi, AST_TUPLE
    mov rdx, [rbp - PG_LINE]
    mov rcx, [rbp - PG_MARK]
    call par_finish_list
    pop rbx
    leave
    ret
.fail:
    xor eax, eax
    pop rbx
    leave
    ret
END_FUNC pf_group

;; ============================================================================
;; pf_list(Comp *c) -> node   -- [a, b] or [e for x in it]
;;
;; A display and a comprehension are the same production until the token after
;; the first element, so the element is parsed once and the shape decided
;; afterwards.
;; ============================================================================
DEF_FUNC_LOCAL pf_list, PG_FRAME
    push rbx
    mov rbx, rdi
    call par_peek
    mov ecx, [rax + Token.lineno]
    mov [rbp - PG_LINE], rcx
    mov rdi, rbx
    call par_advance                    ; consume '['
    mov qword [rbp - PG_COMMA], 0

    mov rdi, rbx
    call ast_mark
    mov [rbp - PG_MARK], rax

    mov rdi, rbx
    call par_kind
    cmp eax, TOK_RSQB
    je .close                           ; []

    mov rdi, rbx
    mov esi, BP_NONE
    call par_expr
    test rax, rax
    jz .fail
    mov r8, rax

    mov rdi, rbx
    call par_kind
    cmp eax, TOK_FOR
    je .comp
    cmp eax, TOK_ASYNC
    je .comp

    mov rdi, rbx
    mov rsi, r8
    call ast_push
.more:
    mov rdi, rbx
    call par_kind
    cmp eax, TOK_COMMA
    jne .close
    mov rdi, rbx
    call par_advance
    mov rdi, rbx
    call par_kind
    cmp eax, TOK_RSQB
    je .close                           ; a trailing comma
    mov rdi, rbx
    mov esi, BP_NONE
    call par_expr
    test rax, rax
    jz .fail
    mov rdi, rbx
    mov rsi, rax
    call ast_push
    jmp .more

.close:
    mov rdi, rbx
    mov esi, TOK_RSQB
    CSTRING rdx, "'[' was never closed"
    call par_expect
    test eax, eax
    jz .fail
    mov rdi, rbx
    mov esi, AST_LIST
    mov rdx, [rbp - PG_LINE]
    mov rcx, [rbp - PG_MARK]
    call par_finish_list
    pop rbx
    leave
    ret

.comp:
    mov rdi, rbx
    mov esi, AST_LISTCOMP
    mov rdx, r8
    xor ecx, ecx
    mov r8, [rbp - PG_LINE]
    mov r9, TOK_RSQB
    call par_comprehension
    pop rbx
    leave
    ret
.fail:
    xor eax, eax
    pop rbx
    leave
    ret
END_FUNC pf_list

;; ============================================================================
;; pf_dictset(Comp *c) -> node   -- {}, {a: b}, {a, b}, {**m}
;;
;; One opening brace, three possible displays.  Which it is is decided by the
;; token after the first element: a colon means a dict, anything else a set, and
;; a bare `**` means a dict too.  An empty {} is a dict, not a set, because
;; there is no set literal for the empty set.
;; ============================================================================
PD_COMP  equ 8
PD_LINE  equ 16
PD_MARK  equ 24
PD_ISDICT equ 32
PD_KEY    equ 40
PD_VALUE  equ 48
PD_FRAME equ 56          ; + 1 push = 64
DEF_FUNC_LOCAL pf_dictset, PD_FRAME
    push rbx
    mov rbx, rdi
    call par_peek
    mov ecx, [rax + Token.lineno]
    mov [rbp - PD_LINE], rcx
    mov rdi, rbx
    call par_advance                    ; consume '{'

    mov rdi, rbx
    call ast_mark
    mov [rbp - PD_MARK], rax
    mov qword [rbp - PD_ISDICT], 1      ; an empty {} is a dict

    mov rdi, rbx
    call par_kind
    cmp eax, TOK_RBRACE
    je .close

    ; The first element decides the shape.
    mov rdi, rbx
    call par_kind
    cmp eax, TOK_DOUBLESTAR
    je .dict_unpack                     ; {**m, ...}: a dict, and parse it now
    mov rdi, rbx
    mov esi, BP_NONE
    call par_expr
    test rax, rax
    jz .fail
    mov r8, rax
    mov rdi, rbx
    call par_kind
    cmp eax, TOK_COLON
    je .dict_first
    cmp eax, TOK_FOR
    je .setcomp
    cmp eax, TOK_ASYNC
    je .setcomp
    ; A set: push what we already parsed and carry on.
    mov qword [rbp - PD_ISDICT], 0
    mov rdi, rbx
    mov rsi, r8
    call ast_push
    jmp .more
.setcomp:
    mov rdi, rbx
    mov esi, AST_SETCOMP
    mov rdx, r8
    xor ecx, ecx
    mov r8, [rbp - PD_LINE]
    mov r9, TOK_RBRACE
    call par_comprehension
    pop rbx
    leave
    ret
.dict_first:
    mov [rbp - PD_KEY], r8
    mov rdi, rbx
    call par_advance                    ; consume ':'
    mov rdi, rbx
    mov esi, BP_NONE
    call par_expr
    test rax, rax
    jz .fail
    mov [rbp - PD_VALUE], rax
    mov rdi, rbx
    call par_kind
    cmp eax, TOK_FOR
    je .dictcomp
    cmp eax, TOK_ASYNC
    je .dictcomp
    mov rdi, rbx
    mov rsi, [rbp - PD_KEY]
    call ast_push
    mov rdi, rbx
    mov rsi, [rbp - PD_VALUE]
    call ast_push
    jmp .more
.dictcomp:
    mov rdi, rbx
    mov esi, AST_DICTCOMP
    mov rdx, [rbp - PD_KEY]
    mov rcx, [rbp - PD_VALUE]
    mov r8, [rbp - PD_LINE]
    mov r9, TOK_RBRACE
    call par_comprehension
    pop rbx
    leave
    ret

.more:
    mov rdi, rbx
    call par_kind
    cmp eax, TOK_COMMA
    jne .close
    mov rdi, rbx
    call par_advance
    mov rdi, rbx
    call par_kind
    cmp eax, TOK_RBRACE
    je .close                           ; a trailing comma

    cmp qword [rbp - PD_ISDICT], 0
    je .set_item

    ; dict: either `**m` or `key: value`
    mov rdi, rbx
    call par_kind
    cmp eax, TOK_DOUBLESTAR
    je .dict_unpack
    mov rdi, rbx
    mov esi, BP_NONE
    call par_expr
    test rax, rax
    jz .fail
    mov rdi, rbx
    mov rsi, rax
    call ast_push
    mov rdi, rbx
    mov esi, TOK_COLON
    CSTRING rdx, "expected ':' in dict display"
    call par_expect
    test eax, eax
    jz .fail
    mov rdi, rbx
    mov esi, BP_NONE
    call par_expr
    test rax, rax
    jz .fail
    mov rdi, rbx
    mov rsi, rax
    call ast_push
    jmp .more
.dict_unpack:
    mov rdi, rbx
    mov esi, BP_NONE
    call par_expr                       ; pf_starred builds the ** node
    test rax, rax
    jz .fail
    mov rdi, rbx
    mov rsi, rax
    call ast_push
    ; A ** entry occupies one slot where a pair occupies two; pushing the node
    ; twice keeps the list in key/value pairs so codegen can walk it uniformly.
    mov rdi, rbx
    xor esi, esi
    call ast_push
    jmp .more

.set_item:
    mov rdi, rbx
    mov esi, BP_NONE
    call par_expr
    test rax, rax
    jz .fail
    mov rdi, rbx
    mov rsi, rax
    call ast_push
    jmp .more

.close:
    mov rdi, rbx
    mov esi, TOK_RBRACE
    CSTRING rdx, "'{' was never closed"
    call par_expect
    test eax, eax
    jz .fail

    mov esi, AST_SET
    cmp qword [rbp - PD_ISDICT], 0
    je .have_kind
    mov esi, AST_DICT
.have_kind:
    mov rdi, rbx
    mov rdx, [rbp - PD_LINE]
    mov rcx, [rbp - PD_MARK]
    call par_finish_list
    pop rbx
    leave
    ret
.fail:
    xor eax, eax
    pop rbx
    leave
    ret
END_FUNC pf_dictset

;; ============================================================================
;; pf_starred(Comp *c) -> node   -- *x and **x
;; Legal only inside a display, a call or a target list; the emitters reject it
;; anywhere else, which is what makes `*x + 1` a syntax error.
;; ============================================================================
PST_COMP  equ 8
PST_LINE  equ 16
PST_KIND  equ 24
PST_FRAME equ 24         ; + 1 push = 32
DEF_FUNC_LOCAL pf_starred, PST_FRAME
    push rbx
    mov rbx, rdi
    call par_peek
    mov ecx, [rax + Token.lineno]
    mov [rbp - PST_LINE], rcx
    movzx eax, word [rax + Token.kind]
    mov edx, AST_STARRED
    cmp eax, TOK_DOUBLESTAR
    jne .have
    mov edx, AST_DOUBLESTARRED
.have:
    mov [rbp - PST_KIND], rdx
    mov rdi, rbx
    call par_advance
    mov rdi, rbx
    ; A star takes an or_expr: `[*a | b]` is `[*(a | b)]`, while `[*a in b]`
    ; and `[*a or b]` are syntax errors.  Recursing below BP_COMPARE swallowed
    ; the `in` of a for statement, so `for a, *b in z` had no loop keyword left
    ; -- while `for *a, b in z` parsed, because there the star was not the
    ; element the `in` followed.
    mov esi, BP_STAROP
    call par_expr
    test rax, rax
    jz .fail
    mov r8, rax
    mov rdi, rbx
    mov rsi, [rbp - PST_KIND]
    xor edx, edx
    mov rcx, [rbp - PST_LINE]
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
END_FUNC pf_starred

;; ============================================================================
;; in_attr(Comp *c, node value) -> node   -- value.name
;; ============================================================================
IA_COMP  equ 8
IA_VAL   equ 16
IA_LINE  equ 24
IA_FRAME equ 24          ; + 1 push = 32
DEF_FUNC_LOCAL in_attr, IA_FRAME
    push rbx
    mov rbx, rdi
    mov [rbp - IA_VAL], rsi
    call par_peek
    mov ecx, [rax + Token.lineno]
    mov [rbp - IA_LINE], rcx
    mov rdi, rbx
    call par_advance                    ; consume '.'

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
    mov r9, rax
    mov rdi, rbx
    call par_advance

    mov rdi, rbx
    mov esi, AST_ATTRIBUTE
    mov edx, CTX_LOAD
    mov rcx, [rbp - IA_LINE]
    mov r8, [rbp - IA_VAL]
    call ast_make
    pop rbx
    leave
    ret
.need_name:
    mov rdi, rbx
    CSTRING rsi, "expected an attribute name after '.'"
    call par_syntax_error
.fail:
    xor eax, eax
    pop rbx
    leave
    ret
END_FUNC in_attr

;; ============================================================================
;; par_slice_piece(Comp *c) -> node, or 0 for an omitted bound
;; ============================================================================
DEF_FUNC par_slice_piece, 8
    push rbx
    mov rbx, rdi
    call par_kind
    cmp eax, TOK_COLON
    je .omitted
    cmp eax, TOK_RSQB
    je .omitted
    cmp eax, TOK_COMMA
    je .omitted
    mov rdi, rbx
    mov esi, BP_NONE
    call par_expr
    pop rbx
    leave
    ret
.omitted:
    xor eax, eax
    pop rbx
    leave
    ret
END_FUNC par_slice_piece

;; ============================================================================
;; par_subscript_item(Comp *c) -> node   -- one index or one slice
;; The piece between the brackets, or between two commas: `a`, `a:b`, `a:b:c`,
;; `:`, `::2` all land here.
;; ============================================================================
SI_LOWER equ 8
SI_UPPER equ 16
SI_STEP  equ 24
SI_SLICE equ 32
SI_LINE  equ 40
SI_FRAME equ 40           ; + 1 push = 48
DEF_FUNC_LOCAL par_subscript_item, SI_FRAME
    push rbx
    mov rbx, rdi
    call par_peek
    mov ecx, [rax + Token.lineno]
    mov [rbp - SI_LINE], rcx
    mov qword [rbp - SI_UPPER], 0
    mov qword [rbp - SI_STEP], 0
    mov qword [rbp - SI_SLICE], 0

    mov rdi, rbx
    call par_slice_piece
    mov [rbp - SI_LOWER], rax

    mov rdi, rbx
    call par_kind
    cmp eax, TOK_COLON
    jne .plain

    mov qword [rbp - SI_SLICE], 2       ; at least lower:upper
    mov rdi, rbx
    call par_advance
    mov rdi, rbx
    call par_slice_piece
    mov [rbp - SI_UPPER], rax

    mov rdi, rbx
    call par_kind
    cmp eax, TOK_COLON
    jne .make_slice
    mov qword [rbp - SI_SLICE], 3
    mov rdi, rbx
    call par_advance
    mov rdi, rbx
    call par_slice_piece
    mov [rbp - SI_STEP], rax
    jmp .make_slice

.plain:
    ; A missing index is only legal as part of a slice: `d[]` is not.
    cmp qword [rbp - SI_LOWER], 0
    je .need_index
    mov rax, [rbp - SI_LOWER]
    pop rbx
    leave
    ret

.make_slice:
    mov rdi, rbx
    mov esi, AST_SLICE
    mov rdx, [rbp - SI_SLICE]           ; subkind: 2 or 3 pieces
    mov rcx, [rbp - SI_LINE]
    mov r8, [rbp - SI_LOWER]
    mov r9, [rbp - SI_UPPER]
    call ast_make
    test rax, rax
    jz .fail
    mov [rbp - SI_LOWER], rax
    mov rdi, rbx
    mov rsi, rax
    call ast_at
    mov rdx, [rbp - SI_STEP]
    mov [rax + AstNode.c], edx
    mov rax, [rbp - SI_LOWER]
    pop rbx
    leave
    ret

.need_index:
    mov rdi, rbx
    CSTRING rsi, "expected an index expression"
    call par_syntax_error
.fail:
    xor eax, eax
    pop rbx
    leave
    ret
END_FUNC par_subscript_item

;; ============================================================================
;; in_subscript(Comp *c, node value) -> node   -- value[index]
;;
;; A comma inside the brackets makes the index a tuple: `d[1, 2]` subscripts
;; with `(1, 2)`, and `d[a:b, c]` with `(slice(a, b), c)`.  There is no
;; separate n-ary subscript node -- the tuple IS the index, which is why
;; `d[1,]` and `d[(1,)]` are the same expression.
;; ============================================================================
IS_COMP  equ 8
IS_VAL   equ 16
IS_LINE  equ 24
IS_IDX   equ 32
IS_MARK  equ 40
IS_FRAME equ 40           ; + 1 push = 48
DEF_FUNC_LOCAL in_subscript, IS_FRAME
    push rbx
    mov rbx, rdi
    mov [rbp - IS_VAL], rsi
    call par_peek
    mov ecx, [rax + Token.lineno]
    mov [rbp - IS_LINE], rcx
    mov rdi, rbx
    call par_advance                    ; consume '['

    mov rdi, rbx
    call par_subscript_item
    test rax, rax
    jz .fail
    mov [rbp - IS_IDX], rax

    mov rdi, rbx
    call par_kind
    cmp eax, TOK_COMMA
    jne .close

    ; A tuple index.  The first item is already parsed, so it is pushed before
    ; the loop rather than inside it.
    mov rdi, rbx
    call ast_mark
    mov [rbp - IS_MARK], rax
    mov rdi, rbx
    mov rsi, [rbp - IS_IDX]
    call ast_push
.comma_loop:
    mov rdi, rbx
    call par_kind
    cmp eax, TOK_COMMA
    jne .finish_tuple
    mov rdi, rbx
    call par_advance
    mov rdi, rbx
    call par_kind
    cmp eax, TOK_RSQB
    je .finish_tuple                    ; a trailing comma
    mov rdi, rbx
    call par_subscript_item
    test rax, rax
    jz .fail
    mov rdi, rbx
    mov rsi, rax
    call ast_push
    jmp .comma_loop

.finish_tuple:
    mov rdi, rbx
    mov esi, AST_TUPLE
    mov rdx, [rbp - IS_LINE]
    mov rcx, [rbp - IS_MARK]
    call par_finish_list
    test rax, rax
    jz .fail
    mov [rbp - IS_IDX], rax

.close:
    mov rdi, rbx
    mov esi, TOK_RSQB
    CSTRING rdx, "'[' was never closed"
    call par_expect
    test eax, eax
    jz .fail

    mov rdi, rbx
    mov esi, AST_SUBSCRIPT
    mov edx, CTX_LOAD
    mov rcx, [rbp - IS_LINE]
    mov r8, [rbp - IS_VAL]
    mov r9, [rbp - IS_IDX]
    call ast_make
    pop rbx
    leave
    ret

.fail:
    xor eax, eax
    pop rbx
    leave
    ret
END_FUNC in_subscript

;; ============================================================================
;; in_call(Comp *c, node func) -> node   -- func(args)
;;
;; The child list holds the arguments in source order.  A keyword argument is
;; an AST_KEYWORD node carrying its name, and `*a` / `**k` are AST_STARRED and
;; AST_DOUBLESTARRED; codegen decides from what it finds whether a plain CALL
;; will do or whether the call has to be assembled through CALL_FUNCTION_EX.
;; ============================================================================
ICL_COMP  equ 8
ICL_FUNC  equ 16
ICL_LINE  equ 24
ICL_MARK  equ 32
ICL_NAME  equ 40
ICL_FRAME equ 40         ; + 1 push = 48
DEF_FUNC_LOCAL in_call, ICL_FRAME
    push rbx
    mov rbx, rdi
    mov [rbp - ICL_FUNC], rsi
    call par_peek
    mov ecx, [rax + Token.lineno]
    mov [rbp - ICL_LINE], rcx
    mov rdi, rbx
    call par_advance                    ; consume '('

    mov rdi, rbx
    call ast_mark
    mov [rbp - ICL_MARK], rax

.arg_loop:
    mov rdi, rbx
    call par_kind
    cmp eax, TOK_RPAR
    je .close
    cmp eax, TOK_ENDMARKER
    je .close

    ; A keyword argument is NAME '=' expr, and only that: the '=' has to be
    ; the very next token, or `f(a == b)` would be misread.
    mov rdi, rbx
    call par_kind
    cmp eax, TOK_NAME
    jne .positional
    mov rdi, rbx
    call par_peek_next
    cmp eax, TOK_EQUAL
    jne .positional

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
    mov [rbp - ICL_NAME], rax
    mov rdi, rbx
    call par_advance                    ; the name
    mov rdi, rbx
    call par_advance                    ; the '='
    mov rdi, rbx
    mov esi, BP_NONE
    call par_expr
    test rax, rax
    jz .fail
    mov r9, rax
    mov rdi, rbx
    mov esi, AST_KEYWORD
    xor edx, edx
    mov rcx, [rbp - ICL_LINE]
    mov r8, [rbp - ICL_NAME]
    call ast_make
    jmp .push_arg

.positional:
    mov rdi, rbx
    mov esi, BP_NONE
    call par_expr
    test rax, rax
    jz .fail
    ; `f(x for x in y)` is a generator expression as the sole argument, with no
    ; parentheses of its own -- the call's own brackets serve.
    push rax
    mov rdi, rbx
    call par_kind
    mov ecx, eax
    pop rax
    cmp ecx, TOK_FOR
    je .genexp_arg
    cmp ecx, TOK_ASYNC
    je .genexp_arg
    jmp .push_arg
.genexp_arg:
    mov rdi, rbx
    mov esi, AST_GENEXP
    mov rdx, rax
    xor ecx, ecx
    mov r8, [rbp - ICL_LINE]
    xor r9d, r9d                        ; the caller consumes the ')'
    call par_comprehension
    test rax, rax
    jz .fail
.push_arg:
    mov rdi, rbx
    mov rsi, rax
    call ast_push

    mov rdi, rbx
    call par_kind
    cmp eax, TOK_COMMA
    jne .close
    mov rdi, rbx
    call par_advance
    jmp .arg_loop

.close:
    mov rdi, rbx
    mov esi, TOK_RPAR
    CSTRING rdx, "'(' was never closed"
    call par_expect
    test eax, eax
    jz .fail

    mov rdi, rbx
    mov esi, AST_CALL
    xor edx, edx
    mov rcx, [rbp - ICL_LINE]
    mov r8, [rbp - ICL_FUNC]
    xor r9d, r9d
    call ast_make
    mov [rbp - ICL_NAME], rax
    mov rdi, rbx
    mov rsi, [rbp - ICL_MARK]
    call ast_commit
    mov r8, rax
    mov r9, rdx
    mov rdi, rbx
    mov rsi, [rbp - ICL_NAME]
    call ast_at
    mov [rax + AstNode.clist], r8d
    mov [rax + AstNode.nchild], r9d
    mov rax, [rbp - ICL_NAME]
    pop rbx
    leave
    ret
.fail:
    xor eax, eax
    pop rbx
    leave
    ret
END_FUNC in_call

;; ============================================================================
;; in_call_public(Comp *c, node func) -> node
;; The class statement's base list is an argument list, keywords and all, so it
;; is parsed by the same code rather than a near-copy of it.
;; ============================================================================
DEF_FUNC_BARE in_call_public
    jmp in_call
END_FUNC in_call_public

;; ============================================================================
;; par_peek_next(Comp *c) -> eax = the kind of the token after the current one
;; One token of lookahead, which is all the grammar needs at a point where an
;; array of tokens makes it free.
;; ============================================================================
DEF_FUNC_BARE par_peek_next
    mov eax, [rdi + Comp.tok_idx]
    inc eax
    mov rcx, [rdi + Comp.tokens + Buf.len]
    dec rcx
    cmp rax, rcx
    jbe .ok
    mov eax, ecx
.ok:
    mov rdx, [rdi + Comp.tokens + Buf.data]
    shl rax, TOKEN_SHIFT
    movzx eax, word [rdx + rax + Token.kind]
    ret
END_FUNC par_peek_next

;; ============================================================================
;; pf_lambda(Comp *c) -> node   -- `lambda params: expr`
;;
;; The body is parsed one level below the ternary, so `lambda x: a if c else b`
;; is a lambda returning a conditional -- at BP_TERNARY the `if` binds no
;; tighter than the minimum and stops, leaving the lambda as the conditional's
;; body and its parameter out of scope.  A comma is still not part of the body,
;; because a comma is not in the table at all: `lambda: a, b` is a tuple
;; containing a lambda.
;; ============================================================================
PLM_COMP  equ 8
PLM_LINE  equ 16
PLM_ARGS  equ 24
PLM_NODE  equ 32
PLM_FRAME equ 40          ; + 1 push = 48
DEF_FUNC_LOCAL pf_lambda, PLM_FRAME
    push rbx
    mov rbx, rdi
    call par_peek
    mov ecx, [rax + Token.lineno]
    mov [rbp - PLM_LINE], rcx
    mov rdi, rbx
    call par_advance                    ; `lambda`

    mov rdi, rbx
    mov esi, TOK_COLON
    call par_params
    test rax, rax
    jz .fail
    mov [rbp - PLM_ARGS], rax

    mov rdi, rbx
    mov esi, TOK_COLON
    CSTRING rdx, "expected ':' after the lambda parameters"
    call par_expect
    test eax, eax
    jz .fail

    mov rdi, rbx
    mov esi, BP_WALRUS
    call par_expr
    test rax, rax
    jz .fail
    mov [rbp - PLM_NODE], rax

    mov rdi, rbx
    mov esi, AST_LAMBDA
    xor edx, edx
    mov rcx, [rbp - PLM_LINE]
    xor r8d, r8d
    mov r9, [rbp - PLM_ARGS]
    call ast_make
    mov [rbp - PLM_ARGS], rax
    mov rdi, rbx
    mov rsi, rax
    call ast_at
    mov rdx, [rbp - PLM_NODE]
    mov [rax + AstNode.c], edx          ; the body expression
    mov rax, [rbp - PLM_ARGS]
    pop rbx
    leave
    ret
.fail:
    xor eax, eax
    pop rbx
    leave
    ret
END_FUNC pf_lambda

;; ============================================================================
;; par_comprehension(Comp *c, int kind, uint32_t elt, uint32_t val, int line,
;;                   int close) -> node
;;
;; The `for` clauses of a comprehension, after its element has been parsed.
;; Each clause is an AST_COMPREHENSION carrying a target, an iterable and any
;; number of conditions; nesting is left to right, so `[x for a in b for x in a]`
;; is two clauses in that order.
;;
;; The iterable of each clause is parsed at BP_OR so a trailing `if` belongs to
;; the comprehension rather than becoming a conditional expression -- that is
;; what the minimum binding power is for.
;; ============================================================================
PCM_KIND  equ 8
PCM_ELT   equ 16
PCM_VAL   equ 24
PCM_LINE  equ 32
PCM_CLOSE equ 40
PCM_MARK  equ 48
PCM_TGT   equ 56
PCM_ITER  equ 64
PCM_CMARK equ 72
PCM_NODE  equ 80
PCM_ASYNC equ 88
PCM_FRAME equ 88          ; + 1 push = 96
DEF_FUNC par_comprehension, PCM_FRAME
    push rbx
    mov rbx, rdi
    mov [rbp - PCM_KIND], rsi
    mov [rbp - PCM_ELT], rdx
    mov [rbp - PCM_VAL], rcx
    mov [rbp - PCM_LINE], r8
    mov [rbp - PCM_CLOSE], r9

    mov rdi, rbx
    call ast_mark
    mov [rbp - PCM_MARK], rax

.clause:
    mov qword [rbp - PCM_ASYNC], 0
    mov rdi, rbx
    call par_kind
    cmp eax, TOK_ASYNC
    jne .not_async
    mov qword [rbp - PCM_ASYNC], 1
    mov rdi, rbx
    call par_advance
.not_async:
    mov rdi, rbx
    mov esi, TOK_FOR
    CSTRING rdx, "expected 'for' in comprehension"
    call par_expect
    test eax, eax
    jz .fail

    mov rdi, rbx
    call par_for_target
    test rax, rax
    jz .fail
    mov [rbp - PCM_TGT], rax

    mov rdi, rbx
    mov esi, TOK_IN
    CSTRING rdx, "expected 'in' in comprehension"
    call par_expect
    test eax, eax
    jz .fail

    mov rdi, rbx
    mov esi, BP_OR                      ; leaves a trailing `if` to us
    call par_expr
    test rax, rax
    jz .fail
    mov [rbp - PCM_ITER], rax

    ; Any number of conditions.
    mov rdi, rbx
    call ast_mark
    mov [rbp - PCM_CMARK], rax
.conds:
    mov rdi, rbx
    call par_kind
    cmp eax, TOK_IF
    jne .close_clause
    mov rdi, rbx
    call par_advance
    mov rdi, rbx
    mov esi, BP_OR
    call par_expr
    test rax, rax
    jz .fail
    mov rdi, rbx
    mov rsi, rax
    call ast_push
    jmp .conds

.close_clause:
    mov rdi, rbx
    mov esi, AST_COMPREHENSION
    mov rdx, [rbp - PCM_LINE]
    mov rcx, [rbp - PCM_CMARK]
    call par_finish_list
    test rax, rax
    jz .fail
    mov [rbp - PCM_NODE], rax
    mov rdi, rbx
    mov rsi, rax
    call ast_at
    mov rdx, [rbp - PCM_TGT]
    mov [rax + AstNode.a], edx
    mov rdx, [rbp - PCM_ITER]
    mov [rax + AstNode.b], edx
    mov rdx, [rbp - PCM_ASYNC]
    mov [rax + AstNode.subkind], dl
    mov rdi, rbx
    mov rsi, [rbp - PCM_NODE]
    call ast_push

    mov rdi, rbx
    call par_kind
    cmp eax, TOK_FOR
    je .clause
    cmp eax, TOK_ASYNC
    je .clause

    ; Close the comprehension itself.
    mov rdi, rbx
    mov rsi, [rbp - PCM_KIND]
    mov rdx, [rbp - PCM_LINE]
    mov rcx, [rbp - PCM_MARK]
    call par_finish_list
    test rax, rax
    jz .fail
    mov [rbp - PCM_NODE], rax
    mov rdi, rbx
    mov rsi, rax
    call ast_at
    mov rdx, [rbp - PCM_ELT]
    mov [rax + AstNode.a], edx
    mov rdx, [rbp - PCM_VAL]
    mov [rax + AstNode.b], edx

    cmp qword [rbp - PCM_CLOSE], 0
    je .done
    mov rdi, rbx
    mov rsi, [rbp - PCM_CLOSE]
    CSTRING rdx, "the comprehension was never closed"
    call par_expect
    test eax, eax
    jz .fail
.done:
    mov rax, [rbp - PCM_NODE]
    pop rbx
    leave
    ret
.fail:
    xor eax, eax
    pop rbx
    leave
    ret
END_FUNC par_comprehension

;; ============================================================================
;; par_peek_second_is_for(Comp *c) -> rax = 1 when a `for` follows the first
;; element of the bracketed run, without consuming anything.
;;
;; A generator expression cannot be told from a parenthesised expression by one
;; token of lookahead, so this scans ahead to the matching depth.  It is the
;; only place the parser looks further than the next token, and having the
;; whole token array is what makes it a loop rather than a re-lex.
;; ============================================================================
DEF_FUNC par_peek_second_is_for, 8
    push rbx
    push r12
    push r13
    mov rbx, rdi
    mov r12d, [rdi + Comp.tok_idx]
    mov r13, [rdi + Comp.tokens + Buf.len]
    xor ecx, ecx                        ; bracket depth
    ; Start AT the current token, not after it: `([x for x in y], z)` opens a
    ; bracket first, and skipping it would leave the inner `for` looking like a
    ; top-level one.
.loop:
    cmp r12, r13
    jae .no
    mov rax, [rbx + Comp.tokens + Buf.data]
    mov rdx, r12
    shl rdx, TOKEN_SHIFT
    movzx eax, word [rax + rdx + Token.kind]
    cmp eax, TOK_ENDMARKER
    je .no
    cmp eax, TOK_LPAR
    je .deeper
    cmp eax, TOK_LSQB
    je .deeper
    cmp eax, TOK_LBRACE
    je .deeper
    cmp eax, TOK_RPAR
    je .shallower
    cmp eax, TOK_RSQB
    je .shallower
    cmp eax, TOK_RBRACE
    je .shallower
    test ecx, ecx
    jnz .next                           ; inside brackets: keep scanning
    cmp eax, TOK_FOR
    je .yes
    cmp eax, TOK_COMMA
    je .no                              ; a tuple, not a comprehension
    jmp .next
.deeper:
    inc ecx
    jmp .next
.shallower:
    test ecx, ecx
    jz .no                              ; the closing bracket of our own run
    dec ecx
.next:
    inc r12
    jmp .loop
.yes:
    mov eax, 1
    jmp .ret
.no:
    xor eax, eax
.ret:
    pop r13
    pop r12
    pop rbx
    leave
    ret
END_FUNC par_peek_second_is_for

;; ============================================================================
;; pf_yield(Comp *c) -> node   -- `yield`, `yield x`, `yield from x`
;;
;; A yield is an expression, not a statement: `x = yield v` is how a generator
;; receives what send() passes back.
;; ============================================================================
PY_LINE  equ 8
PY_KIND  equ 16
PY_FIRST equ 24
PY_MARK  equ 32
PY_FRAME equ 40          ; + 1 push = 48
DEF_FUNC_LOCAL pf_yield, PY_FRAME
    push rbx
    mov rbx, rdi
    call par_peek
    mov ecx, [rax + Token.lineno]
    mov [rbp - PY_LINE], rcx
    mov rdi, rbx
    call par_advance                    ; `yield`

    mov rdi, rbx
    call par_kind
    cmp eax, TOK_FROM
    je .from

    ; A bare `yield` ends wherever an expression would not start.
    mov qword [rbp - PY_KIND], AST_YIELD
    mov rdi, rbx
    call par_kind
    cmp eax, TOK_NEWLINE
    je .bare
    cmp eax, TOK_SEMI
    je .bare
    cmp eax, TOK_ENDMARKER
    je .bare
    cmp eax, TOK_RPAR
    je .bare
    cmp eax, TOK_RSQB
    je .bare
    cmp eax, TOK_RBRACE
    je .bare
    cmp eax, TOK_COMMA
    je .bare
    cmp eax, TOK_COLON
    je .bare
    cmp eax, TOK_DEDENT
    je .bare

    mov rdi, rbx
    mov esi, BP_WALRUS
    call par_expr
    test rax, rax
    jz .fail
    mov [rbp - PY_FIRST], rax

    ; `yield a, b` yields the tuple, the way a bare `a, b` builds one.
    mov rdi, rbx
    call par_kind
    cmp eax, TOK_COMMA
    jne .one
    mov rdi, rbx
    call ast_mark
    mov [rbp - PY_MARK], rax
    mov rdi, rbx
    mov rsi, [rbp - PY_FIRST]
    call ast_push
.tuple_loop:
    mov rdi, rbx
    call par_advance                    ; the comma
    mov rdi, rbx
    call par_kind
    ; A trailing comma ends it: `yield 1,` is a one-tuple.
    cmp eax, TOK_NEWLINE
    je .tuple_done
    cmp eax, TOK_ENDMARKER
    je .tuple_done
    cmp eax, TOK_SEMI
    je .tuple_done
    cmp eax, TOK_RPAR
    je .tuple_done
    cmp eax, TOK_RSQB
    je .tuple_done
    cmp eax, TOK_RBRACE
    je .tuple_done
    cmp eax, TOK_DEDENT
    je .tuple_done
    mov rdi, rbx
    mov esi, BP_WALRUS
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
    mov rdx, [rbp - PY_LINE]
    mov rcx, [rbp - PY_MARK]
    call par_finish_list
    test rax, rax
    jz .fail
    mov [rbp - PY_FIRST], rax
.one:
    mov r8, [rbp - PY_FIRST]
    jmp .build
.bare:
    xor r8d, r8d
    jmp .build

.from:
    mov rdi, rbx
    call par_advance
    mov qword [rbp - PY_KIND], AST_YIELDFROM
    mov rdi, rbx
    mov esi, BP_WALRUS
    call par_expr
    test rax, rax
    jz .fail
    mov r8, rax

.build:
    mov rdi, rbx
    mov rsi, [rbp - PY_KIND]
    xor edx, edx
    mov rcx, [rbp - PY_LINE]
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
END_FUNC pf_yield

;; ============================================================================
;; pf_await(Comp *c) -> node   -- `await x`
;;
;; The operand is a *primary*, not a unary expression: the grammar is
;; `await_primary: AWAIT primary`.  Recursing at BP_AWAIT gets that from the
;; table alone -- calls, subscripts and attributes (BP_POSTFIX) bind into the
;; operand, while `**` (BP_POWER, just below) does not, so `await f() ** 2` is
;; `(await f()) ** 2` and `await -x` is rejected.
;; ============================================================================
PAW_LINE  equ 8
PAW_FRAME equ 16          ; + 1 push = 24
DEF_FUNC_LOCAL pf_await, PAW_FRAME
    push rbx
    mov rbx, rdi
    call par_peek
    mov ecx, [rax + Token.lineno]
    mov [rbp - PAW_LINE], rcx
    mov rdi, rbx
    call par_advance                    ; `await`

    mov rdi, rbx
    mov esi, BP_AWAIT
    call par_expr
    test rax, rax
    jz .fail
    mov r8, rax

    mov rdi, rbx
    mov esi, AST_AWAIT
    xor edx, edx
    mov rcx, [rbp - PAW_LINE]
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
END_FUNC pf_await

;; ============================================================================
;; in_walrus(Comp *c, node left) -> node   -- `name := value`
;;
;; Only a plain name may be assigned to.  `(a.b := 1)` and `(a[0] := 1)` are
;; syntax errors in CPython too, which is why the check is here rather than in
;; the code generator: there is nothing sensible to emit for them.
;; ============================================================================
IW_LINE  equ 8
IW_LEFT  equ 16
IW_FRAME equ 24           ; + 1 push = 32
DEF_FUNC_LOCAL in_walrus, IW_FRAME
    push rbx
    mov rbx, rdi
    mov [rbp - IW_LEFT], rsi
    call par_peek
    mov ecx, [rax + Token.lineno]
    mov [rbp - IW_LINE], rcx

    mov rdi, rbx
    mov rsi, [rbp - IW_LEFT]
    call ast_at
    movzx ecx, byte [rax + AstNode.kind]
    cmp ecx, AST_NAME
    jne .bad_target
    mov byte [rax + AstNode.subkind], CTX_STORE

    mov rdi, rbx
    call par_advance                    ; `:=`
    mov rdi, rbx
    mov esi, BP_LAMBDA
    call par_expr
    test rax, rax
    jz .fail
    mov r9, rax
    mov rdi, rbx
    mov esi, AST_NAMEDEXPR
    xor edx, edx
    mov rcx, [rbp - IW_LINE]
    mov r8, [rbp - IW_LEFT]
    call ast_make
    pop rbx
    leave
    ret
.bad_target:
    mov rdi, rbx
    CSTRING rsi, "cannot use ':=' with that target"
    call par_syntax_error
.fail:
    xor eax, eax
    pop rbx
    leave
    ret
END_FUNC in_walrus

section .rodata

;; ---------------------------------------------------------------------------
;; prule_table - the expression grammar, one row per token kind.
;;
;; GENERATED.  Edit ROWS in compiler/gen_prule.py and re-run it.
;;
;; Reading a row: `prefix` runs when the token starts an expression, `infix`
;; when it follows one.  lbp is how tightly the token binds to what is already
;; parsed -- 0 means it is not an infix operator and therefore ends the
;; expression.  rbp is the minimum its handler recurses at, which is what
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
    dq pf_string   , 0           
    db BP_NONE    , BP_NONE    , 0                 , 0           ; TOK_STRING -- consumes a whole run: adjacent literals concatenate
    dd 0
    dq 0           , 0           
    db BP_NONE    , BP_NONE    , 0                 , 0           ; TOK_FSTRING
    dd 0
    dq pf_group    , in_call     
    db BP_POSTFIX , BP_POSTFIX , 0                 , 0           ; TOK_LPAR -- group or tuple; as an infix, a call
    dd 0
    dq 0           , 0           
    db BP_NONE    , BP_NONE    , 0                 , 0           ; TOK_RPAR
    dd 0
    dq pf_list     , in_subscript
    db BP_POSTFIX , BP_POSTFIX , 0                 , 0           ; TOK_LSQB
    dd 0
    dq 0           , 0           
    db BP_NONE    , BP_NONE    , 0                 , 0           ; TOK_RSQB
    dd 0
    dq pf_dictset  , 0           
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
    dq 0           , in_attr     
    db BP_POSTFIX , BP_POSTFIX , 0                 , 0           ; TOK_DOT
    dd 0
    dq pf_const    , 0           
    db BP_NONE    , BP_NONE    , 0                 , 0           ; TOK_ELLIPSIS
    dd 0
    dq pf_unary    , in_binop    
    db BP_ARITH   , BP_ARITH   , NB_ADD            , 0           ; TOK_PLUS -- aux is the BINARY op; pf_unary reads the token, not aux
    dd 0
    dq pf_unary    , in_binop    
    db BP_ARITH   , BP_ARITH   , NB_SUBTRACT       , 0           ; TOK_MINUS
    dd 0
    dq pf_starred  , in_binop    
    db BP_TERM    , BP_TERM    , NB_MULTIPLY       , 0           ; TOK_STAR
    dd 0
    dq pf_starred  , in_binop    
    db BP_POWER   , BP_UNARY   , NB_POWER          , 0           ; TOK_DOUBLESTAR -- rbp one level BELOW lbp: right-associative, and its RHS takes a unary
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
    dq 0           , in_walrus   
    db BP_WALRUS  , BP_LAMBDA  , 0                 , 0           ; TOK_COLONEQUAL -- right-associative, and its RHS may be a lambda but not a ternary
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
    dq pf_await    , 0           
    db BP_NONE    , BP_AWAIT   , 0                 , 0           ; TOK_AWAIT -- operand is a primary: BP_AWAIT sits between `**` and postfix
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
    dq pf_lambda   , 0           
    db BP_NONE    , BP_NONE    , 0                 , 0           ; TOK_LAMBDA -- body one level below the ternary: `lambda: a, b` is still a tuple
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
    dq pf_yield    , 0           
    db BP_NONE    , BP_NONE    , 0                 , 0           ; TOK_YIELD -- an expression, not a statement: `x = yield v` receives from send()
    dd 0

ASM_INIT
