; compiler/prule.asm - the expression grammar
;
; GENERATED.  Edit ROWS in src/compiler/gen_prule.py and re-run it; `make
; regen` does that along with the other three generators.
;
; It lives in a file of its own because parse.asm crossed the size cap with it
; inside, and because a generated table has nothing to say to a reader of the
; parser: prule_table is data the driver indexes, and the only hand-written
; thing about it is the ROWS dict the generator reads.

%include "macros.inc"
%include "object.inc"
%include "opcodes.inc"
%include "compiler.inc"

; The prefix and infix handlers the rows point at.
extern in_attr
extern in_binop
extern in_boolop
extern in_call
extern in_compare
extern in_subscript
extern in_ternary
extern in_walrus
extern pf_await
extern pf_const
extern pf_dictset
extern pf_group
extern pf_lambda
extern pf_list
extern pf_name
extern pf_number
extern pf_starred
extern pf_string
extern pf_unary
extern pf_yield

section .rodata

;; ============================================================================
;; prule_table - the expression grammar, one row per token kind.
;;
;; GENERATED.  Edit ROWS in src/compiler/gen_prule.py and re-run it.
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
;; ============================================================================
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
    dq 0           , 0           
    db BP_NONE    , BP_NONE    , 0                 , 0           ; TOK_TYPE_COMMENT
    dd 0
    dq 0           , 0           
    db BP_NONE    , BP_NONE    , 0                 , 0           ; TOK_TYPE_IGNORE
    dd 0

global prule_table
