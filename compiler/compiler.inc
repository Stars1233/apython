; compiler.inc - Shared definitions for the Python source compiler
;
; The compiler turns Python 3.12 source into a PyCodeObject that apython's own
; eval loop runs.  Pipeline:
;
;   source -> lex.asm    -> Token[]      (whole file, materialized)
;          -> parse.asm  -> AstNode[]    (32-byte nodes, u32 indices)
;          -> symtab.asm -> Scope[]      (bindings classified local/global/cell/free)
;          -> codegen.asm-> Instr[]      (flat, labels unresolved)
;          -> assemble.asm-> PyCodeObject
;
; THE CARDINAL RULE OF THIS SUBSYSTEM: no function here may call
; raise_exception.  That function tail-jumps into eval_exception_unwind, which
; calls fatal_error when there is no live interpreter frame -- and compiling
; `./apython foo.py` from main.asm has no frame at all.  Errors are reported
; with comp_error(), which records the exception and RETURNS; callers propagate
; 0/NULL upward until compile_source() tears down its arenas and hands the
; pending exception to its caller.

%ifndef COMPILER_INC
%define COMPILER_INC

;; ===========================================================================
;; Token kinds.  Dense and below 128 so the Pratt table can be a flat array
;; indexed by kind.
;; ===========================================================================

TOK_ENDMARKER   equ 0
TOK_NEWLINE     equ 1
TOK_INDENT      equ 2
TOK_DEDENT      equ 3
TOK_NAME        equ 4
TOK_NUMBER      equ 5
TOK_STRING      equ 6
TOK_FSTRING     equ 7

; Delimiters and operators.  Order is not significant, density is.
TOK_LPAR             equ 8    ; (
TOK_RPAR             equ 9    ; )
TOK_LSQB             equ 10   ; [
TOK_RSQB             equ 11   ; ]
TOK_LBRACE           equ 12   ; {
TOK_RBRACE           equ 13   ; }
TOK_COLON            equ 14   ; :
TOK_COMMA            equ 15   ; ,
TOK_SEMI             equ 16   ; ;
TOK_DOT              equ 17   ; .
TOK_ELLIPSIS         equ 18   ; ...
TOK_PLUS             equ 19   ; +
TOK_MINUS            equ 20   ; -
TOK_STAR             equ 21   ; *
TOK_DOUBLESTAR       equ 22   ; **
TOK_SLASH            equ 23   ; /
TOK_DOUBLESLASH      equ 24   ; //
TOK_PERCENT          equ 25   ; %
TOK_AT               equ 26   ; @
TOK_VBAR             equ 27   ; |
TOK_AMPER            equ 28   ; &
TOK_CIRCUMFLEX       equ 29   ; ^
TOK_TILDE            equ 30   ; ~
TOK_LEFTSHIFT        equ 31   ; <<
TOK_RIGHTSHIFT       equ 32   ; >>
TOK_LESS             equ 33   ; <
TOK_GREATER          equ 34   ; >
TOK_LESSEQUAL        equ 35   ; <=
TOK_GREATEREQUAL     equ 36   ; >=
TOK_EQEQUAL          equ 37   ; ==
TOK_NOTEQUAL         equ 38   ; !=
TOK_EQUAL            equ 39   ; =
TOK_COLONEQUAL       equ 40   ; :=
TOK_RARROW           equ 41   ; ->
TOK_PLUSEQUAL        equ 42   ; +=
TOK_MINEQUAL         equ 43   ; -=
TOK_STAREQUAL        equ 44   ; *=
TOK_DOUBLESTAREQUAL  equ 45   ; **=
TOK_SLASHEQUAL       equ 46   ; /=
TOK_DOUBLESLASHEQUAL equ 47   ; //=
TOK_PERCENTEQUAL     equ 48   ; %=
TOK_ATEQUAL          equ 49   ; @=
TOK_VBAREQUAL        equ 50   ; |=
TOK_AMPEREQUAL       equ 51   ; &=
TOK_CIRCUMFLEXEQUAL  equ 52   ; ^=
TOK_LEFTSHIFTEQUAL   equ 53   ; <<=
TOK_RIGHTSHIFTEQUAL  equ 54   ; >>=

; Keywords.  TOK_KW_FIRST..TOK_KW_LAST is a contiguous run so the lexer can
; test "is this token a keyword" with two compares.
TOK_KW_FIRST    equ 55
TOK_FALSE       equ 55
TOK_NONE        equ 56
TOK_TRUE        equ 57
TOK_AND         equ 58
TOK_AS          equ 59
TOK_ASSERT      equ 60
TOK_ASYNC       equ 61
TOK_AWAIT       equ 62
TOK_BREAK       equ 63
TOK_CLASS       equ 64
TOK_CONTINUE    equ 65
TOK_DEF         equ 66
TOK_DEL         equ 67
TOK_ELIF        equ 68
TOK_ELSE        equ 69
TOK_EXCEPT      equ 70
TOK_FINALLY     equ 71
TOK_FOR         equ 72
TOK_FROM        equ 73
TOK_GLOBAL      equ 74
TOK_IF          equ 75
TOK_IMPORT      equ 76
TOK_IN          equ 77
TOK_IS          equ 78
TOK_LAMBDA      equ 79
TOK_NONLOCAL    equ 80
TOK_NOT         equ 81
TOK_OR          equ 82
TOK_PASS        equ 83
TOK_RAISE       equ 84
TOK_RETURN      equ 85
TOK_TRY         equ 86
TOK_WHILE       equ 87
TOK_WITH        equ 88
TOK_YIELD       equ 89
TOK_KW_LAST     equ 89

TOK_COUNT       equ 90

; Token.flags bits
TF_SOFT_MATCH   equ 0x0001   ; the NAME is literally "match"
TF_SOFT_CASE    equ 0x0002   ; ... "case"
TF_SOFT_TYPE    equ 0x0004   ; ... "type"
TF_SOFT_UNDER   equ 0x0008   ; ... "_"
TF_STR_RAW      equ 0x0010   ; string prefix r
TF_STR_BYTES    equ 0x0020   ; string prefix b
TF_STR_FMT      equ 0x0040   ; string prefix f
TF_NUM_FLOAT    equ 0x0080   ; NUMBER has a fractional part or exponent
TF_NUM_IMAG     equ 0x0100   ; NUMBER has a j suffix

struc Token
    .kind:   resw 1  ; +0
    .flags:  resw 1  ; +2
    .lineno: resd 1  ; +4   1-based
    .start:  resq 1  ; +8   pointer into the source buffer
    .len:    resd 1  ; +16  bytes
    .col:    resd 1  ; +20  0-based column within the line
    .val:    resq 1  ; +24  reserved: materialized literal, or 0
endstruc                 ;  32 -> index by shl 5
TOKEN_SHIFT equ 5

;; ===========================================================================
;; Character classes.  One 256-byte table drives every scanning decision in
;; the lexer.
;; ===========================================================================
CC_SPACE   equ 0x01   ; space, tab, formfeed, vertical tab -- not newline
CC_NEWLINE equ 0x02   ; \n \r
CC_DIGIT   equ 0x04
CC_IDSTART equ 0x08   ; A-Za-z_ and 0x80..0xFF (permissive UTF-8 lead byte)
CC_IDCONT  equ 0x10   ; IDSTART or DIGIT
CC_QUOTE   equ 0x20   ; ' "
CC_OPSTART equ 0x40   ; first byte of some operator or delimiter
CC_HEX     equ 0x80

;; ===========================================================================
;; AST.  Fixed 32-byte nodes in a bump arena, addressed by u32 index.  Node 0
;; is the reserved null node, so 0 doubles as "absent".
;;
;; Variable-arity children live in a separate u32 child-list arena: .clist is
;; the offset, .nchild the count.  They are built through a LIFO pending stack
;; (ast_mark / ast_push / ast_commit), because nested constructs are under
;; construction simultaneously -- f(a, [b, c], d) has three lists open at once.
;;
;; Anything that needs a PyObject (a constant, an identifier) stores a u32
;; index into comp.objs, a Buf of owned references.  That keeps nodes 32 bytes
;; and gives the error path a single place to release every object the parse
;; produced.
;; ===========================================================================

AST_NULL        equ 0
; --- expressions ---
AST_CONST       equ 1    ; a=obj index
AST_NAME        equ 2    ; a=obj index (identifier), subkind=ctx
AST_BINOP       equ 3    ; a=left b=right subkind=NB_*
AST_UNARYOP     equ 4    ; a=operand subkind=UOP_*
AST_BOOLOP      equ 5    ; clist=operands subkind=0 and / 1 or
AST_COMPARE     equ 6    ; a=left, clist=(op, node) pairs, nchild=pair count
AST_IFEXP       equ 7    ; a=test b=body c=orelse
AST_LAMBDA      equ 8    ; a=args b=body
AST_TUPLE       equ 9    ; clist, subkind=ctx
AST_LIST        equ 10   ; clist, subkind=ctx
AST_SET         equ 11   ; clist
AST_DICT        equ 12   ; clist=(key, value) pairs
AST_CALL        equ 13   ; a=func, clist=args, b=keyword list node
AST_ATTRIBUTE   equ 14   ; a=value b=obj index (name) subkind=ctx
AST_SUBSCRIPT   equ 15   ; a=value b=index subkind=ctx
AST_SLICE       equ 16   ; a=lower b=upper c=step
AST_STARRED     equ 17   ; a=value
AST_DOUBLESTARRED equ 18 ; a=value (only inside a call or dict display)
AST_KEYWORD     equ 19   ; a=obj index (name, 0 for **) b=value
AST_NAMEDEXPR   equ 20   ; a=target b=value  (walrus)
AST_YIELD       equ 21   ; a=value or 0
AST_YIELDFROM   equ 22   ; a=value
AST_AWAIT       equ 23   ; a=value
AST_JOINEDSTR   equ 24   ; clist=pieces
AST_FORMATTEDVALUE equ 25; a=value b=spec subkind=conversion
AST_LISTCOMP    equ 26   ; a=element, clist=comprehension clauses
AST_SETCOMP     equ 27
AST_DICTCOMP    equ 28   ; a=key b=value, clist=clauses
AST_GENEXP      equ 29
AST_COMPREHENSION equ 30 ; a=target b=iter, clist=conditions, subkind=is_async

; --- statements ---
AST_MODULE      equ 40   ; clist=body
AST_EXPRESSION  equ 41   ; a=expr        (the "eval" mode root)
AST_EXPR_STMT   equ 42   ; a=expr
AST_ASSIGN      equ 43   ; clist=targets b=value
AST_AUGASSIGN   equ 44   ; a=target b=value subkind=NB_*
AST_ANNASSIGN   equ 45   ; a=target b=annotation c=value
AST_IF          equ 46   ; a=test, clist=body, b=orelse block node
AST_WHILE       equ 47   ; a=test, clist=body, b=orelse
AST_FOR         equ 48   ; a=target b=iter, clist=body, c=orelse, subkind=is_async
AST_BLOCK       equ 49   ; clist=statements  (an else/elif tail)
AST_PASS        equ 50
AST_BREAK       equ 51
AST_CONTINUE    equ 52
AST_RETURN      equ 53   ; a=value or 0
AST_DELETE      equ 54   ; clist=targets
AST_RAISE       equ 55   ; a=exc b=cause
AST_ASSERT      equ 56   ; a=test b=msg
AST_GLOBAL      equ 57   ; clist=obj indices
AST_NONLOCAL    equ 58   ; clist=obj indices
AST_IMPORT      equ 59   ; clist=alias nodes
AST_IMPORTFROM  equ 60   ; a=obj index (module), clist=aliases, subkind=level
AST_ALIAS       equ 61   ; a=obj index (name) b=obj index (asname or 0)
AST_FUNCTIONDEF equ 62   ; a=obj index (name) b=args, clist=body, c=extra node
AST_CLASSDEF    equ 63   ; a=obj index (name) b=bases call, clist=body, c=extra
AST_TRY         equ 64   ; clist=body, a=handlers block, b=orelse, c=finalbody
AST_HANDLER     equ 65   ; a=type b=obj index (name), clist=body
AST_WITH        equ 66   ; clist=items, a=body block, subkind=is_async
AST_WITHITEM    equ 67   ; a=context b=optional vars
AST_ARGUMENTS   equ 68   ; clist=arg nodes, a=extra node
AST_ARG         equ 69   ; a=obj index (name) b=annotation c=default
AST_MATCH       equ 70   ; a=subject, clist=cases
AST_EXTRA       equ 71   ; raw payload continuation for nodes needing >5 slots
AST_DECORATED   equ 72   ; clist=decorators, a=the def or class they wrap

; --- match ---
; A pattern is not an expression: `case x` binds x rather than reading it, and
; `case C(a)` calls nothing.  They get their own node kinds for that reason --
; sharing AST_NAME and AST_CALL would make every consumer ask which it was.
AST_CASE        equ 73   ; a=pattern b=guard, clist=body
AST_PAT_VALUE   equ 74   ; a=expr; subkind 1 = compare by identity (None/True/False)
AST_PAT_CAPTURE equ 75   ; a=obj index (name), 0 for the `_` wildcard
AST_PAT_SEQUENCE equ 76  ; clist=sub-patterns, b=1+index of the starred one, 0 if none
AST_PAT_MAPPING equ 77   ; clist=key expr / value pattern pairs, b=obj index of **rest
AST_PAT_CLASS   equ 78   ; a=class expr, clist=positional then keyword patterns,
                         ; b=how many are positional
AST_PAT_KEYWORD equ 79   ; a=obj index (name) b=pattern
AST_PAT_OR      equ 80   ; clist=alternatives
AST_PAT_AS      equ 81   ; a=pattern (0 = wildcard) b=obj index (name)

AST_COUNT       equ 88

; AstNode.subkind for expression context
CTX_LOAD  equ 0
CTX_STORE equ 1
CTX_DEL   equ 2

; AstNode.subkind for AST_UNARYOP
UOP_POS    equ 0   ; +x
UOP_NEG    equ 1   ; -x
UOP_INVERT equ 2   ; ~x
UOP_NOT    equ 3   ; not x

; AstNode.subkind for AST_BOOLOP
BOOL_AND equ 0
BOOL_OR  equ 1

; Comparison operators, as stored in an AST_COMPARE child list.  The first six
; match CPython's COMPARE_OP codes so codegen can shift them straight into the
; oparg; the last four are separate opcodes (IS_OP and CONTAINS_OP) rather than
; comparisons, which is why the enum has to be wider than CMP_*.
CMPOP_LT    equ 0
CMPOP_LE    equ 1
CMPOP_EQ    equ 2
CMPOP_NE    equ 3
CMPOP_GT    equ 4
CMPOP_GE    equ 5
CMPOP_IN    equ 6
CMPOP_NOTIN equ 7
CMPOP_IS    equ 8
CMPOP_ISNOT equ 9

struc AstNode
    .kind:    resb 1  ; +0   AST_*
    .subkind: resb 1  ; +1   operator / context / async flag
    .flags:   resw 1  ; +2
    .lineno:  resd 1  ; +4
    .a:       resd 1  ; +8
    .b:       resd 1  ; +12
    .c:       resd 1  ; +16
    .clist:   resd 1  ; +20  offset into the child-list arena
    .nchild:  resd 1  ; +24
    .col:     resd 1  ; +28
endstruc                  ; 32 -> index by shl 5
AST_SHIFT equ 5

;; ===========================================================================
;; Buf - the growable array this codebase otherwise lacks.  Used for the token
;; array, the child-list arena, the instruction stream, and all three byte
;; emitters.  ap_malloc/ap_realloc are fatal on OOM, so no call here can fail.
;; ===========================================================================
struc Buf
    .data:  resq 1  ; +0
    .len:   resq 1  ; +8   elements in use
    .cap:   resq 1  ; +16  elements allocated
    .elsz:  resq 1  ; +24  element size in bytes
endstruc                ; 32
BUF_MIN_CAP equ 64

;; ===========================================================================
;; Arena - bump allocation, released wholesale.  A compilation is a bounded
;; episode, so the AST and the symbol table never need individual frees.  The
;; exception is anything holding a PyObject: those live in comp.objs and are
;; refcounted.
;; ===========================================================================
struc Arena
    .cur:    resq 1  ; +0   next free byte
    .end:    resq 1  ; +8   end of the current chunk
    .chunks: resq 1  ; +16  chunk list head: [next][size][payload...]
    .total:  resq 1  ; +24  high-water mark, for --selftest-compile
endstruc                 ; 32
ARENA_CHUNK equ 65536

;; ===========================================================================
;; Instr - one emitted instruction, before assembly.  Never a CACHE and never
;; an EXTENDED_ARG: both are synthesized by the writer from the opcode metadata
;; table and the resolved oparg, so no emitter can forget them.
;; ===========================================================================
struc Instr
    .opcode:  resb 1  ; +0
    .flags:   resb 1  ; +1   IF_*
    .handler: resw 1  ; +2   Handler index + 1; 0 = not inside a try
    .oparg:   resd 1  ; +4   literal oparg, or a label id when IF_LABELARG
    .line:    resd 1  ; +8   source line; 0 = no location
    .offset:  resd 1  ; +12  code-unit offset, filled by the sizing fixpoint
endstruc                  ; 16 -> index by shl 4
INSTR_SHIFT equ 4

IF_LABELARG  equ 0x01   ; .oparg names a label, resolve it to a delta
IF_JREL_FWD  equ 0x02   ; delta = target - (offset + size)
IF_JREL_BACK equ 0x04   ; delta = (offset + size) - target
IF_NOFALL    equ 0x08   ; control does not reach the next instruction
IF_NOLINE    equ 0x10   ; emit "no location" in the line table

;; ===========================================================================
;; Opcode metadata.  One table drives four things: CACHE padding, instruction
;; sizing, stack-depth accounting, and successor computation.  Routing every
;; emission through it is what makes "I forgot the caches on LOAD_SUPER_ATTR"
;; structurally impossible rather than a per-call-site discipline.
;; ===========================================================================
struc OpMeta
    .cache:  resb 1  ; +0  trailing CACHE words
    .effect: resb 1  ; +1  signed net stack effect, or SE_VAR
    .flags:  resb 1  ; +2  OM_*
    .jeff:   resb 1  ; +3  signed net effect along the taken jump edge
endstruc                 ; 4 -> index by shl 2

SE_VAR       equ 0x80   ; effect depends on the oparg: call comp_effect_var
OM_HASARG    equ 0x01
OM_JUMP      equ 0x02   ; has a jump edge (oparg names a target)
OM_JUMPBACK  equ 0x04   ; the jump edge runs backward
OM_NOFALL    equ 0x08   ; control does not fall through

;; ===========================================================================
;; Compiler error codes.  Every compiler function returns 0/NULL on failure
;; with comp.err_set non-zero; nothing here raises.
;; ===========================================================================
struc CompErr
    .type:   resq 1  ; +0  exception type object
    .msg:    resq 1  ; +8  const char*
    .lineno: resd 1  ; +16
    .col:    resd 1  ; +20
    .set:    resd 1  ; +24  non-zero once an error is recorded
    .pad:    resd 1  ; +28
endstruc

; Compilation modes, matching CPython's start symbols.
CMODE_EXEC   equ 0
CMODE_EVAL   equ 1
CMODE_SINGLE equ 2

; The parser's own recursion guard.  apython's recursion_depth counts Python
; frames only, so deeply nested parentheses would otherwise blow the machine
; stack before anything noticed.
COMP_MAX_DEPTH equ 200

;; ===========================================================================
;; Pratt binding powers.  Higher binds tighter; a handler that recurses at its
;; own power is left-associative, one below is right-associative.
;; ===========================================================================
BP_NONE     equ 0
BP_LAMBDA   equ 2
BP_WALRUS   equ 3
BP_TERNARY  equ 4
BP_OR       equ 6
BP_AND      equ 8
BP_NOT      equ 10
BP_COMPARE  equ 12
; A starred element's operand: an or_expr, so `|` is absorbed and `in` is not.
; It sits between the two rather than on either, because the driver continues
; only while lbp > min_bp.
BP_STAROP   equ 13
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


;; ===========================================================================
;; Lexer state.  One pass over the source fills comp.tokens; the parser then
;; walks a plain array with a u32 cursor, so backtracking -- which Python needs
;; for `with (a as x, b as y)`, `def f[T](...)`, and starred-target validation
;; -- is an index reset rather than a snapshot of all of this.
;; ===========================================================================
LEX_MAX_INDENT equ 100
LEX_TABSIZE    equ 8

struc Lexer
    .cur:          resq 1   ; +0   read cursor into comp.src
    .end:          resq 1   ; +8   one past the last source byte
    .line_start:   resq 1   ; +16  first byte of the current line, for columns
    .lineno:       resd 1   ; +24  1-based
    .paren_depth:  resd 1   ; +28  suppresses NEWLINE and INDENT while > 0
    .indent_top:   resd 1   ; +32  index of the live entry in .indents
    .atbol:        resd 1   ; +36  at the beginning of a logical line
    .indents:      resd LEX_MAX_INDENT  ; +40  column stack, .indents[0] == 0
endstruc

;; ===========================================================================
;; Comp - the whole state of one compilation.
;;
;; Everything indexed is a Buf, never an Arena: nodes and child lists are
;; addressed by u32 index and the backing store is reallocated as it grows, so
;; an index stays valid where a pointer would not.
;;
;; .objs is the one place refcounts live.  Every PyObject the front end
;; produces -- interned identifiers, literal constants -- is appended there and
;; referenced from an AstNode by index, which keeps nodes at 32 bytes and gives
;; the error path a single loop that releases everything the parse allocated.
;; ===========================================================================
struc Comp
    .src:       resq 1              ; +0   owned, NUL-padded normalized source
    .srclen:    resq 1              ; +8
    .filename:  resq 1              ; +16  PyStrObject*, owned
    .mode:      resd 1              ; +24  CMODE_*
    .depth:     resd 1              ; +28  parser recursion depth
    .tok_idx:   resd 1              ; +32  parser cursor into .tokens
    .root:      resd 1              ; +36  AST index of the parse root
    .tokens:    resb Buf_size       ; +40  Token[]
    .nodes:     resb Buf_size       ; AstNode[]; index 0 is the null node
    .children:  resb Buf_size       ; u32[] committed child lists
    .pending:   resb Buf_size       ; u32[] LIFO scratch for lists under construction
    .objs:      resb Buf_size       ; PyObject*[], owned
    .scopes:    resb Buf_size       ; Scope[]; index 0 reserved
    .cur_unit:  resq 1              ; the CompUnit being emitted into
    .cur_scope: resd 1
    .pad1:      resd 1
    .err:       resb CompErr_size
    .lex:       resb Lexer_size
endstruc

;; ===========================================================================
;; CompUnit - the code object under construction for one scope.
;;
;; Instructions are a flat array with symbolic labels, not a control-flow
;; graph.  Fidelity here is semantic rather than byte-for-byte, so CPython's
;; peephole and dead-code passes have nothing to do, and stack depth works as
;; a worklist over the flat array treating labels as join points.  That single
;; decision is the largest saving against CPython's flowgraph.c.
;;
;; .consts and .names hold BORROWED references: every literal already lives in
;; comp.objs, which owns it.  The tuples built at assembly time take their own.
;; ===========================================================================
;; A protected region under construction.  Handlers form a tree through
;; .parent; every instruction is stamped with the innermost one that covers it,
;; and the ranges are recovered afterwards by run-length encoding that stamp.
;; Computing ranges that way rather than from a stack of open regions is what
;; makes them come out disjoint and sorted by construction -- which matters,
;; because exc_table_find_handler scans linearly and takes the FIRST match, so
;; overlapping entries would silently pick the wrong handler.
struc Handler
    .target:  resd 1          ; label the handler starts at
    .lasti:   resd 1          ; 0 or 1: does the unwinder push the offset?
    .depth:   resd 1          ; value-stack depth to truncate to; -1 until the
                              ; depth pass runs
    .parent:  resd 1          ; enclosing handler index, -1 = none
    .open:    resd 1          ; instruction index where the region was opened
    .bias:    resd 1          ; items live at `open` that the handler unwinds
                              ; away: a `with` opens its region with the
                              ; enter-result still on the stack, so the depth
                              ; read there is one above what the handler wants
endstruc                      ; 24

;; A loop being generated: where `break` and `continue` go, and how much the
;; value stack has to be unwound to get there.  A for loop leaves its iterator
;; on the stack across the body, so `break` must drop it; `continue` jumps back
;; to the FOR_ITER, which still wants it.
struc LoopFrame
    .brk:    resd 1          ; label for `break`
    .cont:   resd 1          ; label for `continue`
    .npop:   resd 1          ; stack items `break` must pop first
    .fdepth: resd 1          ; height of the finally stack at loop entry, so
                             ; break and continue unwind only what they leave
endstruc                     ; 16

struc CompUnit
    .instrs:    resb Buf_size   ; Instr[]
    .loops:     resb Buf_size   ; LoopFrame[], innermost last
    .handlers:  resb Buf_size   ; Handler[]
    .finallys:  resb Buf_size   ; per enclosing block: (handler << 32) | node
    .cur_handler: resd 1        ; index into .handlers, -1 = unprotected
    .pad4:      resd 1
    .labels:    resb Buf_size   ; int32[]: label id -> instruction index, -1 unbound
    .consts:    resb Buf_size   ; Value[]   (borrowed)
    .names:     resb Buf_size   ; PyStrObject*[] (borrowed)
    .prefix:    resb Buf_size   ; uint8[]: EXTENDED_ARG words per instruction
    .flags:     resd 1
    .argcount:  resd 1
    .posonly:   resd 1
    .kwonly:    resd 1
    .nlocals:   resd 1
    .stacksize: resd 1
    .firstline: resd 1
    .curline:   resd 1
    .name:      resq 1          ; PyStrObject*, borrowed
    .qualname:  resq 1
    .filename:  resq 1
    .comp:      resq 1          ; the owning Comp, for scope lookups
    .scope:     resd 1          ; this unit's Scope index
    .pad3:      resd 1
endstruc

; co_stacksize is emitted with this much slack.  frame_free recomputes a
; frame's size from co_nlocalsplus + co_stacksize and hands it back to the
; frame pool, so an over-estimate costs a few bytes per frame while an
; under-estimate corrupts the pool's free list and crashes somewhere unrelated.
; The margin makes a single-slot modelling error in a rarely-exercised opcode
; harmless rather than fatal; it comes out once the depth pass is trusted.
COMP_STACK_SLACK equ 4

;; ===========================================================================
;; Symbol table.
;;
;; One Scope per binding block.  Two passes: the first records what each block
;; binds and uses, the second classifies every name as local, cell, free,
;; explicit global or implicit global.  The classification is what decides
;; which opcode a name compiles to, and the rule that matters most is a
;; negative one: module and class blocks are NOT function-like, so nothing in
;; them is ever a fast local.  Every name there goes through LOAD_NAME against
;; the frame's locals mapping -- which is exactly the dict exec(src, d) passes,
;; and the reason that call can hand results back.
;;
;; Scopes live in a Buf and are addressed by index; index 0 is reserved so that
;; a 0 parent means "no enclosing scope".
;; ===========================================================================

SCOPE_MODULE   equ 0
SCOPE_CLASS    equ 1
SCOPE_FUNCTION equ 2
SCOPE_LAMBDA   equ 3
SCOPE_COMP     equ 4

; What a block does with a name.  Mirrors CPython's symtable flags closely
; enough that its analyze_name rules can be followed step for step.
DEF_GLOBAL   equ 0x001
DEF_LOCAL    equ 0x002
DEF_PARAM    equ 0x004
DEF_NONLOCAL equ 0x008
DEF_IMPORT   equ 0x010
DEF_USE      equ 0x020
; The name is unbound somewhere in this block -- an explicit `del`, or the
; implicit one at the end of an `except E as e` clause.  A local that can be
; unbound has to be read with LOAD_FAST_CHECK, or reading it after the delete
; returns whatever was in the slot instead of raising.
DEF_UNBOUND  equ 0x040

; The resolved answer, stored in bits 12..14.
SCOPE_SHIFT      equ 12
SCOPE_MASK       equ 0x7000
SYM_LOCAL           equ 1
SYM_GLOBAL_EXPLICIT equ 2
SYM_GLOBAL_IMPLICIT equ 3
SYM_FREE            equ 4
SYM_CELL            equ 5

; Scope.flags
SCF_GENERATOR equ 0x01
SCF_COROUTINE equ 0x02
SCF_VARARGS   equ 0x04
SCF_VARKW     equ 0x08
SCF_NESTED    equ 0x10

struc Scope
    .parent:    resd 1          ; scope index, 0 = none
    .kind:      resd 1          ; SCOPE_*
    .flags:     resd 1          ; SCF_*
    .node:      resd 1          ; the AST node this block belongs to
    .symbols:   resq 1          ; PyDictObject*: name -> SmallInt(flags)
    .name:      resq 1          ; PyStrObject*, borrowed from comp.objs
    .argcount:  resd 1
    .posonly:   resd 1
    .kwonly:    resd 1
    .co_flags:  resd 1
    .varnames:  resb Buf_size   ; u64 name Values, parameters first
    .cellvars:  resb Buf_size
    .freevars:  resb Buf_size
    .localsplus: resb Buf_size  ; varnames ++ new cellvars ++ freevars
    .children:  resb Buf_size   ; u32 scope indices, in source order
    .nlocals:   resd 1          ; == len(varnames)
    .ncells:    resd 1
    .nfrees:    resd 1
    .pad2:      resd 1
endstruc

%endif ; COMPILER_INC
