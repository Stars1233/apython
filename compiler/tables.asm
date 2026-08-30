; tables.asm - Static lookup tables for the Python source compiler
;
; GENERATED FILE.  Regenerate with:
;     python3 compiler/gen_tables.py > compiler/tables.asm
; The output is committed so that building apython never needs Python.
;
; The opcode metadata is taken from CPython 3.12.3's own opcode and dis
; modules, not transcribed by hand: one wrong CACHE count corrupts the
; instruction after it, and one wrong stack effect yields a co_stacksize that
; silently corrupts apython's frame pool.

%include "macros.inc"
%include "object.inc"
%include "opcodes.inc"
%include "compiler.inc"

section .rodata

;; ---------------------------------------------------------------------------
;; cc_table - one byte of class flags per source byte.  Every scanning decision
;; in the lexer is a load and a test against this table.
;;
;; Bytes 0x80..0xFF are marked CC_IDSTART|CC_IDCONT: a permissive UTF-8 lead.
;; That accepts a few identifiers CPython rejects (non-XID_Start code points)
;; and avoids shipping Unicode property tables.  Deliberate deviation.
;; ---------------------------------------------------------------------------
align 64
global cc_table
cc_table:
    db 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x01, 0x02, 0x01, 0x01, 0x02, 0x00, 0x00   ; 0x00
    db 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00   ; 0x10
    db 0x01, 0x40, 0x20, 0x00, 0x00, 0x40, 0x40, 0x20, 0x40, 0x40, 0x40, 0x40, 0x40, 0x40, 0x40, 0x40   ; 0x20
    db 0x94, 0x94, 0x94, 0x94, 0x94, 0x94, 0x94, 0x94, 0x94, 0x94, 0x40, 0x40, 0x40, 0x40, 0x40, 0x00   ; 0x30
    db 0x40, 0x98, 0x98, 0x98, 0x98, 0x98, 0x98, 0x18, 0x18, 0x18, 0x18, 0x18, 0x18, 0x18, 0x18, 0x18   ; 0x40
    db 0x18, 0x18, 0x18, 0x18, 0x18, 0x18, 0x18, 0x18, 0x18, 0x18, 0x18, 0x40, 0x00, 0x40, 0x40, 0x18   ; 0x50
    db 0x00, 0x98, 0x98, 0x98, 0x98, 0x98, 0x98, 0x18, 0x18, 0x18, 0x18, 0x18, 0x18, 0x18, 0x18, 0x18   ; 0x60
    db 0x18, 0x18, 0x18, 0x18, 0x18, 0x18, 0x18, 0x18, 0x18, 0x18, 0x18, 0x40, 0x40, 0x40, 0x40, 0x00   ; 0x70
    db 0x18, 0x18, 0x18, 0x18, 0x18, 0x18, 0x18, 0x18, 0x18, 0x18, 0x18, 0x18, 0x18, 0x18, 0x18, 0x18   ; 0x80
    db 0x18, 0x18, 0x18, 0x18, 0x18, 0x18, 0x18, 0x18, 0x18, 0x18, 0x18, 0x18, 0x18, 0x18, 0x18, 0x18   ; 0x90
    db 0x18, 0x18, 0x18, 0x18, 0x18, 0x18, 0x18, 0x18, 0x18, 0x18, 0x18, 0x18, 0x18, 0x18, 0x18, 0x18   ; 0xa0
    db 0x18, 0x18, 0x18, 0x18, 0x18, 0x18, 0x18, 0x18, 0x18, 0x18, 0x18, 0x18, 0x18, 0x18, 0x18, 0x18   ; 0xb0
    db 0x18, 0x18, 0x18, 0x18, 0x18, 0x18, 0x18, 0x18, 0x18, 0x18, 0x18, 0x18, 0x18, 0x18, 0x18, 0x18   ; 0xc0
    db 0x18, 0x18, 0x18, 0x18, 0x18, 0x18, 0x18, 0x18, 0x18, 0x18, 0x18, 0x18, 0x18, 0x18, 0x18, 0x18   ; 0xd0
    db 0x18, 0x18, 0x18, 0x18, 0x18, 0x18, 0x18, 0x18, 0x18, 0x18, 0x18, 0x18, 0x18, 0x18, 0x18, 0x18   ; 0xe0
    db 0x18, 0x18, 0x18, 0x18, 0x18, 0x18, 0x18, 0x18, 0x18, 0x18, 0x18, 0x18, 0x18, 0x18, 0x18, 0x18   ; 0xf0

;; ---------------------------------------------------------------------------
;; Keyword recognition.  Every Python keyword is 2..8 bytes, so an identifier
;; of length L in that range is matched with one unaligned 8-byte load, a mask
;; from kw_masks, and a compare against a packed constant.  kw_index narrows
;; the search to the entries sharing a first byte -- under two compares on
;; average, and adding a keyword is one row.
;; ---------------------------------------------------------------------------
align 8
global kw_masks
kw_masks:               ; kw_masks[L] keeps the low L bytes of an 8-byte load
    dq 0x0000000000000000   ; length 0
    dq 0x00000000000000ff   ; length 1
    dq 0x000000000000ffff   ; length 2
    dq 0x0000000000ffffff   ; length 3
    dq 0x00000000ffffffff   ; length 4
    dq 0x000000ffffffffff   ; length 5
    dq 0x0000ffffffffffff   ; length 6
    dq 0x00ffffffffffffff   ; length 7
    dq 0xffffffffffffffff   ; length 8

align 8
global kw_table
kw_table:               ; { packed text, token, length }
    dq 0x00000065736c6146
    dw TOK_FALSE             , 5
    dd 0
    dq 0x00000000656e6f4e
    dw TOK_NONE              , 4
    dd 0
    dq 0x0000000065757254
    dw TOK_TRUE              , 4
    dd 0
    dq 0x0000000000646e61
    dw TOK_AND               , 3
    dd 0
    dq 0x0000000000007361
    dw TOK_AS                , 2
    dd 0
    dq 0x0000747265737361
    dw TOK_ASSERT            , 6
    dd 0
    dq 0x000000636e797361
    dw TOK_ASYNC             , 5
    dd 0
    dq 0x0000007469617761
    dw TOK_AWAIT             , 5
    dd 0
    dq 0x0000006b61657262
    dw TOK_BREAK             , 5
    dd 0
    dq 0x0000007373616c63
    dw TOK_CLASS             , 5
    dd 0
    dq 0x65756e69746e6f63
    dw TOK_CONTINUE          , 8
    dd 0
    dq 0x0000000000666564
    dw TOK_DEF               , 3
    dd 0
    dq 0x00000000006c6564
    dw TOK_DEL               , 3
    dd 0
    dq 0x0000000066696c65
    dw TOK_ELIF              , 4
    dd 0
    dq 0x0000000065736c65
    dw TOK_ELSE              , 4
    dd 0
    dq 0x0000747065637865
    dw TOK_EXCEPT            , 6
    dd 0
    dq 0x00796c6c616e6966
    dw TOK_FINALLY           , 7
    dd 0
    dq 0x0000000000726f66
    dw TOK_FOR               , 3
    dd 0
    dq 0x000000006d6f7266
    dw TOK_FROM              , 4
    dd 0
    dq 0x00006c61626f6c67
    dw TOK_GLOBAL            , 6
    dd 0
    dq 0x0000000000006669
    dw TOK_IF                , 2
    dd 0
    dq 0x000074726f706d69
    dw TOK_IMPORT            , 6
    dd 0
    dq 0x0000000000006e69
    dw TOK_IN                , 2
    dd 0
    dq 0x0000000000007369
    dw TOK_IS                , 2
    dd 0
    dq 0x00006164626d616c
    dw TOK_LAMBDA            , 6
    dd 0
    dq 0x6c61636f6c6e6f6e
    dw TOK_NONLOCAL          , 8
    dd 0
    dq 0x0000000000746f6e
    dw TOK_NOT               , 3
    dd 0
    dq 0x000000000000726f
    dw TOK_OR                , 2
    dd 0
    dq 0x0000000073736170
    dw TOK_PASS              , 4
    dd 0
    dq 0x0000006573696172
    dw TOK_RAISE             , 5
    dd 0
    dq 0x00006e7275746572
    dw TOK_RETURN            , 6
    dd 0
    dq 0x0000000000797274
    dw TOK_TRY               , 3
    dd 0
    dq 0x000000656c696877
    dw TOK_WHILE             , 5
    dd 0
    dq 0x0000000068746977
    dw TOK_WITH              , 4
    dd 0
    dq 0x000000646c656979
    dw TOK_YIELD             , 5
    dd 0
KW_ENT_SIZE equ 16

align 8
global kw_index
kw_index:               ; kw_index[byte] = { first entry, count }
    db   0, 0
    db   0, 0
    db   0, 0
    db   0, 0
    db   0, 0
    db   0, 0
    db   0, 0
    db   0, 0
    db   0, 0
    db   0, 0
    db   0, 0
    db   0, 0
    db   0, 0
    db   0, 0
    db   0, 0
    db   0, 0
    db   0, 0
    db   0, 0
    db   0, 0
    db   0, 0
    db   0, 0
    db   0, 0
    db   0, 0
    db   0, 0
    db   0, 0
    db   0, 0
    db   0, 0
    db   0, 0
    db   0, 0
    db   0, 0
    db   0, 0
    db   0, 0
    db   0, 0
    db   0, 0
    db   0, 0
    db   0, 0
    db   0, 0
    db   0, 0
    db   0, 0
    db   0, 0
    db   0, 0
    db   0, 0
    db   0, 0
    db   0, 0
    db   0, 0
    db   0, 0
    db   0, 0
    db   0, 0
    db   0, 0
    db   0, 0
    db   0, 0
    db   0, 0
    db   0, 0
    db   0, 0
    db   0, 0
    db   0, 0
    db   0, 0
    db   0, 0
    db   0, 0
    db   0, 0
    db   0, 0
    db   0, 0
    db   0, 0
    db   0, 0
    db   0, 0
    db   0, 0
    db   0, 0
    db   0, 0
    db   0, 0
    db   0, 0
    db   0, 1   ; 'F'
    db   0, 0
    db   0, 0
    db   0, 0
    db   0, 0
    db   0, 0
    db   0, 0
    db   0, 0
    db   1, 1   ; 'N'
    db   0, 0
    db   0, 0
    db   0, 0
    db   0, 0
    db   0, 0
    db   2, 1   ; 'T'
    db   0, 0
    db   0, 0
    db   0, 0
    db   0, 0
    db   0, 0
    db   0, 0
    db   0, 0
    db   0, 0
    db   0, 0
    db   0, 0
    db   0, 0
    db   0, 0
    db   3, 5   ; 'a'
    db   8, 1   ; 'b'
    db   9, 2   ; 'c'
    db  11, 2   ; 'd'
    db  13, 3   ; 'e'
    db  16, 3   ; 'f'
    db  19, 1   ; 'g'
    db   0, 0
    db  20, 4   ; 'i'
    db   0, 0
    db   0, 0
    db  24, 1   ; 'l'
    db   0, 0
    db  25, 2   ; 'n'
    db  27, 1   ; 'o'
    db  28, 1   ; 'p'
    db   0, 0
    db  29, 2   ; 'r'
    db   0, 0
    db  31, 1   ; 't'
    db   0, 0
    db   0, 0
    db  32, 2   ; 'w'
    db   0, 0
    db  34, 1   ; 'y'
    db   0, 0
    db   0, 0
    db   0, 0
    db   0, 0
    db   0, 0
    db   0, 0
    db   0, 0
    db   0, 0
    db   0, 0
    db   0, 0
    db   0, 0
    db   0, 0
    db   0, 0
    db   0, 0
    db   0, 0
    db   0, 0
    db   0, 0
    db   0, 0
    db   0, 0
    db   0, 0
    db   0, 0
    db   0, 0
    db   0, 0
    db   0, 0
    db   0, 0
    db   0, 0
    db   0, 0
    db   0, 0
    db   0, 0
    db   0, 0
    db   0, 0
    db   0, 0
    db   0, 0
    db   0, 0
    db   0, 0
    db   0, 0
    db   0, 0
    db   0, 0
    db   0, 0
    db   0, 0
    db   0, 0
    db   0, 0
    db   0, 0
    db   0, 0
    db   0, 0
    db   0, 0
    db   0, 0
    db   0, 0
    db   0, 0
    db   0, 0
    db   0, 0
    db   0, 0
    db   0, 0
    db   0, 0
    db   0, 0
    db   0, 0
    db   0, 0
    db   0, 0
    db   0, 0
    db   0, 0
    db   0, 0
    db   0, 0
    db   0, 0
    db   0, 0
    db   0, 0
    db   0, 0
    db   0, 0
    db   0, 0
    db   0, 0
    db   0, 0
    db   0, 0
    db   0, 0
    db   0, 0
    db   0, 0
    db   0, 0
    db   0, 0
    db   0, 0
    db   0, 0
    db   0, 0
    db   0, 0
    db   0, 0
    db   0, 0
    db   0, 0
    db   0, 0
    db   0, 0
    db   0, 0
    db   0, 0
    db   0, 0
    db   0, 0
    db   0, 0
    db   0, 0
    db   0, 0
    db   0, 0
    db   0, 0
    db   0, 0
    db   0, 0
    db   0, 0
    db   0, 0
    db   0, 0
    db   0, 0
    db   0, 0
    db   0, 0
    db   0, 0
    db   0, 0
    db   0, 0
    db   0, 0
    db   0, 0
    db   0, 0
    db   0, 0
    db   0, 0
    db   0, 0
    db   0, 0
    db   0, 0
    db   0, 0
    db   0, 0
    db   0, 0
    db   0, 0
    db   0, 0
    db   0, 0
    db   0, 0
    db   0, 0
    db   0, 0
    db   0, 0
    db   0, 0
    db   0, 0
    db   0, 0
    db   0, 0
    db   0, 0
    db   0, 0
    db   0, 0
    db   0, 0
    db   0, 0
    db   0, 0
    db   0, 0

;; ---------------------------------------------------------------------------
;; Operator recognition.  Entries are sorted LONGEST FIRST within each first
;; byte, so the first match found is already the maximal munch -- no separate
;; longest-match pass.  The compare reads up to 4 bytes past the cursor, which
;; is why the source buffer carries a NUL-padded tail.
;; ---------------------------------------------------------------------------
align 4
global op_masks
op_masks:               ; op_masks[L] keeps the low L bytes of a 4-byte load
    dd 0x00000000   ; length 0
    dd 0x000000ff   ; length 1
    dd 0x0000ffff   ; length 2
    dd 0x00ffffff   ; length 3
    dd 0xffffffff   ; length 4

align 8
global op_table
op_table:               ; { packed text, length, token }
    dd 0x00003d21
    db 2, TOK_NOTEQUAL             ; '!='
    dw 0
    dd 0x00003d25
    db 2, TOK_PERCENTEQUAL         ; '%='
    dw 0
    dd 0x00000025
    db 1, TOK_PERCENT              ; '%'
    dw 0
    dd 0x00003d26
    db 2, TOK_AMPEREQUAL           ; '&='
    dw 0
    dd 0x00000026
    db 1, TOK_AMPER                ; '&'
    dw 0
    dd 0x00000028
    db 1, TOK_LPAR                 ; '('
    dw 0
    dd 0x00000029
    db 1, TOK_RPAR                 ; ')'
    dw 0
    dd 0x003d2a2a
    db 3, TOK_DOUBLESTAREQUAL      ; '**='
    dw 0
    dd 0x00002a2a
    db 2, TOK_DOUBLESTAR           ; '**'
    dw 0
    dd 0x00003d2a
    db 2, TOK_STAREQUAL            ; '*='
    dw 0
    dd 0x0000002a
    db 1, TOK_STAR                 ; '*'
    dw 0
    dd 0x00003d2b
    db 2, TOK_PLUSEQUAL            ; '+='
    dw 0
    dd 0x0000002b
    db 1, TOK_PLUS                 ; '+'
    dw 0
    dd 0x0000002c
    db 1, TOK_COMMA                ; ','
    dw 0
    dd 0x00003d2d
    db 2, TOK_MINEQUAL             ; '-='
    dw 0
    dd 0x00003e2d
    db 2, TOK_RARROW               ; '->'
    dw 0
    dd 0x0000002d
    db 1, TOK_MINUS                ; '-'
    dw 0
    dd 0x002e2e2e
    db 3, TOK_ELLIPSIS             ; '...'
    dw 0
    dd 0x0000002e
    db 1, TOK_DOT                  ; '.'
    dw 0
    dd 0x003d2f2f
    db 3, TOK_DOUBLESLASHEQUAL     ; '//='
    dw 0
    dd 0x00002f2f
    db 2, TOK_DOUBLESLASH          ; '//'
    dw 0
    dd 0x00003d2f
    db 2, TOK_SLASHEQUAL           ; '/='
    dw 0
    dd 0x0000002f
    db 1, TOK_SLASH                ; '/'
    dw 0
    dd 0x00003d3a
    db 2, TOK_COLONEQUAL           ; ':='
    dw 0
    dd 0x0000003a
    db 1, TOK_COLON                ; ':'
    dw 0
    dd 0x0000003b
    db 1, TOK_SEMI                 ; ';'
    dw 0
    dd 0x003d3c3c
    db 3, TOK_LEFTSHIFTEQUAL       ; '<<='
    dw 0
    dd 0x00003c3c
    db 2, TOK_LEFTSHIFT            ; '<<'
    dw 0
    dd 0x00003d3c
    db 2, TOK_LESSEQUAL            ; '<='
    dw 0
    dd 0x0000003c
    db 1, TOK_LESS                 ; '<'
    dw 0
    dd 0x00003d3d
    db 2, TOK_EQEQUAL              ; '=='
    dw 0
    dd 0x0000003d
    db 1, TOK_EQUAL                ; '='
    dw 0
    dd 0x003d3e3e
    db 3, TOK_RIGHTSHIFTEQUAL      ; '>>='
    dw 0
    dd 0x00003d3e
    db 2, TOK_GREATEREQUAL         ; '>='
    dw 0
    dd 0x00003e3e
    db 2, TOK_RIGHTSHIFT           ; '>>'
    dw 0
    dd 0x0000003e
    db 1, TOK_GREATER              ; '>'
    dw 0
    dd 0x00003d40
    db 2, TOK_ATEQUAL              ; '@='
    dw 0
    dd 0x00000040
    db 1, TOK_AT                   ; '@'
    dw 0
    dd 0x0000005b
    db 1, TOK_LSQB                 ; '['
    dw 0
    dd 0x0000005d
    db 1, TOK_RSQB                 ; ']'
    dw 0
    dd 0x00003d5e
    db 2, TOK_CIRCUMFLEXEQUAL      ; '^='
    dw 0
    dd 0x0000005e
    db 1, TOK_CIRCUMFLEX           ; '^'
    dw 0
    dd 0x0000007b
    db 1, TOK_LBRACE               ; '{'
    dw 0
    dd 0x00003d7c
    db 2, TOK_VBAREQUAL            ; '|='
    dw 0
    dd 0x0000007c
    db 1, TOK_VBAR                 ; '|'
    dw 0
    dd 0x0000007d
    db 1, TOK_RBRACE               ; '}'
    dw 0
    dd 0x0000007e
    db 1, TOK_TILDE                ; '~'
    dw 0
OP_ENT_SIZE equ 8

align 8
global op_index
op_index:               ; op_index[byte] = { first entry, count }
    db   0, 0
    db   0, 0
    db   0, 0
    db   0, 0
    db   0, 0
    db   0, 0
    db   0, 0
    db   0, 0
    db   0, 0
    db   0, 0
    db   0, 0
    db   0, 0
    db   0, 0
    db   0, 0
    db   0, 0
    db   0, 0
    db   0, 0
    db   0, 0
    db   0, 0
    db   0, 0
    db   0, 0
    db   0, 0
    db   0, 0
    db   0, 0
    db   0, 0
    db   0, 0
    db   0, 0
    db   0, 0
    db   0, 0
    db   0, 0
    db   0, 0
    db   0, 0
    db   0, 0
    db   0, 1   ; '!'
    db   0, 0
    db   0, 0
    db   0, 0
    db   1, 2   ; '%'
    db   3, 2   ; '&'
    db   0, 0
    db   5, 1   ; '('
    db   6, 1   ; ')'
    db   7, 4   ; '*'
    db  11, 2   ; '+'
    db  13, 1   ; ','
    db  14, 3   ; '-'
    db  17, 2   ; '.'
    db  19, 4   ; '/'
    db   0, 0
    db   0, 0
    db   0, 0
    db   0, 0
    db   0, 0
    db   0, 0
    db   0, 0
    db   0, 0
    db   0, 0
    db   0, 0
    db  23, 2   ; ':'
    db  25, 1   ; ';'
    db  26, 4   ; '<'
    db  30, 2   ; '='
    db  32, 4   ; '>'
    db   0, 0
    db  36, 2   ; '@'
    db   0, 0
    db   0, 0
    db   0, 0
    db   0, 0
    db   0, 0
    db   0, 0
    db   0, 0
    db   0, 0
    db   0, 0
    db   0, 0
    db   0, 0
    db   0, 0
    db   0, 0
    db   0, 0
    db   0, 0
    db   0, 0
    db   0, 0
    db   0, 0
    db   0, 0
    db   0, 0
    db   0, 0
    db   0, 0
    db   0, 0
    db   0, 0
    db   0, 0
    db   0, 0
    db  38, 1   ; '['
    db   0, 0
    db  39, 1   ; ']'
    db  40, 2   ; '^'
    db   0, 0
    db   0, 0
    db   0, 0
    db   0, 0
    db   0, 0
    db   0, 0
    db   0, 0
    db   0, 0
    db   0, 0
    db   0, 0
    db   0, 0
    db   0, 0
    db   0, 0
    db   0, 0
    db   0, 0
    db   0, 0
    db   0, 0
    db   0, 0
    db   0, 0
    db   0, 0
    db   0, 0
    db   0, 0
    db   0, 0
    db   0, 0
    db   0, 0
    db   0, 0
    db   0, 0
    db   0, 0
    db  42, 1   ; '{'
    db  43, 2   ; '|'
    db  45, 1   ; '}'
    db  46, 1   ; '~'
    db   0, 0
    db   0, 0
    db   0, 0
    db   0, 0
    db   0, 0
    db   0, 0
    db   0, 0
    db   0, 0
    db   0, 0
    db   0, 0
    db   0, 0
    db   0, 0
    db   0, 0
    db   0, 0
    db   0, 0
    db   0, 0
    db   0, 0
    db   0, 0
    db   0, 0
    db   0, 0
    db   0, 0
    db   0, 0
    db   0, 0
    db   0, 0
    db   0, 0
    db   0, 0
    db   0, 0
    db   0, 0
    db   0, 0
    db   0, 0
    db   0, 0
    db   0, 0
    db   0, 0
    db   0, 0
    db   0, 0
    db   0, 0
    db   0, 0
    db   0, 0
    db   0, 0
    db   0, 0
    db   0, 0
    db   0, 0
    db   0, 0
    db   0, 0
    db   0, 0
    db   0, 0
    db   0, 0
    db   0, 0
    db   0, 0
    db   0, 0
    db   0, 0
    db   0, 0
    db   0, 0
    db   0, 0
    db   0, 0
    db   0, 0
    db   0, 0
    db   0, 0
    db   0, 0
    db   0, 0
    db   0, 0
    db   0, 0
    db   0, 0
    db   0, 0
    db   0, 0
    db   0, 0
    db   0, 0
    db   0, 0
    db   0, 0
    db   0, 0
    db   0, 0
    db   0, 0
    db   0, 0
    db   0, 0
    db   0, 0
    db   0, 0
    db   0, 0
    db   0, 0
    db   0, 0
    db   0, 0
    db   0, 0
    db   0, 0
    db   0, 0
    db   0, 0
    db   0, 0
    db   0, 0
    db   0, 0
    db   0, 0
    db   0, 0
    db   0, 0
    db   0, 0
    db   0, 0
    db   0, 0
    db   0, 0
    db   0, 0
    db   0, 0
    db   0, 0
    db   0, 0
    db   0, 0
    db   0, 0
    db   0, 0
    db   0, 0
    db   0, 0
    db   0, 0
    db   0, 0
    db   0, 0
    db   0, 0
    db   0, 0
    db   0, 0
    db   0, 0
    db   0, 0
    db   0, 0
    db   0, 0
    db   0, 0
    db   0, 0
    db   0, 0
    db   0, 0
    db   0, 0
    db   0, 0
    db   0, 0
    db   0, 0
    db   0, 0
    db   0, 0
    db   0, 0
    db   0, 0
    db   0, 0
    db   0, 0
    db   0, 0
    db   0, 0

;; ---------------------------------------------------------------------------
;; op_meta - the keystone table.  One row per opcode drives four things:
;;   1. CACHE padding      (writer emits .cache zero words after the opcode)
;;   2. instruction sizing (1 + EXTENDED_ARG prefixes + .cache code units)
;;   3. stack depth        (.effect, or comp_effect_var when .effect == SE_VAR)
;;   4. successors         (.flags OM_JUMP / OM_NOFALL, and .jeff for the
;;                          taken edge, which differs for FOR_ITER and SEND)
;;
;; Because every emission routes through this table, 'I forgot the caches on
;; LOAD_SUPER_ATTR' is structurally impossible rather than a per-call-site
;; discipline.  The effects are CPython's, deliberately: apython's
;; op_cleanup_throw pops two where CPython's CLEANUP_THROW is net -1, and the
;; exception-table depths must be computed on CPython's numbers.
;; ---------------------------------------------------------------------------
align 64
global op_meta
op_meta:                ; { cache, effect, flags, jump effect }
    db 0     , 0     , 0                                 , 0        ;   0 CACHE
    db 0     , -1    , 0                                 , 0        ;   1 POP_TOP
    db 0     , 1     , 0                                 , 0        ;   2 PUSH_NULL
    db 0     , -1    , OM_NOFALL                         , 0        ;   3 INTERPRETER_EXIT
    db 0     , -2    , 0                                 , 0        ;   4 END_FOR
    db 0     , -1    , 0                                 , 0        ;   5 END_SEND
    db 0     , 0     , 0                                 , 0        ;   6
    db 0     , 0     , 0                                 , 0        ;   7
    db 0     , 0     , 0                                 , 0        ;   8
    db 0     , 0     , 0                                 , 0        ;   9 NOP
    db 0     , 0     , 0                                 , 0        ;  10
    db 0     , 0     , 0                                 , 0        ;  11 UNARY_NEGATIVE
    db 0     , 0     , 0                                 , 0        ;  12 UNARY_NOT
    db 0     , 0     , 0                                 , 0        ;  13
    db 0     , 0     , 0                                 , 0        ;  14
    db 0     , 0     , 0                                 , 0        ;  15 UNARY_INVERT
    db 0     , 0     , 0                                 , 0        ;  16
    db 0     , 0     , 0                                 , 0        ;  17 RESERVED
    db 0     , 0     , 0                                 , 0        ;  18
    db 0     , 0     , 0                                 , 0        ;  19
    db 0     , 0     , 0                                 , 0        ;  20
    db 0     , 0     , 0                                 , 0        ;  21
    db 0     , 0     , 0                                 , 0        ;  22
    db 0     , 0     , 0                                 , 0        ;  23
    db 0     , 0     , 0                                 , 0        ;  24
    db 1     , -1    , 0                                 , 0        ;  25 BINARY_SUBSCR
    db 0     , -2    , 0                                 , 0        ;  26 BINARY_SLICE
    db 0     , -4    , 0                                 , 0        ;  27 STORE_SLICE
    db 0     , 0     , 0                                 , 0        ;  28
    db 0     , 0     , 0                                 , 0        ;  29
    db 0     , 1     , 0                                 , 0        ;  30 GET_LEN
    db 0     , 1     , 0                                 , 0        ;  31 MATCH_MAPPING
    db 0     , 1     , 0                                 , 0        ;  32 MATCH_SEQUENCE
    db 0     , 1     , 0                                 , 0        ;  33 MATCH_KEYS
    db 0     , 0     , 0                                 , 0        ;  34
    db 0     , 1     , 0                                 , 0        ;  35 PUSH_EXC_INFO
    db 0     , 0     , 0                                 , 0        ;  36 CHECK_EXC_MATCH
    db 0     , 0     , 0                                 , 0        ;  37 CHECK_EG_MATCH
    db 0     , 0     , 0                                 , 0        ;  38
    db 0     , 0     , 0                                 , 0        ;  39
    db 0     , 0     , 0                                 , 0        ;  40
    db 0     , 0     , 0                                 , 0        ;  41
    db 0     , 0     , 0                                 , 0        ;  42
    db 0     , 0     , 0                                 , 0        ;  43
    db 0     , 0     , 0                                 , 0        ;  44
    db 0     , 0     , 0                                 , 0        ;  45
    db 0     , 0     , 0                                 , 0        ;  46
    db 0     , 0     , 0                                 , 0        ;  47
    db 0     , 0     , 0                                 , 0        ;  48
    db 0     , 1     , 0                                 , 0        ;  49 WITH_EXCEPT_START
    db 0     , 0     , 0                                 , 0        ;  50 GET_AITER
    db 0     , 1     , 0                                 , 0        ;  51 GET_ANEXT
    db 0     , 1     , 0                                 , 0        ;  52 BEFORE_ASYNC_WITH
    db 0     , 1     , 0                                 , 0        ;  53 BEFORE_WITH
    db 0     , -2    , 0                                 , 0        ;  54 END_ASYNC_FOR
    db 0     , -1    , 0                                 , 0        ;  55 CLEANUP_THROW
    db 0     , 0     , 0                                 , 0        ;  56
    db 0     , 0     , 0                                 , 0        ;  57
    db 0     , 0     , 0                                 , 0        ;  58
    db 0     , 0     , 0                                 , 0        ;  59
    db 1     , -3    , 0                                 , 0        ;  60 STORE_SUBSCR
    db 0     , -2    , 0                                 , 0        ;  61 DELETE_SUBSCR
    db 0     , 0     , 0                                 , 0        ;  62
    db 0     , 0     , 0                                 , 0        ;  63
    db 0     , 0     , 0                                 , 0        ;  64
    db 0     , 0     , 0                                 , 0        ;  65
    db 0     , 0     , 0                                 , 0        ;  66
    db 0     , 0     , 0                                 , 0        ;  67
    db 0     , 0     , 0                                 , 0        ;  68 GET_ITER
    db 0     , 0     , 0                                 , 0        ;  69 GET_YIELD_FROM_ITER
    db 0     , 0     , 0                                 , 0        ;  70
    db 0     , 1     , 0                                 , 0        ;  71 LOAD_BUILD_CLASS
    db 0     , 0     , 0                                 , 0        ;  72
    db 0     , 0     , 0                                 , 0        ;  73
    db 0     , 1     , 0                                 , 0        ;  74 LOAD_ASSERTION_ERROR
    db 0     , 1     , 0                                 , 0        ;  75 RETURN_GENERATOR
    db 0     , 0     , 0                                 , 0        ;  76
    db 0     , 0     , 0                                 , 0        ;  77
    db 0     , 0     , 0                                 , 0        ;  78
    db 0     , 0     , 0                                 , 0        ;  79
    db 0     , 0     , 0                                 , 0        ;  80
    db 0     , 0     , 0                                 , 0        ;  81
    db 0     , 0     , 0                                 , 0        ;  82
    db 0     , -1    , OM_NOFALL                         , 0        ;  83 RETURN_VALUE
    db 0     , 0     , 0                                 , 0        ;  84
    db 0     , 0     , 0                                 , 0        ;  85 SETUP_ANNOTATIONS
    db 0     , 0     , 0                                 , 0        ;  86
    db 0     , 1     , 0                                 , 0        ;  87 LOAD_LOCALS
    db 0     , 0     , 0                                 , 0        ;  88
    db 0     , -1    , 0                                 , 0        ;  89 POP_EXCEPT
    db 0     , -1    , OM_HASARG                         , 0        ;  90 STORE_NAME
    db 0     , 0     , OM_HASARG                         , 0        ;  91 DELETE_NAME
    db 1     , SE_VAR, OM_HASARG                         , 0        ;  92 UNPACK_SEQUENCE
    db 1     , 1     , OM_HASARG|OM_JUMP                 , 1        ;  93 FOR_ITER
    db 0     , SE_VAR, OM_HASARG                         , 0        ;  94 UNPACK_EX
    db 4     , -2    , OM_HASARG                         , 0        ;  95 STORE_ATTR
    db 0     , -1    , OM_HASARG                         , 0        ;  96 DELETE_ATTR
    db 0     , -1    , OM_HASARG                         , 0        ;  97 STORE_GLOBAL
    db 0     , 0     , OM_HASARG                         , 0        ;  98 DELETE_GLOBAL
    db 0     , 0     , OM_HASARG                         , 0        ;  99 SWAP
    db 0     , 1     , OM_HASARG                         , 0        ; 100 LOAD_CONST
    db 0     , 1     , OM_HASARG                         , 0        ; 101 LOAD_NAME
    db 0     , SE_VAR, OM_HASARG                         , 0        ; 102 BUILD_TUPLE
    db 0     , SE_VAR, OM_HASARG                         , 0        ; 103 BUILD_LIST
    db 0     , SE_VAR, OM_HASARG                         , 0        ; 104 BUILD_SET
    db 0     , SE_VAR, OM_HASARG                         , 0        ; 105 BUILD_MAP
    db 9     , SE_VAR, OM_HASARG                         , 0        ; 106 LOAD_ATTR
    db 1     , -1    , OM_HASARG                         , 0        ; 107 COMPARE_OP
    db 0     , -1    , OM_HASARG                         , 0        ; 108 IMPORT_NAME
    db 0     , 1     , OM_HASARG                         , 0        ; 109 IMPORT_FROM
    db 0     , 0     , OM_HASARG|OM_JUMP|OM_NOFALL       , 0        ; 110 JUMP_FORWARD
    db 0     , 0     , 0                                 , 0        ; 111
    db 0     , 0     , 0                                 , 0        ; 112
    db 0     , 0     , 0                                 , 0        ; 113
    db 0     , -1    , OM_HASARG|OM_JUMP                 , -1       ; 114 POP_JUMP_IF_FALSE
    db 0     , -1    , OM_HASARG|OM_JUMP                 , -1       ; 115 POP_JUMP_IF_TRUE
    db 4     , SE_VAR, OM_HASARG                         , 0        ; 116 LOAD_GLOBAL
    db 0     , -1    , OM_HASARG                         , 0        ; 117 IS_OP
    db 0     , -1    , OM_HASARG                         , 0        ; 118 CONTAINS_OP
    db 0     , -1    , OM_HASARG|OM_NOFALL               , 0        ; 119 RERAISE
    db 0     , 1     , OM_HASARG                         , 0        ; 120 COPY
    db 0     , 0     , OM_HASARG|OM_NOFALL               , 0        ; 121 RETURN_CONST
    db 1     , -1    , OM_HASARG                         , 0        ; 122 BINARY_OP
    db 1     , 0     , OM_HASARG|OM_JUMP                 , 0        ; 123 SEND
    db 0     , 1     , OM_HASARG                         , 0        ; 124 LOAD_FAST
    db 0     , -1    , OM_HASARG                         , 0        ; 125 STORE_FAST
    db 0     , 0     , OM_HASARG                         , 0        ; 126 DELETE_FAST
    db 0     , 1     , OM_HASARG                         , 0        ; 127 LOAD_FAST_CHECK
    db 0     , -1    , OM_HASARG|OM_JUMP                 , -1       ; 128 POP_JUMP_IF_NOT_NONE
    db 0     , -1    , OM_HASARG|OM_JUMP                 , -1       ; 129 POP_JUMP_IF_NONE
    db 0     , SE_VAR, OM_HASARG|OM_NOFALL               , 0        ; 130 RAISE_VARARGS
    db 0     , 0     , OM_HASARG                         , 0        ; 131 GET_AWAITABLE
    db 0     , SE_VAR, OM_HASARG                         , 0        ; 132 MAKE_FUNCTION
    db 0     , SE_VAR, OM_HASARG                         , 0        ; 133 BUILD_SLICE
    db 0     , 0     , OM_HASARG|OM_JUMP|OM_JUMPBACK|OM_NOFALL, 0        ; 134 JUMP_BACKWARD_NO_INTERRUPT
    db 0     , 0     , OM_HASARG                         , 0        ; 135 MAKE_CELL
    db 0     , 1     , OM_HASARG                         , 0        ; 136 LOAD_CLOSURE
    db 0     , 1     , OM_HASARG                         , 0        ; 137 LOAD_DEREF
    db 0     , -1    , OM_HASARG                         , 0        ; 138 STORE_DEREF
    db 0     , 0     , OM_HASARG                         , 0        ; 139 DELETE_DEREF
    db 0     , 0     , OM_HASARG|OM_JUMP|OM_JUMPBACK|OM_NOFALL, 0        ; 140 JUMP_BACKWARD
    db 1     , SE_VAR, OM_HASARG                         , 0        ; 141 LOAD_SUPER_ATTR
    db 0     , SE_VAR, OM_HASARG                         , 0        ; 142 CALL_FUNCTION_EX
    db 0     , 1     , OM_HASARG                         , 0        ; 143 LOAD_FAST_AND_CLEAR
    db 0     , 0     , OM_HASARG                         , 0        ; 144 EXTENDED_ARG
    db 0     , -1    , OM_HASARG                         , 0        ; 145 LIST_APPEND
    db 0     , -1    , OM_HASARG                         , 0        ; 146 SET_ADD
    db 0     , -2    , OM_HASARG                         , 0        ; 147 MAP_ADD
    db 0     , 0     , 0                                 , 0        ; 148
    db 0     , 0     , OM_HASARG                         , 0        ; 149 COPY_FREE_VARS
    db 0     , 0     , OM_HASARG                         , 0        ; 150 YIELD_VALUE
    db 0     , 0     , OM_HASARG                         , 0        ; 151 RESUME
    db 0     , -2    , OM_HASARG                         , 0        ; 152 MATCH_CLASS
    db 0     , 0     , 0                                 , 0        ; 153
    db 0     , 0     , 0                                 , 0        ; 154
    db 0     , SE_VAR, OM_HASARG                         , 0        ; 155 FORMAT_VALUE
    db 0     , SE_VAR, OM_HASARG                         , 0        ; 156 BUILD_CONST_KEY_MAP
    db 0     , SE_VAR, OM_HASARG                         , 0        ; 157 BUILD_STRING
    db 0     , 0     , 0                                 , 0        ; 158
    db 0     , 0     , 0                                 , 0        ; 159
    db 0     , 0     , 0                                 , 0        ; 160
    db 0     , 0     , 0                                 , 0        ; 161
    db 0     , -1    , OM_HASARG                         , 0        ; 162 LIST_EXTEND
    db 0     , -1    , OM_HASARG                         , 0        ; 163 SET_UPDATE
    db 0     , -1    , OM_HASARG                         , 0        ; 164 DICT_MERGE
    db 0     , -1    , OM_HASARG                         , 0        ; 165 DICT_UPDATE
    db 0     , 0     , 0                                 , 0        ; 166
    db 0     , 0     , 0                                 , 0        ; 167
    db 0     , 0     , 0                                 , 0        ; 168
    db 0     , 0     , 0                                 , 0        ; 169
    db 0     , 0     , 0                                 , 0        ; 170
    db 3     , SE_VAR, OM_HASARG                         , 0        ; 171 CALL
    db 0     , 0     , OM_HASARG                         , 0        ; 172 KW_NAMES
    db 0     , 0     , OM_HASARG                         , 0        ; 173 CALL_INTRINSIC_1
    db 0     , -1    , OM_HASARG                         , 0        ; 174 CALL_INTRINSIC_2
    db 0     , 0     , OM_HASARG                         , 0        ; 175 LOAD_FROM_DICT_OR_GLOBALS
    db 0     , 0     , OM_HASARG                         , 0        ; 176 LOAD_FROM_DICT_OR_DEREF
    db 0     , 0     , 0                                 , 0        ; 177
    db 0     , 0     , 0                                 , 0        ; 178
    db 0     , 0     , 0                                 , 0        ; 179
    db 0     , 0     , 0                                 , 0        ; 180
    db 0     , 0     , 0                                 , 0        ; 181
    db 0     , 0     , 0                                 , 0        ; 182
    db 0     , 0     , 0                                 , 0        ; 183
    db 0     , 0     , 0                                 , 0        ; 184
    db 0     , 0     , 0                                 , 0        ; 185
    db 0     , 0     , 0                                 , 0        ; 186
    db 0     , 0     , 0                                 , 0        ; 187
    db 0     , 0     , 0                                 , 0        ; 188
    db 0     , 0     , 0                                 , 0        ; 189
    db 0     , 0     , 0                                 , 0        ; 190
    db 0     , 0     , 0                                 , 0        ; 191
    db 0     , 0     , 0                                 , 0        ; 192
    db 0     , 0     , 0                                 , 0        ; 193
    db 0     , 0     , 0                                 , 0        ; 194
    db 0     , 0     , 0                                 , 0        ; 195
    db 0     , 0     , 0                                 , 0        ; 196
    db 0     , 0     , 0                                 , 0        ; 197
    db 0     , 0     , 0                                 , 0        ; 198
    db 0     , 0     , 0                                 , 0        ; 199
    db 0     , 0     , 0                                 , 0        ; 200
    db 0     , 0     , 0                                 , 0        ; 201
    db 0     , 0     , 0                                 , 0        ; 202
    db 0     , 0     , 0                                 , 0        ; 203
    db 0     , 0     , 0                                 , 0        ; 204
    db 0     , 0     , 0                                 , 0        ; 205
    db 0     , 0     , 0                                 , 0        ; 206
    db 0     , 0     , 0                                 , 0        ; 207
    db 0     , 0     , 0                                 , 0        ; 208
    db 0     , 0     , 0                                 , 0        ; 209
    db 0     , 0     , 0                                 , 0        ; 210
    db 0     , 0     , 0                                 , 0        ; 211
    db 0     , 0     , 0                                 , 0        ; 212
    db 0     , 0     , 0                                 , 0        ; 213
    db 0     , 0     , 0                                 , 0        ; 214
    db 0     , 0     , 0                                 , 0        ; 215
    db 0     , 0     , 0                                 , 0        ; 216
    db 0     , 0     , 0                                 , 0        ; 217
    db 0     , 0     , 0                                 , 0        ; 218
    db 0     , 0     , 0                                 , 0        ; 219
    db 0     , 0     , 0                                 , 0        ; 220
    db 0     , 0     , 0                                 , 0        ; 221
    db 0     , 0     , 0                                 , 0        ; 222
    db 0     , 0     , 0                                 , 0        ; 223
    db 0     , 0     , 0                                 , 0        ; 224
    db 0     , 0     , 0                                 , 0        ; 225
    db 0     , 0     , 0                                 , 0        ; 226
    db 0     , 0     , 0                                 , 0        ; 227
    db 0     , 0     , 0                                 , 0        ; 228
    db 0     , 0     , 0                                 , 0        ; 229
    db 0     , 0     , 0                                 , 0        ; 230
    db 0     , 0     , 0                                 , 0        ; 231
    db 0     , 0     , 0                                 , 0        ; 232
    db 0     , 0     , 0                                 , 0        ; 233
    db 0     , 0     , 0                                 , 0        ; 234
    db 0     , 0     , 0                                 , 0        ; 235
    db 0     , 0     , 0                                 , 0        ; 236
    db 0     , SE_VAR, OM_HASARG                         , 0        ; 237 INSTRUMENTED_LOAD_SUPER_ATTR
    db 0     , 0     , OM_HASARG                         , 0        ; 238 INSTRUMENTED_POP_JUMP_IF_NONE
    db 0     , 0     , OM_HASARG                         , 0        ; 239 INSTRUMENTED_POP_JUMP_IF_NOT_NONE
    db 0     , 0     , OM_HASARG                         , 0        ; 240 INSTRUMENTED_RESUME
    db 0     , 0     , OM_HASARG                         , 0        ; 241 INSTRUMENTED_CALL
    db 0     , -1    , OM_HASARG                         , 0        ; 242 INSTRUMENTED_RETURN_VALUE
    db 0     , 0     , OM_HASARG                         , 0        ; 243 INSTRUMENTED_YIELD_VALUE
    db 0     , 0     , OM_HASARG                         , 0        ; 244 INSTRUMENTED_CALL_FUNCTION_EX
    db 0     , 0     , OM_HASARG                         , 0        ; 245 INSTRUMENTED_JUMP_FORWARD
    db 0     , 0     , OM_HASARG                         , 0        ; 246 INSTRUMENTED_JUMP_BACKWARD
    db 0     , 0     , OM_HASARG                         , 0        ; 247 INSTRUMENTED_RETURN_CONST
    db 0     , 0     , OM_HASARG                         , 0        ; 248 INSTRUMENTED_FOR_ITER
    db 0     , 0     , OM_HASARG                         , 0        ; 249 INSTRUMENTED_POP_JUMP_IF_FALSE
    db 0     , 0     , OM_HASARG                         , 0        ; 250 INSTRUMENTED_POP_JUMP_IF_TRUE
    db 0     , -2    , OM_HASARG                         , 0        ; 251 INSTRUMENTED_END_FOR
    db 0     , -1    , OM_HASARG                         , 0        ; 252 INSTRUMENTED_END_SEND
    db 0     , 0     , OM_HASARG                         , 0        ; 253 INSTRUMENTED_INSTRUCTION
    db 0     , 0     , OM_HASARG                         , 0        ; 254 INSTRUMENTED_LINE
    db 0     , 0     , 0                                 , 0        ; 255

ASM_INIT
