#!/usr/bin/env python3
"""Regenerate the prule_table block inside compiler/parse.asm.

The expression grammar lives in the ROWS dict below rather than in hand-written
assembly, so adding a construct is a one-line edit and the 90 rows stay aligned
with the token enum.  Rewrites parse.asm in place:

    python3 compiler/gen_prule.py
"""
import re, os, sys

# tok -> (prefix, infix, lbp, rbp, aux, flags, note)
ROWS = {}
def R(tok, prefix=0, infix=0, lbp="BP_NONE", rbp="BP_NONE", aux=0, flags=0, note=""):
    ROWS[tok] = (prefix, infix, lbp, rbp, aux, flags, note)

# --- atoms --------------------------------------------------------------
R("TOK_NUMBER",   prefix="pf_number")
R("TOK_STRING",   prefix="pf_string",
  note="consumes a whole run: adjacent literals concatenate")
R("TOK_NAME",     prefix="pf_name")
R("TOK_TRUE",     prefix="pf_const")
R("TOK_FALSE",    prefix="pf_const")
R("TOK_NONE",     prefix="pf_const")
R("TOK_ELLIPSIS", prefix="pf_const")
R("TOK_YIELD",    prefix="pf_yield",
  note="an expression, not a statement: `x = yield v` receives from send()")
R("TOK_COLONEQUAL", infix="in_walrus", lbp="BP_WALRUS", rbp="BP_LAMBDA",
  note="right-associative, and its RHS may be a lambda but not a ternary")
R("TOK_AWAIT",    prefix="pf_await",  rbp="BP_AWAIT",
  note="operand is a primary: BP_AWAIT sits between `**` and postfix")
R("TOK_LAMBDA",   prefix="pf_lambda",
  note="body one level below the ternary: `lambda: a, b` is still a tuple")

# --- brackets: prefix opens a display, infix continues a primary ---------
R("TOK_LPAR",   prefix="pf_group",  infix="in_call",
  lbp="BP_POSTFIX", rbp="BP_POSTFIX",
  note="group or tuple; as an infix, a call")
R("TOK_LSQB",   prefix="pf_list",   infix="in_subscript",
  lbp="BP_POSTFIX", rbp="BP_POSTFIX")
R("TOK_LBRACE", prefix="pf_dictset")
R("TOK_DOT",    infix="in_attr", lbp="BP_POSTFIX", rbp="BP_POSTFIX")

# --- unary prefixes -----------------------------------------------------
R("TOK_TILDE", prefix="pf_unary", rbp="BP_UNARY", aux="UOP_INVERT")
R("TOK_NOT",   prefix="pf_unary", rbp="BP_NOT",   aux="UOP_NOT",
  note="operand at BP_NOT, below BP_COMPARE: `not a == b` is `not (a == b)`")

# `*` and `**` are prefixes only inside displays and calls; the handlers
# reject them anywhere else, which is what makes `*x + 1` a syntax error.
R("TOK_STAR",       prefix="pf_starred", infix="in_binop",
  lbp="BP_TERM", rbp="BP_TERM", aux="NB_MULTIPLY")
R("TOK_DOUBLESTAR", prefix="pf_starred", infix="in_binop",
  lbp="BP_POWER", rbp="BP_UNARY", aux="NB_POWER",
  note="rbp one level BELOW lbp: right-associative, and its RHS takes a unary")

R("TOK_PLUS",  prefix="pf_unary", infix="in_binop",
  lbp="BP_ARITH", rbp="BP_ARITH", aux="NB_ADD",
  note="aux is the BINARY op; pf_unary reads the token, not aux")
R("TOK_MINUS", prefix="pf_unary", infix="in_binop",
  lbp="BP_ARITH", rbp="BP_ARITH", aux="NB_SUBTRACT")

# --- ordinary left-associative binaries ---------------------------------
for tok, lbp, nb in [
    ("TOK_SLASH",       "BP_TERM",   "NB_TRUE_DIVIDE"),
    ("TOK_DOUBLESLASH", "BP_TERM",   "NB_FLOOR_DIVIDE"),
    ("TOK_PERCENT",     "BP_TERM",   "NB_REMAINDER"),
    ("TOK_AT",          "BP_TERM",   "NB_MATRIX_MULTIPLY"),
    ("TOK_LEFTSHIFT",   "BP_SHIFT",  "NB_LSHIFT"),
    ("TOK_RIGHTSHIFT",  "BP_SHIFT",  "NB_RSHIFT"),
    ("TOK_AMPER",       "BP_BITAND", "NB_AND"),
    ("TOK_CIRCUMFLEX",  "BP_BITXOR", "NB_XOR"),
    ("TOK_VBAR",        "BP_BITOR",  "NB_OR"),
]:
    R(tok, infix="in_binop", lbp=lbp, rbp=lbp, aux=nb)

# --- booleans and the conditional ---------------------------------------
R("TOK_AND", infix="in_boolop",  lbp="BP_AND",     rbp="BP_AND")
R("TOK_OR",  infix="in_boolop",  lbp="BP_OR",      rbp="BP_OR")
R("TOK_IF",  infix="in_ternary", lbp="BP_TERNARY", rbp="BP_TERNARY")

# --- comparisons: one binding power, folded into a single n-ary node ----
for tok in ["TOK_LESS", "TOK_GREATER", "TOK_LESSEQUAL", "TOK_GREATEREQUAL",
            "TOK_EQEQUAL", "TOK_NOTEQUAL", "TOK_IN", "TOK_IS"]:
    R(tok, infix="in_compare", lbp="BP_COMPARE", rbp="BP_COMPARE", flags="PR_CHAIN")
# `not` is already a prefix; as an infix it can only begin `not in`.
ROWS["TOK_NOT"] = ("pf_unary", "in_compare", "BP_COMPARE", "BP_NOT",
                   "UOP_NOT", "PR_CHAIN",
                   "prefix `not x`; as an infix it can only start `not in`")

ORDER = [
 "TOK_ENDMARKER","TOK_NEWLINE","TOK_INDENT","TOK_DEDENT","TOK_NAME","TOK_NUMBER",
 "TOK_STRING","TOK_FSTRING","TOK_LPAR","TOK_RPAR","TOK_LSQB","TOK_RSQB","TOK_LBRACE",
 "TOK_RBRACE","TOK_COLON","TOK_COMMA","TOK_SEMI","TOK_DOT","TOK_ELLIPSIS","TOK_PLUS",
 "TOK_MINUS","TOK_STAR","TOK_DOUBLESTAR","TOK_SLASH","TOK_DOUBLESLASH","TOK_PERCENT",
 "TOK_AT","TOK_VBAR","TOK_AMPER","TOK_CIRCUMFLEX","TOK_TILDE","TOK_LEFTSHIFT",
 "TOK_RIGHTSHIFT","TOK_LESS","TOK_GREATER","TOK_LESSEQUAL","TOK_GREATEREQUAL",
 "TOK_EQEQUAL","TOK_NOTEQUAL","TOK_EQUAL","TOK_COLONEQUAL","TOK_RARROW",
 "TOK_PLUSEQUAL","TOK_MINEQUAL","TOK_STAREQUAL","TOK_DOUBLESTAREQUAL","TOK_SLASHEQUAL",
 "TOK_DOUBLESLASHEQUAL","TOK_PERCENTEQUAL","TOK_ATEQUAL","TOK_VBAREQUAL",
 "TOK_AMPEREQUAL","TOK_CIRCUMFLEXEQUAL","TOK_LEFTSHIFTEQUAL","TOK_RIGHTSHIFTEQUAL",
 "TOK_FALSE","TOK_NONE","TOK_TRUE","TOK_AND","TOK_AS","TOK_ASSERT","TOK_ASYNC",
 "TOK_AWAIT","TOK_BREAK","TOK_CLASS","TOK_CONTINUE","TOK_DEF","TOK_DEL","TOK_ELIF",
 "TOK_ELSE","TOK_EXCEPT","TOK_FINALLY","TOK_FOR","TOK_FROM","TOK_GLOBAL","TOK_IF",
 "TOK_IMPORT","TOK_IN","TOK_IS","TOK_LAMBDA","TOK_NONLOCAL","TOK_NOT","TOK_OR",
 "TOK_PASS","TOK_RAISE","TOK_RETURN","TOK_TRY","TOK_WHILE","TOK_WITH","TOK_YIELD",
]

HEADER = """;; ---------------------------------------------------------------------------
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
prule_table:"""

def main():
    root = os.path.dirname(os.path.dirname(os.path.abspath(__file__)))
    path = os.path.join(root, 'compiler', 'parse.asm')
    src = open(path).read()

    out = [HEADER]
    for tok in ORDER:
        p, i, lbp, rbp, aux, flags, note = ROWS.get(
            tok, (0, 0, "BP_NONE", "BP_NONE", 0, 0, ""))
        c = "   ; %s" % tok + (" -- " + note if note else "")
        out.append("    dq %-12s, %-12s" % (p, i))
        out.append("    db %-11s, %-11s, %-18s, %-9s" % (lbp, rbp, aux, flags) + c)
        out.append("    dd 0")
    block = "\n".join(out)

    start = src.index(';; ---------------------------------------------------------------------------\n;; prule_table')
    end = src.index('\nASM_INIT')
    m = type('M', (), {'start': lambda self: start, 'end': lambda self: end})()
    src = src[:start] + block + "\n" + src[end:]
    open(path, 'w').write(src)
    print("prule_table: %d rows" % len(ORDER))

if __name__ == '__main__':
    main()
