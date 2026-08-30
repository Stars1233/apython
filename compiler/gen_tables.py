#!/usr/bin/env python3
"""Generate compiler/tables.asm — the compiler's static lookup tables.

Run from the repository root with a CPython 3.12:

    python3 compiler/gen_tables.py > compiler/tables.asm

The output is committed, so building apython never needs Python.  Regenerate
only when the target bytecode version changes.

The opcode metadata comes from the running interpreter's own `opcode` and `dis`
modules rather than from a hand-transcribed list, because a single wrong CACHE
count corrupts the instruction that follows it and a single wrong stack effect
produces a co_stacksize that silently corrupts the frame pool.
"""
import sys, dis, opcode

if sys.version_info[:2] != (3, 12):
    sys.exit("must be run under CPython 3.12; got %s" % sys.version.split()[0])

out = []
w = out.append

# --------------------------------------------------------------------------
KEYWORDS = [
    ("False","TOK_FALSE"), ("None","TOK_NONE"), ("True","TOK_TRUE"),
    ("and","TOK_AND"), ("as","TOK_AS"), ("assert","TOK_ASSERT"),
    ("async","TOK_ASYNC"), ("await","TOK_AWAIT"), ("break","TOK_BREAK"),
    ("class","TOK_CLASS"), ("continue","TOK_CONTINUE"), ("def","TOK_DEF"),
    ("del","TOK_DEL"), ("elif","TOK_ELIF"), ("else","TOK_ELSE"),
    ("except","TOK_EXCEPT"), ("finally","TOK_FINALLY"), ("for","TOK_FOR"),
    ("from","TOK_FROM"), ("global","TOK_GLOBAL"), ("if","TOK_IF"),
    ("import","TOK_IMPORT"), ("in","TOK_IN"), ("is","TOK_IS"),
    ("lambda","TOK_LAMBDA"), ("nonlocal","TOK_NONLOCAL"), ("not","TOK_NOT"),
    ("or","TOK_OR"), ("pass","TOK_PASS"), ("raise","TOK_RAISE"),
    ("return","TOK_RETURN"), ("try","TOK_TRY"), ("while","TOK_WHILE"),
    ("with","TOK_WITH"), ("yield","TOK_YIELD"),
]
assert len(KEYWORDS) == 35
assert all(2 <= len(k) <= 8 for k, _ in KEYWORDS), "packed compare assumes <= 8 bytes"

OPERATORS = [
    ("(","TOK_LPAR"), (")","TOK_RPAR"), ("[","TOK_LSQB"), ("]","TOK_RSQB"),
    ("{","TOK_LBRACE"), ("}","TOK_RBRACE"), (",","TOK_COMMA"), (";","TOK_SEMI"),
    ("~","TOK_TILDE"),
    (":=","TOK_COLONEQUAL"), (":","TOK_COLON"),
    ("...","TOK_ELLIPSIS"), (".","TOK_DOT"),
    ("**=","TOK_DOUBLESTAREQUAL"), ("**","TOK_DOUBLESTAR"),
    ("*=","TOK_STAREQUAL"), ("*","TOK_STAR"),
    ("//=","TOK_DOUBLESLASHEQUAL"), ("//","TOK_DOUBLESLASH"),
    ("/=","TOK_SLASHEQUAL"), ("/","TOK_SLASH"),
    ("+=","TOK_PLUSEQUAL"), ("+","TOK_PLUS"),
    ("->","TOK_RARROW"), ("-=","TOK_MINEQUAL"), ("-","TOK_MINUS"),
    ("%=","TOK_PERCENTEQUAL"), ("%","TOK_PERCENT"),
    ("@=","TOK_ATEQUAL"), ("@","TOK_AT"),
    ("|=","TOK_VBAREQUAL"), ("|","TOK_VBAR"),
    ("&=","TOK_AMPEREQUAL"), ("&","TOK_AMPER"),
    ("^=","TOK_CIRCUMFLEXEQUAL"), ("^","TOK_CIRCUMFLEX"),
    ("<<=","TOK_LEFTSHIFTEQUAL"), ("<<","TOK_LEFTSHIFT"),
    ("<=","TOK_LESSEQUAL"), ("<","TOK_LESS"),
    (">>=","TOK_RIGHTSHIFTEQUAL"), (">>","TOK_RIGHTSHIFT"),
    (">=","TOK_GREATEREQUAL"), (">","TOK_GREATER"),
    ("==","TOK_EQEQUAL"), ("=","TOK_EQUAL"),
    ("!=","TOK_NOTEQUAL"),
]
assert all(1 <= len(o) <= 3 for o, _ in OPERATORS), "packed compare assumes <= 3 bytes"

w("; tables.asm - Static lookup tables for the Python source compiler")
w(";")
w("; GENERATED FILE.  Regenerate with:")
w(";     python3 compiler/gen_tables.py > compiler/tables.asm")
w("; The output is committed so that building apython never needs Python.")
w(";")
w("; The opcode metadata is taken from CPython %s's own opcode and dis" % ".".join(map(str, sys.version_info[:3])))
w("; modules, not transcribed by hand: one wrong CACHE count corrupts the")
w("; instruction after it, and one wrong stack effect yields a co_stacksize that")
w("; silently corrupts apython's frame pool.")
w("")
w('%include "macros.inc"')
w('%include "object.inc"')
w('%include "opcodes.inc"')
w('%include "compiler.inc"')
w("")
w("section .rodata")
w("")

# --- character classes ----------------------------------------------------
w(";; ---------------------------------------------------------------------------")
w(";; cc_table - one byte of class flags per source byte.  Every scanning decision")
w(";; in the lexer is a load and a test against this table.")
w(";;")
w(";; Bytes 0x80..0xFF are marked CC_IDSTART|CC_IDCONT: a permissive UTF-8 lead.")
w(";; That accepts a few identifiers CPython rejects (non-XID_Start code points)")
w(";; and avoids shipping Unicode property tables.  Deliberate deviation.")
w(";; ---------------------------------------------------------------------------")
w("align 64")
w("global cc_table")
w("cc_table:")
cls = [0]*256
for c in b' \t\f\v':  cls[c] |= 0x01                      # CC_SPACE
for c in b'\n\r':     cls[c] |= 0x02                      # CC_NEWLINE
for c in range(ord('0'), ord('9')+1): cls[c] |= 0x04|0x10|0x80
for c in list(range(ord('a'), ord('z')+1)) + list(range(ord('A'), ord('Z')+1)) + [ord('_')]:
    cls[c] |= 0x08|0x10                                   # IDSTART|IDCONT
for c in range(0x80, 0x100): cls[c] |= 0x08|0x10
for c in b"'\"":      cls[c] |= 0x20                      # CC_QUOTE
for c in set(o[0] for o, _ in OPERATORS): cls[ord(c)] |= 0x40
for c in b'abcdefABCDEF': cls[c] |= 0x80                  # CC_HEX
for i in range(0, 256, 16):
    w("    db " + ", ".join("0x%02x" % v for v in cls[i:i+16])
      + "   ; 0x%02x" % i)
w("")

# --- keywords -------------------------------------------------------------
w(";; ---------------------------------------------------------------------------")
w(";; Keyword recognition.  Every Python keyword is 2..8 bytes, so an identifier")
w(";; of length L in that range is matched with one unaligned 8-byte load, a mask")
w(";; from kw_masks, and a compare against a packed constant.  kw_index narrows")
w(";; the search to the entries sharing a first byte -- under two compares on")
w(";; average, and adding a keyword is one row.")
w(";; ---------------------------------------------------------------------------")
w("align 8")
w("global kw_masks")
w("kw_masks:               ; kw_masks[L] keeps the low L bytes of an 8-byte load")
for L in range(9):
    m = (1 << (8*L)) - 1 if L < 8 else (1<<64)-1
    w("    dq 0x%016x   ; length %d" % (m, L))
w("")
kws = sorted(KEYWORDS, key=lambda kv: (kv[0][0], kv[0]))
w("align 8")
w("global kw_table")
w("kw_table:               ; { packed text, token, length }")
for k, tok in kws:
    packed = int.from_bytes(k.encode().ljust(8, b'\0'), 'little')
    w("    dq 0x%016x" % packed)
    w("    dw %-22s, %d" % (tok, len(k)))
    w("    dd 0")
w("KW_ENT_SIZE equ 16")
w("")
w("align 8")
w("global kw_index")
w("kw_index:               ; kw_index[byte] = { first entry, count }")
idx = {}
for i, (k, _) in enumerate(kws):
    c = ord(k[0])
    if c not in idx: idx[c] = [i, 0]
    idx[c][1] += 1
for c in range(256):
    s, n = idx.get(c, (0, 0))
    w("    db %3d, %d" % (s, n) + ("   ; '%s'" % chr(c) if n else ""))
w("")

# --- operators ------------------------------------------------------------
w(";; ---------------------------------------------------------------------------")
w(";; Operator recognition.  Entries are sorted LONGEST FIRST within each first")
w(";; byte, so the first match found is already the maximal munch -- no separate")
w(";; longest-match pass.  The compare reads up to 4 bytes past the cursor, which")
w(";; is why the source buffer carries a NUL-padded tail.")
w(";; ---------------------------------------------------------------------------")
w("align 4")
w("global op_masks")
w("op_masks:               ; op_masks[L] keeps the low L bytes of a 4-byte load")
for L in range(5):
    m = (1 << (8*L)) - 1 if L < 4 else 0xFFFFFFFF
    w("    dd 0x%08x   ; length %d" % (m, L))
w("")
ops = sorted(OPERATORS, key=lambda kv: (kv[0][0], -len(kv[0]), kv[0]))
w("align 8")
w("global op_table")
w("op_table:               ; { packed text, length, token }")
for o, tok in ops:
    packed = int.from_bytes(o.encode().ljust(4, b'\0'), 'little')
    w("    dd 0x%08x" % packed)
    w("    db %d, %-22s" % (len(o), tok) + "   ; %r" % o)
    w("    dw 0")
w("OP_ENT_SIZE equ 8")
w("")
w("align 8")
w("global op_index")
w("op_index:               ; op_index[byte] = { first entry, count }")
oidx = {}
for i, (o, _) in enumerate(ops):
    c = ord(o[0])
    if c not in oidx: oidx[c] = [i, 0]
    oidx[c][1] += 1
for c in range(256):
    s, n = oidx.get(c, (0, 0))
    w("    db %3d, %d" % (s, n) + ("   ; '%s'" % chr(c) if n else ""))
w("")

# --- opcode metadata ------------------------------------------------------
# dis.stack_effect reports 0 for RETURN_GENERATOR, but the generator prologue
# is always RETURN_GENERATOR; POP_TOP; RESUME -- and that POP_TOP does not run
# when the prologue is emitted.  It runs on the first resume, discarding the
# value send() pushed.  Modelling the opcode as pushing one keeps the pair
# balanced, which is what the depth pass needs; nothing else emits it.
EFFECT_OVERRIDE = {"RETURN_GENERATOR": 1}

NOFALL = {"RETURN_VALUE","RETURN_CONST","RAISE_VARARGS","RERAISE",
          "JUMP_FORWARD","JUMP_BACKWARD","JUMP_BACKWARD_NO_INTERRUPT",
          "INTERPRETER_EXIT"}
BACKWARD = {"JUMP_BACKWARD","JUMP_BACKWARD_NO_INTERRUPT"}
PROBE_ARGS = [0,1,2,3,4,5,7,8,16,255,256]

def effects(op, jump):
    """Return the set of stack effects over probe args, or None if unavailable."""
    seen = set()
    args = PROBE_ARGS if op >= opcode.HAVE_ARGUMENT else [None]
    for a in args:
        try:
            seen.add(dis.stack_effect(op, a, jump=jump))
        except (ValueError, RuntimeError):
            return None
    return seen or None

meta = {}
for op in range(256):
    name = opcode.opname[op]
    if name.startswith("<"):
        meta[op] = (0, 0, 0, 0, None)
        continue
    cache = opcode._inline_cache_entries[op]
    isjump = (op in dis.hasjrel) or (op in dis.hasjabs)
    flags = 0
    if op >= opcode.HAVE_ARGUMENT: flags |= 0x01     # OM_HASARG
    if isjump:                     flags |= 0x02     # OM_JUMP
    if name in BACKWARD:           flags |= 0x04     # OM_JUMPBACK
    if name in NOFALL:             flags |= 0x08     # OM_NOFALL

    if name in EFFECT_OVERRIDE:
        meta[op] = (cache, EFFECT_OVERRIDE[name], flags, 0, name)
        continue

    e = effects(op, False)
    if e is None:
        eff = 0
    elif len(e) == 1:
        eff = e.pop()
        if not -127 <= eff <= 127: eff = 0x80
    else:
        eff = 0x80                                   # SE_VAR

    if isjump:
        je = effects(op, True)
        if je is None:      jeff = 0
        elif len(je) == 1:
            jeff = je.pop()
            if not -127 <= jeff <= 127: jeff = 0x80
        else:               jeff = 0x80
    else:
        jeff = 0
    meta[op] = (cache, eff, flags, jeff, name)

w(";; ---------------------------------------------------------------------------")
w(";; op_meta - the keystone table.  One row per opcode drives four things:")
w(";;   1. CACHE padding      (writer emits .cache zero words after the opcode)")
w(";;   2. instruction sizing (1 + EXTENDED_ARG prefixes + .cache code units)")
w(";;   3. stack depth        (.effect, or comp_effect_var when .effect == SE_VAR)")
w(";;   4. successors         (.flags OM_JUMP / OM_NOFALL, and .jeff for the")
w(";;                          taken edge, which differs for FOR_ITER and SEND)")
w(";;")
w(";; Because every emission routes through this table, 'I forgot the caches on")
w(";; LOAD_SUPER_ATTR' is structurally impossible rather than a per-call-site")
w(";; discipline.  The effects are CPython's, deliberately: apython's")
w(";; op_cleanup_throw pops two where CPython's CLEANUP_THROW is net -1, and the")
w(";; exception-table depths must be computed on CPython's numbers.")
w(";; ---------------------------------------------------------------------------")
w("align 64")
w("global op_meta")
w("op_meta:                ; { cache, effect, flags, jump effect }")
for op in range(256):
    cache, eff, flags, jeff, name = meta[op]
    e = "SE_VAR" if eff == 0x80 else str(eff)
    j = "SE_VAR" if jeff == 0x80 else str(jeff)
    fl = []
    if flags & 0x01: fl.append("OM_HASARG")
    if flags & 0x02: fl.append("OM_JUMP")
    if flags & 0x04: fl.append("OM_JUMPBACK")
    if flags & 0x08: fl.append("OM_NOFALL")
    fs = "|".join(fl) if fl else "0"
    tail = ("   ; %3d %s" % (op, name)) if name else ("   ; %3d" % op)
    w("    db %-6s, %-6s, %-34s, %-6s" % (cache, e, fs, j) + tail)
w("")
w("ASM_INIT")

sys.stdout.write("\n".join(out) + "\n")
