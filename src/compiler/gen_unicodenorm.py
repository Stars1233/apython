#!/usr/bin/env python3
"""Generate src/modules/unicodenorm.asm -- the normalization tables.

The four normal forms need three things the property tables do not carry: the
decomposition mappings (canonical AND compatibility), the canonical combining
classes, and the pairs that RECOMPOSE.  Like gen_unicodedata.py beside it, all
of it is taken from the running interpreter's own `unicodedata` rather than
transcribed from the UCD files, so the two agree by construction.

Three things are worth knowing about the shape of the data:

  * A decomposition is recursive.  U+1E17 decomposes to U+0113 U+0301, and
    U+0113 decomposes again.  The table here is FULLY EXPANDED -- each entry
    is the final sequence -- because an interpreter that expands at run time
    pays a loop per character and can recurse arbitrarily deep, and the
    expanded form is barely larger: it is what CPython's own decomp_data
    stores too.

  * The composition table is not the decomposition table read backwards.  A
    pair only recomposes when the composite is not in the composition
    exclusions, is not a singleton (a one-character decomposition), and does
    not begin with a non-starter.  Rather than transcribe
    CompositionExclusions.txt, each candidate is TESTED: compose it and ask
    the interpreter whether NFC of the pair really is the composite.

  * Hangul is algorithmic in both directions and is not in either table;
    unicodenorm.asm carries the arithmetic.

Refuses to run on anything but CPython 3.12, for the reason gen_tables.py
gives: the outputs are committed, so a regeneration under a different version
would silently retable the interpreter.
"""
import sys
import unicodedata

if sys.implementation.name != "cpython" or sys.version_info[:2] != (3, 12):
    sys.exit("gen_unicodenorm.py must run on CPython 3.12")

MAX = 0x110000

# Hangul, which is arithmetic rather than data.  The asm has the same
# constants; they are here only to keep the syllables out of the tables.
SBASE, LBASE, VBASE, TBASE = 0xAC00, 0x1100, 0x1161, 0x11A7
LCOUNT, VCOUNT, TCOUNT = 19, 21, 28
SCOUNT = LCOUNT * VCOUNT * TCOUNT


def raw_decomposition(ucd, cp):
    """(tag, [code points]) or None.  The tag is "" for a canonical mapping.

    It is kept rather than reduced to a flag because `decomposition()` reports
    it verbatim: U+00A0 is "<noBreak> 0020" and not "<compat> 0020", and there
    are sixteen such tags.
    """
    d = ucd.decomposition(chr(cp))
    if not d:
        return None
    parts = d.split()
    tag = ""
    if parts[0].startswith("<"):
        tag = parts[0]
        parts = parts[1:]
    return tag, [int(p, 16) for p in parts]


def build(ucd):
    """The three tables for one version of the database."""
    raw = {}
    for cp in range(MAX):
        if SBASE <= cp < SBASE + SCOUNT:
            continue                    # Hangul is computed, not stored
        d = raw_decomposition(ucd, cp)
        if d is not None:
            raw[cp] = d

    # The fully expanded forms are taken from the database's OWN normalize()
    # rather than expanded here.  Recursing over the raw mappings gives the
    # same answer for the current version and the WRONG one for the frozen
    # 3.2 copy: Unicode Corrigendum #4 corrected five decompositions, and
    # ucd_3_2_0 keeps the pre-corrigendum expansion -- which is the point of a
    # frozen database -- while its decomposition() reports the corrected
    # mapping.  Asking normalize() picks that up for free, and it is also one
    # fewer thing to get right.
    canon = {}
    compat = {}
    for cp in sorted(raw):
        c = [ord(ch) for ch in ucd.normalize("NFD", chr(cp))]
        if c != [cp]:
            canon[cp] = c
        k = [ord(ch) for ch in ucd.normalize("NFKD", chr(cp))]
        if k != [cp]:
            compat[cp] = k

    # Composition: every canonical PAIR whose composite really is what NFC
    # produces.  A singleton decomposition never composes, and neither does
    # one whose composite is excluded -- both fall out of the test.
    comp = {}
    for cp, (tag, parts) in raw.items():
        if tag or len(parts) != 2:
            continue
        a, b = parts
        if ucd.normalize("NFC", chr(a) + chr(b)) != chr(cp):
            continue                    # excluded, or the pair does not stand
                                        # -- asked of the SAME database, which
                                        # is what makes this reusable for the
                                        # frozen 3.2 copy (gen_ucd32.py)
        comp[(a, b)] = cp

    combining = {cp: ucd.combining(chr(cp))
                 for cp in range(MAX) if ucd.combining(chr(cp))}
    return canon, compat, comp, combining, raw


def emit_decomp(prefix, canon, compat):
    """One index table over one shared pool of code points.

    Both forms are in the same index, keyed by code point: the row carries
    the canonical sequence's span and the compatibility one's, either of
    which may be empty.  NFD reads the first and falls back to the character
    itself; NFKD reads the second and falls back to the first.
    """
    pool = []
    spans = {}

    def place(seq):
        key = tuple(seq)
        if key in spans:
            return spans[key]
        # A sequence that is already a suffix of the pool need not be stored
        # twice; the saving is small but the search is cheap at this size.
        start = len(pool)
        pool.extend(seq)
        spans[key] = (start, len(seq))
        return spans[key]

    rows = []
    for cp in sorted(set(canon) | set(compat)):
        c = place(canon[cp]) if cp in canon else (0, 0)
        k = place(compat[cp]) if cp in compat else (0, 0)
        rows.append((cp, c[0], c[1], k[0], k[1]))

    print(";; %s: %d code points with a decomposition, over a pool of %d"
          % (prefix, len(rows), len(pool)))
    print("align 8")
    print("global %s_index" % prefix)
    print("%s_index:" % prefix)
    for cp, co, cl, ko, kl in rows:
        print("    dd 0x%06x, %d, %d, %d, %d ; canon off/len, compat off/len"
              % (cp, co, cl, ko, kl))
    print("global %s_index_count" % prefix)
    print("%s_index_count: dq %d" % (prefix, len(rows)))
    print("align 8")
    print("global %s_pool" % prefix)
    print("%s_pool:" % prefix)
    for i in range(0, len(pool), 8):
        print("    dd " + ", ".join("0x%06x" % cp for cp in pool[i:i + 8]))
    print()


def emit_raw(prefix, raw):
    """The mapping as the UCD writes it, one level deep and tagged.

    `decomposition()` reports that and not the expanded form, and the tag is
    part of it: U+00A0 is "<noBreak> 0020".  Hangul is absent from `raw` and
    unicodenorm.asm computes it, which is what CPython reports for a syllable
    too.
    """
    tags = sorted({tag for tag, _ in raw.values()})
    pool = []
    rows = []
    for cp in sorted(raw):
        tag, parts = raw[cp]
        rows.append((cp, tags.index(tag), len(pool), len(parts)))
        pool.extend(parts)
    print(";; %s_raw: %d one-level mappings over %d tags"
          % (prefix, len(rows), len(tags)))
    print("align 8")
    print("global %s_tags" % prefix)
    print("%s_tags:" % prefix)
    for i in range(len(tags)):
        print("    dq %s_tag%d" % (prefix, i))
    for i, tag in enumerate(tags):
        print('%s_tag%d: db "%s", 0' % (prefix, i, tag + (" " if tag else "")))
    print("align 8")
    print("global %s_raw_index" % prefix)
    print("%s_raw_index:" % prefix)
    for cp, tag, off, length in rows:
        print("    dd 0x%06x, %d, %d, %d ; tag, offset, length"
              % (cp, tag, off, length))
    print("global %s_raw_count" % prefix)
    print("%s_raw_count: dq %d" % (prefix, len(rows)))
    print("align 8")
    print("global %s_raw_pool" % prefix)
    print("%s_raw_pool:" % prefix)
    for i in range(0, len(pool), 8):
        print("    dd " + ", ".join("0x%06x" % cp for cp in pool[i:i + 8]))
    print()


def emit_comp(prefix, comp):
    """The recomposition table, keyed by the pair packed into one 64-bit word
    so that a binary search compares a single number."""
    rows = sorted((a << 21) | b for a, b in comp)
    print(";; %s: %d canonical pairs that recompose" % (prefix, len(rows)))
    print("align 8")
    print("global %s_pairs" % prefix)
    print("%s_pairs:" % prefix)
    inverse = {(a << 21) | b: cp for (a, b), cp in comp.items()}
    for key in rows:
        print("    dq 0x%011x, 0x%06x" % (key, inverse[key]))
    print("global %s_pair_count" % prefix)
    print("%s_pair_count: dq %d" % (prefix, len(rows)))
    print()


def emit_combining(prefix, combining):
    """Only the nonzero classes, sorted, binary searched: everything else is
    a starter and the answer is 0."""
    rows = sorted(combining.items())
    print(";; %s: %d code points with a nonzero combining class" % (prefix, len(rows)))
    print("align 8")
    print("global %s_ccc" % prefix)
    print("%s_ccc:" % prefix)
    for cp, ccc in rows:
        print("    dd 0x%06x, %d" % (cp, ccc))
    print("global %s_ccc_count" % prefix)
    print("%s_ccc_count: dq %d" % (prefix, len(rows)))
    print()


def main():
    print(";; unicodenorm.asm - GENERATED by src/compiler/gen_unicodenorm.py.")
    print(";; Do not edit.  Unicode %s, from the running interpreter's own"
          % unicodedata.unidata_version)
    print(";; unicodedata -- the same source the property and name tables take")
    print(";; theirs from.")
    print(";;")
    print(";; The decompositions are FULLY EXPANDED, so a normalization walks")
    print(";; each character once and never recurses.  Hangul is not here: it")
    print(";; is arithmetic, and unicodenorm.asm carries it.")
    print()
    print('%include "macros.inc"')
    print()
    print("section .rodata")
    print()

    canon, compat, comp, combining, raw = build(unicodedata)
    emit_decomp("udn", canon, compat)
    emit_raw("udn", raw)
    emit_comp("udn", comp)
    emit_combining("udn", combining)


if __name__ == "__main__":
    main()
