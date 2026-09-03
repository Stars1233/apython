#!/usr/bin/env python3
"""Regenerate src/compiler/unicodecase.asm -- the Unicode case mappings.

    python3 src/compiler/gen_unicodecase.py > src/compiler/unicodecase.asm

The mappings come from the running interpreter's own str.upper/lower/title/
casefold, the same way gen_unicodename.py takes its names from unicodedata and
gen_tables.py its opcode metadata from `opcode`: asking the reference
implementation is what keeps a table like this from going quietly out of date.
A wrong entry here is a wrong answer, not a crash, so nothing else would catch
it.

Two tables, both sorted by codepoint and both binary-searched:

  ucase_ranges   32-byte entries {lo, hi, d_upper, d_lower, d_title, d_fold,
                 exp_index, pad}.  Every codepoint in [lo, hi] maps by adding
                 the delta.  Only ranges that map to something other than
                 themselves are here, so a lookup that finds nothing means the
                 character has no case.

                 exp_index is -1 for an ordinary range.  Where it is not, the
                 range is a single codepoint whose mapping is not one
                 character -- eszett upper-cases to SS -- and it indexes:

  ucase_expand   56-byte entries {cp, pad, upper[12], lower[12], title[12],
                 fold[12]}, each field the replacement as NUL-padded UTF-8.
                 All four are filled even where only one of them expands, so
                 the caller copies bytes and never has to ask which case it is
                 looking at.

  uflag_starts   8-byte entries {start, flags}, sorted.  The flags hold from
                 `start` until the next entry's, so a lookup is one binary
                 search and no bounds test.  These are what the is* predicates
                 ask, and what str.title() asks to decide where a word begins.

Run-compression is what makes this small: the deltas and the categories are
constant across long stretches, so ~1.1M codepoints become a few thousand
entries in each table.
"""
import sys
import unicodedata

# Kept in step with UF_* in src/methods/str_case.asm.
UF_ALPHA      = 1 << 0
UF_DECIMAL    = 1 << 1
UF_DIGIT      = 1 << 2
UF_NUMERIC    = 1 << 3
UF_SPACE      = 1 << 4
UF_UPPER      = 1 << 5
UF_LOWER      = 1 << 6
UF_TITLECASE  = 1 << 7
UF_PRINTABLE  = 1 << 8
UF_XID_CONT   = 1 << 9
UF_XID_START  = 1 << 10
UF_CASED      = 1 << 11
UF_CASE_IGN   = 1 << 12


def char_flags(cp):
    c = chr(cp)
    f = 0
    if c.isalpha():
        f |= UF_ALPHA
    if c.isdecimal():
        f |= UF_DECIMAL
    if c.isdigit():
        f |= UF_DIGIT
    if c.isnumeric():
        f |= UF_NUMERIC
    if c.isspace():
        f |= UF_SPACE
    if c.isupper():
        f |= UF_UPPER
    if c.islower():
        f |= UF_LOWER
    if unicodedata.category(c) == "Lt":
        f |= UF_TITLECASE
    if c.isprintable():
        f |= UF_PRINTABLE
    # isidentifier() answers for a whole string, so XID_Start is the character
    # on its own and XID_Continue is it after one that already starts one.
    if ("a" + c).isidentifier():
        f |= UF_XID_CONT
    if c.isidentifier():
        f |= UF_XID_START
    # Cased and Case_Ignorable, which the final-sigma rule in str.lower() needs
    # and which unicodedata does not expose.  Cased is Uppercase | Lowercase |
    # Lt, which is exactly isupper | islower | category Lt.
    #
    # Case_Ignorable is derived from CPython's own answer rather than from the
    # categories, because it is not a category: Word_Break MidLetter and
    # friends are in it too, and U+0387 is the sort of entry a transcription
    # would miss.  Two probes of handle_capital_sigma pin it exactly --
    #   A: "a" + c + "S" lower-cases to a final sigma  <=>  c is ignorable or cased
    #   B: "a" + "S" + c lower-cases to a final sigma  <=>  c is ignorable or uncased
    # so the conjunction is "ignorable", whichever way cased comes out.
    if c.isupper() or c.islower() or unicodedata.category(c) == "Lt":
        f |= UF_CASED
    if ("a" + c + "\u03A3").lower().endswith("\u03C2") and \
       ("a\u03A3" + c).lower()[1] == "\u03C2":
        f |= UF_CASE_IGN
    return f

MAXCP = 0x110000
EXP_FIELD = 12          # bytes per replacement, NUL-padded


def emit(out, s=""):
    out.write(s + "\n")


def main():
    ranges = []         # (lo, hi, du, dl, dt, df, exp_index)
    expand = []         # (cp, upper, lower, title, fold) as str
    prev_key = None
    uflags = []         # (start, flags)
    prev_flags = None

    for cp in range(MAXCP):
        ff = char_flags(cp)
        if ff != prev_flags:
            uflags.append((cp, ff))
            prev_flags = ff

        c = chr(cp)
        u, l, t, f = c.upper(), c.lower(), c.title(), c.casefold()
        multi = len(u) > 1 or len(l) > 1 or len(t) > 1 or len(f) > 1
        if multi:
            ranges.append((cp, cp, 0, 0, 0, 0, len(expand)))
            expand.append((cp, u, l, t, f))
            prev_key = None
            continue
        du, dl = ord(u) - cp, ord(l) - cp
        dt, df = ord(t) - cp, ord(f) - cp
        if du == dl == dt == df == 0:
            prev_key = None                 # no case: not in the table at all
            continue
        key = (du, dl, dt, df)
        if prev_key == key and ranges[-1][1] == cp - 1 and ranges[-1][6] < 0:
            lo, _hi, a, b, cc, d, e = ranges[-1]
            ranges[-1] = (lo, cp, a, b, cc, d, e)
        else:
            ranges.append((cp, cp, du, dl, dt, df, -1))
        prev_key = key

    # Two bounds the reader depends on.  The field is what the table holds;
    # the four-to-one is what lets str_case_map size its output buffer at four
    # bytes per input byte without a counting pass.
    for cp, u, l, t, f in expand:
        src = len(chr(cp).encode("utf-8"))
        for s in (u, l, t, f):
            n = len(s.encode("utf-8"))
            if n >= EXP_FIELD:
                raise SystemExit("expansion too long for the field: %r" % s)
            if n > 4 * src:
                raise SystemExit("expansion over 4x its source: U+%04X -> %r"
                                 % (cp, s))

    out = sys.stdout
    emit(out, ";; unicodecase.asm - GENERATED by src/compiler/gen_unicodecase.py.")
    emit(out, ";; Do not edit.")
    emit(out, ";;")
    emit(out, ";; The Unicode case mappings, taken from the running interpreter's own")
    emit(out, ";; str.upper/lower/title/casefold.  See the generator for the layout;")
    emit(out, ";; in short, %d ranges of equal delta, %d codepoints whose mapping is"
              % (len(ranges), len(expand)))
    emit(out, ";; not one character, and %d runs of equal character flags." % len(uflags))
    emit(out, ";; A codepoint in no case range has no case.")
    emit(out)
    # A data-only object still needs the marker, or the linker assumes an
    # executable stack for the whole program and says so.
    emit(out, "section .note.GNU-stack noalloc noexec nowrite progbits")
    emit(out)
    emit(out, "section .rodata")
    emit(out)
    emit(out, "global ucase_range_count")
    emit(out, "global ucase_ranges")
    emit(out, "global ucase_expand")
    emit(out, "global uflag_count")
    emit(out, "global uflag_starts")
    emit(out)
    emit(out, "align 8")
    emit(out, "ucase_range_count: dq %d" % len(ranges))
    emit(out)
    emit(out, "align 32")
    emit(out, "ucase_ranges:")
    for lo, hi, du, dl, dt, df, ei in ranges:
        emit(out, "    dd 0x%X, 0x%X, %d, %d, %d, %d, %d, 0"
                  % (lo, hi, du, dl, dt, df, ei))
    emit(out)
    emit(out, "align 32")
    emit(out, "ucase_expand:")
    for cp, u, l, t, f in expand:
        emit(out, "    dd 0x%X, 0" % cp)
        for s in (u, l, t, f):
            b = s.encode("utf-8")
            if b:
                emit(out, "    db " + ",".join("0x%02X" % x for x in b))
            if len(b) < EXP_FIELD:
                emit(out, "    times %d db 0" % (EXP_FIELD - len(b)))
    emit(out)

    emit(out, "align 8")
    emit(out, "uflag_count: dq %d" % len(uflags))
    emit(out)
    emit(out, "align 8")
    emit(out, "uflag_starts:")
    for start, ff in uflags:
        emit(out, "    dd 0x%X, 0x%X" % (start, ff))
    emit(out)


if __name__ == "__main__":
    main()
