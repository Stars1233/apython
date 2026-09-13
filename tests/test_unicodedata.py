# unicodedata: the half of it the tree already had tables for.
#
# `import unicodedata` was a ModuleNotFoundError, and a test module that
# imports one dies at COLLECTION with every one of its tests unrun -- 308 of
# them across CPython's own suite.  The names it resolves were already here,
# generated for `\N{...}` by src/compiler/gen_unicodename.py from a running
# CPython, and the decimal values were already derived from the flag table
# str.isdecimal() reads.
#
# The property half needed tables nothing else in the tree wanted, and
# src/modules/unicodedataprops.asm is those, generated the same way from the
# same source.  decomposition() and normalize() are still absent -- they are
# the expensive pair -- and absent rather than approximated, which is an
# AttributeError a caller can see.
import unicodedata

# --- lookup(), which is the same search \N{...} uses -------------------------

NAMES = [
    "LATIN SMALL LETTER A",
    "LATIN CAPITAL LETTER Z",
    "SPACE",
    "EURO SIGN",
    "GREEK SMALL LETTER ALPHA",
    "CYRILLIC CAPITAL LETTER ZHE",
    "HEBREW LETTER ALEF",
    "ARABIC-INDIC DIGIT SEVEN",
    "HANGUL SYLLABLE GA",
    "CJK UNIFIED IDEOGRAPH-4E00",
    "MUSICAL SYMBOL G CLEF",
    "MICRO SIGN",
    "GREEK SMALL LETTER MU",
    "BLACK STAR",
    "SNOWMAN",
]

for name in NAMES:
    print("%-32s %r" % (name, unicodedata.lookup(name)))

# Case does not matter, as it does not for \N{...}.
print("lowercase:", repr(unicodedata.lookup("latin small letter a")))
print("mixed:", repr(unicodedata.lookup("Euro Sign")))

# The aliases are a way IN and not a name: lookup finds them, name() does not.
for alias in ("LF", "NUL", "ESC", "BOM", "ZWJ", "NBSP"):
    print("alias %-5s %r" % (alias, unicodedata.lookup(alias)))

for bad in ("NO SUCH CHARACTER", "", "LATIN SMALL LETTER", "CJK UNIFIED IDEOGRAPH-FFFFFF"):
    try:
        unicodedata.lookup(bad)
        print("accepted %r" % (bad,))
    except KeyError:
        print("refused %r" % (bad,))

# --- name() ------------------------------------------------------------------

for ch in "aZ €αא٧가一鿿\U0001d11eµμ":
    print("name %-4r %s" % (ch, unicodedata.name(ch)))

# Astral and the far end of the CJK ranges.
print("astral:", unicodedata.name("\U00020000"))
print("astral 2:", unicodedata.name("\U0002a700"))

# A character with no name.
for ch in ("\n", "\x00", "\x1b", ""):
    try:
        print("named %r: %s" % (ch, unicodedata.name(ch)))
    except ValueError:
        print("unnamed %r" % (ch,))
    print("  default: %r" % (unicodedata.name(ch, "<none>"),))

# name() and lookup() are inverses wherever both answer.
round_tripped = 0
checked = 0
for cp in list(range(0, 0x350)) + list(range(0x2000, 0x2100)) + \
          [0x4E00, 0x9FFF, 0xAC00, 0xD7A3, 0x1D11E, 0x20000, 0x2A6DF]:
    ch = chr(cp)
    try:
        n = unicodedata.name(ch)
    except ValueError:
        continue
    checked += 1
    if unicodedata.lookup(n) == ch:
        round_tripped += 1
print("round trips: %d of %d" % (round_tripped, checked))

# --- decimal() ---------------------------------------------------------------

print()
for ch in "0123456789":
    assert unicodedata.decimal(ch) == int(ch)
print("ascii digits: ok")
print("arabic-indic:", [unicodedata.decimal(chr(0x660 + i)) for i in range(10)])
print("devanagari:", [unicodedata.decimal(chr(0x966 + i)) for i in range(10)])
print("fullwidth:", [unicodedata.decimal(chr(0xFF10 + i)) for i in range(10)])
print("math bold:", [unicodedata.decimal(chr(0x1D7CE + i)) for i in range(10)])

for ch in ("a", " ", "½", "Ⅰ"):
    try:
        print("decimal %r: %d" % (ch, unicodedata.decimal(ch)))
    except ValueError:
        print("not a decimal: %r" % (ch,))
    print("  default: %r" % (unicodedata.decimal(ch, -1),))

# --- the arguments each one insists on ---------------------------------------

print()
for fn, arg in ((unicodedata.name, "ab"), (unicodedata.name, ""),
                (unicodedata.name, 97), (unicodedata.decimal, "12"),
                (unicodedata.decimal, None)):
    try:
        fn(arg)
        print("accepted %r" % (arg,))
    except TypeError:
        print("refused %r" % (arg,))

print("version present:", isinstance(unicodedata.unidata_version, str))

# --- the property half -------------------------------------------------------
#
# A differential sweep rather than a handful of cases: the tables are
# generated from CPython's own unicodedata, so what is being tested is the
# run-compression and the binary search over it, and those fail at BOUNDARIES
# -- which a sample of interesting characters would step right over.

print()
SAMPLE = (list(range(0, 0x600)) + list(range(0x2000, 0x2200)) +
          list(range(0x3000, 0x3100)) + list(range(0xFF00, 0xFF70)) +
          list(range(0x1D400, 0x1D420)) + list(range(0x1F300, 0x1F320)) +
          [0x4E00, 0x9FFF, 0xAC00, 0xD7A3, 0xE000, 0xFFFD, 0x10000,
           0x20000, 0x2A6DF, 0x10FFFF, 0xD800, 0xDFFF])

def digest(values):
    # Not hash(): a str's hash is randomised per process, so it says nothing
    # when the two sides are two processes.
    h = 0
    for i, v in enumerate(values):
        for c in str(v):
            h = (h * 131 + ord(c)) % (2 ** 61 - 1)
        h = (h * 131 + i) % (2 ** 61 - 1)
    return h


props = ("category", "bidirectional", "east_asian_width", "combining",
         "mirrored")
for name in props:
    fn = getattr(unicodedata, name)
    print("%-18s digest %s" % (name, digest([fn(chr(cp)) for cp in SAMPLE])))

print("sampled:", len(SAMPLE))

# Spot checks, so a digest mismatch has something legible beside it.
for ch in "aZ0 \n(\u0301\u0e33":
    print("%-8r cat=%-3s bidi=%-3s eaw=%-3s comb=%-4d mirror=%d"
          % (ch, unicodedata.category(ch), unicodedata.bidirectional(ch),
             unicodedata.east_asian_width(ch), unicodedata.combining(ch),
             unicodedata.mirrored(ch)))

# digit() is wider than decimal(): the superscripts and the circled digits
# are digits and not decimals.
print()
for ch in "7\u00b2\u2460\u0660a":
    for fn in (unicodedata.decimal, unicodedata.digit):
        try:
            print("%s %r: %d" % (fn.__name__, ch, fn(ch)))
        except ValueError:
            print("%s %r: not one" % (fn.__name__, ch))

# numeric() carries values a float cannot say exactly, so the table stores an
# exact numerator and denominator.
print()
for ch in "7\u00bd\u2153\u216b\u0bf0\u137c":
    print("numeric %r: %r" % (ch, unicodedata.numeric(ch)))
print("numeric digest:",
      digest([unicodedata.numeric(chr(cp), None) for cp in SAMPLE]))
print("digit digest:",
      digest([unicodedata.digit(chr(cp), None) for cp in SAMPLE]))

for ch in ("a", " "):
    print("numeric default %r: %r" % (ch, unicodedata.numeric(ch, None)))
    try:
        unicodedata.numeric(ch)
    except ValueError:
        print("  raises without one")

print("done")
