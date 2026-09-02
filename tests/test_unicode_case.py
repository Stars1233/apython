# The six case methods, over the whole of Unicode.
#
# They were ASCII byte loops that allocated the result at the input's byte
# length before deciding anything, so "e" with an acute accent upper-cased to
# itself -- and the allocation is why it could not simply be fixed in place:
# eszett upper-cases to two characters and the dotless i shortens.
#
# The sweep is checksummed rather than printed: a million lines would not be
# diffable.  The named cases below it are the ones worth reading when the
# checksum moves.

def fold(seq):
    h = 0
    for s in seq:
        for ch in s:
            h = (h * 1000003 + ord(ch)) & 0xFFFFFFFFFFFF
        h = (h * 1000003 + 1) & 0xFFFFFFFFFFFF
    return h

print("=== a checksum of every mapping, plane by plane ===")
for plane in range(17):
    base = plane << 16
    acc = []
    for off in range(0, 0x10000, 1):
        cp = base + off
        if 0xD800 <= cp <= 0xDFFF:
            continue
        c = chr(cp)
        acc.append(c.upper())
        acc.append(c.lower())
        acc.append(c.title())
        acc.append(c.casefold())
        acc.append(c.swapcase())
        acc.append(c.capitalize())
    print("plane %d %012x" % (plane, fold(acc)))

print("=== words, where the context rules live ===")
WORDS = [
    "", "a", "A", "abc", "ABC", "Hello World", "hello world", "HELLO WORLD",
    "a1b2", "don't", "DON'T", "o'brien", "l'ecole", "  spaced  out  ",
    "ß", "straße", "STRASSE", "Straße", "ﬁle", "ﬄuent",
    "İstanbul", "ıstanbul", "IJsland", "ǅungla", "ǄUNGLA", "ǆungla",
    "ΣΟΦΟΣ", "σοφος", "Σοφος", "ΑΣ", "ας", "ΑΣΑ", "Α'Σ", "Α Σ", "ΑΣ.",
    "ὈΔΥΣΣΕΎΣ", "ὀδυσσεύς", "ΐ", "ΰ", "ǰ", "ŉ", "ǳ", "Ǳ", "ǲ",
    "ÉLAN vital", "élan Vital", "ÅNGSTRÖM", "ångström",
    "ЖУРНАЛ", "журнал", "Ⅷ", "ⅷ", "𐐨𐐩", "𐐀𐐁", "ᾀ", "ᾈ", "ᾼ", "ᾳ",
    "中文 english", "עברית ABC", "ʰa", "aʰb", "áb", "́A",
]
for s in WORDS:
    print(repr(s))
    print("  upper", repr(s.upper()), "lower", repr(s.lower()))
    print("  title", repr(s.title()), "cap", repr(s.capitalize()))
    print("  swap ", repr(s.swapcase()), "fold", repr(s.casefold()))

print("=== lengths, since these are the mappings that change them ===")
for s in ("ß", "ﬁ", "ﬃ", "İ", "ŉ", "ΐ", "ǰ", "ǅ", "ΣΣΣ"):
    print(repr(s), len(s), len(s.upper()), len(s.lower()), len(s.title()),
          len(s.casefold()))

print("=== round trips ===")
for s in ("Hello", "ΣΟΦΟΣ", "straße", "İstanbul"):
    print(repr(s), s.upper().lower(), s.lower().upper(), s.title().swapcase())

print("=== they still answer str ===")
print(type("a".upper()).__name__, type("a".casefold()).__name__)

class S(str):
    pass

print(repr(S("aBc").upper()), type(S("aBc").upper()).__name__)
