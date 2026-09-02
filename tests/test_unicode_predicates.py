# The twelve str is* predicates, over the whole of Unicode.
#
# All of them were ASCII byte loops that answered False for anything above
# 127: "é".isalpha() was False, Arabic-Indic digits were not digits, and
# isnumeric was an alias for isdecimal.  They now read the same generated
# character-flag table the case mappings do.
#
# The sweep is checksummed, for the same reason the case sweep is; the named
# cases below are what to read when the checksum moves.

NAMES = ("isalpha", "isdecimal", "isdigit", "isnumeric", "isalnum", "isspace",
         "isupper", "islower", "istitle", "isprintable", "isidentifier",
         "isascii")

print("=== a checksum of every predicate, plane by plane ===")
for plane in range(17):
    base = plane << 16
    h = 0
    for off in range(0x10000):
        cp = base + off
        if 0xD800 <= cp <= 0xDFFF:
            continue
        c = chr(cp)
        bits = 0
        for i, n in enumerate(NAMES):
            if getattr(c, n)():
                bits |= 1 << i
        h = (h * 1000003 + bits) & 0xFFFFFFFFFFFF
    print("plane %d %012x" % (plane, h))

print("=== strings, where the sequence rules live ===")
CASES = [
    "", " ", "a", "A", "1", "_", "abc", "ABC", "Abc", "aBc", "A B", "Ab Cd",
    "a1", "1a", "a_b", "_a", "0x", "3.14", "  ", "\t\n\r\v\f", " ",
    "é", "É", "Éa", "ß", "ﬁ", "İ", "ı", "ǅ", "ǅa", "aǅ", "Ǆ", "ǆ",
    "٣", "٣٤", "½", "Ⅷ", "²", "⑦", "〇", "一", "中文", "עברית", "ʰ",
    "café", "CAFÉ", "Café", "über", "ÜBER", "Über", "Straße", "STRASSE",
    "π", "Π", "Ππ", "πΠ", "Ǳ Ǆ", "ǲǳ", "hello­world", "á",
    "Á", "́a", "naïve", "NAÏVE", "Naïve", "𝐀", "𝐚", "𝟏",
    "ⅷ", "Ⅷⅷ", "he said", "He Said", "HE SAID", "he Said",
]
for s in CASES:
    print(repr(s), "".join("1" if getattr(s, n)() else "0" for n in NAMES))

print("=== identifiers ===")
for s in ("a", "_", "_a", "a1", "1a", "", " a", "a ", "über", "π", "变量",
          "á", "́a", "a-b", "a.b", "True", "class", "𝐀", "𝟏"):
    print(repr(s), s.isidentifier())

print("=== the three cased ones agree with the case methods ===")
for s in ("abc", "ABC", "Abc", "aBc", "", "1", "a1", "A1", "É", "é", "ǅ",
          "ΣΟΦΟΣ", "σοφος", "Σοφος"):
    print(repr(s), s.isupper(), s.islower(), s.istitle(),
          s == s.upper(), s == s.lower(), s == s.title())

print("=== repr and ascii escape by codepoint, not by byte ===")
ESC = ["", "a", "a'b", 'a"b', "\n\t\r", "\x00\x1f\x7f", "\x80", "\xa0",
       "\xad", "\x85", "é", "É", "ß", "µ", "中文", "́", "á", "​",
       "\U0001F600", "￿", "�", "𝐀", "\U000E0001", "naïve", "→",
       "\N{SNOWMAN}", "日本語 mixed ascii"]
for s in ESC:
    print(repr(s), ascii(s), len(repr(s)), len(ascii(s)))
print(repr(["\xa0", "é"]), ascii(["\xa0", "é"]))
print(repr({"\xad": "é"}), ascii({"\xad": "é"}))
print(repr(("☃",)), ascii(("☃",)))
