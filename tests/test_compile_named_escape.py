# \N{NAME} resolves a Unicode character name.  It used to pass through as the
# literal text `\N{NAME}` -- silently wrong data, not an error.  The table is
# generated from the running interpreter's unicodedata; CJK ideographs are
# algorithmic and stay out of it, with only the ranges they are valid over
# stored so that a bogus one is still rejected.
print("\N{GREEK SMALL LETTER ALPHA}", "\N{LATIN SMALL LETTER A WITH ACUTE}")
print("\N{EM SPACE}" == " ", "\N{EN SPACE}" == " ")
print("\N{SNOWMAN}" == "☃", "\N{BULLET}" == "•")

# Case-insensitive, as CPython's is.
print("\N{latin small letter a}", "\N{Greek Small Letter Beta}" == "β")

# Control-character aliases resolve too.
print("\N{NUL}" == "\x00", "\N{LINE FEED}" == "\n", "\N{BOM}" == "﻿")

# The algorithmic family, in and out of range.
print("\N{CJK UNIFIED IDEOGRAPH-4E00}" == "一")
print("\N{CJK UNIFIED IDEOGRAPH-20000}" == "\U00020000")
print("\N{HANGUL SYLLABLE GA}" == "가", "\N{HANGUL SYLLABLE HIH}" == "힣")

# In f-strings, raw strings and concatenations.
v = 1
print(f"\N{DEGREE SIGN}{v}", "\N{DEGREE SIGN}" "\N{PLUS-MINUS SIGN}")
print(repr(r"\N{DEGREE SIGN}"))

# In a bytes literal it is not an escape at all: the value is the backslash
# and the text.  (CPython warns about it, so it is spelled without one here.)
print(b"\\N{DEGREE SIGN}" == bytes([92]) + b"N{DEGREE SIGN}")

# Lengths are in code points, and the text is real UTF-8.
s = "\N{GREEK SMALL LETTER ALPHA}\N{GREEK SMALL LETTER BETA}"
print(len(s), s.encode("utf-8"), s[0], s[1])


def check(src):
    try:
        return eval(src)
    except SyntaxError:
        return "SyntaxError"


print(check(r'"\N{NO SUCH CHARACTER AT ALL}"'))
print(check(r'"\N{CJK UNIFIED IDEOGRAPH-0041}"'))
print(check(r'"\N{"'))
print(check(r'"\N"'))
print(check(r'"\N{GREEK SMALL LETTER ALPHA}"'))
