# Two things int() got wrong about the string it was handed.
#
# 1. int(s, base) did not run the Unicode transform that int(s) runs.
#
#    CPython puts every string through
#    _PyUnicode_TransformDecimalAndSpaceToASCII first: a Unicode decimal digit
#    becomes the ASCII digit of the same value and a Unicode space becomes an
#    ASCII space.  The one-argument path did that; the two-argument path read
#    PyStrObject.data straight out and handed it to the parser, so
#    int("１２") was 12 and int("１２", 10) was a ValueError.
#
#    It also made the base-0 leading-zero rule blind: "٠٩" is "09",
#    which base 0 must reject as ambiguous old-style octal, and it was instead
#    read as nothing the scanner recognised.
#
# 2. A SECOND sign was passed through to GMP, which accepted it.
#
#    int("+-1") answered -1.  The parser strips one leading sign itself and
#    then copied whatever followed into the buffer it hands to __gmpz_set_str,
#    and GMP is happy to parse a sign of its own there.  '+' and '-' are not
#    digits in any base, so the copy loop now rejects both.

DIGITS = [
    "٠٩",          # Arabic-Indic          U+0660..
    "۴۲",          # Extended Arabic-Indic U+06F0..
    "०१",          # Devanagari            U+0966..
    "１２３",    # Fullwidth             U+FF10..
    "๑๒",          # Thai                  U+0E50..
    "᱄᱅",          # Warang Citi           U+1C40..
    "\U0001d7ce\U0001d7cf",  # Mathematical bold     U+1D7CE..
    "４２",
    "٥",
    "1２",               # mixed ASCII and not
    "１_２",         # an underscore between them
    "٠٠١",    # leading zeros
]

for s in DIGITS:
    for base in (0, 2, 8, 10, 16, 36):
        try:
            r = int(s, base)
        except ValueError:
            r = "ValueError"
        print(repr(s), base, r)
    try:
        r = int(s)
    except ValueError:
        r = "ValueError"
    print(repr(s), "one-arg", r)

# A Unicode SPACE becomes an ASCII space, so it is legal padding.
SPACES = [" 12", "12 ", " 12 ", "　12", " 12"]
for s in SPACES:
    for base in (0, 10, 16):
        try:
            r = int(s, base)
        except ValueError:
            r = "ValueError"
        print(repr(s), base, r)

# A Unicode digit that is NOT decimal (superscripts, fractions) stays illegal.
for s in ("²", "½", "Ⅰ", "①"):
    for base in (0, 10):
        try:
            r = int(s, base)
        except ValueError:
            r = "ValueError"
        print(repr(s), base, r)

# --- a second sign -------------------------------------------------------
SIGNS = ["+-1", "-+1", "--1", "++1", "+-", "-1-", "1-2", "1+2", "+1-",
         "0x-1", "-0x-1", "+0b-1", "- 1", "+ 1", "-", "+", "-_1", "+1_",
         "3-", "3+", "-3-4"]
for s in SIGNS:
    for base in (0, 2, 10, 16):
        try:
            r = int(s, base)
        except ValueError:
            r = "ValueError"
        print(repr(s), base, r)
    try:
        r = int(s)
    except ValueError:
        r = "ValueError"
    print(repr(s), "one-arg", r)

# --- and the ordinary signs must still work ------------------------------
for s in ("-1", "+1", "-0", "+0", "  -42  ", "-0x1f", "+0b101", "-" + "9" * 30):
    for base in (0, 16 if "x" in s else 10):
        try:
            r = int(s, base)
        except ValueError:
            r = "ValueError"
        print(repr(s), base, r)

# bytes goes through the same parser and has no Unicode to transform.
for v in (b"+-1", b"-1", b"+1", b"\xff"):
    for base in (0, 10):
        try:
            r = int(v, base)
        except ValueError:
            r = "ValueError"
        print(repr(v), base, r)
