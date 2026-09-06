# Whitespace that is not ASCII, and separators that are not one byte.
#
# split() and strip() walked a byte table, so they saw the six ASCII spaces
# and nothing else: "a\xa0b".split() answered ['a\xa0b'] where CPython answers
# ['a', 'b'].  The table cannot be extended -- every character they were
# missing is two or three bytes in UTF-8 -- but the data already exists.
# uflags_of answers UF_SPACE for exactly CPython's set, because
# gen_unicodecase.py generated it from str.isspace(), which is why
# "\xa0".isspace() was already True while "\xa0 ".split() was already wrong.
#
# strip(chars) was worse than incomplete: it compared BYTES, so
# "éèx".strip("è") stripped the 0xC3 lead byte the two share
# and answered a string that is not valid UTF-8.
#
# splitlines has its own set, and it is a different one: it takes \x1c, \x1d,
# \x1e and U+2028 but NOT \x1f, U+00A0 or U+3000.

SPACES = ["\x09", "\x0a", "\x0b", "\x0c", "\x0d", "\x1c", "\x1d", "\x1e",
          "\x1f", "\x20", "\x85", "\xa0", " ", " ", " ",
          " ", " ", " ", " ", " ", " ",
          " ", " ", " ", " ", " ", " ",
          " ", "　"]

print("--- isspace, which was already right ---")
print([c for c in SPACES if not c.isspace()])

print("--- split() sees the same set ---")
for c in SPACES:
    got = ("a" + c + "b").split()
    print("%-8r %r" % (c, got))

print("--- and the leading, trailing and repeated forms ---")
for c in ("\xa0", "　", " ", "\x85"):
    s = c + c + "x" + c + "y" + c
    print("%-8r %r %r %r" % (c, s.split(), s.rsplit(), s.split(None, 1)))
    print("        ", repr(s.strip()), repr(s.lstrip()), repr(s.rstrip()))

print("--- an explicit maxsplit still counts ---")
print("a\xa0b\xa0c".split(None, 1), "a\xa0b\xa0c".rsplit(None, 1))

print("--- strip(chars) is by code point, not by byte ---")
print(repr("\xe9\xe8x".strip("\xe8")))
print(repr("āx".strip("Ā")))
print(repr("中x中".strip("中")))
print(repr("\xe9x\xe9".strip("\xe9")))
print(repr("abc".strip("a\xe9")))
print(repr("\xe9\xe8x\xe8\xe9".strip("\xe9\xe8")))
print(repr("xy".lstrip("　x")), repr("xy".rstrip("y　")))
print(repr("".strip("\xa0")), repr("\xa0\xa0".strip("\xa0")))

print("--- splitlines has its own set ---")
for c in SPACES + ["\r\n"]:
    print("%-8r %r" % (c, ("a" + c + "b").splitlines()))
print(repr("a\r\nb".splitlines(True)))
print(repr("a b".splitlines(True)))

print("--- ASCII stays exactly as it was ---")
print("  a  b  ".split(), "  a  b  ".strip(), "a\tb\nc".split())
print("".split(), " ".split(), "abc".split("b"), "a,b,,c".split(","))
print("x".center(5), repr("  x  ".strip()), repr("\t\nx\r ".strip()))

print("--- a non-ASCII string with no whitespace in it ---")
print("éè中".split(), repr("é中".strip()))
