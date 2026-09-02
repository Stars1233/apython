# The string-shaped bytes and bytearray methods: case, the is* predicates,
# justification, splitlines, expandtabs, translate, maketrans and the two
# affix removals.  bytes had none of them, so `[a-zA-Z0-9_]+` could not even
# be compiled -- re._compiler's _mk_bitmap ends in bits.translate(_BITS_TRANS)
# on a bytearray.
#
# They are ASCII-only on purpose: a bytes has no encoding to consult, so
# b'\xe9'.upper() is b'\xe9' in CPython too, and this file pins that.

SAMPLES = [
    b"", b"a", b"A", b"hello world", b"Hello World", b"HELLO",
    b"h3ll0 w0rld", b"  spaced  ", b"\xe9\xff\x00", b"MiXeD cAsE",
    b"don't stop", b"123", b"1a", b"a1", b"_", b"\ttab\there",
]

print("=== case ===")
for s in SAMPLES:
    for m in ("upper", "lower", "title", "capitalize", "swapcase"):
        print(m, s, getattr(s, m)(), getattr(bytearray(s), m)())

print("=== predicates ===")
for s in SAMPLES:
    print(s,
          s.isalpha(), s.isdigit(), s.isspace(), s.isalnum(), s.isascii(),
          s.isupper(), s.islower(), s.istitle())
    b = bytearray(s)
    print(b.isalpha(), b.isdigit(), b.isspace(), b.isalnum(), b.isascii(),
          b.isupper(), b.islower(), b.istitle())

print("=== istitle corners ===")
for s in (b"A B", b"Ab Cd", b"AB", b"aB", b"A1b", b"A1B", b"1A", b" A ",
          b"Don'T", b"Don't"):
    print(s, s.istitle())

print("=== justification ===")
for s in (b"", b"a", b"ab", b"abc"):
    for w in (0, 1, 2, 3, 4, 5, 6):
        print(s, w, s.ljust(w), s.rjust(w), s.center(w))
        print(s, w, s.ljust(w, b"-"), s.rjust(w, b"-"), s.center(w, b"-"))
        print(bytearray(s).center(w, b"*"))

print("=== zfill ===")
for s in (b"", b"5", b"-5", b"+5", b"abc", b"-", b"+", b"--5"):
    for w in (0, 1, 3, 6):
        print(s, w, s.zfill(w), bytearray(s).zfill(w))

print("=== expandtabs ===")
for s in (b"", b"\t", b"a\tb", b"ab\tc", b"abcdefgh\tx", b"a\nb\tc",
          b"a\rb\tc", b"\t\t", b"a\tb\tc"):
    for n in (0, 1, 4, 8):
        print(s, n, s.expandtabs(n), bytearray(s).expandtabs(n))
    print(s, "default", s.expandtabs())

print("=== splitlines ===")
for s in (b"", b"a", b"a\n", b"a\nb", b"a\r\nb", b"a\rb", b"\n", b"\r\n",
          b"a\n\nb", b"a\r\n\r\nb", b"line\n", b"a\vb", b"a\fb"):
    print(s, s.splitlines(), s.splitlines(True))
    print(bytearray(s).splitlines())

print("=== translate ===")
tbl = bytes.maketrans(b"abc", b"xyz")
print(len(tbl), tbl[97:100])
print(b"aabbcc".translate(tbl))
print(b"aabbcc".translate(None))
print(b"aabbcc".translate(None, b"b"))
print(b"aabbcc".translate(tbl, b"a"))
print(b"aabbcc".translate(tbl, b"abc"))
print(bytearray(b"aabbcc").translate(tbl))
print(bytearray(b"aabbcc").translate(None, b"c"))
print(bytearray.maketrans(b"ab", b"ba"))
print(b"abc".translate(bytes.maketrans(b"", b"")))

print("=== translate errors ===")
try:
    b"x".translate(b"short")
except ValueError as e:
    print("ValueError", e)
try:
    bytes.maketrans(b"ab", b"c")
except ValueError as e:
    print("ValueError", e)

print("=== affixes ===")
for s in (b"", b"abc", b"abcabc", b"abcdef"):
    for a in (b"", b"abc", b"def", b"abcdef", b"x"):
        print(s, a, s.removeprefix(a), s.removesuffix(a))
        print(bytearray(s).removeprefix(a), bytearray(s).removesuffix(a))

print("=== result types ===")
print(type(bytearray(b"ab").upper()).__name__)
print(type(b"ab".upper()).__name__)
print(type(bytearray(b"a\nb").splitlines()[0]).__name__)
print(type(bytearray(b"ab").isascii()).__name__)
print(type(bytearray(b"ab").maketrans(b"a", b"b")).__name__)

print("=== the regex compiler's own call ===")
# _mk_bitmap: a bit array reversed and mapped through a 256-byte table.
_BITS_TRANS = b'0' + b'1'
bits = bytearray(b"0101")
print(bytes(bits))
print(bits.translate(bytes.maketrans(b"01", b"10")))

print("=== construction from a str, and reversed ===")
print(bytes("abc", "utf-8"), bytearray("abc", "utf-8"))
print(bytes("héllo", "utf-8"), bytearray("héllo", "utf-8"))
print(bytes("abc", "ascii"), bytes("abc", "latin-1"))
print(bytes("héllo", "ascii", "ignore"), bytes("héllo", "ascii", "replace"))
print(bytearray("abc", encoding="utf-8"))
print(list(reversed(bytearray(b"abc"))), bytes(reversed(bytearray(b"abc"))))
print(list(reversed(bytearray())), list(reversed(b"xy")))
for bad in (lambda: bytes("abc"), lambda: bytearray("abc"),
            lambda: bytes(1, 2), lambda: bytearray(b"x", "utf-8")):
    try:
        bad()
    except TypeError as e:
        print("TypeError", e)
