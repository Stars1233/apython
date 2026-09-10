# A compiled BYTES pattern is not a str, and the two do not share a layout.
#
# sre_pattern_hash called str_hash on `.pattern` whatever it held, and
# str_hash reads a cached hash from PyStrObject.ob_hash at +24 and WRITES the
# one it computes back there.  PyBytesObject.data starts at +24, so for a
# bytes pattern that read the first eight bytes of the pattern text as a hash
# -- past the end of the object when the pattern is shorter than that -- and,
# on the one-in-2^64 chance those bytes read as -1, overwrote them with the
# hash.  `b"\xff" * 8` is that chance, spelled out.
#
# sre_pattern_richcompare had the same mistake with str_compare, which reads
# the text at PyStrObject.data (+40) where a bytes keeps it at +24.
#
# CPython's test_re.test_pattern_compare_bytes is where the read showed.

import re

# --- the write ------------------------------------------------------------
pat = re.escape(b"\xff" * 8 + b"x")
p = re.compile(pat)
print(p.pattern == pat)
h = hash(p)
print(p.pattern == pat, "after hash")
print(p.pattern)

# --- the read, on a pattern shorter than eight bytes -----------------------
short = re.compile(b"ab")
print(hash(short) == hash(re.compile(b"ab")))
print(short.pattern)

# --- hashing agrees with equality ------------------------------------------
for text in (b"abc", b"ab", b"a", b"", b"\xff\xff", b"x" * 40,
             b"\x00\x00\x00\x00\x00\x00\x00\x00"):
    a = re.compile(text)
    re.purge()
    b = re.compile(text)
    print(repr(text), hash(a) == hash(b), a == b, a != b)

# --- and str patterns still do ---------------------------------------------
for text in ("abc", "", "é" * 5, "x" * 40):
    a = re.compile(text)
    re.purge()
    b = re.compile(text)
    print(repr(text), hash(a) == hash(b), a == b)

# --- a str pattern and a bytes pattern are never equal ---------------------
print(re.compile("abc") == re.compile(b"abc"))
print(re.compile(b"abc") == re.compile("abc"))
print(re.compile("abc") != re.compile(b"abc"))

# --- flags are part of both -------------------------------------------------
print(re.compile(b"abc") == re.compile(b"abc", re.I))
print(re.compile("abc", re.I) == re.compile("abc", re.I))
print(re.compile(b"a(b)c") == re.compile(b"a(b)c"))
print(re.compile(b"abc") == re.compile(b"abd"))

# --- patterns work as dict keys and set members ----------------------------
d = {}
for text in (b"abc", b"abd", "abc", b"\xff" * 8):
    d[re.compile(text)] = text
print(len(d))
for text in (b"abc", b"abd", "abc", b"\xff" * 8):
    re.purge()
    print(repr(text), d[re.compile(text)] == text)

s = {re.compile(b"q"), re.compile(b"q"), re.compile("q")}
print(len(s))

# --- and the patterns still MATCH after all that ---------------------------
print(re.compile(b"a(b)c").match(b"abc").group(1))
print(re.compile(re.escape(b"\xff" * 8)).match(b"\xff" * 8).group())
print(re.compile("a(b)c").match("abc").group(1))
print("done")
