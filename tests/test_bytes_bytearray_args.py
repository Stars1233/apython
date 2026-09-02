"""A bytes method handed a bytearray argument.

bytearray moved its bytes out of line earlier on this branch: the object is a
fixed header now and `data` is a pointer at +32, where a bytes keeps its
first byte.  Every method that read an ARGUMENT through the bytes layout kept
working and started answering wrongly -- +24 is the bytearray's capacity, so
the length came from the wrong word and the data pointer landed inside the
header.  No crash, just False where True belonged.

The self argument was converted at the time, through the wrappers that give
bytearray its shared methods; the arguments were not.
"""


def check(label, fn):
    try:
        got = fn()
    except Exception as exc:
        got = type(exc).__name__ + ": " + str(exc)
    print(label.ljust(38), repr(got))


BA = bytearray
MV = memoryview

# --- startswith / endswith ---
check("startswith bytearray", lambda: b"abc".startswith(BA(b"ab")))
check("startswith memoryview", lambda: b"abc".startswith(MV(b"ab")))
check("startswith bytes", lambda: b"abc".startswith(b"ab"))
check("startswith miss", lambda: b"abc".startswith(BA(b"zz")))
check("startswith empty", lambda: b"abc".startswith(BA(b"")))
check("startswith longer", lambda: b"ab".startswith(BA(b"abc")))
check("startswith on a bytearray", lambda: BA(b"abc").startswith(BA(b"ab")))
check("startswith a str", lambda: b"abc".startswith("ab"))
check("endswith bytearray", lambda: b"abc".endswith(BA(b"bc")))
check("endswith memoryview", lambda: BA(b"abc").endswith(MV(b"bc")))
check("endswith miss", lambda: b"abc".endswith(BA(b"ab")))
check("endswith empty", lambda: b"abc".endswith(BA(b"")))
check("endswith a str", lambda: b"abc".endswith("bc"))

# --- count ---
check("count bytearray", lambda: b"aaa".count(BA(b"a")))
check("count two-byte", lambda: b"ababab".count(BA(b"ab")))
check("count memoryview", lambda: BA(b"abab").count(MV(b"ab")))
check("count miss", lambda: b"abc".count(BA(b"z")))
check("count empty", lambda: b"abc".count(BA(b"")))
check("count an int", lambda: b"abc".count(98))
check("count overlapping", lambda: b"aaaa".count(BA(b"aa")))

# --- find ---
check("find bytearray", lambda: b"abcabc".find(BA(b"bc")))
check("find with a start", lambda: b"abcabc".find(BA(b"bc"), 2))
check("find with start and end", lambda: b"abcabc".find(BA(b"bc"), 1, 4))
check("find a miss", lambda: b"abcabc".find(BA(b"zz")))
check("find an int", lambda: b"abc".find(98))
check("find empty", lambda: b"abc".find(BA(b"")))
check("find on a bytearray", lambda: BA(b"abcabc").find(MV(b"ca")))
check("find past the end", lambda: b"abc".find(BA(b"a"), 5))

# --- replace ---
check("replace old bytearray", lambda: b"abcabc".replace(BA(b"bc"), b"Z"))
check("replace new bytearray", lambda: b"abcabc".replace(b"bc", BA(b"ZZ")))
check("replace both", lambda: b"aaaa".replace(BA(b"aa"), BA(b"b")))
check("replace with empty", lambda: b"aaa".replace(BA(b"a"), b""))
check("replace a miss", lambda: b"abc".replace(BA(b"z"), b"y"))
check("replace on a bytearray", lambda: BA(b"abcabc").replace(MV(b"a"), b"X"))

# --- split ---
check("split bytearray", lambda: b"a,b,c".split(BA(b",")))
check("split multibyte", lambda: b"aXXbXXc".split(BA(b"XX")))
check("split memoryview", lambda: BA(b"a-b").split(MV(b"-")))
check("split a miss", lambda: b"abc".split(BA(b"x")))
check("split empty parts", lambda: b",,".split(BA(b",")))
check("split whitespace", lambda: b"a b  c".split())

# --- join ---
check("join mixed items", lambda: b"-".join([b"a", BA(b"b"), MV(b"c")]))
check("join a bytearray sep", lambda: BA(b",").join([b"x", b"y"]))
check("join one item", lambda: b"-".join([BA(b"solo")]))
check("join nothing", lambda: b"-".join([]))
check("join a str item", lambda: b"-".join([b"a", "b"]))
check("join a tuple", lambda: b"+".join((BA(b"p"), b"q")))

# --- and the plain bytes cases still work ---
check("plain startswith", lambda: b"hello".startswith(b"he"))
check("plain count", lambda: b"hello".count(b"l"))
check("plain find", lambda: b"hello".find(b"ll"))
check("plain replace", lambda: b"hello".replace(b"l", b"L"))
check("plain split", lambda: b"a:b".split(b":"))
check("plain join", lambda: b":".join([b"a", b"b"]))
