# rsplit/rfind/rpartition, which all walk a string backwards looking for a
# separator.  The backward scan and the forward scan take different code, so
# every case here is checked against its forward mirror as well as printed for
# the diff against CPython.

cases = [
    ("", ","), ("a", ","), (",", ","), (",,", ","), ("a,b", ","),
    ("a,,b", ","), (",a,b,", ","), ("aaa", "a"), ("aaaa", "aa"),
    ("abcabcabc", "abc"), ("abcabcabc", "bc"), ("xyz", "abc"),
    ("a" * 40 + "," + "b" * 40, ","),
    ("ab" * 50, "ab"), ("ab" * 50, "ba"),
    ("hello world hello", "hello"), ("hello world hello", "o w"),
    ("\x00a\x00b\x00", "\x00"),          # a NUL is an ordinary character
    ("x" * 100, "x" * 7), ("x" * 100, "x" * 8), ("x" * 100, "x" * 9),
]

for s, sep in cases:
    for n in (-1, 0, 1, 2, 3):
        r = s.rsplit(sep, n) if n >= 0 else s.rsplit(sep)
        # the pieces must rejoin to the original
        assert sep.join(r) == s, (s, sep, n, r)
        print(repr(s[:20]), repr(sep), n, r)
    assert s.rfind(sep) == max((i for i in range(len(s) - len(sep) + 1)
                                if s[i:i + len(sep)] == sep), default=-1), (s, sep)
    print("rfind", repr(s[:20]), repr(sep), s.rfind(sep), s.rpartition(sep))

# maxsplit interacts with the backward scan: each piece re-searches the head.
long = ",".join(str(i) for i in range(30))
for n in range(0, 8):
    print(n, long.rsplit(",", n))
print(long.rsplit(","))
print("abc".rsplit("", 1) if False else "skip-empty-sep")
