# The byte scanners, at every length across their vector/tail seam.
#
# find, index, count, replace, split, partition, `in` and the bytes versions
# all bottom out in one byte-search primitive, and the ASCII probe that counts
# a new string's code points runs on every string these build.  A vectorised
# scanner processes a fixed block while enough bytes remain and hands the
# remainder to a scalar tail, so the interesting lengths are the ones either
# side of the block size and the interesting positions are the last few.

sizes = list(range(0, 40)) + [47, 48, 49, 63, 64, 65, 127, 128, 129, 255, 256, 257, 4095, 4096, 4097]

# A needle at EVERY position in a haystack of every size: the seam is hit by
# whichever (size, position) pair straddles it.
for n in sizes:
    base = "a" * n
    assert base.find("b") == -1, n
    assert base.count("b") == 0, n
    assert ("b" in base) is False, n
    for pos in range(n):
        s = "a" * pos + "b" + "a" * (n - pos - 1)
        assert s.find("b") == pos, (n, pos, s.find("b"))
        assert s.rfind("b") == pos, (n, pos)
        assert s.index("b") == pos, (n, pos)
        assert s.count("b") == 1, (n, pos)
        assert ("b" in s) is True, (n, pos)
        assert s.split("b") == ["a" * pos, "a" * (n - pos - 1)], (n, pos)
        assert s.replace("b", "XY") == "a" * pos + "XY" + "a" * (n - pos - 1), (n, pos)
        assert s.partition("b") == ("a" * pos, "b", "a" * (n - pos - 1)), (n, pos)
print("single-byte needle:", len(sizes), "sizes, every position")

# Multi-byte needles, whose first byte is found by the scanner and whose tail
# is settled by a compare.
for n in [16, 17, 31, 32, 33, 64, 65]:
    for nl in [2, 3, 8, 9, 16, 17]:
        if nl > n:
            continue
        needle = "".join(chr(ord("A") + i % 26) for i in range(nl))
        for pos in range(n - nl + 1):
            s = "z" * pos + needle + "z" * (n - nl - pos)
            assert s.find(needle) == pos, (n, nl, pos)
            assert s.rfind(needle) == pos, (n, nl, pos)
            assert s.count(needle) == 1, (n, nl, pos)
print("multi-byte needles: ok")

# The same over bytes, which uses the same primitives through a different
# wrapper, and over non-ASCII, where the code-point count is not the byte
# count and the ASCII probe must bail out at the right byte.
for n in sizes:
    b = b"a" * n
    assert b.find(b"b") == -1, n
    if n:
        c = b"a" * (n - 1) + b"b"
        assert c.find(b"b") == n - 1, n
        assert c.rfind(b"b") == n - 1, n
print("bytes: ok")

for n in sizes:
    if n > 300:
        continue
    for tail in range(0, min(n, 17)):
        s = "a" * (n - tail) + "é" * tail       # 2 bytes each in UTF-8
        assert len(s) == n, (n, tail, len(s))
        assert s.count("é") == tail, (n, tail)
        if tail:
            assert s.find("é") == n - tail, (n, tail)
        assert s.encode().decode() == s, (n, tail)
print("non-ascii tails: ok")

print("abcdefghijklmnopqrstuvwxyz".find("z"), "abc".count("a"), "aaa".replace("a", ""))
print(("x" * 33 + "needle").find("needle"), ("x" * 33 + "needle").rfind("e"))
