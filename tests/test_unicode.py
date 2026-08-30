# A str stores UTF-8 but Python counts code points.  Everything here is
# indistinguishable for ASCII; the point is that it stays right when the two
# lengths differ.
s = "héllo wörld"
t = "日本語"

print(len(s), len(t), len(""), len("a"))
print(s[0], s[1], s[-1], t[0], t[1], t[2])
print(s[1:4], s[:3], s[-3:], t[1:], t[:2])
print(s[::-1], t[::-1], s[::2], t[::2], s[1::3])
print(list(s), list(t), list(reversed(t)))
print(ord("é"), ord("日"), chr(233), chr(0x65E5), len(chr(233)), len(chr(0)))
print(chr(0) == "\x00", len("\x00\x00"))

# Searching reports positions in code points.
print(s.find("l"), s.rfind("l"), s.index("o"), s.rindex("o"), s.count("l"))
print(t.find("本"), t.index("語"), t.count("日"), t.find("x"))
print(s.startswith("hé"), s.endswith("ld"), "ö" in s, "z" in s)

# Widths and precisions count characters.
print(s.center(15, "-"), s.ljust(15, "."), s.rjust(15, "."))
print("|%10s|%-10s|" % ("é", "é"))
print("[{:>6}][{:<6}][{:^6}]".format(t[:2], t[:2], t[:2]))
print("{:.2}".format(t), "{:.5}".format(s))
print(f"{'é':>5}|{'é':<5}|{'é':^5}|")

# Splitting and joining keep whole characters.
print(s.split(), s.split("ö"), t.partition("本"), t.rpartition("本"))
print("|".join([t, s]), (t + s)[3], (t * 2)[3], len(t * 3))
print(s.replace("ö", "o"), s.replace("é", "ee"), len(s.replace("é", "ee")))

# The round trip through bytes.
print(s.encode(), s.encode().decode() == s, len(s.encode()), len(s))
print(t.encode(), t.encode().decode() == t)

# Comparison and hashing are by bytes, which orders the same as by code point.
print("é" == "é", "é" < "f", sorted(["b", "é", "a", "日"]))
d = {t: 1, s: 2}
print(d[t], d[s], len(d))

# Bytes that are not valid UTF-8 still have to make a self-consistent string:
# len, indexing, slicing and iteration all walk the bytes, and they must agree
# on where each code point starts.  A stray continuation byte or a truncated
# sequence is one code point of one byte.  CPython rejects the bytes outright,
# so both sides report only whether the result hangs together.
try:
    bad = b"a\x80b\xc3".decode()
except UnicodeDecodeError:
    bad = None
if bad is None:
    print("consistent")
else:
    print("consistent" if (
        len(bad) == len(bad.encode())
        and len(list(bad)) == len(bad)
        and all(len(bad[i:i + 1].encode()) == 1 for i in range(len(bad)))
        and bad.encode() == b"a\x80b\xc3"
    ) else "INCONSISTENT")
