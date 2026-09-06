# The three primitives every substring search and every byte comparison in
# the tree stands on: ap_memcmp, ap_memchr and ap_memfind.  They are exercised
# here through the str and bytes methods that reach them -- find, rfind, index,
# rindex, count, `in`, replace, partition, split -- and through comparison and
# sorting, which is what ap_memcmp is for.
#
# The cases are chosen for the boundaries the implementations actually have:
#
#   - every length from 0 to 39, which crosses the 8/16/32 splits ap_memcpy
#     picks between and the 8-byte stride ap_memcmp and ap_memchr walk in
#   - a needle of length 0, 1 and more, since a one-byte needle takes a
#     different path (straight to ap_memchr) from a longer one
#   - embedded NULs, because these are counted strings and a NUL is an
#     ordinary byte; ap_strstr got this wrong once already, which is why
#     ap_memfind exists
#   - non-ASCII, where a needle may match a continuation byte rather than a
#     character boundary -- UTF-8 is self-synchronising, so byte search is
#     exactly right, and these cases are what says so
#   - haystacks just under, at and just over 16 bytes, the size of the byte
#     prologue ap_memchr runs before it sets up its word loop

H = "abcdefghij" * 30
W = "αβγδε" * 30
SUBS = ("", "a", "z", "ab", "ij", "zz", "abc", "α", "αβ", "\x00", "a\x00b", "xxxxxxxx")
STRS = (H, W, "", "a", "ab", "aaa", "a\x00b\x00c", "x"*7, "x"*8, "x"*9,
        "x"*15, "x"*16, "x"*17, "x"*31, "x"*32, "x"*33, "abcabcabcabc")
for s in STRS:
    for sub in SUBS:
        print(s.find(sub), s.rfind(sub), s.count(sub), sub in s,
              len(s.replace(sub, "-")) if sub else "",
              s.partition(sub) if sub else "", s.rpartition(sub) if sub else "",
              s.split(sub) if sub else "", s.rsplit(sub) if sub else "",
              s.startswith(sub), s.endswith(sub))
for sub in ("zz", "ω"):
    for m in ("index", "rindex"):
        try: getattr(H, m)(sub); print(m, sub, "found")
        except ValueError: print(m, sub, "ValueError")
# bytes shares ap_memcmp and ap_memfind
for x in (H.encode(), W.encode(), b"", b"a", b"ab", b"a\x00b", b"y"*33):
    for y in (b"", b"a", b"ij", b"zz", b"\x00", b"y"*33, x):
        print(x.find(y), x.rfind(y), x.count(y), y in x,
              len(x.replace(y, b"-")) if y else "", x == y, x < y, x > y, x != y)
# every length either side of the 8/16/32 boundaries ap_memcpy and ap_memcmp use
for n in range(0, 40):
    a = "q" * n
    b = "q" * n
    c = ("q" * (n - 1) + "r") if n else "r"
    print(n, a == b, a == c, a < c, a > c, len(a + b), (a + b)[n:], a[:n//2], a.upper(),
          a.encode() == b.encode(), a.encode() < c.encode())
# a deterministic shuffle, so sorting exercises comparison at every length
words = []
seed = 12345
for _ in range(300):
    seed = (seed * 1103515245 + 12345) & 0x7fffffff
    ln = seed % 13
    w = ""
    for _ in range(ln):
        seed = (seed * 1103515245 + 12345) & 0x7fffffff
        w += "abcxyzα"[seed % 7]
    words.append(w)
print(sorted(words))
print(sorted(set(words)))
print(len({w: i for i, w in enumerate(words)}))
