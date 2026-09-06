# BUILD_STRING joins an f-string's fragments, and it used to do that by
# calling str_concat once per fragment and DECREFing the intermediate.  That
# is quadratic: an N-fragment f-string did N-1 allocations and re-copied a
# growing prefix each time.
#
#     f"{a}-{b}-{c}-{d}-{e}-{a}-{b}-{c}-{d}-{e}"
#
# is nineteen fragments, so nineteen allocations and roughly ten copies of the
# whole answer.  It now measures the fragments in one pass, allocates once,
# and does one memcpy each.
#
# What has to keep working:
#
#   - both PyStrObject lengths.  ob_size is bytes and ob_length is code
#     points, and they differ for anything non-ASCII; every code-point-aware
#     operation on the result reads the second, so the sizing pass sums both.
#   - the eight-byte NUL pad every str carries, which ap_strcmp reads through.
#   - empty fragments, which contribute nothing and must not confuse the
#     offsets.
#   - a str SUBCLASS as a fragment: it keeps its data inline at the same
#     offsets, and the answer is a plain str, as CPython's is.
#   - the fragment count boundaries: zero fragments answer "", one fragment is
#     handed back untouched, and two is the first that joins.

a, b, c, d, e = 1, "two", 3.5, [4], None
n = 0
print(f"")
print(f"{a}")
print(f"x{a}")
print(f"{a}x")
print(f"{a}{b}")
print(f"{a}-{b}")
print(f"{a}-{b}-{c}")
print(f"{a}-{b}-{c}-{d}-{e}")
print(f"{a}-{b}-{c}-{d}-{e}-{a}-{b}-{c}-{d}-{e}")
print(f"{a!r} {b!r} {c!r} {d!r} {e!r}")
print(f"{a:5d}|{c:8.3f}|{b:>10}|{b:<10}|{b:^10}")
print(f"{'':s}{'':s}{'':s}")
print(f"{''}{a}{''}{b}{''}")
u = "héllo wörld ünïcode"
print(f"{u}", f"{u}{u}", f"[{u}]-[{u}]-[{u}]", len(f"{u}{u}{u}"))
e2 = ""
print(repr(f"{e2}"), repr(f"{e2}{e2}"), repr(f"{e2}{e2}{e2}"), len(f"{e2}{e2}"))
print(f"{u!r}")
# nested and expressions
print(f"{a + 1}{b.upper()}{c * 2}{len(d)}{e is None}")
print(f"{f'{a}{b}'}{f'{c}'}")
# a long one, to exercise the sizing pass
parts = "".join(f"[{i}]" for i in range(50))
print(parts, len(parts))
big = f"{u}{u}{u}{u}{u}{u}{u}{u}{u}{u}{u}{u}{u}{u}{u}{u}{u}{u}{u}{u}"
print(len(big), big[:20], big[-20:], big.count("héllo"))
# subclass fragments
class S(str): pass
s1 = S("abc")
print(f"{s1}{s1}", type(f"{s1}{s1}").__name__, len(f"{s1}{s1}"))
# str methods on the result, which need ob_length right
r = f"{u}-{u}"
print(len(r), r.upper(), r[3], r[-1], r.index("-"), list(r[:4]))
print(r.encode("utf-8").decode("utf-8") == r)
print(hash(r) == hash(u + "-" + u), r == u + "-" + u)
# the exact byte content
import sys
print(sum(ord(ch) for ch in big) % 100000)
