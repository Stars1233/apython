# A range whose bounds do not fit an int64.
#
# The three bounds were int64 fields where CPython holds objects, so
# `range(1 << 1000)` was range(0, 2**63 - 1): the bound was CLAMPED, because
# refusing it takes the standard library with it -- _collections_abc builds
# one at import to name the type its iterator has.  The bounds are objects
# now, with the int64s kept beside them for every ordinary range, and a
# second iterator type for the wide case, as CPython has.

B = 1 << 70
CASES = [
 "range(B)", "range(B, B+5)", "range(B, B+10, 3)", "range(-B, -B+4)",
 "len(range(B, B+5))", "list(range(B, B+4))", "list(reversed(range(B, B+4)))",
 "range(B)[0]", "range(B)[1]", "range(B)[-1]", "range(B)[-2]",
 "range(B)[0:2]", "range(B)[:5]", "range(B)[::-1]", "range(B)[-3:]",
 "range(B)[B-2:]", "range(B, B+10, 3)[1:3]", "range(B)[5:2]",
 "list(range(B, B+9, 3)[::-1])", "list(range(B, B+9, 3)[::2])",
 "B in range(2*B)", "B-1 in range(B)", "B in range(B)", "0 in range(B)",
 "range(B) == range(B)", "range(B) == range(B+1)", "range(B) != range(B)",
 "hash(range(B)) == hash(range(B))", "hash(range(B)) == hash(range(B+1))",
 "range(B).index(5)", "range(2*B).index(B)", "range(B).count(7)",
 "range(B).count(-1)", "bool(range(B))", "bool(range(B, B))",
 "range(B).start", "range(B).stop", "range(B).step",
 "type(iter(range(B))).__name__", "type(iter(range(3))).__name__",
 "range(B)[10**30:]", "range(0, B, B//2)[1]",
 "sum(range(B, B+5))", "min(range(B, B+5))", "max(range(B, B+5))",
 "list(range(B))[:0]",
]
for e in CASES:
    try:
        print(e, "->", repr(eval(e))[:70])
    except BaseException as exc:
        print(e, "->", type(exc).__name__, exc)
