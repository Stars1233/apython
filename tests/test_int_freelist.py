# Heap integers come from a free list now rather than from libc, and
# int_dealloc pushes them back onto it.  A block is handed out RAW, so what
# has to keep working is every combination of what the previous owner left in
# it and what the next one expects:
#
#   - a compact block reused as a GMP-backed one, and the other way round
#   - a GMP-backed block whose mpz was cleared before it was recycled, so the
#     next owner's __gmpz_init starts from nothing
#   - the cap, past which blocks go back to libc: a list of more than 256 big
#     integers, dropped, then rebuilt
#   - the dead block's refcount word, which holds the free-list link, being
#     overwritten by the next owner before anything reads it as a refcount
#
# The failure this would produce is not subtle -- a wrong value or a crash --
# but it needs the SAME block to be reused with a different shape, which only
# an allocate/free/allocate sequence produces.  Everything below is written to
# force that.

import gc


def churn(make, n):
    """Allocate and drop n integers one at a time, so each reuses the last."""
    out = 0
    for i in range(n):
        v = make(i)
        out ^= v & 0xFF
        del v
    return out


# --- compact blocks recycled as compact ------------------------------------
print(churn(lambda i: (1 << 55) + i, 2000))
print(churn(lambda i: -(1 << 55) - i, 2000))

# --- GMP-backed blocks recycled as GMP-backed ------------------------------
print(churn(lambda i: (10 ** 40) + i, 2000))
print(churn(lambda i: -(10 ** 100) - i, 500))

# --- alternating, so a block's previous shape is never the next one --------
print(churn(lambda i: ((1 << 55) + i) if i % 2 else ((10 ** 40) + i), 2000))
print(churn(lambda i: ((10 ** 200) + i) if i % 3 else ((1 << 51) + i), 1000))

# --- an accumulator climbing through both boundaries, which frees one
# --- object per iteration and immediately allocates another ----------------
def climb(step, n):
    s = 0
    for _ in range(n):
        s += step
    return s


for step in (2 ** 49, 2 ** 50, 2 ** 55, 10 ** 20, -(2 ** 49), 1):
    print(step, climb(step, 5000))

# --- past the cap: a long list built, dropped, and built again -------------
def build(n, base):
    return [base + i for i in range(n)]


for n in (10, 255, 256, 257, 1000):
    a = build(n, 10 ** 30)
    print(n, sum(a) % 1000000007, a[0], a[-1])
    del a
    b = build(n, 1 << 55)
    print(n, sum(b) % 1000000007, b[0], b[-1])
    del b
    gc.collect()

# --- the values must survive every round trip ------------------------------
VALUES = [2 ** 50, 2 ** 55, 2 ** 63, 2 ** 64, 10 ** 30, 10 ** 100,
          -(2 ** 50), -(2 ** 63), -(10 ** 30), 0, 1, -1]
for _ in range(3):
    tmp = []
    for v in VALUES:
        tmp.append(v + 0)
    for a, b in zip(tmp, VALUES):
        if a != b:
            print("MISMATCH", a, b)
    del tmp
print("values survive")

# --- every operation that makes a heap integer, back to back ---------------
x = 10 ** 25
ops = [
    x + 1, x - 1, x * 3, x // 7, x % 1000003, -x, abs(-x), x ** 2,
    x << 3, x >> 3, x & 0xFFFF, x | 1, x ^ 1, ~x,
    int(str(x)), int(str(x), 10), divmod(x, 7)[0], divmod(x, 7)[1],
    pow(x, 2, 1000003), x + (1 << 55), (1 << 55) + x,
]
for v in ops:
    print(v)
for _ in range(200):
    ops = [v + 1 for v in ops]
print(sum(ops) % 1000000007)

# --- and the compact/GMP flag itself: a value that fits int64 after
# --- arithmetic that did not ----------------------------------------------
for _ in range(500):
    big = 10 ** 30
    small = big // (10 ** 25)
    print(small) if False else None
    assert small == 10 ** 5
print("shrink ok")
print(len({(1 << 55) + i for i in range(500)}))
print(sorted({10 ** 30 + i for i in range(5)}))
