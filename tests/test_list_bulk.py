# The list builders that move a whole run of Values at once instead of one
# call per element: list.copy, list.extend, +=, LIST_EXTEND (which is what a
# list literal and a `*x` unpacking compile to), list(iterable), and
# BUILD_LIST.
#
# What has to keep working:
#   - the references.  Every one of these transfers or takes exactly one per
#     element, and a bulk memcpy plus a bulk INCREF gets that wrong in a way
#     no output diff shows -- so the aliasing cases below matter more than
#     the values do.
#   - a source that IS the destination.  l.extend(l) and l += l hand the
#     helper the array it is about to grow, and the grow can move it.
#   - an empty source and an empty destination, where the capacity floor of
#     four means there are slots above ob_size that nobody wrote.
#   - a subclass, which may define __iter__ and must not take the array path.
import sys


def refs(o):
    return sys.getrefcount(o) if hasattr(sys, "getrefcount") else -1


# --- copy -----------------------------------------------------------------
for src in ([], [1], [1, 2, 3], list(range(100)), [None, True, "a", 1.5]):
    c = src.copy()
    print(len(c), c == src, c is not src)
nested = [[1], [2]]
nc = nested.copy()
print(nc == nested, nc[0] is nested[0])
nc[0].append(9)
print(nested[0])

# --- extend ---------------------------------------------------------------
a = [1, 2]
a.extend([3, 4])
a.extend((5, 6))
a.extend(range(7, 9))
a.extend(x for x in (9, 10))
a.extend("ab")
a.extend([])
a.extend(())
print(a)

# a list extending itself, and a tuple made from itself
b = [1, 2, 3]
b.extend(b)
print(b)
c = list(range(5))
c.extend(c)
c.extend(c)
print(len(c), c)

# += with every source shape, including itself
d = [1]
d += [2]
d += (3,)
d += range(4, 6)
d += "xy"
d += d
print(d)

# --- literals, which are BUILD_LIST 0 + LOAD_CONST tuple + LIST_EXTEND ----
print([], [1], [1, 2, 3, 4, 5])
print([1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20])
print([None, True, False, 1.5, "s", (1, 2)])

# BUILD_LIST proper: a literal whose elements are not constants
x, y = 7, 8
print([x, y], [x + y, x - y], [x, [y], (x, y)])

# a `*` unpacking, which is LIST_EXTEND from whatever it is given
t = (1, 2, 3)
l5 = list(range(3))
print([*t], [*l5], [*t, *l5], [0, *t, 9], [*"ab"], [*{1: 2}])

# --- list(iterable) -------------------------------------------------------
print(list(), list([]), list(()), list(""))
print(list((1, 2, 3)), list([4, 5]), list(range(4)), list("abc"))
print(list(x for x in range(3)), list({1: "a", 2: "b"}))
print(len(list(range(1000))), list(range(1000))[999])


class LSub(list):
    def __iter__(self):
        return iter([99])


class TSub(tuple):
    def __iter__(self):
        return iter([98])


print(list(LSub([1, 2, 3])), list(TSub((1, 2, 3))))


class Sized:
    def __len__(self):
        return 3

    def __iter__(self):
        return iter((7, 8, 9))


print(list(Sized()))


class BadLen:
    def __len__(self):
        raise ValueError("no len")

    def __iter__(self):
        return iter(())


try:
    list(BadLen())
except ValueError as e:
    print("ValueError", e)


class BadIter:
    def __iter__(self):
        raise KeyError("no iter")


try:
    list(BadIter())
except KeyError as e:
    print("KeyError", e)

try:
    list(1)
except TypeError:
    print("TypeError")
try:
    list([1], [2])
except TypeError:
    print("TypeError")

# --- refcounting, which the memcpy could silently get wrong ---------------
# Each element is dropped exactly as many times as it was taken, so an
# object that has been through all of these still dies when the last list
# holding it does.
seen = []


class Watch:
    def __init__(self, tag):
        self.tag = tag

    def __del__(self):
        seen.append(self.tag)


def churn():
    w = Watch("w")
    holder = [w]
    a = holder.copy()
    b = []
    b.extend(holder)
    c = []
    c += holder
    d = list(holder)
    e = [*holder]
    f = holder + holder
    g = holder * 3
    del a, b, c, d, e, f, g, holder, w


churn()
print(seen)

# and the same for a list that grew past its capacity while being extended
seen2 = []


class Watch2:
    def __del__(self):
        seen2.append(1)


def churn2():
    src = [Watch2() for _ in range(50)]
    dst = []
    for _ in range(4):
        dst.extend(src)
    del dst
    print(len(seen2))
    del src


churn2()
print(len(seen2))

# --- concat and repeat ----------------------------------------------------
print([1, 2] + [3], [] + [1], [1] + [], [] + [])
print([1, 2] * 3, [1] * 0, [] * 5, 2 * [7])
e2 = [[0]]
r = e2 * 3
print(r, r[0] is r[1])
try:
    [1] + (2,)
except TypeError:
    print("TypeError")

# --- repeat's doubling copy -----------------------------------------------
# `a * k` writes one copy of a and then copies the RESULT onto itself, each
# time doubling what is written, so the last block is a partial one whenever
# k is not a power of two.  Every (len, count) pair below 9x9 exercises a
# different final chunk, and the refcount is a single addition of k rather
# than k separate increments -- so a wrong k is a leak or a crash, never a
# wrong value.
sizes_ok = True
for n in range(0, 9):
    for k in range(0, 9):
        s2 = list(range(n))
        r2 = s2 * k
        if len(r2) != n * k:
            sizes_ok = False
        if any(r2[i] != s2[i % n] for i in range(n * k)):
            sizes_ok = False
print("repeat sizes", sizes_ok)
for k in (1, 2, 3, 7, 8, 15, 16, 17, 100):
    r2 = [1, 2, 3] * k
    print(k, len(r2), r2[:3], r2[-3:])

seen3 = []


class Watch3:
    def __del__(self):
        seen3.append(1)


def churn3():
    w = Watch3()
    a2 = [w]
    b2 = a2 * 20
    c2 = a2 * 0
    d2 = a2 * 1
    e2 = (a2 + a2) * 3
    del a2, b2, c2, d2, e2, w


churn3()
print(len(seen3))

try:
    [1] * (2 ** 64)
except (OverflowError, MemoryError) as ex:
    print(type(ex).__name__)
try:
    [1] * None
except TypeError:
    print("TypeError")


class RMul:
    def __rmul__(self, o):
        return "rmul"


print([1] * RMul())

# --- the reversal, which moves four Values an iteration -------------------
# Two from each end, through a 16-byte load and a pshufd, so the tail is
# whatever is left when fewer than four remain: every length below 40 lands
# on a different one, and the two boundaries that matter are 3 and 4.  The
# same helper turns a[::-1] round, so both callers are here.
rev_ok = True
for n in range(0, 40):
    a3 = list(range(n))
    b3 = list(a3)
    b3.reverse()
    if b3 != a3[::-1]:
        rev_ok = False
    b3.reverse()
    if b3 != a3:
        rev_ok = False
    if a3[::-1] != list(reversed(a3)):
        rev_ok = False
print("reverse shapes", rev_ok)
for n in (0, 1, 2, 3, 4, 5, 8, 9, 16, 17, 1000):
    a3 = list(range(n))
    a3.reverse()
    print(n, a3[:3], a3[-3:])
a3 = [1, "x", None, 2.5, (1,), [0]]
a3.reverse()
print(a3)
print(list(range(9))[::-1], list(range(9))[::-2], list(range(9))[7:2:-1])

seen4 = []


class Watch4:
    def __del__(self):
        seen4.append(1)


def churn4():
    l4 = [Watch4() for _ in range(9)]
    l4.reverse()
    m4 = l4[::-1]
    del l4, m4


churn4()
print(len(seen4))
