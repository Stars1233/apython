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
