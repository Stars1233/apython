# list.sort is timsort, and almost none of what timsort does is visible in the
# output of a sort that works.  What this file checks is the parts that fail
# silently:
#
#   - STABILITY.  A merge that takes from the wrong run on a tie, a
#     descending-run detector that admits equal neighbours, or a gallop that
#     uses gallop_left where it should use gallop_right, all still produce a
#     sorted list.  Sorting (key, index) pairs by key and asking whether index
#     still ascends within each key is the only way to see any of it.
#   - EVERY LENGTH.  minrun, the run stack and galloping switch on at
#     different sizes: below 64 a sort is one binary insertion and nothing
#     else, at 64 the first merge happens, and the run stack does not reach
#     three entries until several hundred.  So every length from 0 to 70 and
#     then a spread up into the thousands.
#   - EVERY SHAPE.  Timsort is defined by what it does with structure:
#     already sorted, reverse-sorted, all equal, sorted runs, nearly sorted,
#     two-valued.  Each takes a different path and three of them are the
#     cases the old merge sort was slowest on.
#   - EVERY TYPE THE PRE-SORT SCAN CAN CHOOSE.  One pass over the list picks a
#     comparator: int immediates, floats, exact str, or the general one.  A
#     scan that says "all ints" about a list holding one float compares two
#     encodings with an integer instruction, and the answer is wrong rather
#     than absent.
#   - THE ERROR PATHS.  A comparison that raises, a comparison that mutates
#     the list, and a key that raises each have to leave the list holding
#     exactly its own elements, once each.
#
# Nothing here uses `random`: the sequences are a seeded LCG, so a failure is
# reproducible.


def lcg(n, m=1009):
    x = 12345
    out = []
    for _ in range(n):
        x = (x * 1103515245 + 12345) % 2147483648
        out.append(x % m)
    return out


def issorted(l):
    return all(l[i] <= l[i + 1] for i in range(len(l) - 1))


def same_bag(a, b):
    if len(a) != len(b):
        return False
    c = list(a)
    for x in b:
        if x not in c:
            return False
        c.remove(x)
    return True


# --- shapes, at every length that can change the algorithm ----------------
LENGTHS = list(range(0, 71)) + [100, 127, 128, 129, 200, 511, 512, 1000, 2000]


def shapes(n):
    r = lcg(n)
    yield "random", r
    yield "sorted", sorted(r)
    yield "reversed", sorted(r)[::-1]
    yield "equal", [7] * n
    yield "two", [i % 2 for i in range(n)]
    yield "runs", [i % 100 for i in range(n)]
    near = sorted(r)
    for i in range(0, n - 1, 37):
        near[i], near[i + 1] = near[i + 1], near[i]
    yield "near", near


bad = []
for n in LENGTHS:
    for name, src in shapes(n):
        l = list(src)
        l.sort()
        if not (issorted(l) and len(l) == n and same_bag(l, src)):
            bad.append((n, name))
        l = list(src)
        l.sort(reverse=True)
        if not (issorted(l[::-1]) and same_bag(l, src)):
            bad.append((n, name, "reverse"))
print("shapes:", "ok" if not bad else bad[:6])

# --- stability ------------------------------------------------------------
# The index rides along as the second half of a tuple and is never compared,
# because the key is the first half alone.
stable = []
for n in (0, 1, 2, 31, 32, 63, 64, 65, 200, 1000):
    pairs = [(v % 5, i) for i, v in enumerate(lcg(n))]
    s = sorted(pairs, key=lambda p: p[0])
    for k in range(5):
        idx = [p[1] for p in s if p[0] == k]
        if idx != sorted(idx):
            stable.append((n, k))
    # and reverse=, which must reverse the ORDER OF KEYS and not of ties
    s = sorted(pairs, key=lambda p: p[0], reverse=True)
    for k in range(5):
        idx = [p[1] for p in s if p[0] == k]
        if idx != sorted(idx):
            stable.append((n, k, "reverse"))
print("stability:", "ok" if not stable else stable[:6])

# a descending run must be STRICTLY descending to be reversed in place, or
# equal elements come back the wrong way round
runs = [(3, 0), (3, 1), (2, 2), (2, 3), (1, 4), (1, 5)]
print(sorted(runs, key=lambda p: p[0]))

# --- the types the pre-sort scan chooses ----------------------------------
print(sorted([5, 3, 9, 1, 3]))
print(sorted([2 ** 60, -(2 ** 60), 0, 2 ** 60 + 1]))
print(sorted([2.5, -1.0, 0.0, 2.5, 1e300, -1e300]))
print(sorted([1, 2.5, 0, -3.5, 4]))
print(sorted(["pear", "apple", "fig", "apple", "", "Zebra"]))
print(sorted(["é", "e", "z", "中"]))
print(sorted([True, False, True, False]))
print(sorted([(2, "a"), (1, "b"), (2, "A"), (1, "a")]))
print(sorted([[3], [1], [2, 0], [2]]))
print(sorted([None] * 3, key=lambda x: 0))

# a float that is equal to an int, and a bool that is equal to both: all
# three encodings, so no single-type comparator may be chosen
mixed = [1, 1.0, True, 0, 0.0, False]
print(sorted(mixed, key=lambda x: (x, str(type(x).__name__))))

# NaN does not order, and CPython does not promise anything but "no crash"
nan = float("nan")
n2 = sorted([1.0, nan, 2.0])
print(len(n2), n2[0], n2[2])


class Rich:
    def __init__(self, v):
        self.v = v

    def __lt__(self, o):
        return self.v < o.v

    def __repr__(self):
        return "R%d" % self.v


print(sorted([Rich(3), Rich(1), Rich(2)]))


class OnlyGt:
    # __lt__ is the only operator a sort may ask for; a type that defines the
    # reflected one is answered through it, not by a second comparison.
    def __init__(self, v):
        self.v = v

    def __gt__(self, o):
        return self.v > o.v

    def __repr__(self):
        return "G%d" % self.v


print(sorted([OnlyGt(3), OnlyGt(1), OnlyGt(2)]))

# --- key= and reverse= ----------------------------------------------------
words = ["banana", "Apple", "cherry", "date", "Fig"]
print(sorted(words, key=len))
print(sorted(words, key=str.lower))
print(sorted(words, key=len, reverse=True))
print(sorted(range(10), key=lambda x: -x))
print(sorted([1, 2, 3], key=None), sorted([1, 2, 3], reverse=False))
l = [3, 1, 2]
print(l.sort(), l)
l.sort(key=lambda x: -x)
print(l)
l.sort(reverse=True)
print(l)

# reverse= takes any object's truth, not just a bool
for rv in (1, 0, [], [0], "", "x", None, 2.5):
    l = [2, 1, 3]
    l.sort(reverse=rv)
    print(repr(rv), l)

# --- the error paths ------------------------------------------------------
try:
    sorted([1, "a"])
except TypeError as e:
    print("TypeError", e)
try:
    sorted([1, None, 2])
except TypeError:
    print("TypeError")


class Raises:
    def __lt__(self, o):
        raise ZeroDivisionError("nope")


try:
    sorted([Raises(), Raises(), Raises()])
except ZeroDivisionError as e:
    print("ZeroDivisionError", e)

# a comparison that raises part way through a real merge, so the sort is
# abandoned with the list already emptied
class RaisesLate:
    def __init__(self, v):
        self.v = v

    def __lt__(self, o):
        if self.v == 500:
            raise KeyError("late")
        return self.v < o.v


victim = [RaisesLate(v) for v in lcg(600, 1000)] + [RaisesLate(500)]
try:
    victim.sort()
except KeyError as e:
    print("KeyError", e)
print(len(victim))

# a key that raises, at the first element and part way in
def key_bad(x):
    raise ValueError("key")


try:
    [3, 1, 2].sort(key=key_bad)
except ValueError as e:
    print("ValueError", e)

state = {"n": 0}


def key_late(x):
    state["n"] += 1
    if state["n"] == 40:
        raise ValueError("late key")
    return x


l = list(range(100))
try:
    l.sort(key=key_late)
except ValueError as e:
    print("ValueError", e)
print(len(l), l == list(range(100)))

# a comparison that mutates the list it is sorting
class Mutates:
    def __init__(self, v, target):
        self.v = v
        self.target = target

    def __lt__(self, o):
        self.target.append(1)
        return self.v < o.v


m = []
m.extend(Mutates(v, m) for v in (3, 1, 2))
try:
    m.sort()
except ValueError as e:
    print("ValueError", e)
print(len(m))

# and a key that mutates
m2 = [3, 1, 2]


def key_mutates(x):
    m2.append(9)
    return x


try:
    m2.sort(key=key_mutates)
except ValueError as e:
    print("ValueError", e)
print(len(m2))

# sort() takes no positional argument
try:
    [1, 2].sort(1)
except TypeError as e:
    print("TypeError")
try:
    sorted()
except TypeError:
    print("TypeError")
try:
    sorted([1], key=0)
except TypeError:
    print("TypeError")

# --- the references -------------------------------------------------------
# A sort moves Values and takes no reference of its own, so an object that
# has been through one dies exactly once.
seen = []


class Watch:
    def __init__(self, v):
        self.v = v

    def __lt__(self, o):
        return self.v < o.v

    def __del__(self):
        seen.append(self.v)


def churn():
    l = [Watch(v) for v in (3, 1, 2, 1)]
    l.sort()
    l.sort(reverse=True)
    l.sort(key=lambda w: -w.v)
    del l


churn()
print(sorted(seen))

# the keys a key= builds are released too, including when the sort fails
kseen = []


class KeyVal:
    def __init__(self, v):
        self.v = v

    def __lt__(self, o):
        return self.v < o.v

    def __del__(self):
        kseen.append(self.v)


def churn2():
    l = [3, 1, 2]
    l.sort(key=KeyVal)
    print(l)


churn2()
print(sorted(kseen))

# --- sorted() over every iterable, which is a list build and then a sort ---
print(sorted((3, 1, 2)), sorted("cba"), sorted({3: 0, 1: 0, 2: 0}))
print(sorted(range(5, 0, -1)), sorted(x for x in (2, 3, 1)))
print(sorted([]), sorted(()), sorted(""))
print(sorted({3, 1, 2}))
