# Test set query methods: union, intersection, difference,
# symmetric_difference, issubset, issuperset, isdisjoint

a = {1, 2, 3, 4}
b = {3, 4, 5, 6}

# union
u = a.union(b)
print(sorted(list(u)))  # [1, 2, 3, 4, 5, 6]

# intersection
i = a.intersection(b)
print(sorted(list(i)))  # [3, 4]

# difference
d = a.difference(b)
print(sorted(list(d)))  # [1, 2]

# symmetric_difference
sd = a.symmetric_difference(b)
print(sorted(list(sd)))  # [1, 2, 5, 6]

# issubset
print({1, 2}.issubset({1, 2, 3}))   # True
print({1, 2, 4}.issubset({1, 2, 3}))  # False

# issuperset
print({1, 2, 3}.issuperset({1, 2}))   # True
print({1, 2}.issuperset({1, 2, 3}))   # False

# isdisjoint
print({1, 2}.isdisjoint({3, 4}))   # True
print({1, 2}.isdisjoint({2, 3}))   # False

# === Operator syntax ===
# Union |
print(sorted(a | b))

# Intersection &
print(sorted(a & b))

# Difference -
print(sorted(a - b))
print(sorted(b - a))

# Symmetric difference ^
print(sorted(a ^ b))

# Empty set
e = set()
print(sorted(a | e))
print(sorted(a & e))
print(sorted(a - e))


# The three predicates take any iterable, as the four builders beside them do.
# They read the argument as a hash table whatever it was: a list's header has
# nothing occupied in it, so issubset answered True for anything and
# isdisjoint answered True for everything -- and a two-element list made the
# probe run off the end of a table it had measured wrong, into "set: hash
# table full", which is a fatal_error and not an exception.
print("=== the predicates over any iterable ===")
s = {"a", "b"}
print(s.issubset(["a", "b", "c"]), s.issubset(["a"]), s.issubset([]))
print(s.issuperset(["a"]), s.issuperset(["a", "z"]), s.issuperset([]))
print(s.isdisjoint(["a"]), s.isdisjoint(["z"]), s.isdisjoint([]))
print(s.issubset("ab"), s.issuperset("a"), s.isdisjoint("z"))
print(s.issubset(x for x in "abc"), s.isdisjoint(x for x in "az"))
print(s.issubset(("a", "b")), s.issuperset({"a": 1}), s.isdisjoint({"z": 1}))
print(s.issubset(frozenset({"a", "b"})), s.isdisjoint(frozenset({"z"})))
print(frozenset({"a"}).issubset(["a"]), frozenset({"a"}).isdisjoint(["b"]))
try:
    s.issubset(5)
except TypeError:
    print("not iterable rejected")
try:
    s.isdisjoint(None)
except TypeError:
    print("None rejected")

# The OPERATORS stay strict, which is where CPython draws the line.
for expr in ("s | ['c']", "s & ['a']", "s - ['a']", "s ^ ['a']",
             "s <= ['a']", "s >= ['a']"):
    try:
        eval(expr, {"s": s})
        print(expr, "accepted")
    except TypeError:
        print(expr, "TypeError")

# --- intersection and isdisjoint walk the SMALLER operand ------------------
# Both used to walk `self` always, so `set(range(5000)) & {0, 1, 2}` visited
# five thousand slots where three would do.  The swap is only legal because
# both questions are symmetric; what it must NOT change is the result's TYPE,
# which still comes from the left operand, nor the answer for any relative
# size, operand kind, or argument shape.  So every pair below is asked in
# both directions and the type is printed with it.


def shown(x):
    return sorted(x)


pairs = [
    (set(), set()), (set(), {1}), ({1}, set()),
    ({1, 2, 3}, {2, 3, 4}), ({1, 2, 3}, {1, 2, 3}), ({1}, {2}),
    (set(range(50)), {0, 1, 2}), ({0, 1, 2}, set(range(50))),
    (set(range(50)), {99}), ({99}, set(range(50))),
    (set(range(200)), set(range(100, 300))),
    ({1, 2}, frozenset({2, 3})), (frozenset({1, 2}), {2, 3}),
    (frozenset({1, 2}), frozenset({2, 3})),
]
for a, b in pairs:
    print(shown(a & b), shown(b & a), a.isdisjoint(b), b.isdisjoint(a),
          type(a & b).__name__, type(b & a).__name__)

small_set = {1, 2, 3}
for other in ([2, 3, 4], (2, 3), range(2, 5), "23", {2: 0, 3: 0}, iter([2]),
              frozenset({2}), set()):
    print(shown(small_set.intersection(other)), small_set.isdisjoint(other))
print(shown(small_set.intersection([2, 3], [3, 4])),
      shown(small_set.intersection()))
print(shown(small_set & small_set), small_set.isdisjoint(small_set),
      shown(set() & set()), set().isdisjoint(set()))

narrowed = set(range(10))
narrowed &= {2, 4, 6}
print(sorted(narrowed))
updated = set(range(10))
updated.intersection_update([1, 3])
print(sorted(updated))

for bad in (5, None, 1.5):
    try:
        small_set & bad
    except TypeError:
        print("TypeError op")
    try:
        small_set.intersection(bad)
    except TypeError:
        print("TypeError method")


# Keys whose __eq__ is real Python, so the walk runs arbitrary code whichever
# side it ends up on.
class Clash:
    def __init__(self, v):
        self.v = v

    def __hash__(self):
        return self.v % 3

    def __eq__(self, o):
        return isinstance(o, Clash) and self.v == o.v


many = set(Clash(i) for i in range(30))
few = set(Clash(i) for i in range(3))
print(sorted(x.v for x in (many & few)), sorted(x.v for x in (few & many)))
print(many.isdisjoint(few), few.isdisjoint(many))
print(many.isdisjoint({Clash(99)}), {Clash(99)}.isdisjoint(many))

# --- union and update now copy a whole table rather than re-inserting -----
# `a | b` clones a's table into the result and reserves b's room; update()
# into an EMPTY set clones the source outright.  Neither hashes or probes an
# element on the way, so what has to be checked is what a bulk copy can get
# wrong and a per-element insert cannot: the references, the tombstones the
# copy carries with it, and a source that IS the destination.
import sys


def refs(o):
    return sys.getrefcount(o) if hasattr(sys, "getrefcount") else -1


for a, b in (
    (set(), set()),
    (set(), {1, 2, 3}),
    ({1, 2, 3}, set()),
    ({1, 2, 3}, {3, 4, 5}),
    ({1, 2, 3}, {1, 2, 3}),
    (set(range(50)), set(range(40, 90))),
    (frozenset(range(20)), set(range(10, 30))),
    (set("abc"), frozenset("cde")),
):
    u = a | b if type(a) is type(b) else a.union(b)
    print(sorted(a.union(b)), sorted(b.union(a)), type(a.union(b)).__name__)
    s = set(a)
    s.update(b)
    print(sorted(s), len(s))

# a set that has been punched full of holes, then cloned
h = set(range(200))
for i in range(0, 200, 3):
    h.discard(i)
c = set()
c.update(h)
print(len(c), sorted(c) == sorted(h), c == h)
print(sorted(h | set()) == sorted(h), len(h | {1000}) == len(h) + 1)

# update from itself, empty and not
e = set()
e.update(e)
print(len(e), sorted(e))
n = {1, 2, 3}
n.update(n)
print(sorted(n))
n |= n
print(sorted(n))

# the references a clone transfers
seen = []


class W:
    def __init__(self, t):
        self.t = t

    def __hash__(self):
        return hash(self.t)

    def __eq__(self, o):
        return isinstance(o, W) and self.t == o.t

    def __del__(self):
        seen.append(self.t)


def churn():
    src = {W(i) for i in range(20)}
    d = set()
    d.update(src)
    u = src | d
    v = set(src)
    del d, u, v
    print(len(seen))
    del src


churn()
print(len(seen))

# update from a non-set iterable, whose room is now taken in one step
for src in ([1, 2, 3], (4, 5), range(6, 9), "xy", {10: 0, 11: 0}, iter([12])):
    q = set()
    q.update(src)
    print(sorted(q, key=repr))
q = {1}
q.update([2], (3,), range(4, 6))
print(sorted(q))

# --- the walks stop when they have seen every live element ----------------
# Each operator's loop now counts ob_size down instead of running to the end
# of the table, so what has to hold is that ob_size and "number of occupied
# slots" never disagree.  They disagree if a tombstone is miscounted, if a
# resize forgets one, or if an __eq__ mutates the set mid-walk -- so the
# operands below are punched full of holes, are of very different sizes, and
# in the last group carry an __eq__ that empties the other side.
holed = set(range(400))
for i in range(400):
    if i % 5:
        holed.discard(i)
plain = set(range(0, 400, 10))
drained = set(range(100))
for i in range(100):
    drained.discard(i)
big = set(range(2000))
small = {3, 7, 11}
empty = set()

sets = [holed, plain, drained, big, small, empty]
for a in sets:
    for b in sets:
        print(len(a | b), len(a & b), len(a - b), len(a ^ b),
              a <= b, a >= b, a < b, a > b, a == b, a.isdisjoint(b))

# and the method forms, which coerce their argument first
for a in (holed, small, empty):
    print(len(a.union([1, 2])), len(a.intersection(range(50))),
          len(a.difference("xy")), len(a.symmetric_difference((1, 2, 3))),
          a.issubset(range(2000)), a.issuperset([]), a.isdisjoint([9999]))


# an __eq__ that clears the other operand while the walk is running
class Bomb:
    target = None

    def __hash__(self):
        return 12345

    def __eq__(self, o):
        t = Bomb.target
        if t is not None:
            Bomb.target = None
            t.clear()
        return False


def blow(op):
    # How MANY elements survive is slot order, and slot order is not the
    # same in two implementations -- what has to hold is that the walk ends,
    # that it answers a set, and that every element in the answer came from
    # an operand rather than from a slot the clear left behind.
    victim = set(range(30))
    live = set(range(20))
    live.add(Bomb())
    victim.add(Bomb())
    everything = set(range(30)) | set(range(20))
    Bomb.target = victim
    try:
        r = op(live, victim)
    except (RuntimeError, TypeError):
        return "raised"
    ok = all(x in everything or isinstance(x, Bomb) for x in r)
    return (type(r).__name__, ok, len(r) <= 51)


for op in (lambda a, b: a | b, lambda a, b: a & b, lambda a, b: a - b,
           lambda a, b: a ^ b):
    print(blow(op))
