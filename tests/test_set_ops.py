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
