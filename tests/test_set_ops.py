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
