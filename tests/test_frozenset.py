# frozenset: the result of an operator keeps the left operand's kind, and a
# frozenset hashes.
#
# Every set result was built with set_new, which bakes in set_type, so
# frozenset({1}) | frozenset({2}) was a set and frozenset({1}).copy() was a
# set.  frozenset_type.tp_hash was 0 besides, so the one type that exists to
# be a dict key or a set member could be neither.
#
# The method forms are the third half of it: union, intersection, difference
# and symmetric_difference take any iterable where the operators take only a
# set, and all four read the argument as a hash table whatever it was --
# s.difference([1]) found nothing occupied in a list's header and answered s.

f1 = frozenset({1, 2})
f2 = frozenset({2, 3})
s1 = {1, 2}

for name, v in (("f|f", f1 | f2), ("f&f", f1 & f2), ("f-f", f1 - f2),
                ("f^f", f1 ^ f2), ("s|s", s1 | {3}), ("f|s", f1 | {3}),
                ("s|f", s1 | f2), ("s&f", s1 & f2), ("s-f", s1 - f2),
                ("s^f", s1 ^ f2)):
    print(name, type(v).__name__, sorted(v))

# The inplace forms degrade to the binary slot and inherit the same rule.
a = frozenset({1})
a |= frozenset({2})
print("f|=", type(a).__name__, sorted(a))
b = {1}
b |= {2}
print("s|=", type(b).__name__, sorted(b))

# The method forms, with a set and with an arbitrary iterable.
print(type(f1.copy()).__name__, type(s1.copy()).__name__)
print(f1.copy() is f1, s1.copy() is s1)
for name, v in (("union", f1.union([9])), ("inter", f1.intersection([2])),
                ("diff", f1.difference([1])),
                ("sym", f1.symmetric_difference([1])),
                ("s.union", s1.union((9,))), ("s.inter", s1.intersection("12")),
                ("s.diff", s1.difference(iter([1]))),
                ("s.sym", s1.symmetric_difference({1: "v"}))):
    print(name, type(v).__name__, sorted(v, key=repr))

# An operand that is not iterable at all is a TypeError, not a wrong answer.
for expr in ("s1.union(5)", "s1.difference(None)", "s1.intersection(3.5)"):
    try:
        eval(expr)
        print(expr, "=> no error")
    except TypeError:
        print(expr, "=> TypeError")

# The operators stay strict where CPython is strict.
for expr in ("s1 | [1]", "f1 - [1]", "s1 ^ 5"):
    try:
        eval(expr)
        print(expr, "=> no error")
    except TypeError:
        print(expr, "=> TypeError")

# Hashing.
g = frozenset({2, 1})
print(hash(f1) == hash(g), f1 == g)
print({f1: "a"}[g])
print(len({frozenset(), frozenset({1}), frozenset({1, 2}), frozenset({2, 1})}))
print(frozenset({1}) in {frozenset({1})})
try:
    hash({1})
except TypeError:
    print("set is unhashable")

# Subclasses: CPython answers with the plain base, not the subclass.
class F(frozenset): pass
class S(set): pass
print(type(F([1]).copy()).__name__, type(S([1]).copy()).__name__)
print(type(F([1]) | F([2])).__name__, type(S([1]) | S([2])).__name__)
print(type(F([1]).union([2])).__name__, type(S([1]).union([2])).__name__)


# --- frozenset stops sharing set's dict -------------------------------------
#
# The two used to share one dict object, stored into both types, so frozenset
# carried add, remove, discard, pop, clear and update as well.  Those bodies
# do not inspect self, so they did not raise -- they WORKED:
# frozenset({1}).add(2) mutated the frozenset in place.  A frozenset is the
# one type that exists to be a dict key, and mutating one after it had been
# used as a key left the dict able to find neither the old key nor the new.
#
# It also had no __hash__ of its own, so frozenset.__hash__ resolved up the
# MRO to object's, which answers the address.

print()
print("--- the mutators are gone ---")
f = frozenset({1, 2})
for name in ("add", "remove", "discard", "pop", "clear", "update"):
    print(name, hasattr(f, name))

d = {f: "value"}
try:
    f.add(3)
    print("MUTATED to", sorted(f))
except AttributeError:
    print("add raises AttributeError")
print("key still found:", d[frozenset({1, 2})])
print("unchanged:", sorted(f))

print("--- the readers are all still there ---")
for name in ("copy", "union", "intersection", "difference",
             "symmetric_difference", "issubset", "issuperset", "isdisjoint",
             "__contains__", "__len__", "__iter__", "__hash__", "__new__",
             "__class_getitem__"):
    print(name, hasattr(frozenset, name))

print("--- set keeps its own, and still mutates ---")
s = {1}
s.add(2)
s.update([3])
s.discard(1)
print(sorted(s), [n for n in ("add", "remove", "discard", "pop", "clear",
                              "update") if not hasattr(set(), n)])

print("--- __hash__ names frozenset's own ---")
print("agrees with hash():", frozenset.__hash__(f) == hash(f))
print("empty agrees:", frozenset.__hash__(frozenset()) == hash(frozenset()))
print("order-insensitive:", hash(frozenset([1, 2])) == hash(frozenset([2, 1])))

print("--- a subclass is filled by __new__, since there is no __init__ ---")
class F(frozenset):
    pass
class G(frozenset):
    def __new__(cls, it):
        return super().__new__(cls, it)
print(sorted(F([1, 2])), len(F([1, 2])), type(F([1, 2])).__name__)
print(sorted(F()), sorted(F("ab")), sorted(F(F([3]))))
print(sorted(G([4, 5])))
print("hashes as itself:", hash(F([1, 2])) == hash(frozenset([1, 2])))
print("usable as a key:", {F([1, 2]): "v"}[frozenset([1, 2])])
class Sub(set):
    pass
print("set subclass still fills:", sorted(Sub([7, 8])))

# CPython's frozenset carries the reflected four as well as the forward four
# -- the comment here used to claim otherwise -- and both types hold the same
# slots, so they belong in the block the two share.
print(hasattr(frozenset, "__rsub__"), hasattr(frozenset, "__rand__"),
      hasattr(frozenset, "__rxor__"), hasattr(frozenset, "__ror__"))
print(frozenset({2}).__rsub__({1}))
print(sorted(frozenset({1, 2}).__rand__({2, 3})))
print(sorted(frozenset({1}).__rxor__({2})))
print(sorted(frozenset({1}).__ror__({2})))
