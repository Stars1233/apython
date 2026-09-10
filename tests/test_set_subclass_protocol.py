# A set or frozenset SUBCLASS is a set, and CPython reads its TABLE.
#
# Two places compared the type pointer against set_type and frozenset_type
# exactly, so a subclass fell through to the generic iterator path and its
# own __iter__ was asked -- which CPython never does.  `{*FS([1,2,3])}` was
# {99} here and {1, 2, 3} there, and the same for set(), frozenset(),
# .update() and every constructor that presizes from a source.
#
# set_contains had the third: `SubSet() in s` raised "unhashable type" where
# CPython answers False, because the retry-as-a-frozenset that makes
# `set() in {1, 2}` legal was gated on the exact type.  The retry belongs to
# UNHASHABLE set-likes only -- a subclass that defines __hash__ is hashable
# and must be looked up as itself.


class FS(frozenset):
    def __iter__(self):
        return iter([99])


class SS(set):
    def __iter__(self):
        return iter([99])


class H(set):
    def __hash__(self):
        return 12345


# The constructors read the table.
print(sorted(set(FS([1, 2, 3]))))
print(sorted(set(SS([1, 2, 3]))))
print(sorted(frozenset(SS([1, 2]))))
print(sorted(frozenset(FS([1, 2]))))

# ...and so does the set display, which is BUILD_SET + SET_UPDATE.
print(sorted({*FS([1, 2, 3])}))
print(sorted({*SS([1, 2, 3])}))
print(sorted({0, *SS([1, 2])}))

# ...and update(), by name.
s = {0}
s.update(FS([1, 2]))
print(sorted(s))
s = {0}
s.update(SS([1, 2]), FS([3]))
print(sorted(s))

# The methods that take any iterable were already right; they must stay so.
print(sorted(set().union(FS([7, 8]))))
print(sorted({1, 2, 3}.intersection(SS([2, 3]))))
print(sorted({1, 2, 3}.difference(SS([2]))))
print(sorted({1, 2}.symmetric_difference(SS([2, 3]))))
print({1, 2}.issubset(SS([1, 2, 3])), {1, 2, 3}.issuperset(SS([1, 2])))
print({1, 2}.isdisjoint(SS([3])))

s = {1, 2}
s.difference_update(SS([1]))
print(sorted(s))
s = {1, 2}
s.intersection_update(SS([1]))
print(sorted(s))
s = {1, 2}
s.symmetric_difference_update(SS([2, 3]))
print(sorted(s))

# Membership: an unhashable set-like is retried as a frozenset...
print(set([1]) in {frozenset([1])})
print(SS([1]) in {frozenset([1])})
print(SS([9]) in {frozenset([1])})
print(FS([1]) in {frozenset([1])})
# ...and one that defines __hash__ is looked up as itself.
print(H([1]) in {H([1])})
print(H([1]) in {frozenset([1])})

# A non-set that is unhashable still raises.
try:
    [1] in {frozenset([1])}
except TypeError as e:
    print("list:", e)
try:
    {1: 2} in {frozenset([1])}
except TypeError as e:
    print("dict:", e)

# An empty subclass, and one bigger than its own presize hint.
print(sorted(set(SS())))
print(sorted(set(SS(range(40)))) == list(range(40)))

# The result of an operator over a subclass is still the plain base.
print(type(FS([1]) | frozenset([2])).__name__)
print(type(SS([1]) | {2}).__name__)
print(type(set(SS([1]))).__name__, type(frozenset(FS([1]))).__name__)
