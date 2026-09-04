# set's variadic method forms, and the three mutating ones.
#
# union, intersection and difference take any number of iterables in CPython:
# `s.difference(a, b)` is `(s - a) - b`, and the no-argument form is a copy.
# All three took exactly one here and raised for everything else, so
# `set().union(*parts)` -- the ordinary way to flatten a list of sets -- was a
# TypeError.  `update` was worse: it took the first source and silently
# ignored the rest.  intersection_update, difference_update and
# symmetric_difference_update did not exist at all.


def show(label, fn):
    try:
        print(label, "=>", repr(fn()))
    except BaseException as e:
        print(label, "!!", type(e).__name__, e)


S = {1, 2, 3, 4}

# --- the variadic non-mutating forms
show("union()", lambda: sorted(S.union()))
show("union(a)", lambda: sorted(S.union([9])))
show("union(a,b)", lambda: sorted(S.union([9], [10])))
show("union(a,b,c)", lambda: sorted(S.union([9], [10], {11})))
show("union(*parts)", lambda: sorted(set().union(*[{1}, {2}, {3}])))

show("intersection()", lambda: sorted(S.intersection()))
show("intersection(a)", lambda: sorted(S.intersection([1, 2, 9])))
show("intersection(a,b)", lambda: sorted(S.intersection([1, 2, 3], [2, 3])))
show("intersection empty", lambda: sorted(S.intersection([1], [9])))

show("difference()", lambda: sorted(S.difference()))
show("difference(a)", lambda: sorted(S.difference([1])))
show("difference(a,b)", lambda: sorted(S.difference([1], [2])))
show("difference all", lambda: sorted(S.difference([1, 2], [3, 4])))

# The source can be any iterable, at any position.
show("union of strs", lambda: sorted(set("ab").union("bc", "cd")))
show("union of gen", lambda: sorted({1}.union(x for x in (2, 3))))
show("difference tuple", lambda: sorted(S.difference((1,), (2,))))
show("intersection dict", lambda: sorted(S.intersection({1: 'a', 2: 'b'})))

# The receiver is untouched by all three.
show("S unchanged", lambda: sorted(S))

# frozenset gets the same variadic forms, and answers a frozenset.
show("frozenset union", lambda: sorted(frozenset({1}).union([2], [3])))
show("frozenset kind", lambda: type(frozenset({1}).union([2])).__name__)
show("frozenset diff()", lambda: sorted(frozenset({1, 2}).difference()))

# symmetric_difference is the one that really does take exactly one.
show("symdiff(a)", lambda: sorted(S.symmetric_difference([1, 9])))
show("symdiff(a,b)", lambda: S.symmetric_difference([1], [2]))
show("symdiff()", lambda: S.symmetric_difference())


# --- update, which is variadic too
def upd(*sources):
    s = {1, 2}
    ref = s
    s.update(*sources)
    return sorted(ref), s is ref


show("update()", lambda: upd())
show("update(a)", lambda: upd([3]))
show("update(a,b)", lambda: upd([3], [4]))
show("update(a,b,c)", lambda: upd([3], {4}, (5,)))
show("update(*parts)", lambda: upd(*[{7}, {8}]))
show("update non-iterable", lambda: upd([3], 5))


# --- the three mutating forms
def mutate(name, *sources):
    s = {1, 2, 3, 4}
    ref = s
    r = getattr(s, name)(*sources)
    return r, sorted(ref), s is ref


show("intersection_update", lambda: mutate("intersection_update", [2, 3, 9]))
show("intersection_update 2",
     lambda: mutate("intersection_update", [1, 2, 3], [2, 3]))
show("intersection_update 0", lambda: mutate("intersection_update"))
show("intersection_update empty", lambda: mutate("intersection_update", []))

show("difference_update", lambda: mutate("difference_update", [1]))
show("difference_update 2", lambda: mutate("difference_update", [1], [2]))
show("difference_update 0", lambda: mutate("difference_update"))
show("difference_update all",
     lambda: mutate("difference_update", [1, 2, 3, 4]))

show("symmetric_difference_update",
     lambda: mutate("symmetric_difference_update", [3, 9]))
show("symmetric_difference_update 2",
     lambda: mutate("symmetric_difference_update", [1], [2]))
show("symmetric_difference_update 0",
     lambda: mutate("symmetric_difference_update"))

# They take any iterable, and a bad one raises without leaving a wreck.
show("update from str", lambda: mutate("difference_update", "x"))


def half_applied():
    s = {1, 2, 3}
    try:
        s.intersection_update([1, 2], 5)
    except TypeError as e:
        return sorted(s), type(e).__name__
    return "no raise"


show("bad second source", half_applied)

# --- the names exist on set and not on frozenset
for _n in ("intersection_update", "difference_update",
           "symmetric_difference_update"):
    show("set has " + _n, lambda n=_n: hasattr(set, n))
    show("frozenset has " + _n, lambda n=_n: hasattr(frozenset, n))


# --- a set subclass mutates in place and keeps its type
class S2(set):
    pass


def subclass():
    s = S2({1, 2, 3})
    ref = s
    s.intersection_update([2, 3])
    return sorted(ref), type(s).__name__, s is ref


show("subclass update", subclass)


# --- the set is a working table afterwards
def still_a_table():
    s = set(range(30))
    s.intersection_update(range(5, 25), range(10, 30))
    s.difference_update([12], [13])
    s.symmetric_difference_update([10, 100])
    s.add(12)
    s.discard(11)
    return sorted(s), len(s), (100 in s, 10 in s, 11 in s)


show("still a table", still_a_table)

print("done")
