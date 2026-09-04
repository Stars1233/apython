# The in-place set operators.
#
# `s &= t` used to compute the right contents and then REBIND the name, so any
# other reference to the same set went on seeing the old value.  set.__iand__
# and its three siblings did not exist at all.  frozenset has none of them in
# CPython either, and must not grow them here.


def show(label, fn):
    try:
        print(label, "=>", repr(fn()))
    except BaseException as e:
        print(label, "!!", type(e).__name__, e)


# --- the operators mutate the object the name is bound to
def inplace(op, other):
    s = {1, 2, 3}
    ref = s
    if op == '&':
        s &= other
    elif op == '|':
        s |= other
    elif op == '-':
        s -= other
    elif op == '^':
        s ^= other
    # `ref` is the interesting half: it is the same object, so it must show
    # the new contents.
    return sorted(s), sorted(ref), s is ref


show("&=", lambda: inplace('&', {2, 3, 4}))
show("|=", lambda: inplace('|', {4, 5}))
show("-=", lambda: inplace('-', {1, 9}))
show("^=", lambda: inplace('^', {3, 4}))

# Emptying and self-application are the two edges of the swap.
show("&= disjoint", lambda: inplace('&', {9}))
show("-= all", lambda: inplace('-', {1, 2, 3}))
show("^= self", lambda: (lambda s: (s.__ixor__(s), sorted(s)))({1, 2, 3}))
show("&= self", lambda: (lambda s: (sorted(s.__iand__(s)),))({1, 2, 3}))
show("|= empty", lambda: inplace('|', set()))
show("&= empty", lambda: inplace('&', set()))


# --- the names exist, and answer for the object they are called on
def byname(name, other):
    s = {1, 2, 3}
    ref = s
    r = getattr(s, name)(other)
    return sorted(r), sorted(ref), r is ref


for _n, _o in (("__iand__", {2, 3}), ("__ior__", {7}),
               ("__isub__", {1}), ("__ixor__", {3, 7})):
    show(_n, lambda n=_n, o=_o: byname(n, o))
    show("has " + _n, lambda n=_n: hasattr(set, n))

# --- the operators take a set, and only a set
for _n in ("__iand__", "__ior__", "__isub__", "__ixor__"):
    show(_n + "(list)", lambda n=_n: getattr({1}, n)([1]))
    show(_n + "(int)", lambda n=_n: getattr({1}, n)(5))


def opbad(op):
    s = {1, 2}
    if op == '&':
        s &= [1]
    elif op == '|':
        s |= [1]
    elif op == '-':
        s -= [1]
    elif op == '^':
        s ^= [1]
    return s


for _op in '&|-^':
    show("%s= list" % _op, lambda op=_op: opbad(op))

# --- frozenset has none of them, and rebinds instead
for _n in ("__iand__", "__ior__", "__isub__", "__ixor__"):
    show("frozenset has " + _n, lambda n=_n: hasattr(frozenset, n))


def frozen_inplace():
    f = frozenset({1, 2, 3})
    ref = f
    f &= {2, 3}
    return sorted(f), sorted(ref), f is ref


show("frozenset &=", frozen_inplace)


# --- a set subclass mutates in place and keeps its own type
class S(set):
    pass


def subclass_inplace():
    s = S({1, 2, 3})
    ref = s
    s &= {2, 3}
    return sorted(s), type(s).__name__, s is ref


show("subclass &=", subclass_inplace)


# A frozenset subclass is still immutable.
class F(frozenset):
    pass


def frozen_subclass():
    f = F({1, 2, 3})
    ref = f
    f -= {1}
    return sorted(f), f is ref


show("frozenset subclass -=", frozen_subclass)

# --- the mutated set is still a working hash table afterwards
def rehash():
    s = set(range(20))
    s &= set(range(5, 15))
    s |= {100, 101}
    s -= {7}
    out = [x in s for x in (6, 7, 100, 999)]
    s.add(7)
    s.discard(6)
    return sorted(s), out, len(s)


show("still a table", rehash)


# An iterator over a set that is then mutated in place must notice.
def mutate_during_iter():
    s = {1, 2, 3}
    it = iter(s)
    next(it)
    s &= {1, 2, 3}
    return list(it)


show("mutate during iter", mutate_during_iter)

# --- the value the operator produces is the receiver, not a copy
def chain():
    s = {1, 2, 3}
    a = s
    b = s
    s |= {4}
    s &= {2, 3, 4}
    return sorted(a), sorted(b), a is b is s


show("aliases agree", chain)

print("done")
