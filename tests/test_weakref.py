# Weak references.  The links live in a side table rather than in the object,
# so the interesting cases are the ones where the referent dies.
import _weakref


class C:
    def __init__(self, n):
        self.n = n


c = C(1)
r = _weakref.ref(c)
print(r() is c, _weakref.getweakrefcount(c))

# Without a callback the reference is shared, as CPython's is.
print(_weakref.ref(c) is r)

fired = []
r2 = _weakref.ref(c, lambda ref: fired.append(ref() is None))
print(r2 is not r, _weakref.getweakrefcount(c))

# Equal while both are alive, by the referents.
d = C(1)
C.__eq__ = lambda self, other: isinstance(other, C) and self.n == other.n
print(_weakref.ref(c) == _weakref.ref(d))

del C.__eq__
del c
print(r(), r2(), fired)

# A dead reference keeps the hash it had, so a set can still find it.
live = C(2)
h = _weakref.ref(live)
before = hash(h)
holder = {h}
del live
print(hash(h) == before, h in holder, h() is None)

# Proxies forward attribute access, and raise once the referent is gone.
p_target = C(3)
p = _weakref.proxy(p_target)
print(p.n)
p.n = 4
print(p_target.n)
del p_target
try:
    p.n
except (TypeError, ReferenceError):
    print("dead proxy")

# A class with no weak references at all costs nothing and behaves normally.
print([C(i).n for i in range(3)])

# A ref subclass must be able to reach its base constructor by name.  This is
# exactly weakref.KeyedRef's shape, and `super().__new__` finding only
# object.__new__ is what broke WeakValueDictionary: object refuses the extra
# arguments, because ref keeps its constructor in a slot rather than in
# ref.__dict__.
print("__new__" in _weakref.ref.__dict__, _weakref.ref.__new__ is object.__new__)


class KeyedRef(_weakref.ref):
    def __new__(type, ob, callback, key):
        self = super().__new__(type, ob, callback)
        return self

    def __init__(self, ob, callback, key):
        super().__init__(ob, callback)
        self.key = key


kr_target = C(7)
kr = KeyedRef(kr_target, None, "k")
print(kr() is kr_target, kr.key, type(kr) is KeyedRef)

# ref.__new__ called directly, and refused when the class is unrelated.
direct = _weakref.ref.__new__(KeyedRef, kr_target, None)
print(type(direct) is KeyedRef, direct() is kr_target)
try:
    _weakref.ref.__new__(dict)
except TypeError:
    print("unrelated class refused")


# Dropping the last reference to a REF takes its callback with it.  The side
# table's chain used to hold an owned reference, so a ref nobody else held
# stayed alive for as long as its referent did and its callback still ran --
# which CPython's does not.  The chain is borrowed now, and a ref leaves it as
# it dies.
order = []


def when_ref_dies_first():
    target = C(1)
    r = _weakref.ref(target, lambda ref: order.append("fired"))
    del r
    del target
    return order


print(when_ref_dies_first())
order.clear()


def when_referent_dies_first():
    target = C(2)
    r = _weakref.ref(target, lambda ref: order.append("fired"))
    del target
    del r
    return order


print(when_referent_dies_first())
order.clear()

# getweakrefcount and getweakrefs count what is LIVE, not what the chain has
# room for: a slot a dead reference left behind is not a reference.
live = C(3)
a = _weakref.ref(live, lambda ref: None)
b = _weakref.ref(live, lambda ref: None)
print(_weakref.getweakrefcount(live), len(_weakref.getweakrefs(live)))
del a
print(_weakref.getweakrefcount(live), len(_weakref.getweakrefs(live)))
print(b in _weakref.getweakrefs(live))
del b
print(_weakref.getweakrefcount(live), _weakref.getweakrefs(live))

# ...and a new one after that still works, on the same referent.
c = _weakref.ref(live, lambda ref: order.append("late"))
print(_weakref.getweakrefcount(live), c() is live)
del live
print(order)


# The chain the side table keeps is NOT collector-tracked, and it cannot be:
# its entries are borrowed, and the collector walks a tracked list's items and
# counts a reference for each.  Every weak reference in it would look one
# reference short of reachable, and an explicit collect() would free objects a
# live frame is still holding.
import gc


def survives_a_collection():
    target = C(11)
    keep = _weakref.ref(target, lambda ref: None)
    shared = _weakref.ref(target)
    for _ in range(3):
        gc.collect()
    return target.n, keep() is target, shared() is target


print(survives_a_collection())


def a_cycle_through_a_referent():
    class Node:
        pass

    a = Node()
    b = Node()
    a.other = b
    b.other = a
    r = _weakref.ref(a, lambda ref: None)
    alive = r() is a
    del a, b
    gc.collect()
    return alive, r() is None


print(a_cycle_through_a_referent())
