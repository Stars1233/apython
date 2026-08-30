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
