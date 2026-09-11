# A module's own dict is INSTANCE storage: what it holds comes back as it
# stands.  The descriptor protocol ran over it, so a staticmethod in a module
# dict was unwrapped into the function inside it, and a property object would
# have had its getter called.
#
# CPython's module_getattro is generic attribute access over the module's
# __dict__, which never runs a descriptor it finds there.
#
# It matters because a MODULE-level callable that must not bind when a class
# body stores it can only be spelled as a staticmethod here: CPython's
# select.select is a C function, which is not a descriptor, and
# Lib/selectors.py writes `_select = select.select` in a class body.  With the
# staticmethod unwrapped, every call through SelectSelector arrived one
# argument too long -- and subprocess.communicate() over that backend hung
# with the child's pipe still open.

import types

m = types.ModuleType("m")


def f(a, b=2):
    return ("f", a, b)


m.sm = staticmethod(f)
m.cm = classmethod(f)
m.pr = property(f)
m.fn = f


class D:
    def __get__(self, obj, objtype=None):
        return "bound"


m.dr = D()

print(type(m.sm).__name__, type(m.cm).__name__, type(m.pr).__name__)
print(type(m.fn).__name__, type(m.dr).__name__)
print(m.sm(1), m.fn(1))

# getattr() and hasattr() take the same road and have to agree.
print(type(getattr(m, "sm")).__name__, type(getattr(m, "dr")).__name__)
print(hasattr(m, "sm"), hasattr(m, "dr"), hasattr(m, "nope"))
print(m.__dict__["sm"] is m.sm, m.__dict__["dr"] is m.dr)

# A staticmethod reached through a module and then stored in a class body is
# the function, unbound -- which is the whole point.
class C:
    g = m.sm
    h = m.fn


c = C()
print(C.g(5), c.g(5))
r = c.h(5)
print(r[0], r[1] is c, r[2])   # h IS a function, so it binds

# The real one.
import select

# (type(select.select).__name__ is not compared: CPython's is a C function
# and this tree's is a staticmethod over a Python one, which is how a
# module-level callable says "do not bind" here.)


class S:
    _select = select.select


# Whatever select.select is spelled as, reaching it through a class body must
# not bind: the call below passes four arguments, and a bound one would make
# it five.
print(S()._select([], [], [], 0))

# An ordinary class still runs its descriptors, which is what this must not
# have broken.
class P:
    @property
    def v(self):
        return "prop"

    @staticmethod
    def s(a):
        return ("static", a)

    @classmethod
    def k(cls, a):
        return ("class", cls.__name__, a)


p = P()
print(p.v, p.s(1), p.k(1), P.s(1), P.k(1))
print(type(P.__dict__["v"]).__name__, type(P.__dict__["s"]).__name__)

print("done")
