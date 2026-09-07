"""Two wordings that were CPython's from an older version, or from nowhere.

A module with no `__file__` reprs as `<module 'x' (built-in)>` here, because
`module_repr` decided on `__file__` alone.  CPython consults `__spec__` and
`__loader__` first and says `<module 'x'>` for one that is neither built-in nor
loaded from a file -- which is what `types.ModuleType('x')` makes, and what
every module a test builds by hand looks like.

A read-only property answers CPython 3.10's `can't set attribute` where 3.12
says `property 'r' of 'Plain' object has no setter`.  The deleter case is the
same shape.  CPython learns the property's name through `__set_name__`, so this
is a field on the property plus the receiver's type name at the raise site, not
a string swap.
"""

import sys
import types

print("--- a module built by hand ---")
m = types.ModuleType("built")
print("repr:", repr(m))
print("name:", m.__name__)
print("doc:", m.__doc__)
print("has __file__:", hasattr(m, "__file__"))

m2 = types.ModuleType("withdoc", "the doc")
print("with doc:", repr(m2), m2.__doc__)

print("--- after giving it a __file__ ---")
m.__file__ = "/tmp/nowhere.py"
print("repr:", repr(m))
del m.__file__
print("back:", repr(m))

print("--- a real built-in module ---")
print("sys:", repr(sys))
print("has __file__:", hasattr(sys, "__file__"))

print("--- a module imported from a file ---")
# copyreg is a .py in lib/, so it has a real __file__ and a real spec.  A
# helper file beside this one would not do: run from the .pyc, sys.path[0] is
# tests/__pycache__/ and the helper is one directory up.
import copyreg
r = repr(copyreg)
print("from file:", r.startswith("<module 'copyreg' from "), r.endswith("'>"))

print("--- __spec__ and __loader__ decide ---")
m3 = types.ModuleType("spec_none")
m3.__spec__ = None
print("spec None:", repr(m3))

print("--- a read-only property ---")


class Plain:
    @property
    def r(self):
        return 1

    @property
    def both(self):
        return 2

    @both.setter
    def both(self, v):
        self._b = v


p = Plain()
print("read:", p.r)
try:
    p.r = 2
except AttributeError as e:
    print("set:", e)
try:
    del p.r
except AttributeError as e:
    print("del:", e)

print("settable one works:", end=" ")
p.both = 5
print(p._b, p.both)
try:
    del p.both
except AttributeError as e:
    print("del a setter-only:", e)


print("--- on a subclass the message names the subclass ---")


class Sub(Plain):
    pass


try:
    Sub().r = 1
except AttributeError as e:
    print("subclass:", e)


print("--- a property with no getter ---")


class NoGet:
    def _s(self, v):
        pass

    w = property(None, _s)


try:
    NoGet().w
except AttributeError as e:
    print("get:", e)


print("--- an explicitly named property ---")


class Named:
    x = property(lambda self: 1)


try:
    Named().x = 2
except AttributeError as e:
    print("named:", e)

print("--- a property on a slotted class ---")


class Slotted:
    __slots__ = ()

    @property
    def s(self):
        return 3


try:
    Slotted().s = 1
except AttributeError as e:
    print("slotted:", e)

print("done")
