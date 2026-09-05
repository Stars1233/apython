"""`type`'s own dict: the attributes a class has, reachable through the type.

Every one of these answered when it was asked of a class -- `C.__mro__` has
worked for a long time -- because type_getattr special-cases the name before
it walks anything.  What did not exist was the descriptor in `type.__dict__`
that CPython answers them THROUGH, and the stdlib reaches for it directly:

    _static_getmro = type.__dict__['__mro__'].__get__

is line 1793 of CPython's own inspect.py, and it is a KeyError without one.
`type.mro()` is the same gap from the other side -- abc calls `cls.mro()`.
"""

class C:
    pass


class D(C):
    pass


class M(type):
    pass


class E(D, metaclass=M):
    pass


print("--- the descriptors are there ---")
for name in ("__mro__", "__bases__", "__base__", "__name__", "__qualname__",
             "__module__", "__dict__", "mro"):
    print(name, name in type.__dict__)

print("--- and they answer ---")
get_mro = type.__dict__["__mro__"].__get__
print(get_mro(C))
print(get_mro(D))
print(get_mro(E))
print(type.__dict__["__bases__"].__get__(D))
print(type.__dict__["__base__"].__get__(D))
print(type.__dict__["__name__"].__get__(D))
print(type.__dict__["__qualname__"].__get__(D))
print(type.__dict__["__module__"].__get__(D))
print("__module__" in type.__dict__["__dict__"].__get__(C))

print("--- what inspect does with it ---")
def static_getmro(klass):
    return type.__dict__["__mro__"].__get__(klass)

print([c.__name__ for c in static_getmro(E)])
print([c.__name__ for c in static_getmro(int)])
print([c.__name__ for c in static_getmro(type)])

print("--- type.mro ---")
print(C.mro())
print(D.mro())
print(E.mro())
print(int.mro())
print(D.mro() == list(D.__mro__))

print("--- a metaclass inherits it, which is what abc needs ---")
# `M.mro` is the descriptor found on `type`, which M inherits, so reading it
# off M itself gives the UNBOUND one -- CPython refuses this call too.
try:
    M.mro()
except TypeError:
    print("TypeError: unbound off the metaclass itself")
print(M("X", (), {}).mro())
print(E.mro()[0] is E)


class Abstract(type):
    def __new__(mcls, name, bases, ns):
        cls = super().__new__(mcls, name, bases, ns)
        cls._names = [c.__name__ for c in cls.mro()]
        return cls


class Uses(D, metaclass=Abstract):
    pass


print(Uses._names)

print("--- the descriptor reprs the way CPython's does ---")
print(repr(type.__dict__["__mro__"]))
print(repr(type.__dict__["__name__"]))
print(type(type.__dict__["__mro__"]) is type(int.__dict__["real"]))

print("--- asked of something that is not a type ---")
for bad in (1, "x", object()):
    try:
        get_mro(bad)
    except TypeError as e:
        print("TypeError")

print("--- vars() over the things that have a __dict__ ---")
import sys
mod = sys.modules[__name__]
print(type(vars(mod)) is dict)
print("C" in vars(mod), "static_getmro" in vars(mod))
print(sorted(vars(C)) == sorted(C.__dict__))
print(vars(D()) == {})


class WithSlots:
    __slots__ = ("a",)


try:
    vars(WithSlots())
except TypeError as e:
    print("TypeError", e)
try:
    vars(1)
except TypeError as e:
    print("TypeError", e)

print("--- and the module-level shape sre_constants relies on ---")
import _codecs as _
print(len({k: v for k, v in vars(_).items() if k[:2] != "__"}) > 0)
