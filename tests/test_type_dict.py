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

# --- the twelve names type_getattr answers before it walks anything -------
# Each was an ap_strcmp CALL, made in order, before any dict was touched --
# so an ORDINARY class attribute, which is what `C.attr` almost always is,
# matched none of them and paid for all twelve.  ap_strcmp was 17% of the
# case.  All twelve begin with two underscores, so two byte compares now
# stand in front of the lot.
#
# What that must not change: a name that starts with one underscore, or ends
# with two, or is exactly "__", or is empty, still resolves the ordinary way.


class Attrs:
    LIMIT = 7
    _priv = 1
    __mangled = 2

    def m(self):
        return 1

    @property
    def p(self):
        return 2


class Sub(Attrs):
    pass


for t in (Attrs, Sub, Exception, ValueError):
    print(t.__name__, t.__qualname__, t.__module__,
          len(t.__mro__), len(t.__bases__),
          t.__basicsize__ >= 0, isinstance(t.__dictoffset__, int),
          isinstance(t.__weakrefoffset__, int), isinstance(t.__flags__, int),
          type(t.__dict__).__name__)
print(Attrs.LIMIT, Attrs._priv, Attrs._Attrs__mangled, Sub.LIMIT,
      type(Attrs.m).__name__, type(Attrs.p).__name__)
print(sorted(x for x in Attrs.__dict__ if not x.startswith("__")))


class Underscores:
    _ = 1
    __ = 2
    _x__ = 3
    __x = 4
    x__ = 5


print(Underscores._, Underscores.__, Underscores._x__,
      Underscores._Underscores__x, Underscores.x__)


class Doc:
    pass


Doc.__doc__ = "doc"
print(Doc.__doc__, Doc.__name__)

for t, n in ((Attrs, "nope"), (Attrs, "__nope__"), (Attrs, "_nope"),
             (int, "nope"), (Attrs, "")):
    try:
        getattr(t, n)
    except AttributeError as e:
        print("AttributeError", e)
print(getattr(Attrs, "LIMIT"), getattr(Attrs, "nope", "DEF"),
      hasattr(Attrs, "__mro__"))


class Meta(type):
    META = 9


class ViaMeta(metaclass=Meta):
    OWN = 1


print(ViaMeta.OWN, ViaMeta.META, ViaMeta.__name__, type(ViaMeta).__name__)
print(int.__name__, str.__module__, len(list.__mro__), dict.__basicsize__ > 0)
