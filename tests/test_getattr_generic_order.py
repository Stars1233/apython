"""`__dict__` and `__class__` are ordinary resolution, not the __getattr__ hook.

CPython answers both from getsets on the type, inside `__getattribute__`, so a
class that defines `__getattr__` never sees either name.  Asking the hook
first is invisible until a `__getattr__` reads `self.__dict__` -- then it
re-enters itself and recurses until the stack runs out.  typing.py's
`_BaseGenericAlias.__getattr__` is written exactly that way:

    if '__origin__' in self.__dict__ and not _is_dunder(attr):

so the whole typing module, and the seven stdlib modules that import it,
died on a RecursionError.
"""

calls = []


class Hooked:
    def __getattr__(self, attr):
        calls.append(attr)
        return ("hook", attr)


h = Hooked()
print("--- the hook does not see them ---")
print(h.__dict__, h.__class__ is Hooked, calls)
print(h.other, calls)

print("--- and a hook that reads its own __dict__ terminates ---")


class Reads:
    def __getattr__(self, attr):
        if "origin" in self.__dict__:
            return self.__dict__["origin"]
        raise AttributeError(attr)


r = Reads()
try:
    r.anything
except AttributeError as e:
    print("AttributeError", e)
r.origin = 7
print(r.whatever, r.__dict__)

print("--- the dict is real: created on first read and kept ---")


class Empty:
    def __getattr__(self, attr):
        raise AttributeError(attr)


e = Empty()
d = e.__dict__
print(d, d is e.__dict__)
d["x"] = 1
print(e.x, e.__dict__)

print("--- a __slots__ class still has none, and the hook still runs ---")


class Slotted:
    __slots__ = ("a",)

    def __getattr__(self, attr):
        return ("slotted", attr)


s = Slotted()
print(s.__dict__)
print(s.nothing)

print("--- a base with __slots__, a subclass without ---")


class Base:
    __slots__ = ("__weakref__",)


class Derived(Base):
    def __getattr__(self, attr):
        if "__origin__" in self.__dict__:
            return "found"
        raise AttributeError(attr)


dv = Derived()
print(dv.__dict__)
try:
    dv.q
except AttributeError as ex:
    print("AttributeError", ex)

print("--- __getattribute__ still wins over both ---")


class Both:
    def __getattribute__(self, attr):
        if attr == "__dict__":
            return {"faked": True}
        return object.__getattribute__(self, attr)

    def __getattr__(self, attr):
        return ("hook", attr)


b = Both()
print(b.__dict__)
print(b.gone)

print("--- and a real attribute is still found before either ---")


class Normal:
    cls_attr = "class"

    def __init__(self):
        self.inst_attr = "instance"

    def __getattr__(self, attr):
        return ("hook", attr)


n = Normal()
print(n.cls_attr, n.inst_attr, n.missing, sorted(n.__dict__))

print("--- _typing exports what typing.py imports ---")
from _typing import (_idfunc, TypeVar, ParamSpec, TypeVarTuple, ParamSpecArgs,
                     ParamSpecKwargs, TypeAliasType, Generic)
print(_idfunc(1), _idfunc("a"), _idfunc(None))
# CPython's is a C function, so the wording of a wrong-arity call is the C
# one; only that it refuses is common ground.
try:
    _idfunc("a", "b")
    print("accepted two")
except TypeError:
    print("TypeError")
print(TypeVar("T").__name__, ParamSpec("P").__name__, TypeVarTuple("Ts").__name__)


print("--- __getattribute__ intercepts everything, found or not ---")
#
# It is the entry point of the protocol, not a fallback: `c.x` runs it even
# when x is an ordinary class attribute.  It used to be ignored entirely, so
# the only time a user's ran was when the name was missing and __getattr__
# would have run anyway.


class Everything:
    x = 1

    def __init__(self):
        self.y = 2

    def m(self):
        return "method"

    def __getattribute__(self, attr):
        return "always:" + attr


e2 = Everything()
print(e2.x, e2.y, e2.m, e2.zzz)


class Delegating:
    x = 1

    def __init__(self):
        self.y = 2

    def __getattribute__(self, attr):
        if attr == "faked":
            return "fake"
        return object.__getattribute__(self, attr)


d2 = Delegating()
print(d2.x, d2.y, d2.faked, d2.__class__.__name__, sorted(d2.__dict__))
try:
    d2.missing
except AttributeError as exc:
    print("AttributeError", exc)


print("--- and whatever it raises is what the caller sees ---")


class Raises:
    def __getattribute__(self, attr):
        if attr == "boom":
            raise KeyError(attr)
        if attr == "typed":
            raise TypeError("nope")
        return object.__getattribute__(self, attr)


r2 = Raises()
for name in ("boom", "typed"):
    try:
        getattr(r2, name)
    except Exception as exc:
        print(type(exc).__name__, exc)


print("--- non-pointer results survive the round trip ---")


class Values:
    def __getattribute__(self, attr):
        return {"i": 42, "f": 1.5, "s": "text", "b": True, "n": None}[attr]


v = Values()
print(v.i, v.f, v.s, v.b, v.n, type(v.i).__name__, type(v.f).__name__)


print("--- a subclass inherits it, and object's own still delegates ---")


class Sub(Delegating):
    pass


s2 = Sub()
print(s2.x, s2.faked, object.__getattribute__(s2, "x"))
