"""`__init__` is fetched through the descriptor protocol, and `__call__` is a name.

type_call took __init__ straight out of the class dict and prepended self
unconditionally, so anything that is not a plain function got self anyway.
CPython binds through __get__, and a callable that is not a descriptor is
called with the arguments as written.

The plain-function case agrees either way, and that is what makes the bug easy
to miss: a function IS a descriptor, so binding it and prepending self by hand
reach the same place.  Everything below is the cases where they part.

Second half: no builtin type carried __call__ in its tp_dict, so
`hasattr(len, '__call__')` was False for every builtin, function and lambda --
and `isinstance(x, collections.abc.Callable)` is `_check_methods(C, '__call__')`
(lib/_collections_abc.py), so it was wrong for all of them.
"""

import collections.abc
import functools


SEEN = []


def g(*a):
    """__init__ must return None, so this records instead of returning."""
    SEEN.append(tuple(type(x).__name__ for x in a))


print("--- a plain function still gets self ---")


class Plain:
    __init__ = g


SEEN.clear()
Plain()
print("plain gets self:", SEEN)


class Recorded:
    def __init__(self, *a):
        self.got = a


print("normal:", Recorded(1, 2).got)


print("--- a staticmethod is not bound ---")


class Static:
    __init__ = staticmethod(g)


SEEN.clear()
Static()
print("staticmethod:", SEEN)


class SubSM(staticmethod):
    pass


class SubStatic:
    __init__ = SubSM(g)


SEEN.clear()
SubStatic()
print("staticmethod subclass:", SEEN)


print("--- things with no __get__ at all ---")


class Partial:
    __init__ = functools.partial(g)


SEEN.clear()
Partial()
print("partial:", SEEN)


class Inst:
    class C:
        def __call__(self, *a):
            SEEN.append(tuple(type(x).__name__ for x in a))

    __init__ = C()


SEEN.clear()
Inst()
print("callable instance:", SEEN)


print("--- a custom descriptor's __get__ runs ---")
log = []


class Desc:
    def __get__(self, obj, owner):
        log.append("get")
        def bound(*a):
            log.append(("call", len(a)))

        return bound


class WithDesc:
    __init__ = Desc()


WithDesc()
print("descriptor:", log)


print("--- a classmethod __init__ ---")


class CM:
    @classmethod
    def __init__(cls, *a):
        pass


CM()
print("classmethod: constructed")


print("--- __init__ must return None ---")


class BadInit:
    def __init__(self):
        return 1


try:
    BadInit()
    print("returned non-None: accepted - wrong")
except TypeError as e:
    print("returned non-None:", e)


print("--- arguments still reach it ---")


class Args:
    def __init__(self, a, b=2, *rest, kw=None):
        self.v = (a, b, rest, kw)


print("args:", Args(1).v)
print("args:", Args(1, 9, 8, 7, kw="k").v)


print("--- __call__ is reachable by name ---")
# Labelled, not typed: this tree has one builtin callable type where CPython
# has four (DIVERGENCES.md), so type(obj).__name__ differs for reasons that
# have nothing to do with __call__.
for label, obj in (("len", len), ("print", print), ("isinstance", isinstance),
                   ("function", g), ("lambda", lambda: 1), ("int", int),
                   ("str", str), ("type", type), ("list.append", [].append),
                   ("str.upper", "".upper),
                   ("bound __init__", Recorded(1).__init__)):
    print(label, hasattr(obj, "__call__"), callable(obj))


print("--- and the two agree with the ABC ---")
for label, obj in (("len", len), ("print", print), ("function", g),
                   ("lambda", lambda: 1), ("int", int), ("str", str),
                   ("type", type), ("list.append", [].append),
                   ("str.upper", "".upper)):
    if not isinstance(obj, collections.abc.Callable):
        print("NOT Callable:", label)
print("all callables are Callable")

for obj in (1, "s", [], {}, None, 1.5, (), object()):
    if isinstance(obj, collections.abc.Callable):
        print("wrongly Callable:", type(obj).__name__)
print("no non-callable is Callable")


print("--- calling through the name ---")
print("len:", len.__call__([1, 2, 3]))
print("upper:", "ab".upper.__call__())
print("function:", g.__call__(1, 2))


print("--- a class with __call__ still works ---")


class Callee:
    def __call__(self, x):
        return x * 2


c = Callee()
print("instance:", c(3), c.__call__(3), callable(c),
      isinstance(c, collections.abc.Callable))

print("--- __init__ comes from the instance's type, not the called class ---")


class IBase:
    def __new__(cls, *a):
        if cls is IBase:
            return object.__new__(ISub)
        return object.__new__(cls)

    def __init__(self, *a):
        print("IBase.__init__", a)


class ISub(IBase):
    def __init__(self, *a):
        print("ISub.__init__", a)


x = IBase(1, 2)
print("type:", type(x).__name__)
y = ISub(3)
print("type:", type(y).__name__)


class IOther:
    def __init__(self, *a):
        print("IOther.__init__", a)


class IFactory:
    def __new__(cls, *a):
        return object.__new__(IOther)

    def __init__(self, *a):
        print("IFactory.__init__", a)


z = IFactory(4)
print("type:", type(z).__name__, "(no __init__ runs: not an instance of cls)")

print("done")
