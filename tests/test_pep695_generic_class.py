# PEP 695: `class C[T]` is generic, and C[int] means something.
#
# The class got its __type_params__ and everything a program could ask about
# the parameters answered correctly -- but Generic[T] was never put in its
# bases, so C.__mro__ was (C, object) where CPython's is (C, Generic, object)
# and `C[int]` was "TypeError: type 'C' is not subscriptable".  A generic
# class that cannot be subscripted is most of what the syntax is for.
#
# CPython threads the parameter tuple through a cell so the class body can
# see it.  This does not need one: the wrapper scope has already left the
# tuple on the stack, under everything the class call emits, so it is COPIED
# up from that depth just before the CALL and turned into Generic[*params]
# by the intrinsic that was already wired up.
#
# The Generic is `typing`'s when typing can be imported, which is what
# CPython's intrinsic does -- it matters, because a program asking
# `issubclass(C, typing.Generic)` is asking about that class and not about a
# lookalike.
import typing


class Box[T]:
    def __init__(self, item: T):
        self.item = item

    def get(self) -> T:
        return self.item


print("__type_params__:", Box.__type_params__)
print("mro:", [c.__name__ for c in Box.__mro__])
print("the base is typing's:", Box.__mro__[1] is typing.Generic)
print("issubclass:", issubclass(Box, typing.Generic))
print("subscript:", Box[int])
print("and it still constructs:", Box(3).get())

# --- several parameters ------------------------------------------------
class Pair[K, V]:
    pass


print("two parameters:", Pair.__type_params__, Pair[int, str])
print("two-parameter mro:", [c.__name__ for c in Pair.__mro__])


# --- with written bases as well ----------------------------------------
class Base:
    pass


class WithBase[T](Base):
    pass


print("a written base too:", [c.__name__ for c in WithBase.__mro__])
print("and it subscripts:", WithBase[int])


class TwoBases[T](Base, dict):
    pass


print("two written bases:", [c.__name__ for c in TwoBases.__mro__])


# --- a keyword argument in the class header ----------------------------
class Meta(type):
    def __new__(mcls, name, bases, ns, **kw):
        cls = super().__new__(mcls, name, bases, ns)
        cls.kw = kw
        return cls


class WithKeyword[T](metaclass=Meta, extra=1):
    pass


print("a metaclass and a keyword:", WithKeyword.kw,
      [c.__name__ for c in WithKeyword.__mro__])


# --- a bound, a constraint set, a ParamSpec and a TypeVarTuple ---------
class Bounded[T: int]:
    pass


class Constrained[T: (int, str)]:
    pass


class WithSpec[**P]:
    pass


class WithTuple[*Ts]:
    pass


for cls in (Bounded, Constrained, WithSpec, WithTuple):
    print("%-12s %s %s" % (cls.__name__, cls.__type_params__,
                           [c.__name__ for c in cls.__mro__]))

# --- nested, and inside a function -------------------------------------
class Outer[T]:
    class Inner[U]:
        pass


print("nested:", Outer.__type_params__, Outer.Inner.__type_params__,
      [c.__name__ for c in Outer.Inner.__mro__])


def make[T]():
    class Local[U]:
        pass

    return Local


Local = make()
print("inside a function:", Local.__type_params__,
      [c.__name__ for c in Local.__mro__], Local[int])

# --- the body can still see the parameter ------------------------------
class UsesParam[T]:
    annotation: T

    def method(self, x: T) -> T:
        return x


print("the body sees it:", UsesParam.__annotations__,
      UsesParam.method.__annotations__)

# --- a plain class is untouched ----------------------------------------
class Plain:
    pass


print("a plain class:", [c.__name__ for c in Plain.__mro__],
      hasattr(Plain, "__type_params__") and Plain.__type_params__)
try:
    Plain[int]
    print("a plain class subscripts: WRONG")
except TypeError as exc:
    print("a plain class does not subscript:", exc)

# --- __qualname__, which the hidden wrapper scope was eating -----------
# PEP 695 wraps a `def f[T]` or a `class C[T]` in a hidden function scope
# that binds T.  It has no name, and the qualname walk gave up on an unnamed
# link rather than skipping it -- so EVERY generic def and class lost its
# prefix: `class TopG[T]: class Nested[U]` reported "Nested" where CPython
# reports "TopG.Nested", and a generic method reported its bare name.
class TopG[T]:
    class Nested[U]:
        pass

    def method[V](self):
        pass


print("qualnames:", TopG.__qualname__, TopG.Nested.__qualname__,
      TopG.method.__qualname__)


def holder():
    class InAFunction[U]:
        pass

    def generic_inner[V]():
        pass

    return InAFunction, generic_inner


cls, fn = holder()
print("inside a function:", cls.__qualname__, fn.__qualname__)


def generic_holder[T]():
    class Deep[U]:
        pass

    return Deep


print("inside a generic function:", generic_holder().__qualname__)


class Plain2:
    class AlsoPlain:
        pass


print("a plain nesting is unchanged:", Plain2.AlsoPlain.__qualname__)

# --- generic functions and aliases, which already worked ---------------
def identity[T](x: T) -> T:
    return x


print("a generic function:", identity.__type_params__, identity(7))

type Alias[T] = list[T]
print("a type alias:", Alias, Alias[int], Alias.__type_params__)
print("survived")
