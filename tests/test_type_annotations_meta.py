# __annotations__ and __type_params__ come from the class, then the METATYPE.
#
# They were answered at the END of the attribute walk, which made
# `class Sub(Base): pass` report Base's -- and, because the dict is handed out
# by reference, `Sub.__annotations__['z'] = int` wrote into Base's.  Moving
# them in FRONT of the walk fixed that and broke the other half: the metatype
# walk is what a metaclass's own annotations arrive through, and answering in
# the ladder skipped it.  `class Meta(type): registry: dict = {}` then gave
# `C.__annotations__` as {}.
#
# Both now: the class's own dict, then the metatype's MRO, and only then the
# empty value each has to invent.

class Meta(type):
    registry: dict = {}


class C(metaclass=Meta):
    pass


class D(metaclass=Meta):
    own: int = 1


class Base:
    y: str = "s"


class Sub(Base):
    pass


print(C.__annotations__)
print(D.__annotations__)
print(Base.__annotations__, Sub.__annotations__)
Sub.__annotations__["z"] = int
print(Base.__annotations__, Sub.__annotations__)
# A static type has no dict to keep one in, and CPython refuses too.
for t in (int, str, list):
    try:
        t.__annotations__
        print(t.__name__, "has one")
    except AttributeError:
        print(t.__name__, "AttributeError")
print(int.__type_params__, str.__type_params__)


class MT(type):
    __type_params__ = ("mt",)


class E(metaclass=MT):
    pass


print(E.__type_params__)


class F[T]:
    pass


print(len(F.__type_params__), C.__type_params__)


class G:
    pass


print(G.__annotations__, G.__type_params__)


# A metaclass whose annotations a subclass of the class must NOT share, and a
# deeper metatype MRO.
class Meta2(Meta):
    extra: str = "s"


class H(metaclass=Meta2):
    pass


print(sorted(H.__annotations__))


class I(metaclass=Meta2):
    mine: float = 1.0


print(sorted(I.__annotations__))
I.__annotations__["added"] = bool
print(sorted(I.__annotations__), sorted(H.__annotations__))


# And a plain class still gets its own, created and kept.
class J:
    pass


a1 = J.__annotations__
a1["k"] = int
print(J.__annotations__, J.__annotations__ is a1)
