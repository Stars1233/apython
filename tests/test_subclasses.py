# type.__subclasses__(), and the two things that were waiting on it.
#
# A class kept no list of its subclasses, so the name did not exist -- and
# _abc_subclasscheck's step 6, "was this registered against a SUBCLASS of the
# ABC rather than against the ABC itself?", had no way to ask.  B.register(X)
# for a B deriving from A left issubclass(X, A) answering False.
#
# The links live in a side table keyed by the base's address, the shape
# _weakref already uses, so no field had to be added to PyTypeObject and no
# static type table had to be edited.  The entries are borrowed and removed
# when the class is freed, which is what CPython's weak-referenced
# tp_subclasses amounts to.

import abc
import gc


class Base: pass
class Left(Base): pass
class Right(Base): pass
class Deep(Left): pass
class Both(Left, Right): pass


def names(cls):
    return sorted(c.__name__ for c in cls.__subclasses__())


print("=== direct subclasses only ===")
print("Base :", names(Base))
print("Left :", names(Left))
print("Right:", names(Right))
print("Deep :", names(Deep))

print("=== the list is the caller's ===")
a = Base.__subclasses__()
b = Base.__subclasses__()
print("distinct:", a is not b)
a.append("scribble")
print("unchanged:", names(Base))

print("=== a class that dies leaves the list ===")
def make_temporary():
    class Temporary(Base): pass
gc.collect()
before = names(Base)
make_temporary()
gc.collect()
print("same as before:", names(Base) == before)

print("=== bool is int's static subclass, and is listed ===")
print("bool in int:", "bool" in [c.__name__ for c in int.__subclasses__()])
class MyInt(int): pass
print("and so is a new one:", "MyInt" in [c.__name__ for c in int.__subclasses__()])

print("=== _abc step 6: a registration on a subclass counts ===")
class Shape(abc.ABC): pass
class Polygon(Shape): pass
class Duck: pass
Polygon.register(Duck)
print("issubclass(Duck, Polygon):", issubclass(Duck, Polygon))
print("issubclass(Duck, Shape)  :", issubclass(Duck, Shape))
print("isinstance(Duck(), Shape):", isinstance(Duck(), Shape))

class Unrelated: pass
print("and an unrelated class is still no:", issubclass(Unrelated, Shape))

# Two levels down, and a registration directly on the ABC, both still work.
class Triangle(Polygon): pass
class Duck2: pass
Triangle.register(Duck2)
print("two levels down:", issubclass(Duck2, Shape))
class Duck3: pass
Shape.register(Duck3)
print("direct:", issubclass(Duck3, Shape))

print("=== the argument has to be a class ===")
try:
    type.__subclasses__(42)
except TypeError:
    print("non-type => TypeError")

# A metatype's builtin functions bind like any other method.  type's
# __subclasses__ is one, and the metatype walk is the ONLY road to it for a
# class whose metatype is a metaclass of its own -- every ABC and every Enum,
# which is every class the stdlib calls it on.  Unbound, it answered
# "__subclasses__() takes no arguments".
from abc import ABC


class AbcBase(ABC):
    pass


class AbcChild(AbcBase):
    pass


print(sorted(c.__name__ for c in AbcBase.__subclasses__()))


class OwnMeta(type):
    pass


class MetaBase(metaclass=OwnMeta):
    pass


class MetaChild(MetaBase):
    pass


print(sorted(c.__name__ for c in MetaBase.__subclasses__()))
