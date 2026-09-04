"""types.GenericAlias as a constructor, and `X[...]` on a class with a metaclass.

Two halves of the same failure.  `os.PathLike` is written

    __class_getitem__ = classmethod(GenericAlias)

so `os.PathLike[str]` needs GenericAlias to be callable as a type -- it had no
tp_new, so the call fell through to ordinary class construction, allocated a
GC-headed block, left both fields holding whatever was there, and freed it at
the wrong address -- and it needs `X[...]` to recognise a class whose metatype
is a metaclass of its own, which it did by comparing against the two metatypes
this tree ships.
"""

from types import GenericAlias
import abc

print("--- the constructor ---")
print(GenericAlias(list, str))
print(GenericAlias(dict, (str, int)))
g = GenericAlias(list, int)
print(g.__origin__, g.__args__, type(g).__name__)
print(g == list[int], hash(g) == hash(list[int]))
for bad in ((), (list,), (list, str, int)):
    try:
        GenericAlias(*bad)
        print("accepted", len(bad))
    except TypeError:
        print("TypeError for %d arguments" % len(bad))

print("--- __class_getitem__ as a classmethod ---")


class Plain:
    __class_getitem__ = classmethod(GenericAlias)


class Meta(type):
    pass


class WithMeta(metaclass=Meta):
    __class_getitem__ = classmethod(GenericAlias)


class Abstract(abc.ABC):
    __class_getitem__ = classmethod(GenericAlias)


class Written(abc.ABC):
    def __class_getitem__(cls, item):
        return GenericAlias(cls, item)


for cls in (Plain, WithMeta, Abstract, Written):
    a = cls[str]
    print(cls.__name__, a, a.__origin__ is cls, a.__args__)


class Inherits(WithMeta):
    pass


print(Inherits[int], Inherits[int].__origin__ is Inherits)

print("--- and the repr is qualified the way CPython's is ---")
print(list[int], dict[str, int], tuple[int, ...], list[list[int]])
print(list[Plain], list[Abstract], frozenset[WithMeta])
print(type(list[int])(list, (str,)))

print("--- something with no __class_getitem__ at all ---")
#
# CPython names what was subscripted, and names a class differently from an
# instance, which is how a program tells a missing __class_getitem__ from a
# missing __getitem__.
for cls in (Meta("Bare", (), {}), type("AlsoBare", (), {}), int, object,
            Exception):
    try:
        cls[int]
        print("accepted")
    except TypeError as e:
        print("TypeError", e)
# ("x".upper is left out: this tree has one bound-method type where
# CPython has three, so type(...).__name__ differs by design.)
for value in (1, 1.5, None, object(), True, len, print, Ellipsis):
    try:
        value[0]
    except TypeError as e:
        print("TypeError", e)

print("--- a getset descriptor knows its own name ---")
d = type.__dict__["__mro__"]
print(type(d).__name__, d.__name__, d.__qualname__, d.__objclass__ is type)
print(repr(d))
print(int.__dict__["real"].__name__)

print("--- a bound method's two attributes ---")


class Owner:
    def method(self, a, b=1):
        return a + b


o = Owner()
print(o.method.__self__ is o, o.method.__func__ is Owner.method)
print(o.method.__func__.__name__, o.method.__name__)
print("x".upper.__self__)

print("--- time's zone globals ---")
import time

print(len(time.tzname), all(isinstance(n, str) for n in time.tzname))
print(isinstance(time.timezone, int), isinstance(time.altzone, int))
print(time.daylight in (0, 1))
print(time.altzone == time.timezone - (3600 if time.daylight else 0))
