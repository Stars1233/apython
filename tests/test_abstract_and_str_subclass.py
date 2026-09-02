# Two entries bugs.md carried that no longer reproduce.
#
# "@abc.abstractmethod is not enforced" -- it is: type_call consults
# __abstractmethods__ and abc's bookkeeping fills it.  Only the message
# differs, which is why this file compares the exception type rather than its
# text.
#
# "A str subclass has no instance __dict__" -- it has one.  A str keeps its
# characters inline, so the dict cannot sit at a fixed offset past the header;
# TP_DICT_AT_TAIL puts it after the data instead, and bytes and tuple
# subclasses get theirs the same way.

import abc


class Base(abc.ABC):
    @abc.abstractmethod
    def f(self):
        ...

    @abc.abstractmethod
    def g(self):
        ...


class Half(Base):
    def f(self):
        return "f"


class Full(Half):
    def g(self):
        return "g"


print(sorted(Base.__abstractmethods__), sorted(Half.__abstractmethods__))
print(Full.__abstractmethods__ == frozenset())
for cls in (Base, Half):
    try:
        cls()
        print(cls.__name__, "instantiated")
    except TypeError:
        print(cls.__name__, "=> TypeError")
o = Full()
print(o.f(), o.g(), isinstance(o, Base), issubclass(Full, Base))


# A subclass of a variable-size builtin carries an instance dict.
class S(str):
    pass


class B(bytes):
    pass


class T(tuple):
    pass


s = S("hello")
s.x = 1
s.y = [2]
print(s, len(s), s.upper(), s.x, s.y, sorted(s.__dict__))
print(s + "!", s[1:], "ell" in s, S("a") < S("b"))

b = B(b"hi")
b.tag = "t"
print(b, len(b), b.tag, sorted(b.__dict__), b + b"!")

t = T((1, 2))
t.z = 3
print(t, len(t), t.z, sorted(t.__dict__), t + (3,))

# The data and the dict do not tread on each other, whatever the length.
for n in (0, 1, 7, 8, 9, 64, 200):
    v = S("a" * n)
    v.n = n
    if len(v) != n or v.n != n or v != "a" * n:
        print("clash at", n)
print("no clash")

# (An empty __slots__ on a str subclass still gets a dict here, where CPython
# suppresses it.  Recorded in bugs.md; it is not what this file is about.)
