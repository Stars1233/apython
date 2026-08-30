# A class could only ever have one base: `class C(A, B)` stored A and dropped
# B silently, so C().b() was an AttributeError and isinstance(C(), B) was
# False.  Every base is recorded now, and lookups and subclass tests follow
# the C3 linearization.


class A:
    def a(self): return "A.a"
    def who(self): return "A"


class B:
    def b(self): return "B.b"
    def who(self): return "B"


class C(A, B):
    pass


class D(B, A):
    pass


c = C()
print(c.a(), c.b(), c.who(), D().who())
print([t.__name__ for t in C.__mro__])
print(C.__bases__ == (A, B), D.__bases__ == (B, A))
print(isinstance(c, A), isinstance(c, B), isinstance(c, C), isinstance(c, D))
print(issubclass(C, A), issubclass(C, B), issubclass(C, D))
print(isinstance(c, (D, B)), issubclass(C, (D, B)))


# Diamond: super() follows the instance's MRO, not the defining class's
class Base:
    def m(self): return ["Base"]


class L(Base):
    def m(self): return ["L"] + super().m()


class R(Base):
    def m(self): return ["R"] + super().m()


class Dia(L, R):
    def m(self): return ["Dia"] + super().m()


print(Dia().m(), [t.__name__ for t in Dia.__mro__])
print(L().m(), R().m())


# An inconsistent hierarchy is refused
def build():
    class X(L, Dia):
        pass


try:
    build()
except TypeError:
    print("inconsistent MRO refused")


# A mixin over a builtin base
class Mix:
    def shout(self): return "!" + str(self)


class MyList(Mix, list):
    pass


ml = MyList([1, 2])
ml.append(3)
print(ml, ml.shout(), len(ml), ml[0], isinstance(ml, list), type(ml).__name__)


class MyDict(Mix, dict):
    pass


md = MyDict()
md["k"] = 1
print(md, md.shout(), md["k"], isinstance(md, dict))


# Dunders reached through a secondary base
class Sized:
    def __len__(self): return 3


class HasIter:
    def __iter__(self): return iter([1, 2, 3])


class Both(Sized, HasIter):
    pass


bo = Both()
print(len(bo), list(bo), [x * 2 for x in bo], bool(bo))


# Exceptions with several bases
class AppError(Exception):
    pass


class IOish:
    def where(self): return "io"


class FileError(AppError, IOish):
    pass


try:
    raise FileError("f")
except AppError as e:
    print("caught", type(e).__name__, e.where(), e.args)

print(issubclass(FileError, Exception), isinstance(FileError("x"), AppError))


# A method inherited from an exception's base is bound, not raw
class WithMethod(Exception):
    def describe(self): return "described " + str(self)


class Deeper(WithMethod):
    pass


print(Deeper("d").describe())


# ExceptionGroup is a BaseExceptionGroup and an Exception at once
eg = ExceptionGroup("g", [ValueError("a"), TypeError("b")])
print(isinstance(eg, Exception), isinstance(eg, BaseException))
try:
    raise eg
except Exception as e:
    print("caught group", type(e).__name__, len(e.exceptions))


# dir() sees every branch
class M1:
    def am(self): pass


class M2:
    def bm(self): pass


class M3(M1, M2):
    def cm(self): pass


print(sorted(x for x in dir(M3()) if x.endswith("m")))


# Three parents, first match wins
class P1:
    def f(self): return 1


class P2:
    def f(self): return 2


class P3:
    def g(self): return 3


class Q(P1, P2, P3):
    pass


print(Q().f(), Q().g(), [t.__name__ for t in Q.__mro__])


# A property on a secondary base still runs on assignment
class PropMix:
    @property
    def v(self): return self._v

    @v.setter
    def v(self, x): self._v = x * 2


class Plain:
    pass


class Uses(PropMix, Plain):
    pass


u = Uses()
u.v = 5
print(u.v)


# type() with three arguments takes the same path
T = type("T", (A, B), {"extra": lambda self: "extra"})
t = T()
print(t.a(), t.b(), t.extra(), [x.__name__ for x in T.__mro__])
