# Cases a review of this branch found.  Each was a crash or a wrong answer on
# ordinary Python that the suite did not cover.


def err(fn, *a):
    try:
        return fn(*a)
    except Exception as e:
        return type(e).__name__


# dict.popitem() skipped an occupied slot 0 and reported an empty dict
print({0: "a"}.popitem(), {1: 2}.popitem())
d = {1: "a", 2: "b", 0: "c"}
out = []
while d:
    out.append(d.popitem())
print(sorted(out))

# sorted() inside an except block re-raised the exception being handled
try:
    raise ValueError("boom")
except ValueError:
    print(sorted([2, 1]), sorted({3, 1, 2}), sorted("cba"))


class BadIter:
    def __iter__(self): return self
    def __next__(self): raise KeyError("k")


print(err(sorted, BadIter()))


# A dunder on a builtin subclass used to patch the builtin's own type
class MyInt(int):
    def __neg__(self): return "neg"


print(-(2 ** 60), -MyInt(3), -5)


class MyList(list):
    def __bool__(self): return False


print(bool([1, 2, 3]), bool(MyList([1])), len(MyList([1, 2])))


# Assigning through a non-int subscript
a = [1, 2, 3]
print(err(a.__setitem__, 1.5, 9), err(a.__setitem__, "x", 9))
a[True] = 9
a[0:2] = [5, 6]
print(a)


# raise ... from
try:
    raise TypeError("b") from None
except TypeError as e:
    print("from None:", e.__cause__, e.__suppress_context__)

try:
    try:
        raise ValueError("a")
    except ValueError as v:
        raise TypeError("b") from v
except TypeError as e:
    print("from v:", type(e.__cause__).__name__, e.__suppress_context__)

try:
    raise KeyError("k")
except KeyError as e:
    print("plain:", e.__cause__, e.__suppress_context__)


# A str subclass instance has no slots to walk on the way out
class S(str):
    pass


for i in range(20):
    x = S("oqda%d" % i)
    hash(x)
    del x
print("str subclass dealloc ok")


# object is in every linearization
class D(dict):
    pass


print(isinstance(D(), object), issubclass(D, object), D.__mro__[-1] is object)
print(isinstance(1, object), isinstance("a", object), int.__mro__[-1] is object)


# A base must be a class
def bad_base():
    class C(1):
        pass


print(err(bad_base))


# A bool is an int wherever a float is compared
print(True < 2.5, 1.0 == True, 1.0 in {True}, {1.0: "y"}[True], 2.5 > True)
print(sorted([1.5, True, 0.5, False]))


# A generator that raises after a yield
def g():
    yield 1
    raise ValueError("boom")


it = g()
print(it.send(None), err(it.send, None))


def g2():
    yield 1
    raise ValueError("boom")


print(err(lambda: [x for x in g2()]))


# print() with a long sep or end
print("a", "b", sep="X" * 300)
print("c", end="Y" * 300)
print()


# split/rsplit keep the remainder's whitespace once maxsplit runs out
print(" a b ".split(None, 1), " a b ".rsplit(None, 1))
print(" a  ".split(None, 0), "  a ".rsplit(None, 0), "   ".split(None, 1))
print("a b c".split(None, 2), " a  b  c ".split(None, 1))

# splitlines with keepends over CRLF
for s in ("a\nb\r\nc\n", "a\r\nb", "\r\n", "a\rb", "", "a\n\nb"):
    print(repr(s), s.splitlines(), s.splitlines(True))


# %-formatting pads between the sign and the digits
print("%05d" % -42, "%05.1f" % -4.2, "%+06d" % -42, "%05d" % 42)
print("%05s" % "a", "%-5d|" % -42, "%05x" % 255, "%#07x" % 255)


# Float precision beyond two digits
print(format(1.5, ".1f"), format(1.5, ".15f"), format(1.5, ".100f")[:22])

# A numeric type letter needs a number
print(err(format, "abc", "f"), err(format, "abc", "d"), format("abc", ">5") + "|")


# f-string conversions apply before the format spec
x = "A"
print(f"{x!r}", f"{x!s}", f"{x!a}", f"{x!r:>6}", f"{x:>6}")
print(f"{5!r:>5}", f"{[1, 2]!r:>10}")


# reversed() on the ordinary __len__ + __getitem__ class
class Seq:
    def __len__(self): return 3
    def __getitem__(self, i): return i * 10


print(list(reversed(Seq())))


# An exception raised inside __format__ reaches the caller
class BadFormat:
    def __format__(self, spec): raise KeyError("boom")


print(err(format, BadFormat(), ""), err(format, BadFormat(), ">5"))
try:
    "%s" % 1
    x = f"{BadFormat()}"
except KeyError:
    print("f-string propagated")
try:
    x = f"{BadFormat():>5}"
except KeyError:
    print("f-string with spec propagated")


# An exception's instance attribute wins over a class attribute
class WithDefault(Exception):
    x = 1


w = WithDefault("m")
print(w.x)
w.x = 2
print(w.x, WithDefault.x)


# ord() decodes a multi-byte character
for c in (65, 127, 128, 233, 0x20AC, 0x1F600):
    print(c, ord(chr(c)))
print(err(ord, "ab"), err(ord, ""), err(ord, 5), ord("A"), chr(65))
