# A __slots__ descriptor is a data descriptor, and the stdlib asks it so BY
# NAME: inspect.getattr_static, inspect.isdatadescriptor and anything walking
# type.__dict__ test hasattr(v, '__get__').  member_descriptor published none
# of the three, nor __name__ or __objclass__, so every one of those answered
# 'member_descriptor' object has no attribute '__get__'.  (inspect is not
# importable here, so the classifier is spelled out the way it writes it.)
#
# And `del o.x` on a slot that was never assigned SUCCEEDED, where CPython
# raises AttributeError naming the slot -- so deleting twice worked and the
# attribute's absence could not be told from its presence.

class C:
    __slots__ = ("x", "y")


class D(C):
    __slots__ = ("z",)


d = C.__dict__["x"]
print(type(d).__name__)
for n in ("__get__", "__set__", "__delete__", "__objclass__", "__name__",
          "__qualname__"):
    print(n, hasattr(d, n))
print(d.__name__, d.__qualname__, d.__objclass__ is C)
print(repr(d))

o = C()
d.__set__(o, 5)
print(o.x, d.__get__(o), d.__get__(o, C))
d.__delete__(o)
try:
    d.__get__(o)
except AttributeError as e:
    print("get after delete:", e)
try:
    d.__delete__(o)
except AttributeError as e:
    print("delete twice:", e)

# descr.__get__(None, cls) is the descriptor itself.
print(d.__get__(None, C) is d)
try:
    d.__get__(None)
except TypeError as e:
    print("get(None):", e)

# It is a DATA descriptor, which is what inspect.isdatadescriptor asks, and
# not a method descriptor, which is __get__ without __set__.
print(hasattr(d, "__set__") or hasattr(d, "__delete__"))
print(hasattr(d, "__get__") and not hasattr(d, "__set__"))
# getattr_static walks the MRO's __dict__ and hands back what it finds.
print(type(C()).__mro__[0].__dict__["x"] is d)

# A subclass's instance is a valid receiver; an unrelated one is not.
sub = D()
d.__set__(sub, 7)
print(d.__get__(sub))
for bad in (1, "s", object()):
    try:
        d.__get__(bad)
    except TypeError as e:
        print("get receiver:", e)
    try:
        d.__set__(bad, 1)
    except TypeError as e:
        print("set receiver:", e)

# Arity.
for args in ((), (o, 1, 2, 3)):
    try:
        d.__get__(*args)
    except TypeError:
        print("get arity refused", len(args))
for args in ((o,), (o, 1, 2)):
    try:
        d.__set__(*args)
    except TypeError:
        print("set arity refused", len(args))
try:
    d.__delete__()
except TypeError:
    print("delete arity refused")

# The statement forms, which are what the descriptor exists for.
o2 = C()
try:
    del o2.x
except AttributeError as e:
    print("del unset:", e)
o2.x = 1
del o2.x
try:
    del o2.x
except AttributeError as e:
    print("del twice:", e)
try:
    o2.x
except AttributeError as e:
    print("read unset:", e)

# A str subclass keeps its slots at the TAIL, addressed by a negative
# offset; the same three names have to work there.
class S(str):
    __slots__ = ("tag",)


sd = S.__dict__["tag"]
s = S("hi")
sd.__set__(s, "T")
print(s, sd.__get__(s), s.tag)
sd.__delete__(s)
try:
    sd.__get__(s)
except AttributeError as e:
    print("str slot:", e)

print("done")
