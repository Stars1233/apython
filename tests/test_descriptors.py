# Test general descriptor protocol (__get__, __set__, __delete__)

# === Data descriptor (has __get__ and __set__) ===
class Validated:
    def __init__(self, name):
        self.name = name

    def __get__(self, obj, objtype=None):
        if obj is None:
            return self
        return getattr(obj, "_" + self.name)

    def __set__(self, obj, value):
        if value < 0:
            raise ValueError("negative")
        setattr(obj, "_" + self.name, value)

class Point:
    x = Validated("x")
    y = Validated("y")

    def __init__(self, x, y):
        self.x = x
        self.y = y

p = Point(3, 7)
print(p.x)
print(p.y)
p.x = 10
print(p.x)

# === Non-data descriptor (only __get__) ===
class LazyAttr:
    def __init__(self, val):
        self.val = val

    def __get__(self, obj, objtype=None):
        return self.val

class Bag:
    answer = LazyAttr(42)

b = Bag()
print(b.answer)

# === Inherited descriptor ===
class Base:
    x = Validated("x")

class Child(Base):
    def __init__(self, x):
        self.x = x

c = Child(99)
print(c.x)

# builtin_func_repr INCREFd func_name and handed it back, so repr(len) was
# 'len' and repr(int.bit_length) was 'bit_length'.  CPython has three types
# here -- builtin_function_or_method, method_descriptor and
# wrapper_descriptor -- reprd three ways, and the stdlib reads the repr to
# tell them apart.  This tree has one type, so which kind it is and the type
# it belongs to are fields, stamped once per type after its dict is built.
print(repr(len), repr(print), repr(abs))
print(repr(int.bit_length), repr(str.upper), repr(list.append))
print(repr(dict.get), repr(complex.conjugate), repr(int.to_bytes))
print(repr(float.hex), repr(str.join), repr(int.__round__))
print(repr(int.__getnewargs__), repr(float.__floor__), repr(str.__format__))

# The slot-backed dunders are a different kind again.
print(repr(object.__init__), repr(object.__repr__), repr(int.__add__))
print(repr(int.__neg__), repr(str.__len__), repr(list.__len__))
print(repr(list.__iter__), repr(str.__iter__), repr(dict.__len__))

# Two names go both ways: dict and set answer __contains__ from a real
# method, list answers __getitem__ from one, and str, bytes and tuple answer
# both from a slot.
print(repr(dict.__contains__), repr(set.__contains__))
print(repr(frozenset.__contains__), repr(list.__contains__))
print(repr(str.__contains__), repr(tuple.__contains__))
print(repr(dict.__getitem__), repr(list.__getitem__))
print(repr(str.__getitem__), repr(tuple.__getitem__))

# Calling them unbound is unchanged.
print(int.bit_length(5), str.upper("a"), len([1, 2]), list.__len__([1, 2]))


# --- a staticmethod object is itself callable (3.10+) ------------------------
#
# staticmethod_type.tp_call was 0, so `staticmethod(f)(x)` raised TypeError and
# callable(staticmethod(f)) was False.  The wrapped value is a Value and need
# not be a pointer -- staticmethod(1) is legal to build -- so calling one has
# to report the int the way calling the int directly would.

def _sm(label, fn):
    try:
        print("%-34s %r" % (label, fn()))
    except BaseException as e:
        print("%-34s !! %s: %s" % (label, type(e).__name__, str(e)[:44]))


_sm("callable(sm(len))", lambda: callable(staticmethod(len)))
_sm("sm(len)([1, 2])", lambda: staticmethod(len)([1, 2]))
_sm("sm(len)(*[[1, 2, 3]])", lambda: staticmethod(len)(*[[1, 2, 3]]))
_sm("sm(pow)(2, 10)", lambda: staticmethod(pow)(2, 10))
_sm("sm(sorted) with kwargs", lambda: staticmethod(sorted)([3, 1], reverse=True))
_sm("sm(abs) as a key=", lambda: sorted([-2, 1], key=staticmethod(abs)))
_sm("min(key=sm(abs))", lambda: min([-2, 1], key=staticmethod(abs)))
_sm("sm(len).__func__", lambda: staticmethod(len).__func__ is len)
_sm("callable(classmethod(len))", lambda: callable(classmethod(len)))

for _bad in (1, 1.5, None, [], "x", object()):
    _sm("sm(%s)()" % type(_bad).__name__, (lambda b: lambda: staticmethod(b)())(_bad))
    _sm("callable(sm(%s))" % type(_bad).__name__,
        (lambda b: lambda: callable(staticmethod(b)))(_bad))


class _Static:
    @staticmethod
    def f(a, b=2):
        return (a, b)


print(_Static.f(1), _Static().f(1, 3), _Static.__dict__["f"](9))
