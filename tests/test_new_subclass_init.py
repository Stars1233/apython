"""`__init__` runs when `__new__` returns an instance of a SUBCLASS.

type_call decided whether to run `__init__` with a pointer compare against the
class that was called.  CPython's type_call asks PyObject_TypeCheck -- which is
isinstance, not identity -- so the standard factory shape, where `__new__`
picks a subclass and returns `object.__new__` of it, silently skipped
`__init__` here.

pathlib is the ordinary victim: `Path.__new__` returns a PosixPath, so
`PurePath.__init__` never ran and every method afterwards died on `_raw_paths`.

The cases below cover both directions -- a returned instance that IS of the
class, that is of a subclass, that is of an unrelated class, and that is not an
instance at all -- because the pointer compare got two of the four right and a
test that only checks the broken one cannot tell a fix from a stuck answer.
"""


# --- the factory shape ------------------------------------------------------

class Base:
    def __new__(cls, *a):
        return object.__new__(Sub if cls is Base else cls)

    def __init__(self, *a):
        self.args = a


class Sub(Base):
    pass


b = Base(1, 2)
print("Base(1,2) ->", type(b).__name__, getattr(b, "args", "<__init__ NOT run>"))
s = Sub(3)
print("Sub(3)    ->", type(s).__name__, getattr(s, "args", "<__init__ NOT run>"))


# --- three levels deep ------------------------------------------------------

class L1:
    def __new__(cls, *a):
        return object.__new__(L3)

    def __init__(self, *a):
        self.seen = a


class L2(L1):
    pass


class L3(L2):
    pass


x = L1("deep")
print("L1 ->", type(x).__name__, getattr(x, "seen", "<not run>"))


# --- an unrelated class: __init__ must NOT run ------------------------------

class Other:
    def __init__(self):
        self.marked = True


class MakesOther:
    def __new__(cls):
        return object.__new__(Other)

    def __init__(self):
        self.wrong = True


o = MakesOther()
print("unrelated ->", type(o).__name__,
      "wrong" if hasattr(o, "wrong") else "no wrong attr",
      "marked" if hasattr(o, "marked") else "no marked attr")


# --- __new__ returning something that is not an instance at all -------------

class ReturnsInt:
    def __new__(cls, *a):
        return 42

    def __init__(self, *a):
        raise AssertionError("__init__ must not run")


print("returns int ->", repr(ReturnsInt()), type(ReturnsInt()).__name__)


class ReturnsNone:
    def __new__(cls):
        return None

    def __init__(self):
        raise AssertionError("__init__ must not run")


print("returns None ->", repr(ReturnsNone()))


class ReturnsStr:
    def __new__(cls):
        return "not an instance"

    def __init__(self):
        raise AssertionError("__init__ must not run")


print("returns str ->", repr(ReturnsStr()))


# --- the ordinary cases still work ------------------------------------------

class Plain:
    def __init__(self, v):
        self.v = v


print("plain:", Plain(7).v)


class NewAndInit:
    def __new__(cls, v):
        self = object.__new__(cls)
        self.from_new = v
        return self

    def __init__(self, v):
        self.from_init = v * 2


n = NewAndInit(5)
print("both:", n.from_new, n.from_init)


class OnlyNew:
    def __new__(cls, v):
        self = object.__new__(cls)
        self.v = v
        return self


print("only new:", OnlyNew(9).v)


# --- builtin subclasses -----------------------------------------------------

class MyInt(int):
    def __new__(cls, v):
        return int.__new__(cls, v * 2)

    def __init__(self, v):
        self.orig = v


m = MyInt(4)
print("int subclass:", int(m), m.orig, type(m).__name__)


class MyStr(str):
    def __new__(cls, v):
        return str.__new__(cls, v.upper())

    def __init__(self, v):
        self.orig = v


ms = MyStr("hi")
print("str subclass:", str(ms), ms.orig, type(ms).__name__)


class IntFactory(int):
    def __new__(cls, v):
        return int.__new__(SubInt, v)

    def __init__(self, v):
        self.tag = "ran"


class SubInt(IntFactory):
    pass


f = IntFactory(11)
print("int factory:", int(f), type(f).__name__, getattr(f, "tag", "<not run>"))


# --- a metaclass in the middle ----------------------------------------------

class Meta(type):
    pass


class MBase(metaclass=Meta):
    def __new__(cls, *a):
        return object.__new__(MSub if cls is MBase else cls)

    def __init__(self, *a):
        self.args = a


class MSub(MBase):
    pass


mb = MBase("m")
print("metaclass:", type(mb).__name__, getattr(mb, "args", "<not run>"))


# --- __init__ inherited rather than defined on the returned class -----------

class InheritInit:
    def __init__(self, *a):
        self.got = a


class PicksChild(InheritInit):
    def __new__(cls, *a):
        return object.__new__(Child)


class Child(PicksChild):
    pass


p = PicksChild("c")
print("inherited init:", type(p).__name__, getattr(p, "got", "<not run>"))


# --- exceptions keep working ------------------------------------------------

class MyError(ValueError):
    def __init__(self, code):
        super().__init__("code %d" % code)
        self.code = code


try:
    raise MyError(3)
except ValueError as e:
    print("exception:", e, e.code, type(e).__name__)

print("done")
