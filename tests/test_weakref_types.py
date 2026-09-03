# Which types can be weakly referenced.
#
# CPython refuses a weak reference to most builtins: weakref.ref([]) is a
# TypeError, not a reference.  That is what a zero tp_weaklistoffset means,
# and the refusal is load-bearing -- WeakValueDictionary depends on it to
# reject a value whose death it could never observe.  Every one of them was
# accepted here, so the mistake surfaced much later and somewhere else, as a
# dictionary that quietly never dropped anything.
#
# The links here live in a side table keyed by the referent's address rather
# than in a per-object slot, so there is no offset to test; the answer comes
# from the type instead.  The rule is CPython's: a class is given the word
# unless it declares __slots__ without naming __weakref__, or its layout base
# keeps its value inline and variable-sized.
import _weakref


def can(v):
    try:
        _weakref.ref(v)
        return "yes"
    except TypeError as e:
        return "no: " + str(e)


def show(label, v):
    print("%-24s %s" % (label, can(v)))


# --- the builtins CPython refuses
show("int", 1)
show("big int", 1 << 200)
show("float", 1.5)
show("bool", True)
show("str", "abc")
show("bytes", b"abc")
show("bytearray", bytearray(b"abc"))
show("tuple", (1, 2))
show("list", [])
show("dict", {})
show("NoneType", None)
show("object", object())
show("range", range(3))
show("slice", slice(1))
show("Ellipsis", Ellipsis)
show("NotImplemented", NotImplemented)
show("list_iterator", iter([]))
show("tuple_iterator", iter(()))
show("dict_keys", {}.keys())
show("enumerate", enumerate([]))
show("zip", zip())
show("map", map(str, []))
show("filter", filter(None, []))
show("property", property())
show("classmethod", classmethod(lambda self: None))
show("staticmethod", staticmethod(lambda: None))
show("exception instance", ValueError("x"))
show("BaseException", BaseException())

# --- the ones it allows
show("set", set())
show("frozenset", frozenset())
show("type", int)
show("module", _weakref)
show("builtin function", len)
show("memoryview", memoryview(b"a"))


def f():
    pass


show("function", f)
show("lambda", lambda: 0)
show("code", f.__code__)
show("generator", (x for x in [1]))


# --- classes and their instances
class C:
    def m(self):
        pass


class Slots:
    __slots__ = ('a',)


class SlotsWeak:
    __slots__ = ('a', '__weakref__')


class SlotsEmpty:
    __slots__ = ()


class UnderSlots(SlotsEmpty):
    pass


class UnderSlotsSlots(SlotsEmpty):
    __slots__ = ()


show("class", C)
show("instance", C())
show("bound method", C().m)
show("__slots__ instance", Slots())
show("__slots__ + __weakref__", SlotsWeak())
show("__slots__ = ()", SlotsEmpty())
show("under a slots base", UnderSlots())
show("slots under slots", UnderSlotsSlots())


# --- a subclass follows its layout base
class SubInt(int):
    pass


class SubTuple(tuple):
    pass


class SubBytes(bytes):
    pass


class SubList(list):
    pass


class SubDict(dict):
    pass


class SubStr(str):
    pass


class SubSet(set):
    pass


class SubExc(Exception):
    pass


class SubListSlots(list):
    __slots__ = ('a',)


show("int subclass", SubInt())
show("tuple subclass", SubTuple())
show("bytes subclass", SubBytes())
show("list subclass", SubList())
show("dict subclass", SubDict())
show("str subclass", SubStr())
show("set subclass", SubSet())
show("Exception subclass", SubExc())
show("list subclass + slots", SubListSlots())


# --- what a reference to an allowed type still does
def alive_and_dead():
    class T:
        pass

    t = T()
    r = _weakref.ref(t)
    got = r() is t
    del t
    return got, r() is None


print("alive then dead =>", alive_and_dead())


def callback_runs():
    class T:
        pass

    fired = []
    t = T()
    r = _weakref.ref(t, lambda ref: fired.append(1))
    del t
    return fired, r() is None


print("callback =>", callback_runs())

# proxy refuses the same types, and accepts the same ones.
for _v in ([], (1,), 1, "a", {}, None):
    try:
        _weakref.proxy(_v)
        print("proxy accepted", type(_v).__name__)
    except TypeError as e:
        print("proxy %-10s no: %s" % (type(_v).__name__, e))


def proxy_ok():
    class T:
        x = 5

    t = T()
    p = _weakref.proxy(t)
    return p.x


print("proxy of instance =>", proxy_ok())

# getweakrefcount on something that can never have one is still 0, not an
# error -- CPython answers 0 there too.
print("count(list) =>", _weakref.getweakrefcount([]))
print("count(int) =>", _weakref.getweakrefcount(1))

print("done")
