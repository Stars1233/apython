# __dict__ and __weakref__ in a class's own dict.
#
# type_new puts them there and type_from_parts did not, so sorted(C.__dict__)
# was ['__doc__', '__module__'] against CPython's
# ['__dict__', '__doc__', '__module__', '__weakref__'].
#
# The rule is not "always add two".  CPython's type_new_descriptors adds the
# __dict__ getset only when THIS class contributes the instance dict
# (base->tp_dictoffset == 0) and the __weakref__ getset only when it contributes
# weak-referenceability (base->tp_weaklistoffset == 0 and the base is not
# variable-size).  So an int subclass gets __dict__ and not __weakref__, a list
# subclass gets both, and an Exception subclass gets __weakref__ and not
# __dict__ -- BaseException already has a dict, and weakref.ref(Exception()) is
# a TypeError even in CPython.
#
# inspect.getattr_static is the consumer that makes a real getset mandatory: its
# _shadowed_dict requires type(C.__dict__['__dict__']) to be
# types.GetSetDescriptorType with a matching __name__ and __objclass__, and
# treats anything else as a dict that SHADOWS the real one.


def keys(C):
    return sorted(C.__dict__)


class Plain:
    pass


class Documented:
    "a docstring"


class SlotsEmpty:
    __slots__ = ()


class SlotsNamed:
    __slots__ = ("a",)


class SlotsDict:
    __slots__ = ("__dict__",)


class SlotsWeak:
    __slots__ = ("__weakref__",)


class SlotsBoth:
    __slots__ = ("__dict__", "__weakref__")


class SubOfPlain(Plain):
    pass


class SubOfPlainSlots(Plain):
    __slots__ = ()


class SubOfSlots(SlotsEmpty):
    pass


class SubOfSlotsSlots(SlotsEmpty):
    __slots__ = ()


class SubOfSlotsWeakOnly(SlotsEmpty):
    __slots__ = ("__weakref__",)


class SubInt(int):
    pass


class SubStr(str):
    pass


class SubBytes(bytes):
    pass


class SubTuple(tuple):
    pass


class SubList(list):
    pass


class SubDict(dict):
    pass


class SubFloat(float):
    pass


class SubSet(set):
    pass


class SubExc(Exception):
    pass


class SubBaseExc(BaseException):
    pass


class SubType(type):
    pass


class OtherPlain:
    pass


class TwoPlainBases(Plain, OtherPlain):
    pass


print("--- which classes get which entries ---")
for name, C in (("plain", Plain), ("docstring", Documented),
                ("slots ()", SlotsEmpty), ("slots (a,)", SlotsNamed),
                ("slots (__dict__,)", SlotsDict),
                ("slots (__weakref__,)", SlotsWeak),
                ("slots (both,)", SlotsBoth),
                ("sub of plain", SubOfPlain),
                ("sub of plain + slots", SubOfPlainSlots),
                ("sub of slotted", SubOfSlots),
                ("sub of slotted + slots", SubOfSlotsSlots),
                ("sub of slotted + weak", SubOfSlotsWeakOnly),
                ("sub of int", SubInt), ("sub of str", SubStr),
                ("sub of bytes", SubBytes), ("sub of tuple", SubTuple),
                ("sub of list", SubList), ("sub of dict", SubDict),
                ("sub of float", SubFloat), ("sub of set", SubSet),
                ("sub of Exception", SubExc),
                ("sub of BaseException", SubBaseExc),
                ("sub of type", SubType),
                ("two plain bases", TwoPlainBases)):
    print("%-24s %s" % (name, keys(C)))

print("--- three-argument type() agrees ---")
print("plain :", keys(type("T", (), {})))
print("slots :", keys(type("T", (), {"__slots__": ()})))
print("sub   :", keys(type("T", (Plain,), {})))
print("int   :", keys(type("T", (int,), {})))

print("--- the __dict__ entry is a getset for the INSTANCE dict ---")
d = Plain.__dict__["__dict__"]
print("type:", type(d).__name__)
print("name:", d.__name__)
print("objclass is Plain:", d.__objclass__ is Plain)
p = Plain()
print("get is the instance dict:", d.__get__(p) is p.__dict__)
p.x = 1
print("sees a write:", d.__get__(p) == {"x": 1})
print("and the class's own dict is unaffected:", "x" not in Plain.__dict__)

print("--- __weakref__ reads the slot ---")
w = Plain.__dict__["__weakref__"]
print("type:", type(w).__name__)
print("name:", w.__name__)
print("objclass is Plain:", w.__objclass__ is Plain)
q = Plain()
print("none yet:", q.__weakref__ is None, w.__get__(q) is None)

# CPython answers the HEAD of the reference list, which is the callback-free
# one; the ones with callbacks go after it.  And a reference that DIES leaves a
# hole here -- ref_clear zeroes its slot in place rather than removing it -- so
# reading entry zero answered a NULL, which the getset wrapper turns into
# "attribute is not readable" instead of None.
import gc
import _weakref

a = Plain()
r = _weakref.ref(a)
print("one basic:", a.__weakref__ is r)
del r
gc.collect()
print("after it dies:", repr(a.__weakref__))

b = Plain()
rb = _weakref.ref(b)
rc = _weakref.ref(b, lambda x: None)
print("basic beats a callback:", b.__weakref__ is rb)

c = Plain()
cc = _weakref.ref(c, lambda x: None)
print("callback only:", c.__weakref__ is cc)
cb = _weakref.ref(c)
print("a basic added later still wins:", c.__weakref__ is cb)

print("--- ordinary attribute access is unchanged ---")


class Ordinary:
    def __init__(self):
        self.v = 1

    def m(self):
        return self.v * 2


o = Ordinary()
o.extra = 5
print("reads:", o.v, o.m(), o.extra, sorted(o.__dict__))
o.v = 9
print("writes:", o.v, o.m())


class WithProperty:
    @property
    def p(self):
        return "prop"

    @p.setter
    def p(self, v):
        self.stored = v


wp = WithProperty()
wp.p = "set"
print("property still wins:", wp.p, wp.stored)


class WithSlots:
    __slots__ = ("s",)


ws = WithSlots()
ws.s = 3
print("slots still work:", ws.s)
try:
    ws.nope = 1
    print("slots accepted a stray attribute")
except AttributeError:
    print("slots still refuse a stray attribute")

print("--- vars() and dir() see them ---")
print("vars == __dict__:", sorted(vars(Plain)) == keys(Plain))
print("in dir:", "__dict__" in dir(Plain), "__weakref__" in dir(Plain))

print("--- a subclass does not repeat its base's ---")
print("SubOfPlain has neither:", "__dict__" not in SubOfPlain.__dict__,
      "__weakref__" not in SubOfPlain.__dict__)
print("but still reads them:", SubOfPlain().__dict__ == {},
      SubOfPlain().__weakref__ is None)

print("--- the descriptor a __slots__ name keeps is its own ---")
print("slots (__weakref__,) kind:",
      type(SlotsWeak.__dict__["__weakref__"]).__name__ in
      ("getset_descriptor", "member_descriptor"))

print("done")
