# __slots__ may name __dict__, and then the instances get one.
#
# A class with __slots__ has no instance dict: that is the point of it, and
# type_from_parts drops the dict word and sets TYPE_FLAG_HAS_SLOTS so that
# instance_setattr refuses anything not in the slots.  But a __slots__ that
# NAMES '__dict__' asks for the dict back -- the slots become fast attributes
# and everything else still works -- and that entry was treated as an ordinary
# slot name: it got a member descriptor pointing at a word of its own, and the
# instances still had no dict, so every other attribute raised.
#
# functools.partial is written exactly that way in CPython's pure-Python
# implementation:
#
#     __slots__ = ("func", "args", "keywords", "__dict__", "__weakref__")
#
# which is why configparser could not set .converter on one, and why
# test_pkgutil and test_pyclbr reported "cannot set attribute".

import sys


class WithDict:
    __slots__ = ("a", "__dict__")

    def __init__(self):
        self.a = 1


w = WithDict()
w.anything = 2
print(w.a, w.anything, "a slot and a dict attribute")
print(w.__dict__, "__dict__ holds only the non-slot names")
print("a" in WithDict.__dict__, "the slot is still a descriptor on the class")
print(sorted(vars(w)), "vars() sees the dict")

w.a = 9
print(w.a, w.__dict__ == {"anything": 2}, "the slot does not leak into the dict")
del w.anything
print(w.__dict__, "and deletion works")


class Both:
    __slots__ = ("x", "__dict__", "__weakref__")


b = Both()
b.x = 1
b.y = 2
print(b.x, b.y, "__dict__ and __weakref__ together")

# _weakref rather than weakref: the latter is not in this tree's lib/.
import _weakref

print(_weakref.ref(b)() is b, "__weakref__ still gives a referenceable instance")


# A plain __slots__ still refuses.
class NoDict:
    __slots__ = ("a",)


n = NoDict()
n.a = 1
try:
    n.b = 2
    print(False, "a plain __slots__ must still refuse")
except AttributeError:
    print(True, "a plain __slots__ still refuses")

try:
    n.__dict__
    print(False, "a plain __slots__ has no __dict__")
except AttributeError:
    print(True, "a plain __slots__ has no __dict__")


# Inheriting: a base that already supplies a dict means the subclass may not
# ask for a second one.
class Plain:
    pass


try:
    class TooMany(Plain):
        __slots__ = ("__dict__",)
    print(False, "a second __dict__ must be refused")
except TypeError as e:
    print(True, "a second __dict__ is refused")


# The shape functools.partial has.
class PartialLike:
    __slots__ = ("func", "args", "keywords", "__dict__", "__weakref__")

    def __init__(self, func):
        self.func = func
        self.args = ()
        self.keywords = {}


p = PartialLike(len)
p.converter = "set on a slotted object"
print(p.func is len, p.converter, "the functools.partial shape works")


# --- a str subclass asks the same way ---------------------------------------
#
# A str subclass keeps its characters inline, so its slots go at the TAIL and
# the layout decision is made on a different branch -- one that never read the
# '__dict__' answer, so `class S(str): __slots__ = ('__dict__',)` still had no
# dict and every attribute raised.
class StrWithDict(str):
    __slots__ = ("a", "__dict__")


s = StrWithDict("text")
s.a = 1
s.z = 3
print(s, s.a, s.z, s.__dict__, "a str subclass gets its dict back")


class StrNoDict(str):
    __slots__ = ("a",)


sn = StrNoDict("text")
sn.a = 1
try:
    sn.z = 3
    print("NO ERROR: a plain str subclass with __slots__ took an attribute")
except AttributeError as e:
    print("AttributeError:", e)


# --- what __slots__ may contain ---------------------------------------------
#
# CPython's valid_identifier: every item must be a str, and every str must be
# an identifier.  Non-strings were silently skipped here, which is worse than
# refusing them -- `__slots__ = (42,)` produced a class whose declared slot did
# not exist -- and the '__dict__' test read PyStrObject.data off whatever
# pointer it was handed, so a bytes item was compared as a string.
def refuse(label, body):
    try:
        body()
        print("%-40s accepted" % label)
    except TypeError as e:
        print("%-40s TypeError: %s" % (label, e))


refuse("an int item", lambda: type("C", (), {"__slots__": (42,)}))
refuse("a bytes item", lambda: type("C", (), {"__slots__": (b"__dict__",)}))
refuse("a None item", lambda: type("C", (), {"__slots__": (None,)}))
refuse("a tuple item", lambda: type("C", (), {"__slots__": (("a",),)}))
refuse("a non-identifier", lambda: type("C", (), {"__slots__": ("a b",)}))
refuse("an empty name", lambda: type("C", (), {"__slots__": ("",)}))
refuse("a digit first", lambda: type("C", (), {"__slots__": ("1a",)}))
refuse("__dict__ twice",
       lambda: type("C", (), {"__slots__": ("__dict__", "__dict__")}))
refuse("__dict__ when a base has one",
       lambda: type("C", (Plain,), {"__slots__": ("__dict__",)}))

# And the things that are still fine.
ok = type("C", (), {"__slots__": ("a", "_b", "__c", "d1", "é")})
print(sorted(n for n in vars(ok) if not n.startswith("__module__")) != [],
      "identifiers, including a non-ASCII one, are accepted")
