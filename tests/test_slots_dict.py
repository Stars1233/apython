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
