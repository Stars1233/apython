# operator.index, which is PyNumber_Index and not `a.__index__()`.
#
# CPython's operator.py defines a pure-Python fallback and then overwrites it
# with the C one from _operator.  Here there was no _operator.index, so the
# fallback stood -- and it is `return a.__index__()`, which reports a float as
# an AttributeError.  `random.randrange(0, 42, 0.0)` calls it on each of its
# arguments, so the error a program sees for a float step was AttributeError
# where CPython raises TypeError.
#
# The other half PyNumber_Index does and the fallback does not: it insists
# the result really is an int.
import operator


class WithIndex:
    def __index__(self):
        return 7


class BadIndex:
    def __index__(self):
        return "seven"


class NoIndex:
    pass


class Raises:
    def __index__(self):
        raise ZeroDivisionError("from __index__")


print(operator.index(5), operator.index(-5), operator.index(0))
print(operator.index(True), operator.index(False))
print(operator.index(WithIndex()))
print("int subclass:", operator.index(2 ** 70) == 2 ** 70)

for bad in (1.5, 0.0, "3", None, [1], (1,), b"1", NoIndex()):
    try:
        operator.index(bad)
        print("accepted %r" % (bad,))
    except TypeError as e:
        print("%-14s %s" % (type(bad).__name__, e))

try:
    operator.index(BadIndex())
except TypeError as e:
    print("non-int result:", e)

# An exception from __index__ itself is passed through untouched.
try:
    operator.index(Raises())
except ZeroDivisionError as e:
    print("passed through:", e)

# __index__ found on the TYPE, not the instance -- as every special method is.
class Shadow:
    def __init__(self):
        self.__index__ = lambda: 99


try:
    operator.index(Shadow())
except TypeError as e:
    print("instance attribute ignored:", type(e).__name__)

# The place it actually bit, written out: random.randrange runs each of its
# three arguments through this, and `random` itself is not in the shipped
# lib/, so the call is reproduced rather than imported.
from operator import index as _index

for args in ((0, 42, 0.0), (0.0, 42), (0, 42.5)):
    try:
        [_index(a) for a in args]
        print("randrange would accept %r" % (args,))
    except TypeError:
        print("randrange refuses %r with TypeError" % (args,))

print("done")
