# The types CPython refuses to let you subclass -- the ones without
# Py_TPFLAGS_BASETYPE.  All four were accepted here, and the instances that
# came out were not obviously wrong, merely something CPython would not have
# let you make.
#
# The flag is the inverse of CPython's on purpose: everything here is
# subclassable unless it says otherwise, so this is four type tables rather
# than an audit of ninety-four.

import sys

FINAL = [bool, range, memoryview, slice]
OPEN = [object, int, float, str, bytes, bytearray, list, tuple, dict, set,
        frozenset, complex, type, BaseException, Exception]


def attempt(base):
    try:
        type("X", (base,), {})
        return "subclassable"
    except TypeError as e:
        return "TypeError: %s" % e


for t in FINAL + OPEN:
    print(t.__name__, attempt(t))

# The class statement says the same thing as the three-argument type().
try:
    class B(bool):
        pass
except TypeError as e:
    print("class:", e)

# And a final type among several bases is caught wherever it sits.
try:
    class M(int, memoryview):
        pass
except TypeError as e:
    print("mixed:", e)


class Mixin:
    pass


try:
    class N(Mixin, range):
        pass
except TypeError as e:
    print("second:", e)

# The types themselves still work.
print(bool(1), bool(0), range(3)[1], slice(1, 5, 2).step)
print(bytes(memoryview(b"abc")))
