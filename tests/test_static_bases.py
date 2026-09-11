# A static type's __bases__ is (object,), not ().
#
# Static type tables leave tp_base at 0 and let the MRO walk supply the object
# that anchors the end -- type_mro_next, type_mro_len and type_mro_fill each
# substitute it.  __bases__ and __base__ read tp_base raw and did not, so every
# builtin reported no bases at all while reporting a two-entry __mro__.
#
# Nothing in the language notices until something walks __bases__ itself, and
# then it is not subtle: functools._c3_mro(str) answered [str], _find_impl
# answered None, and singledispatch raised "'NoneType' object is not callable"
# on its very first call -- before anything had been registered.
import functools

for t in (object, type, int, float, complex, bool, str, bytes, bytearray,
          memoryview, list, tuple, dict, set, frozenset, range, slice,
          property, staticmethod, classmethod, super, enumerate, zip, map,
          filter, reversed, BaseException, Exception, ValueError, OSError):
    print("%-14s bases=%r base=%r" % (t.__name__, t.__bases__, t.__base__))

# The MRO agreed all along; the point is that the two now agree with each other.
for t in (str, int, dict, ValueError):
    print(t.__name__, t.__bases__ == t.__mro__[1:2], len(t.__mro__))

# A subclass of a builtin, and a class with no bases written down.
class S(str):
    pass

class P:
    pass

print("S", S.__bases__, S.__base__)
print("P", P.__bases__, P.__base__)
print("multiple", type("M", (S, P), {}).__bases__)

# The tuple compares equal across types even though it is not the same object.
print("equal:", str.__bases__ == int.__bases__)

# What the whole thing was costing: functools' own C3, which walks __bases__
# and not __mro__, and the single-dispatch lookup built on it.  singledispatch
# itself is not exercised here because it wants weakref, and this tree ships no
# lib/weakref.py -- _find_impl is the part that was answering None.
print("_c3_mro(str):", functools._c3_mro(str))
print("_c3_mro(object):", functools._c3_mro(object))
print("_c3_mro(bool):", functools._c3_mro(bool))


class Sub(int):
    pass


registry = {object: "object", int: "int", str: "str"}
for v in (3, "a", 1.5, [], Sub(7), True, b"z"):
    print("%-8r -> %s" % (v, functools._find_impl(type(v), registry)))
