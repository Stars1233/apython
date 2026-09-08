"""`type.__flags__` reports CPython's Py_TPFLAGS_* bits.

tp_flags cannot be handed back raw: the low 32 bits are this tree's own layout
and the high 32 are the type version, so `__flags__` translates.

It is deliberately NOT the whole of CPython's word.  Some bits have no
counterpart here at all -- MANAGED_DICT and MANAGED_WEAKREF above all, whose
absence is a recorded divergence, and MATCH_SELF, SEQUENCE, MAPPING,
HAVE_VECTORCALL and ITEMS_AT_END, which no flag stands for.  So this file
asserts the bits that DO mean the same thing rather than the whole value; a
test written as `int.__flags__ == <cpython's>` would be asserting the
divergence, not the translation.  DIVERGENCES.md carries the list.
"""

HEAPTYPE = 1 << 9
BASETYPE = 1 << 10
READY = 1 << 12
HAVE_GC = 1 << 14
IMMUTABLETYPE = 1 << 8
VALID_VERSION_TAG = 1 << 19
LONG_SUBCLASS = 1 << 24
LIST_SUBCLASS = 1 << 25
TUPLE_SUBCLASS = 1 << 26
BYTES_SUBCLASS = 1 << 27
UNICODE_SUBCLASS = 1 << 28
DICT_SUBCLASS = 1 << 29
BASE_EXC_SUBCLASS = 1 << 30
TYPE_SUBCLASS = 1 << 31

SUBCLASS_BITS = [
    ("LONG", LONG_SUBCLASS), ("LIST", LIST_SUBCLASS),
    ("TUPLE", TUPLE_SUBCLASS), ("BYTES", BYTES_SUBCLASS),
    ("UNICODE", UNICODE_SUBCLASS), ("DICT", DICT_SUBCLASS),
    ("BASE_EXC", BASE_EXC_SUBCLASS), ("TYPE", TYPE_SUBCLASS),
]


def subclass_bits(t):
    f = t.__flags__
    return [name for name, bit in SUBCLASS_BITS if f & bit]


print("--- it exists and is an int ---")
print("type:", type(int.__flags__).__name__)
print("readable on the metatype:", type(type.__flags__).__name__)

print("--- the subclass bits ---")
for t in (object, int, str, list, tuple, dict, set, frozenset, bytes,
          bytearray, float, complex, type, BaseException, Exception,
          ValueError, bool):
    print("%-14s %s" % (t.__name__, subclass_bits(t)))

print("--- and they are inherited ---")


class MyInt(int):
    pass


class MyStr(str):
    pass


class MyList(list):
    pass


class MyDict(dict):
    pass


class MyErr(ValueError):
    pass


class MyMeta(type):
    pass


for t in (MyInt, MyStr, MyList, MyDict, MyErr, MyMeta):
    print("%-8s %s" % (t.__name__, subclass_bits(t)))

print("--- a plain class has none ---")


class Plain:
    pass


print("plain:", subclass_bits(Plain))

print("--- HEAPTYPE tells a class from a builtin ---")
for t in (object, int, str, list, type, BaseException):
    print("%-14s heap=%s" % (t.__name__, bool(t.__flags__ & HEAPTYPE)))
for t in (Plain, MyInt, MyErr, MyMeta):
    print("%-8s heap=%s" % (t.__name__, bool(t.__flags__ & HEAPTYPE)))

print("--- IMMUTABLETYPE is the other side of the same question ---")
print("builtins immutable:",
      all(t.__flags__ & IMMUTABLETYPE for t in (object, int, str, list, type)))
print("classes are not:",
      not any(t.__flags__ & IMMUTABLETYPE for t in (Plain, MyInt, MyErr)))

print("--- BASETYPE says whether it can be subclassed ---")
for t in (object, int, str, list, dict, type, BaseException, Plain):
    print("%-14s base=%s" % (t.__name__, bool(t.__flags__ & BASETYPE)))

# Whatever BASETYPE says, subclassing must agree with it.
for t in (object, int, str, list, dict, tuple, set, frozenset, bytes,
          bytearray, float, complex, type, BaseException, Plain):
    claimed = bool(t.__flags__ & BASETYPE)
    try:
        type("X", (t,), {})
        actual = True
    except TypeError:
        actual = False
    if claimed != actual:
        print("DISAGREES:", t.__name__, "flag", claimed, "actual", actual)
print("BASETYPE agrees with reality")

print("--- READY is on everything that can be asked ---")
print("ready:", all(t.__flags__ & READY
                    for t in (object, int, str, type, Plain, MyInt)))

print("--- a bool is an int subclass ---")
print("bool:", subclass_bits(bool), bool.__flags__ & LONG_SUBCLASS != 0)

print("--- every exception carries BASE_EXC ---")
for t in (BaseException, Exception, ValueError, KeyError, StopIteration,
          OSError, RecursionError, SystemExit, KeyboardInterrupt):
    if not t.__flags__ & BASE_EXC_SUBCLASS:
        print("MISSING BASE_EXC:", t.__name__)
print("all exceptions carry it")
for t in (object, int, str, list, Plain):
    if t.__flags__ & BASE_EXC_SUBCLASS:
        print("WRONGLY BASE_EXC:", t.__name__)
print("nothing else does")

print("--- every metaclass carries TYPE ---")
print("type:", bool(type.__flags__ & TYPE_SUBCLASS))
print("MyMeta:", bool(MyMeta.__flags__ & TYPE_SUBCLASS))
print("not a plain class:", bool(Plain.__flags__ & TYPE_SUBCLASS))

print("--- it refuses a non-type ---")
try:
    type.__dict__["__flags__"].__get__(1)
    print("accepted - wrong")
except TypeError as e:
    print("non-type:", type(e).__name__)

print("done")
