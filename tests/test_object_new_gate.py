"""`object.__new__` must refuse a type whose allocation it does not own.

CPython's gate is `tp_new_wrapper`'s staticbase walk: climb `tp_base` past
every heap type -- those are the ones whose `tp_new` is `slot_tp_new` -- and
refuse when the static type that walk lands on has a `tp_new` that is not the
one being called through.  So `object.__new__(list)` raises, and so does
`object.__new__(C)` for a `C` that inherits from list, however many pure-Python
classes sit in between.

This is not a wording bug.  `object.__new__(str)`, `(float)` and `(bytes)`
aborted the process with glibc's "double free or corruption": object's
allocator builds a header-sized object, and str, float and bytes keep their
payload inline, so the type's own dealloc freed storage that was never there.
The three that abort are checked here first, and separately from the ones that
merely answered the wrong value.

`copyreg._reconstructor` is the ordinary caller, which is why this matters
beyond the direct call: `copy.copy` and `pickle` reach it for any class with
a builtin base.
"""


print("--- the three that used to abort ---")
for t in (str, float, bytes):
    try:
        object.__new__(t)
        print(t.__name__, "-> accepted, wrong")
    except TypeError as e:
        print(t.__name__, "->", e)


print("--- the ones that answered a wrong value ---")
for t in (list, dict, int, tuple, set, frozenset, bytearray, complex):
    try:
        object.__new__(t)
        print(t.__name__, "-> accepted, wrong")
    except TypeError as e:
        print(t.__name__, "->", e)


print("--- what must still be allowed ---")
print("object:", type(object.__new__(object)).__name__)


class Plain:
    pass


print("plain class:", type(object.__new__(Plain)).__name__)


class WithInit:
    def __init__(self):
        self.x = 1


print("with __init__:", type(object.__new__(WithInit)).__name__)


class WithNew:
    def __new__(cls):
        return object.__new__(cls)


print("with __new__:", type(WithNew()).__name__)


class Slotted:
    __slots__ = ("a",)


print("with __slots__:", type(object.__new__(Slotted)).__name__)


class Deep(Plain):
    pass


class Deeper(Deep):
    pass


print("deep hierarchy:", type(object.__new__(Deeper)).__name__)


class MultiPlain(Plain, WithInit):
    pass


print("multiple plain bases:", type(object.__new__(MultiPlain)).__name__)


print("--- a subclass of a builtin is refused too ---")


class MyList(list):
    pass


class MyListDeeper(MyList):
    pass


class MyStr(str):
    pass


class MyInt(int):
    pass


class MyDict(dict):
    pass


for t in (MyList, MyListDeeper, MyStr, MyInt, MyDict):
    try:
        object.__new__(t)
        print(t.__name__, "-> accepted, wrong")
    except TypeError as e:
        print(t.__name__, "->", e)


print("--- but the type's own __new__ still works ---")
print("list:", list.__new__(list))
print("str:", repr(str.__new__(str)))
print("int:", int.__new__(int))
print("MyList:", MyList.__new__(MyList))
print("MyStr:", repr(MyStr.__new__(MyStr)))


print("--- exceptions are their own family ---")
for t in (BaseException, Exception, ValueError):
    try:
        r = object.__new__(t)
        print(t.__name__, "-> ok:", type(r).__name__)
    except TypeError as e:
        print(t.__name__, "->", e)


print("--- argument counts ---")
try:
    object.__new__()
except TypeError as e:
    print("no args:", type(e).__name__)

try:
    object.__new__(1)
except TypeError as e:
    print("not a type:", type(e).__name__)


# object.__new__ takes extra args only when __init__ is overridden and
# __new__ is not.  Plain has neither, so extras are refused.
try:
    object.__new__(Plain, 1, 2)
    print("extras on a plain class: accepted")
except TypeError as e:
    print("extras on a plain class:", type(e).__name__)

# WithInit overrides __init__ and not __new__, so extras are tolerated.
print("extras with __init__:", type(object.__new__(WithInit, 1, 2)).__name__)

print("done")
