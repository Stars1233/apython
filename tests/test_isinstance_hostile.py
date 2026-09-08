"""isinstance() and issubclass() over every argument shape they can be handed.

builtin_isinstance keeps one owned reference in a frame slot -- the object's
declared __class__, when that differs from its real type -- and releases it on
the way out.  The slot was written well after the point where the "this Value
is neither a pointer, an int nor a float" arm left for the exit that releases
it, so that arm decref'd whatever the machine stack happened to hold.

What it held, once, was a pointer into a live code object's bytecode.  The
single decrement turned a RETURN_VALUE (83) into an 82, and the eval loop
stopped on an opcode CPython 3.12 does not assign -- thousands of instructions
away from isinstance, in a different function, with nothing to connect them.

There is no Python expression that reaches that arm directly, so this file
cannot reproduce the crash.  What it can do is walk every arm of both builtins
so that the slot's lifetime is exercised in both directions -- taken and not
taken, released and not released -- against CPython's answers.
"""


class Plain:
    pass


class Sub(Plain):
    pass


class Meta(type):
    pass


class WithMeta(metaclass=Meta):
    pass


class Spoofed:
    @property
    def __class__(self):
        return Plain


class SpoofedBad:
    @property
    def __class__(self):
        return 42


class SpoofedRaises:
    @property
    def __class__(self):
        raise RuntimeError("no class for you")


values = [
    0, 1, -1, 7, -7,
    2 ** 50, -2 ** 50, 2 ** 50 + 1, -2 ** 50 - 1, 2 ** 200,
    0.0, -0.0, 1.5, float("inf"), float("-inf"),
    True, False, None,
    "", "abc", b"", b"abc", bytearray(b"x"),
    (), (1,), [], [1], {}, {"a": 1}, set(), frozenset(),
    range(3), slice(1), Ellipsis, NotImplemented,
    Plain, Sub, Plain(), Sub(), WithMeta, WithMeta(),
    int, type, object, len, str.upper,
]

types_to_try = [
    int, float, bool, str, bytes, bytearray, list, tuple, dict, set,
    frozenset, type, object, range, slice, Plain, Sub, Meta,
    (int, str), (float,), (), ((int,), (str,)),
]

print("--- isinstance over every shape ---")
for v in values:
    row = []
    for t in types_to_try:
        try:
            row.append("1" if isinstance(v, t) else "0")
        except Exception as e:
            row.append(type(e).__name__[:4])
    print(type(v).__name__.ljust(12) if not callable(v) else "callable".ljust(12), "".join(row))

print("--- issubclass over every class ---")
classes = [Plain, Sub, Meta, WithMeta, int, bool, float, str, bytes,
           list, tuple, dict, set, frozenset, type, object, BaseException,
           Exception, ValueError]
for c in classes:
    row = []
    for t in types_to_try:
        try:
            row.append("1" if issubclass(c, t) else "0")
        except Exception as e:
            row.append(type(e).__name__[:4])
    print(c.__name__.ljust(14), "".join(row))


print("--- the second argument that is not a class ---")
for bad in (1, 1.5, "abc", None, [int], {"a": int}, object(), (1,), (int, 1)):
    try:
        isinstance(1, bad)
        print("isinstance ok  ", repr(bad)[:20])
    except TypeError as e:
        print("isinstance TypeError:", e)
    try:
        issubclass(int, bad)
        print("issubclass ok  ", repr(bad)[:20])
    except TypeError as e:
        print("issubclass TypeError:", e)


print("--- a declared __class__ ---")
s = Spoofed()
print("spoofed is Plain:", isinstance(s, Plain))
print("spoofed is Sub:", isinstance(s, Sub))
print("spoofed is Spoofed:", isinstance(s, Spoofed))
print("spoofed in tuple:", isinstance(s, (str, Plain)))
print("spoofed misses:", isinstance(s, (str, int)))

# The declared class is asked once and released once; asking many times in a
# row is where an unbalanced release shows up as a refcount going wrong.
print("repeated:", sum(1 for _ in range(500) if isinstance(s, Plain)))

b = SpoofedBad()
print("bad __class__ still uses the real type:", isinstance(b, SpoofedBad))
print("bad __class__ is not Plain:", isinstance(b, Plain))

r = SpoofedRaises()
try:
    isinstance(r, Plain)
    print("raising __class__: no error")
except RuntimeError as e:
    print("raising __class__:", e)
print("raising __class__ against its own type:", isinstance(r, SpoofedRaises))


print("--- unions ---")
print(isinstance(1, int | str), isinstance("a", int | str),
      isinstance(1.5, int | str), isinstance(None, int | None))
print(issubclass(bool, int | str), issubclass(float, int | str))
try:
    isinstance(1, list[int])
except TypeError as e:
    print("parameterized generic:", e)


print("--- nesting and arity ---")
print(isinstance(1, ((((int,),),),)))
print(isinstance("a", ((int,), (str, bytes))))
for call in (lambda: isinstance(1),
             lambda: isinstance(1, int, int),
             lambda: issubclass(int),
             lambda: issubclass(int, int, int)):
    try:
        call()
        print("no error - wrong")
    except TypeError as e:
        print("arity:", e)

print("done")
