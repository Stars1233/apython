# %-formatting has to refuse an argument its conversion cannot use.
#
# Every numeric conversion in str_mod formatted whatever it was given, so
# "%d" % "x" answered 'x' and "%i" % [] answered '[]' -- a wrong answer with
# no hint that anything was wrong.

GOOD = [
    ("%d", 5), ("%d", -5), ("%i", 7), ("%u", 7), ("%d", True), ("%d", 3.9),
    ("%d", -3.9), ("%x", 255), ("%X", 255), ("%o", 8), ("%e", 1.5),
    ("%f", 1.5), ("%g", 1.5), ("%s", "x"), ("%s", 5), ("%s", [1]),
    ("%r", "x"), ("%a", "é"), ("%c", 65), ("%c", "A"), ("%%", ()),
    ("%5d", 42), ("%-5d|", 42), ("%05.1f", 3.14159), ("%+d", 5),
]
for fmt, arg in GOOD:
    if arg == ():
        print(repr(fmt), "->", repr(fmt % ()))
    else:
        print(repr(fmt), repr(arg), "->", repr(fmt % (arg,)))

print("=== the ones that must raise ===")
BAD = [
    ("%d", "x"), ("%i", []), ("%u", {}), ("%x", "x"), ("%X", None),
    ("%o", "8"), ("%e", "x"), ("%f", []), ("%g", None), ("%d", None),
    ("%d", (1, 2)), ("%x", 1.5), ("%o", 2.5), ("%c", "ab"), ("%c", []),
    ("%d", object()),
]
for fmt, arg in BAD:
    try:
        r = fmt % (arg,)
        print(repr(fmt), type(arg).__name__, "-> NO ERROR", repr(r))
    except TypeError as e:
        print(repr(fmt), type(arg).__name__, "-> TypeError:", e)
    except ValueError as e:
        print(repr(fmt), type(arg).__name__, "-> ValueError:", e)
    except OverflowError as e:
        print(repr(fmt), type(arg).__name__, "-> OverflowError:", e)

print("=== an __index__ is enough for the integer conversions ===")
class Idx:
    def __index__(self):
        return 12

for fmt in ("%d", "%i", "%x", "%X", "%o", "%u"):
    try:
        print(repr(fmt), "->", repr(fmt % (Idx(),)))
    except TypeError as e:
        print(repr(fmt), "-> TypeError:", e)

print("=== a __float__ is enough for the float conversions ===")
class Flt:
    def __float__(self):
        return 2.5

for fmt in ("%e", "%f", "%g"):
    try:
        print(repr(fmt), "->", repr(fmt % (Flt(),)))
    except TypeError as e:
        print(repr(fmt), "-> TypeError:", e)

print("=== an int subclass still works ===")
class MyInt(int):
    pass

print("%d %x %o" % (MyInt(10), MyInt(255), MyInt(8)))

print("=== mappings and tuples ===")
print("%(a)d/%(b)s" % {"a": 1, "b": "x"})
try:
    print("%(a)d" % {"a": "x"})
except TypeError as e:
    print("TypeError:", e)


# A BARE %d -- no flags, no width, no precision -- skips the format-spec
# machinery and calls int.__str__ directly.  The type check is the whole
# reason %d was routed through that machinery in the first place, so the
# fast arm has to make it too: everything that is not an EXACT int puts the
# argument back and goes the long way.
class I(int):
    def __str__(self):
        return "I!"


for v in (0, 1, -1, 42, 2 ** 50, -(2 ** 50), 2 ** 63, -(2 ** 63), 10 ** 30,
          True, False, I(7)):
    print(repr(v), "%d" % (v,), "%i" % (v,), "%u" % (v,), "%s-%d" % ("a", v))

for bad in ("x", 1.5, None, [1], (1,), {1: 2}, object):
    for f in ("%d", "%i", "%u"):
        try:
            print(f, repr(bad), repr(f % (bad,)))
        except TypeError as e:
            print(f, repr(bad), "TypeError", e)

# and the arm must not eat an argument it declines, nor mis-count the rest
print("%d %s %d" % (1, "x", 2))
print("%s %d %s %d" % ("a", 1, "b", 2))
try:
    print("%d %d" % (1,))
except TypeError as e:
    print("TypeError", e)
try:
    print("%d" % (1, 2))
except TypeError as e:
    print("TypeError", e)
print("%(k)d" % {"k": 9}, "%(k)d-%(k)d" % {"k": 9})
print("%5d|%-5d|%05d|%+d" % (42, 42, 42, 42))
