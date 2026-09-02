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
