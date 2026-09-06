# Test float methods

# is_integer
print((1.0).is_integer())     # True
print((1.5).is_integer())     # False
print((0.0).is_integer())     # True
print((-2.0).is_integer())    # True
print((3.14).is_integer())    # False

# conjugate
print((3.14).conjugate())     # 3.14
print((-2.5).conjugate())     # -2.5

# as_integer_ratio
print((0.5).as_integer_ratio())    # (1, 2)
print((1.5).as_integer_ratio())    # (3, 2)
print((2.0).as_integer_ratio())    # (2, 1)
print((0.0).as_integer_ratio())    # (0, 1)
print((-0.5).as_integer_ratio())   # (-1, 2)

# hex
print((0.0).hex())            # 0x0.0p+0
print((1.0).hex())            # 0x1.0000000000000p+0
print((-1.0).hex())           # -0x1.0000000000000p+0
print((0.5).hex())            # 0x1.0000000000000p-1
print((2.0).hex())            # 0x1.0000000000000p+1

# The four dir(float) was short of.  __floor__ and __ceil__ have to do exactly
# what math.floor and math.ceil already did natively: registering them newly
# routes a float SUBCLASS instance through the dunder, because MATH_ROUNDER's
# native arm reaches only an immediate.
print((2.5).__round__(), (2.675).__round__(2), (-0.5).__round__())
print((2.7).__floor__(), (2.7).__ceil__(), (-2.7).__floor__(), (-2.7).__ceil__())
print((2.5).__getnewargs__(), (-0.0).__getnewargs__())
print((1e300).__floor__() == int(1e300), (1e300).__ceil__() == int(1e300))
print((1e300).__round__() == int(1e300))

for bad in (float("inf"), float("nan")):
    for name in ("__floor__", "__ceil__", "__round__"):
        try:
            getattr(bad, name)()
        except (OverflowError, ValueError) as e:
            print(name, type(e).__name__, e)


class F(float):
    pass


print(F(2.5).__getnewargs__(), type(F(2.5).__getnewargs__()[0]).__name__)
print(F(2.7).__floor__(), F(2.7).__ceil__(), F(2.5).__round__())
print(F(2.5).is_integer(), F(2.0).is_integer(), F(2.5).as_integer_ratio())

import math
print(math.floor(F(2.7)), math.ceil(F(2.7)), math.trunc(F(2.7)))
print(math.floor(2.7), math.ceil(2.7), math.floor(10 ** 30 + 1))
print(round(F(2.5)), round(F(2.675), 2))

print(sorted(set(["is_integer", "as_integer_ratio", "__round__", "__ceil__",
                  "__floor__", "__getnewargs__", "__trunc__"]) - set(dir(float))))


# --- float.__getformat__ ------------------------------------------------------
#
# CPython declares it METH_O|METH_CLASS and documents it as being for its own
# test suite -- which is exactly what wanted it.  test.support reads it at
# import time, and every one of CPython's 406 test modules imports
# test.support, so this one missing method kept the whole suite out of reach.

def _gf(label, fn):
    try:
        print("%-40s %r" % (label, fn()))
    except BaseException as e:
        print("%-40s !! %s: %s" % (label, type(e).__name__, str(e)[:60]))


_gf("float.__getformat__('double')", lambda: float.__getformat__("double"))
_gf("float.__getformat__('float')", lambda: float.__getformat__("float"))
_gf("(1.5).__getformat__('double')", lambda: (1.5).__getformat__("double"))
_gf("startswith IEEE", lambda: float.__getformat__("double").startswith("IEEE"))
_gf("bad name", lambda: float.__getformat__("quad"))
_gf("empty name", lambda: float.__getformat__(""))
_gf("int argument", lambda: float.__getformat__(1))
_gf("None argument", lambda: float.__getformat__(None))
_gf("float argument", lambda: float.__getformat__(1.5))
_gf("no argument", lambda: float.__getformat__())
_gf("two arguments", lambda: float.__getformat__("double", "float"))
_gf("str subclass", lambda: float.__getformat__(type("S", (str,), {})("double")))
_gf("in dir(float)", lambda: "__getformat__" in dir(float))
_gf("bound off an instance", lambda: (2.5).__getformat__("float"))
