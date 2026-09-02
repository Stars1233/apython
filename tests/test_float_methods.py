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
