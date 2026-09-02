# round() rounded with cvtsd2si, which answers the integer INDEFINITE value
# -- 0x8000000000000000 -- for anything outside int64 and reports nothing.
# round(1e300), round(float('inf')) and round(float('nan')) were all
# -9223372036854775808.  float_int had already learned this for int(); round()
# had not.
#
# The two-argument arm had three more of its own.  It computed 10**ndigits in
# an int64, which wraps at ndigits >= 19 and loops 400 times for
# round(x, 400); it multiplied x by that, which overflows to infinity for a
# large x; and the cvtsd2si/cvtsi2sd round trip lost the sign of -0.0.
# round(10**30, -5) was "type cannot be rounded" outright, because the arm
# only accepted an int that fits an immediate.
#
# And it rounded the SCALED BINARY VALUE where CPython rounds the DECIMAL
# representation, so round(2.675, 2) was 2.68 against CPython's 2.67.

# --- one argument ---
print(round(5), round(-5), round(0))
print(round(1.5), round(2.5), round(3.5), round(0.5), round(-0.5), round(-2.5))
print(round(2.675), round(-2.675))

# Outside int64.  The exact integer, not the indefinite value.
print(round(1e300) == int(1e300))
print(round(1e300))
print(round(-1e300) == -int(1e300))
print(round(2.0 ** 70), round(-(2.0 ** 70)))
print(round(10 ** 30), round(-(10 ** 30)))
print(round(2 ** 70 + 55))

for bad in (float('inf'), float('-inf'), float('nan')):
    try:
        round(bad)
    except (OverflowError, ValueError) as e:
        print(type(e).__name__, e)

# A float subclass, and a class with __round__ of its own.
class F(float):
    pass


class R:
    def __round__(self, n=None):
        return ("R", n)


print(round(F(2.5)), round(F(-0.5)))
print(round(R()), round(R(), 3))

# --- two arguments, floats ---
print(round(2.675, 2), round(1.005, 2), round(0.125, 2), round(2.345, 2))
print(round(1.234, 2), round(1.235, 2), round(1.0, 0))
print(round(9.995, 1), round(9.995, 2), round(9.995, -2))
print(round(99.5, 0), round(99.5, -3), round(0.6, 0), round(0.5, 0))
print(round(123.456, -1), round(123.456, -2), round(123.456, -3))

# Large magnitudes: the scaling used to overflow and answer the indefinite
# value through the same cvtsd2si.
print(round(1e300, -1), round(1e300, 0), round(1e300, 2), round(1e300, 20))
print(round(1e18, 1), round(1e17, 2), round(1e16, 20), round(5e15, 20))
print(round(-1e300, 2), round(-1e300, -1))

# Past either bound the answer is settled without looking at the digits.
print(round(1e300, 400), round(1e300, -400), round(-1e300, -400))
print(round(0.0, 400), round(-0.0, 400), round(0.0, -400))

# -0.0 keeps its sign.  A cvtsd2si/cvtsi2sd round trip did not.
print(round(-0.0, 1), round(-0.0, -1), round(-0.0, 0), round(-0.0))
import math
print(math.copysign(1.0, round(-0.0, 1)), math.copysign(1.0, round(-0.2, 0)))

# Non-finite is its own answer with an explicit ndigits, and raises without.
for bad in (float('inf'), float('-inf'), float('nan')):
    print(repr(round(bad, 2)), repr(round(bad, -2)))

# A rounded value that no longer fits a double.
try:
    round(1.7976931348623157e308, -308)
except OverflowError as e:
    print("OverflowError", e)

# --- two arguments, ints ---
print(round(42, 2), round(1234, -2), round(1250, -2), round(1350, -2))
print(round(-15, -1), round(-16, -1), round(15, -1), round(25, -1), round(-25, -1))
print(round(10 ** 30, -5), round(-(10 ** 30), -5))
print(round(10 ** 30 + 55555, -5), round(10 ** 18 + 5, -1))
print(round(2 ** 70 + 55, -2), round(999999, -3))
print(round(12345, 100), round(-12345, 100))

# An ndigits too large for an int64 either way: only its sign survives.
# round(anInt, -(10**20)) is not here: CPython computes 10**(10**20) for it
# and does not come back, so there is no oracle to diff against.
print(round(12345, 10 ** 20), round(1.5, 10 ** 20))
print(round(1.5, -(10 ** 20)), round(-1.5, -(10 ** 20)))

# --- the errors ---
for f in (lambda: round(), lambda: round(1, 2, 3), lambda: round("x"),
          lambda: round("x", 2), lambda: round(1, "x")):
    try:
        f()
    except TypeError:
        print("TypeError")
