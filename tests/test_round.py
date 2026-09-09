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

# --- the exact integer fast path, and its four edges -----------------------
# float_round_ndigits answers 0 <= ndigits <= 22 with integer arithmetic when
# the result numerator fits in 53 bits, and hands everything else to the
# rendering path.  Each of the boundaries is where the two have to agree.

# ndigits at and just past the top of the 5**n table.
print(round(1.2345678901234567, 21), round(1.2345678901234567, 22),
      round(1.2345678901234567, 23))
print(round(1e-20, 22), round(1e-20, 23), round(1e-22, 22))

# A numerator that outgrows 53 bits, so the fast path steps aside: 15, 16 and
# 17 significant digits of the same value.
print(round(1234567890.1234567, 5), round(1234567890.1234567, 6),
      round(1234567890.1234567, 7), round(1234567890.1234567, 8))
print(round(9007199254740992.0, 1), round(9007199254740993.0, 1))

# Small enough that the shift takes everything: the underflow rules and the
# round-up-to-10**-n case behind them.
print(round(1e-30, 2), round(4.9e-3, 2), round(5.0e-3, 2), round(5.1e-3, 2))
print(round(-4.9e-3, 2), round(-5.0e-3, 2), round(-5.1e-3, 2))
print(round(1e-320, 300), round(5e-324, 324), round(5e-324, 323))

# A left shift instead of a right one: e + ndigits >= 0.
print(round(4.0, 0), round(1024.0, 0), round(2.0 ** 52, 0),
      round(2.0 ** 52, 1))

# Ties, which is the whole reason the shift rounds half to even.
print(round(0.125, 2), round(0.375, 2), round(0.625, 2), round(0.875, 2))
print(round(-0.125, 2), round(-0.375, 2))
print(round(2.5, 0), round(3.5, 0), round(0.5, 0), round(1.5, 0))

# The exact value decides, not the shortest decimal that prints for it.
print(round(2.675, 2), round(1.005, 2), round(8.835, 2), round(0.145, 2))
print(round(9.995, 1), round(9.995, 2), round(99.5, -3))

# Subnormals and the largest finite double, at both ends of ndigits.
print(round(2.2250738585072014e-308, 320), round(1.7976931348623157e308, 0))
try:
    round(1.7976931348623157e308, -308)
except OverflowError:
    print("OverflowError")

# --- the errors ---
for f in (lambda: round(), lambda: round(1, 2, 3), lambda: round("x"),
          lambda: round("x", 2), lambda: round(1, "x")):
    try:
        f()
    except TypeError:
        print("TypeError")
