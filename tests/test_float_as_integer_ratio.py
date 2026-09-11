# float.as_integer_ratio() is exact for every finite float, not just the ones
# whose numerator happens to fit an int64.
#
# The implementation decomposed the IEEE bits correctly and then built both
# halves in one 64-bit register, so it had three defects at once:
#
#   - anything needing a shift past 62 raised OverflowError, wearing the
#     message meant for inf and NaN.  (1e300), (2.0**70) and Fraction(1e300)
#     all failed that way; 21,998 of CPython's test_statistics errors were
#     this one line.
#   - a subnormal was decoded as if it were normal: the exponent came out one
#     too low and an implicit mantissa bit was set that a subnormal does not
#     have.  Both were invisible only because the range check rejected it
#     first.
#   - and in the band where the shift still fitted the guard but not the
#     register -- 2**63 up to about 2**115 -- it silently WRAPPED.
#     (3.0 * 2**62) is the smallest case.
#
# The numerator and denominator are arbitrary-precision ints, so GMP builds
# them and int_shrink hands back whatever fits.

# `fractions` is not in the in-tree lib/, and `make check` runs without a
# CPython Lib/ on the path, so the Fraction half of this lives in the comment:
# Fraction(x) is `Fraction(*x.as_integer_ratio())`, and Fraction(1e300) was
# unusable for exactly the reason below.


def gcd(a, b):
    while b:
        a, b = b, a % b
    return a


def show(x):
    try:
        n, d = x.as_integer_ratio()
    except OverflowError as e:
        print(repr(x), "-> OverflowError", e)
        return
    except ValueError as e:
        print(repr(x), "-> ValueError", e)
        return
    print(repr(x), "->", n, d)
    # The pair is exact and fully reduced, and the denominator is a power of two.
    assert n / d == x, (x, n, d)
    assert d > 0, (x, n, d)
    assert d & (d - 1) == 0, (x, n, d)
    assert gcd(abs(n), d) == 1, (x, n, d)


# --- the ordinary cases, which already worked ----------------------------
for x in (0.0, -0.0, 1.0, -1.0, 0.5, -0.5, 2.0, 3.0, 0.25, 10.0, 100.0,
          0.1, -0.1, 1.5, 2.5, 1e16, 1e17):
    show(x)

# --- either side of the old 2**62 shift limit ----------------------------
for e in range(58, 70):
    show(float(2 ** e))
    show(-float(2 ** e))

# --- the band that wrapped silently: 2**63 .. 2**115 ---------------------
show(3.0 * 2.0 ** 62)
show(3.0 * 2.0 ** 100)
show(5.0 * 2.0 ** 110)
show(float(2 ** 100 + 2 ** 47))

# --- far out, both directions --------------------------------------------
for x in (1e300, -1e300, 1e100, 2.0 ** 70, 2.0 ** 1023, 1.7976931348623157e308):
    show(x)

# --- subnormals, which were decoded wrongly as well as rejected ----------
show(5e-324)                      # the smallest positive subnormal
show(-5e-324)
show(1e-320)
show(2.2250738585072011e-308)     # the largest subnormal
show(2.2250738585072014e-308)     # the smallest normal
show(2.0 ** -1074)
show(2.0 ** -1073)
show(3.0 * 2.0 ** -1074)
show(1e-200)

# --- inf and NaN still raise, and still say why --------------------------
for x in (float("inf"), float("-inf"), float("nan")):
    show(x)

# --- int and bool answer too ---------------------------------------------
print((7).as_integer_ratio(), (-7).as_integer_ratio(), (0).as_integer_ratio())
print((10 ** 30).as_integer_ratio())
print(True.as_integer_ratio(), False.as_integer_ratio())

# --- a float subclass keeps the value -------------------------------------
class F(float):
    pass


print(F(2.0 ** 70).as_integer_ratio())
print(F(0.5).as_integer_ratio())
print("done")
