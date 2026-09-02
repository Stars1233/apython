# float() of a large int rounds to nearest even, and an int too wide for a
# double compares against one exactly.
#
# float_to_f64 converted a GMP-backed int with mpz_get_d, which truncates
# toward zero: float(10**30) was 9.999999999999999e+29 where CPython's
# PyLong_AsDouble rounds to nearest and gives 1e+30.
#
# Fixing that exposed the other half.  Comparison converted both sides to
# doubles, so `10**30 == 1e30` answered by comparing two rounded values --
# True, where CPython compares the int and the float exactly and says False.
# It had been False before only because the truncation happened to fall the
# other way.

for v in (10**30, -(10**30), 2**70, 2**53, 2**53 + 1, 2**53 + 2, 10**100,
          -(10**100), 2**64 - 1, 12345678901234567890123456789, 3**80, 7**60,
          2**1023, 0, 1, -1, 255):
    print(v, repr(float(v)))

print(float(10**30) == 1e30, float(2**70) == 2.0**70)
print(complex(10**30), complex(2**70))

# The comparisons, both ways round and for every operator.
cases = [(10**30, 1e30), (10**30, 1e31), (10**30, 1e29), (2**53, 2.0**53),
         (2**53 + 1, 2.0**53), (2**70, 2.0**70), (2**70 + 1, 2.0**70),
         (-(10**30), -1e30), (10**30, float("inf")), (10**30, float("-inf")),
         (5, 5.0), (5, 5.5), (-5, -5.5), (2**60, 1.15e18),
         (10**30, 9.999999999999999e+29), (2**53 + 1, 2.0**53 + 2.0)]
for a, b in cases:
    print(a == b, a < b, a > b, a <= b, a >= b, a != b, b == a, b < a, b > a)

print(sorted([10**30, 1e30, 10**30 + 1]))
print(float("nan") == 10**30, float("nan") < 10**30, float("nan") != 10**30)
print(min(10**30, 1e30), max(10**30, 1e30))

# Small ints are exact in a double and take the ordinary path.
print(3 == 3.0, 3 < 3.5, -2 > -2.5, 0 == 0.0, 0 == -0.0)
print(True == 1.0, False < 0.5)

# The arithmetic that goes through the same conversion.
print(10**30 + 0.0, (10**30) * 1.0, (10**30) / 1e30)
