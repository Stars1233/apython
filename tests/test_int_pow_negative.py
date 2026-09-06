# `int ** negative` answers a float, and CPython gets there by converting BOTH
# operands to a double first and running the ordinary float power on them.  So
# the answer depends on how the int became a double:
#
#     (10 ** 30) ** -1     ->  9.999999999999999e-31
#
# We said 1e-30.  int_power's negative-exponent path converted with GMP's
# mpz_get_d, which TRUNCATES toward zero, where PyLong_AsDouble rounds to
# nearest even.  10**30 sits between two doubles and truncation picks the lower
# one; the reciprocal of the wrong neighbour is a different float.
#
# float_to_f64 already had the correct conversion -- it renders the integer to
# a decimal string and lets strtod round it, which is why `float(10**30)` and
# `float(10**30) ** -1.0` were both right while `(10**30) ** -1` was not.
# The path just did not use it.
#
# The boundary is 2**53, above which a double stops holding every integer;
# below it the two conversions agree and nothing was ever wrong.

BASES = [
    2, 3, 7, 10, -2, -3, -7, -10, 1, -1,
    2 ** 20, 2 ** 52, 2 ** 53, 2 ** 53 + 1, 2 ** 53 - 1,
    2 ** 60, -(2 ** 60), 2 ** 62, 2 ** 63, -(2 ** 63), 2 ** 64,
    10 ** 20, 10 ** 30, -(10 ** 30), 10 ** 40, -(10 ** 40),
    3 ** 40, 7 ** 30, 6 ** 25, 123456789012345678901234567890,
]
EXPS = [-1, -2, -3, -4, -5, -7, -10, -20]

for a in BASES:
    for b in EXPS:
        print(a, b, repr(a ** b))

# The same answer must come out of pow(), of the operator, and of the float
# route CPython is really taking.
for a in (10 ** 30, 2 ** 53 + 1, 3 ** 40, -(10 ** 30)):
    for b in (-1, -3):
        print(repr(a ** b), repr(pow(a, b)), repr(float(a) ** float(b)))

# An in-place form goes to the same slot.
def ipow(a, b):
    a **= b
    return a


for a in (10 ** 30, 2 ** 60, 7):
    for b in (-1, -2):
        print(repr(ipow(a, b)))

# The corners the path already handled, which must not move.
print(repr(0.0 ** -1.0) if False else "skip")
try:
    print(0 ** -1)
except ZeroDivisionError:
    print("0 ** -1 -> ZeroDivisionError")
try:
    print(0 ** -(10 ** 20))
except ZeroDivisionError:
    print("0 ** -huge -> ZeroDivisionError")
print(repr(2 ** -1074), repr(2 ** -1075), repr(2 ** -1100))
print(repr((10 ** 30) ** -1000), repr(2 ** -10000))
print(repr((-1) ** -3), repr((-1) ** -4), repr(1 ** -(10 ** 20)))

# A bool is an int here too.
print(repr(True ** -2), repr(True ** -1))

# Three-argument pow() with a negative exponent needs an inverse, not a float.
print(pow(3, -1, 7), pow(10, -1, 1000003))
try:
    pow(2, -1, 4)
except ValueError as exc:
    print("pow(2, -1, 4) -> ValueError")
