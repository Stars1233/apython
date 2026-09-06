# `<<` and `**` used to reach GMP for every operand, however small.
# int_lshift converted the left operand to a heap int and allocated a result
# plus an mpz to compute `1 << 3`; int_power did the same for `2 ** 3`.  malloc
# and free were 27% of a shift loop and 32% of a power loop.  int_rshift had
# had its `sar` arm all along, which is why only half of `i << 3 >> 2` was slow.
#
# Both now have an int64 arm that declines to the old GMP path when the answer
# will not fit.  The shift checks by shifting arithmetically back and comparing
# -- bits that fall off the top do not come back, and the shift back is `sar`
# so a negative left operand works too.  The power squares repeatedly with a
# checked imul at every step, and squares only while another exponent bit
# remains, so an overflow in a squaring whose value would never be used cannot
# push a result that fitted onto the slow path.
#
# What has to keep working is every boundary either arm can decline at, so the
# values below straddle +-2^50 (where an immediate becomes a heap int),
# +-2^63 (where the heap int needs GMP) and the shift counts either side of 63.

VALUES = [
    0, 1, 2, 3, 7, -1, -2, -7, 255, -255,
    2 ** 30, -(2 ** 30), 2 ** 49, -(2 ** 49), 2 ** 50, 2 ** 50 - 1,
    2 ** 62, -(2 ** 62), 2 ** 63, -(2 ** 63), 2 ** 64, 10 ** 30, -(10 ** 30),
]
SHIFTS = [0, 1, 2, 3, 7, 13, 31, 32, 49, 50, 51, 62, 63, 64, 65, 100, 200]


def lsh(a, b):
    return a << b


def rsh(a, b):
    return a >> b


def power(a, b):
    return a ** b


out = []
for a in VALUES:
    for b in SHIFTS:
        out.append("%d << %d = %d" % (a, b, lsh(a, b)))
        out.append("%d >> %d = %d" % (a, b, rsh(a, b)))

BASES = [0, 1, -1, 2, -2, 3, -3, 7, 10, -10, 255, -255,
         2 ** 20, -(2 ** 20), 2 ** 31, 2 ** 49, -(2 ** 49), 2 ** 50,
         2 ** 62, -(2 ** 62), 10 ** 20, -(10 ** 20)]
EXPS = [0, 1, 2, 3, 4, 5, 6, 7, 10, 20, 31, 32, 40, 62, 63, 64, 65, 100]

for a in BASES:
    for b in EXPS:
        out.append("%d ** %d = %d" % (a, b, power(a, b)))

# A negative exponent still answers a float, and does not go near the new arm.
for a in (2, -2, 3, 10, 7, 2 ** 60):
    for b in (-1, -2, -3):
        out.append("%d ** %d = %r" % (a, b, power(a, b)))

# Three-argument pow() is modular and untouched.
for a, b, m in ((2, 10, 1000), (3, 100, 7), (123456789, 12345, 1000003),
                (2, 0, 5), (-3, 5, 7), (2, 62, 2 ** 61), (10, 40, 10 ** 9)):
    out.append("pow(%d, %d, %d) = %d" % (a, b, m, pow(a, b, m)))

print("\n".join(out))

# --- the errors each one raises --------------------------------------------
for a, b in ((1, -1), (2 ** 60, -3), (0, -1)):
    try:
        lsh(a, b)
        print("no error for %d << %d" % (a, b))
    except ValueError as exc:
        print("%d << %d -> ValueError: %s" % (a, b, exc))
    try:
        rsh(a, b)
        print("no error for %d >> %d" % (a, b))
    except ValueError as exc:
        print("%d >> %d -> ValueError: %s" % (a, b, exc))

try:
    power(0, -1)
except ZeroDivisionError as exc:
    print("0 ** -1 -> ZeroDivisionError")

# --- the results are still ordinary integers -------------------------------
vals = [power(2, 60) + 1, lsh(1, 60) + 1, power(3, 40), lsh(-1, 55)]
for v in vals:
    print(v, repr(v), v + 1, v - 1, v * 2, v // 3, v % 7, -v, abs(v),
          hash(v) == hash(v), bool(v), str(v) == repr(v), int(str(v)) == v)
print(sorted(vals))
print(sorted([power(2, 60) + 1, 1.5, 3, 2.5, float(power(2, 60) + 1)]))
