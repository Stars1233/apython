# sum() of floats uses compensated summation, as CPython's has since 3.12
# (gh-100425).  Without it the running total silently loses every bit that
# falls off the bottom, and for a list with a wide dynamic range the discarded
# part IS the answer: sum([1e100, 1.0, -1e100]) came out 0.0 here where CPython
# says 1.0.  The 1.0 is lost when 1e100 is added and lost again when it is
# taken away; the compensation term is what remembers it.
#
# The algorithm is improved Kahan-Babuska, after Neumaier: carry `c` beside the
# total, and pick which way round to recover the lost low bits by comparing
# magnitudes.  An integer item is added straight in with no compensation, which
# is what CPython does too.

import math


def show(label, value):
    print("%-34s %r" % (label, value))


# --- the cancellations the compensation exists for -------------------------
show("[1e100, 1.0, -1e100] start 0.0", sum([1e100, 1.0, -1e100], 0.0))
show("[1e100, 1.0, -1e100]", sum([1e100, 1.0, -1e100]))
show("[1e16, 1.0, 1.0, -1e16]", sum([1e16, 1.0, 1.0, -1e16], 0.0))
show("[1e200, 0.1, -1e200]", sum([1e200, 0.1, -1e200], 0.0))
show("[0.1] * 10", sum([0.1] * 10, 0.0))
show("[0.1] * 10 default start", sum([0.1] * 10))
show("[1.0, 1e-16, 1e-16]", sum([1.0, 1e-16, 1e-16], 0.0))
show("[1e-300, 1e300, -1e300]", sum([1e-300, 1e300, -1e300], 0.0))

# order should not change the compensated answer here
xs = [1e100, 1.0, -1e100, 2.0]
show("forward", sum(xs, 0.0))
show("reversed", sum(list(reversed(xs)), 0.0))

# --- an integer item goes in uncompensated, as CPython does ----------------
show("[1.0, 2, 3.0]", sum([1.0, 2, 3.0], 0.0))
show("[1.0, True, False, 2.0]", sum([1.0, True, False, 2.0], 0.0))
show("[1e100, 1, -1e100]", sum([1e100, 1, -1e100], 0.0))
show("ints only", sum([1, 2, 3]))
show("int start, float items", sum([1.5, 2.5], 3))

# --- the compensation must not manufacture a NaN --------------------------
show("[inf, 1.0, -inf]", sum([float('inf'), 1.0, float('-inf')], 0.0))
show("[1.0, nan]", sum([1.0, float('nan')], 0.0))
show("[1e308, 1e308]", sum([1e308, 1e308], 0.0))
show("[1e308, 1e308, -1e308]", sum([1e308, 1e308, -1e308], 0.0))
show("[inf, -inf]", sum([float('inf'), float('-inf')], 0.0))

# --- signed zero survives --------------------------------------------------
show("[-0.0] start -0.0", sum([-0.0], -0.0))
show("[-0.0] start 0.0", sum([-0.0], 0.0))
show("[0.0, -0.0]", sum([0.0, -0.0], 0.0))
show("[-0.0, -0.0]", sum([-0.0, -0.0], -0.0))
show("empty", sum([], 0.0))
show("empty -0.0", sum([], -0.0))

# --- leaving the float phase for something else ---------------------------
show("[1.0, 1j]", sum([1.0, 1j], 0.0))
show("[1e100, 1.0, -1e100, 1j]", sum([1e100, 1.0, -1e100, 1j], 0.0))


class Addable:
    def __radd__(self, other):
        return "left the float phase at %r" % (other,)


show("[1e100, 1.0, -1e100, obj]", sum([1e100, 1.0, -1e100, Addable()], 0.0))

# --- a generator, so the items are not a list ------------------------------
show("generator", sum((x for x in [1e100, 1.0, -1e100]), 0.0))
show("generator ints", sum((x for x in range(5)), 0.0))

# --- errors still surface --------------------------------------------------
try:
    sum([1.0, "x"], 0.0)
except TypeError:
    print("TypeError for a str item")
try:
    sum([1.0], "")
except TypeError:
    print("TypeError for a str start")

# --- a wide sweep, so a single-bit slip shows up ---------------------------
total = []
for k in range(-40, 41, 4):
    big = math.ldexp(1.0, k * 8)
    for small in (1.0, 0.5, 1e-5):
        total.append(sum([big, small, -big], 0.0))
        total.append(sum([big, small, small, -big], 0.0))
        total.append(sum([small, big, -big], 0.0))
print(len(total))
for v in total:
    print(repr(v))

# A float `start`.  The compensated loop used to be entered only from the
# RESULT of an addition, so the first add -- the one that consumes `start` --
# ran uncompensated, and it is exactly the one that loses the digits.
print(sum([1.0, -1e100], 1e100))
print(sum([-1e100, 1.0], 1e100))
print(sum([1e100, 1.0, -1e100], 0.0))
print(sum([1.0, 1e100, -1e100], 0.0))
print(sum([1e100, -1e100], 1.0))
print(sum([], 1.5), sum([], 0.0), repr(sum([], -0.0)))
print(sum([0.1] * 10, 0.0), sum([0.1] * 10))
print(sum([1, 2, 3], 0.5), sum([2**60, 1.0], 0.5))
# An int start that only becomes a float part-way is the path that already
# worked, and has to keep working.
print(sum([1e100, 1.0, -1e100], 0))
print(sum([1, 1e100, 1.0, -1e100]))
# Infinities and NaN are not compensated away.
print(sum([float("inf"), 1.0], 0.0), sum([1.0], float("inf")))
n = sum([float("nan"), 1.0], 0.0)
print(n != n)
# A float subclass start is a pointer, not an immediate: it must keep the
# generic protocol and still be right.
class F(float):
    pass
print(sum([1.0, -1e100], F(1e100)), type(sum([1.0], F(2.0))).__name__)
