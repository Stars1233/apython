# A NaN compared against an integer too wide for a double answered wrongly:
#
#     2 ** 60 < float('nan')      -> True    (CPython: False)
#     float('nan') >= 2 ** 60     -> True    (CPython: False)
#
# Every comparison against a NaN is false except `!=`, whatever is on the other
# side.  float_compare knows that, but only on the path that compares two
# doubles with ucomisd.  A wide int takes a different path: it cannot go
# through a double at all, because the conversion would round and `10**30 ==
# 1e30` would come out True, so the two are compared exactly in GMP instead.
#
# That path never asked whether the other side was a NaN.  Its comment said
# "NaN never gets here", and the +inf/-inf shortcuts above it silently do the
# wrong thing for one -- a NaN is unordered against both, so neither `je` is
# taken -- after which __gmpz_set_d was handed a NaN, which GMP documents as
# undefined.
#
# The boundary is 2**53, where a double stops holding every integer.  Below it
# the ordinary double path runs and was always right, which is why nothing
# caught this.

nan = float("nan")
inf = float("inf")
ninf = float("-inf")

WIDE = [
    2 ** 53, 2 ** 53 + 1, -(2 ** 53), -(2 ** 53) - 1,
    2 ** 60, -(2 ** 60), 2 ** 63, -(2 ** 63), 2 ** 64,
    10 ** 30, -(10 ** 30), 10 ** 100, -(10 ** 100),
]
NARROW = [0, 1, -1, 2 ** 49, -(2 ** 49), 2 ** 52, 2 ** 53 - 1, -(2 ** 53) + 1]


def all_six(a, b):
    return (a < b, a <= b, a == b, a != b, a > b, a >= b)


for a in WIDE + NARROW:
    print(a, "vs nan ", all_six(a, nan))
    print(a, "nan vs ", all_six(nan, a))

# The infinities on the same path, which the NaN shortcuts sit next to.
for a in WIDE + NARROW:
    print(a, "vs inf ", all_six(a, inf), all_six(inf, a))
    print(a, "vs -inf", all_six(a, ninf), all_six(ninf, a))

# An exact comparison must stay exact: these are the cases the GMP path exists
# for, and a NaN guard placed too early would send them through a double.
print(10 ** 30 == 1e30, 1e30 == 10 ** 30)
print(2 ** 53 + 1 == float(2 ** 53), float(2 ** 53) == 2 ** 53 + 1)
print(2 ** 60 < 1.5e18, 1.5e18 < 2 ** 60)
print(sorted([2 ** 60, 1.5e18, 3, 2.5, -(10 ** 30)]))

# min/max and sorting drive the same slot.
print(max(2 ** 60, 1.0), min(2 ** 60, 1.0))
print(max(-(10 ** 30), -1.0), min(-(10 ** 30), -1.0))

# A NaN is not equal to itself, so it is never *found* in a container of ints.
print(nan in [1, 2 ** 60, nan], nan == nan)
print(2 ** 60 in [nan, 1.0, 2 ** 60])

# bool() of each comparison, which is what an `if` sees.
for a in (2 ** 60, -(2 ** 60), 10 ** 40):
    if a < nan:
        print("wrong: %d < nan" % a)
    if a > nan:
        print("wrong: %d > nan" % a)
    if nan <= a:
        print("wrong: nan <= %d" % a)
    if nan >= a:
        print("wrong: nan >= %d" % a)
    if not (a != nan):
        print("wrong: %d != nan is False" % a)
print("done")
