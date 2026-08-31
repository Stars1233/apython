# A small integer result must come back as an immediate.
#
# Integers in the range +-2^50 are NaN-boxed immediates, so `x is y` is a
# comparison of Values -- there is no small-int cache to consult and no object
# to be distinct from.  An operator that computes a small result through GMP
# and hands back a heap int breaks that: `1 << 0` was not the same 1 that
# `1 & 1` produced, because the bitwise operators had a SmallInt fast path and
# the shifts and power did not.
#
# CPython folds constant arithmetic, so a .pyc rarely executes these opcodes on
# literals; it takes variables, or our own compiler, to reach them.
#
# The comparisons go through names rather than literals because `x is 1` is a
# SyntaxWarning.
ZERO, ONE, TWO, FOUR, NEG = 0, 1, 2, 4, -1
T, F = True, False
a, b, two = 1, 0, 2

print(F << F, T << F, T << T, T ** T, F ** F)
print((F << F) is ZERO, (T << F) is ONE, (T << T) is TWO)
print((T ** T) is ONE, (F ** F) is ONE)

print((a << b) is ONE, (a << a) is TWO, (two >> a) is ONE, (a >> a) is ZERO)
print((a ** a) is ONE, (two ** two) is FOUR, (a ** b) is ONE)
print((a & a) is ONE, (a | b) is ONE, (a ^ a) is ZERO)
print((a + b) is ONE, (a - b) is ONE, (a * a) is ONE, (-a) is NEG)
print((two // two) is ONE, (two % two) is ZERO, abs(-a) is ONE)

# Large results are still heap integers, and still correct.
big = 1 << 200
print(big == 2 ** 200, type(big).__name__, (big >> 200) is ONE)
print((two ** 60) == 1152921504606846976)
print((-two) ** 3, two ** -1, (1 << 60) >> 60 == 1)
