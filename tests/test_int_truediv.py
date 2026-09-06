# int / int rounds ONCE, and says so when the answer will not fit.
#
# int_true_divide converted each operand to a double and divided, which
# rounds three times: __gmpz_get_d truncates toward zero, twice, and then the
# division rounds again.  (10**30) / 7 answered 1.4285714285714283e+29 where
# CPython answers ...285e+29, and 1 / 10**30 was out by an ulp the same way.
#
# CPython's long_true_divide computes the quotient of the two exact integers
# to 54 bits and rounds once, half to even; and it raises OverflowError rather
# than answering inf, which float(10**400) does too.
#
# Operands inside +-2^50 are unaffected and take a specialized opcode: each
# converts to a double exactly, so the single division is the only rounding.


def show(e):
    try:
        print("%-34s %r" % (e, eval(e)))
    except BaseException as ex:
        print("%-34s %s: %s" % (e, type(ex).__name__, ex))


print("--- the case that started it ---")
for e in ["(10**30) / 7", "1 / 10**30", "(10**30) / 3", "(10**25) / 7",
          "(2**64) / 3", "(2**64 + 1) / 3", "(10**100) / (10**50)",
          "-(10**30) / 7", "(10**30) / -7", "(-10**30) / -7"]:
    show(e)

print("--- exact answers stay exact ---")
for e in ["(2**60) / 2", "(2**60) / (2**30)", "(10**20) / (10**10)",
          "(2**53) / 1", "(2**53 + 1) / 1", "(2**54 + 1) / 1",
          "(2**53 - 1) / 1", "0 / (10**30)", "-0 / (10**30)"]:
    show(e)

print("--- half to even, at the boundary ---")
for e in ["(2**53 + 1) / 1", "(2**53 + 2) / 1", "(2**53 + 3) / 1",
          "(2**54 + 1) / 1", "(2**54 + 2) / 1", "(2**54 + 3) / 1",
          "(2**54 + 5) / 1"]:
    show(e)

print("--- too large for a double ---")
for e in ["(10**400) / 1", "1 / (10**-0 * 1)", "(10**400) / (10**100)",
          "(10**400) / (10**399)", "float(10**400)", "float(-10**400)",
          "(10**400).__float__()", "(2**1024) / 1", "(2**1024 - 2**970) / 1"]:
    show(e)

print("--- and too small ---")
for e in ["1 / (10**400)", "(10**300) / (10**400)", "-1 / (10**400)"]:
    show(e)

print("--- zero ---")
for e in ["1 / 0", "(10**30) / 0", "0 / 0"]:
    show(e)

print("--- small operands, which take the fast opcode ---")
for e in ["1 / 3", "7 / 2", "-7 / 2", "7 / -2", "0 / 5", "2**40 / 3",
          "True / 2", "5 / True"]:
    show(e)

print("--- and the same answers through the protocol ---")
print(int.__truediv__(10**30, 7), (10**30).__truediv__(7))
print(sum([10**30]) / 7)

print("--- int() of a float, and float() of an int ---")
for e in ["float(2**53)", "float(2**53 + 1)", "float(2**54 + 1)",
          "float(10**30)", "float(-10**30)", "float(2**1023)",
          "int(1e300) == 10**300", "float(10**308)"]:
    show(e)
