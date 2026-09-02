# float_pow was x**y = 2**(y*log2(x)) on the x87, with fast paths for the
# exponents 0.5 and 2.0 and repeated squaring for an integral one.  It got
# every IEEE corner wrong: fyl2x's invalid-operation NaN was handed back
# unexamined, so (-7.5) ** 2.5 was nan where CPython promotes to complex,
# 0.0 ** -1 was inf where CPython raises, and (-1.0) ** inf, 1.0 ** nan and
# 0.0 ** inf were all nan.  2.0 ** 10000 was inf where CPython raises
# OverflowError.
#
# int_power's negative-exponent arm computed 1.0 / base**|exp| with a LINEAR
# loop -- one multiply per unit of the exponent -- so 0 ** -10**20 ran 10**20
# iterations and never came back.  It divided by the 0.0 it had just built
# rather than raising, and overflowed on the way: 2 ** -1074 is 5e-324, and
# base**1074 is an infinity whose reciprocal is 0.0.  Its exponent came
# through mpz_get_si, which is undefined past int64, so even the SIGN of
# 10**20 was whatever the low bits said.
#
# The three-argument arm was an int64 square-and-multiply, so every operand
# had to fit an immediate; it rejected a negative exponent outright, where
# CPython since 3.8 answers the modular inverse; and it tested the exponent's
# sign before the modulus, so pow(2, -1, 0) named the wrong argument.


def show(label, fn):
    try:
        print(label, "=>", repr(fn()))
    except BaseException as e:
        print(label, "!!", type(e).__name__, e)


# --- the ordinary answers, unchanged ---
print(pow(2, 10), pow(3, 0), pow(5, 3), pow(2, -1), pow(2.0, 3), pow(4.0, 0.5))
print(pow(2, 10, 1000), pow(3, 4, 5), pow(7, 2, 13), 2 ** 10, 2.0 ** 0.5)
print((-2) ** 3, (-2.0) ** 2.0, (-2.0) ** 3.0, (-2.0) ** -2.0, (-2) ** -3)

# --- a zero base with a negative exponent ---
for e in ("0 ** -1", "0 ** -2", "0.0 ** -1.0", "0 ** -1.5", "(-0.0) ** -1",
          "(-0.0) ** -3", "0 ** (-10 ** 20)", "pow(0, -1)"):
    show(e, lambda e=e: eval(e))

# 0 ** 0 is 1 -- even 0.0 ** 0.0, and nan ** 0.0.
print(0 ** 0, 0.0 ** 0.0, float("nan") ** 0.0, float("inf") ** 0.0)

# --- a negative base with a fractional exponent promotes to complex ---
for e in ("(-8) ** (1/3)", "(-1) ** 0.5", "(-8) ** 0.5", "pow(-8, 0.5)",
          "(-7.5) ** 2.5", "(-2.0) ** 0.5"):
    show(e, lambda e=e: eval(e))

# --- the infinities and nan ---
inf, nan = float("inf"), float("nan")
print((-1.0) ** inf, 1.0 ** nan, 0.0 ** inf, 2.0 ** inf, 0.5 ** inf)
print(2.0 ** -inf, 0.5 ** -inf, inf ** 2.0, inf ** -2.0, (-inf) ** 3.0)
print((-inf) ** 2.0, (-inf) ** -3.0, nan ** 2.0, 2.0 ** nan)

# --- underflow and overflow ---
print(2 ** -1074, 2 ** -1075, 2.0 ** -10000)
show("2.0 ** 10000", lambda: 2.0 ** 10000)
show("1e300 ** 2", lambda: 1e300 ** 2)

# --- an exponent past int64, which used to decide its own sign ---
print((-1) ** (10 ** 20), (-1) ** (10 ** 20 + 1))
print((-1) ** (-(10 ** 20)), 1 ** (-(10 ** 20)), 2 ** (-(10 ** 20)))

# --- three arguments ---
for e in ("pow(2, -1, 5)", "pow(2, -3, 5)", "pow(3, -1, 7)", "pow(-3, -1, 7)",
          "pow(2, -1, -5)", "pow(2, -1, 4)", "pow(0, -1, 5)", "pow(2, -1, 0)",
          "pow(2, 3, -5)", "pow(-2, 3, 5)", "pow(0, 0, 5)", "pow(2, 0, 5)",
          "pow(2, 10 ** 20, 7)", "pow(10 ** 30, 3, 10 ** 7)",
          "pow(10 ** 30, 10 ** 6, 10 ** 9 + 7)", "pow(2, 3, 1)",
          "pow(True, 2, 3)", "pow(2, 3, 5.0)", "pow(2.0, 3, 5)"):
    show(e, lambda e=e: eval(e))

# --- the division-by-zero wording, which the same probe turned up ---
for e in ("1 % 0", "1 // 0", "divmod(1, 0)", "1 / 0", "1.0 % 0.0",
          "1.0 // 0.0", "divmod(1.0, 0.0)", "1.0 / 0.0", "(2 ** 70) % 0",
          "(2 ** 70) // 0", "divmod(2 ** 70, 0)", "True % 0", "1 % False",
          "divmod(1.5, 0)", "divmod(1, 0.0)"):
    show(e, lambda e=e: eval(e))

# --- a float subclass and a class with __pow__ still route as before ---
class F(float):
    pass


class P:
    def __pow__(self, o, m=None):
        return ("P", o, m)


print(pow(F(2.0), 2), F(2.0) ** 2, F(-2.0) ** 0.5)
print(pow(P(), 3), pow(P(), 3, 5), P() ** 3)
