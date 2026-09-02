# A NaN divisor is not a zero divisor.
#
# ucomisd sets ZF for UNORDERED as well as for equal, and CF for UNORDERED as
# well as for less-than, so a `je` or a `jb` straight after it fires for NaN.
# float_truediv, float_floordiv and float_mod all read a NaN divisor as zero
# and raised ZeroDivisionError; float_bool read NaN as zero and answered
# False; complex_pow read a NaN exponent as negative.  The tree already knew
# the idiom -- complex_truediv, complex_abs and complex_bool all put a jp
# first.

nan = float("nan")
inf = float("inf")

print(1.0 / nan, 1.0 // nan, 1.0 % nan)
print(nan / 2.0, nan // 2.0, nan % 2.0)
print(nan / nan, nan // nan, nan % nan)
print(divmod(1.0, nan))
print(2 / nan, 2 // nan, 2 % nan)
print(inf / nan, nan / inf)

# The zero divisor still raises, for every spelling of zero.
for expr in ("1.0 / 0.0", "1.0 // 0.0", "1.0 % 0.0",
             "1.0 / -0.0", "1.0 // -0.0", "1.0 % -0.0",
             "1.0 / 0", "nan / 0.0", "divmod(1.0, 0.0)"):
    try:
        eval(expr)
        print(expr, "=> no error")
    except ZeroDivisionError:
        print(expr, "=> ZeroDivisionError")

# bool(nan) is True, through the immediate path and through the slot.
class F(float):
    pass
print(bool(nan), bool(F(nan)))
print(bool(0.0), bool(F(0.0)), bool(-0.0), bool(F(-0.0)))
print(bool(inf), bool(F(inf)), bool(1.5), bool(F(1.5)))
print(not nan, [x for x in (nan, 0.0, 1.0) if x])

# complex(0,0) ** complex(nan, 0): C's `nan < 0` is false, so the result is 0.
print(complex(0, 0) ** complex(nan, 0))
print(complex(0, 0) ** complex(2, 0))
try:
    complex(0, 0) ** complex(-1, 0)
except ZeroDivisionError:
    print("0j ** -1 => ZeroDivisionError")
