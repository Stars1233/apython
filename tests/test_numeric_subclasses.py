"""Numbers where the value is not where the fast path expects it.

A float subclass instance is a POINTER; a float immediate is a NaN-boxed
double.  Four methods decoded self with V_TO_F64 regardless, so subtracting
the box offset from an address produced bits that happen to be a NaN --
F(2.5).hex() answered '-nan' rather than raising anything.

format() had the mirror of it: every arm compared the value's type against
an exact builtin, so a subclass matched none of them and fell out as
"unsupported format string passed to object.__format__".

And two comparisons read a NaN as infinity, because ucomisd sets ZF for
UNORDERED as well as for equal: the `je` after it needs a `jp` in front.
"""


def check(label, fn):
    try:
        got = fn()
    except Exception as exc:
        got = type(exc).__name__ + ": " + str(exc)
    print(label.ljust(34), repr(got))


class F(float):
    pass


class I(int):
    pass


class S(str):
    pass


class C(complex):
    pass


# --- float subclass methods ---
f = F(2.5)
check("hex", lambda: f.hex())
check("conjugate", lambda: (f.conjugate(), type(f.conjugate()).__name__))
check("as_integer_ratio", lambda: f.as_integer_ratio())
check("is_integer", lambda: (F(2.5).is_integer(), F(2.0).is_integer()))
check("real and imag", lambda: (f.real, f.imag))
check("arithmetic", lambda: (f + 1, f * 2, -f))
check("the exact type still", lambda: ((2.5).hex(), (2.5).as_integer_ratio()))
check("negative", lambda: (F(-0.5).hex(), F(-0.5).as_integer_ratio()))
check("zero", lambda: (F(0.0).hex(), F(0.0).as_integer_ratio()))

# --- format() on a subclass ---
check("format a float subclass", lambda: format(F(2.5), ".2f"))
check("format an int subclass", lambda: format(I(7), "03d"))
check("format a str subclass", lambda: format(S("x"), ">3"))
check("format a complex subclass", lambda: format(C(1, 2), ".2f"))
check("no spec", lambda: (format(F(2.5)), format(I(7)), format(C(1, 2))))
check("f-string", lambda: f"{F(2.5):.3f} {I(7):x} {S('y'):_^5}")
check("a bool", lambda: (format(True, "d"), format(False, "03d")))
check("the exact types still", lambda: (format(2.5, ".2f"), format(7, "03d"),
                                        format("x", ">3")))

# --- NaN is not infinity ---
nan = float("nan")
inf = float("inf")
check("abs(nan + inf j)", lambda: abs(complex(nan, inf)))
check("abs(inf + nan j)", lambda: abs(complex(inf, nan)))
check("abs(nan + nan j)", lambda: abs(complex(nan, nan)))
check("abs(3 + 4j)", lambda: abs(complex(3, 4)))
check("abs overflows", lambda: abs(complex(1e308, 1e308)))
check("nan ** 3", lambda: complex(nan, 0) ** 3)
check("(nan+nanj) ** 2", lambda: complex(nan, nan) ** 2)
check("(1+2j) ** 3", lambda: complex(1, 2) ** 3)
check("inf ** 2", lambda: complex(inf, 0) ** 2)

# --- an exact int answers .real with itself ---
# int_type carries the int-subclass family flag so that subclasses inherit
# it, and int_getattr tested the flag rather than the type: an exact heap int
# took the unwrapping path, was reduced to a payload and boxed again.
big = int(2.0 ** 51)
check("big .real is self", lambda: big.real is big)
check("big .numerator", lambda: big.numerator is big)
check("big .imag", lambda: (big.imag, big.denominator))
seven = 7
check("small .real is self", lambda: seven.real is seven)
check("bool .real", lambda: (True.real, True.numerator))
check("subclass .real type", lambda: (I(5).real, type(I(5).real).__name__))
check("subclass .real value", lambda: I(5).real == 5)
check("a big subclass", lambda: (lambda x: (x.real == 2 ** 51,
                                            type(x.real).__name__))(
    I(2 ** 51)))

# Reading it a great many times must not accumulate anything.
for _ in range(20000):
    _ = big.real
print("many reads".ljust(34), repr(True))
