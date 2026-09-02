# The complex type: construction, arithmetic, repr, hash, comparison.
#
# Mixed arithmetic here is what exercises op_binary_op's binary_op1 sequence:
# `1 + 2j` works only because int_add declines the pair and the right
# operand's slot is asked next.  There is no complex-specific arm in
# src/opcodes/arith.asm, by design.


# The expressions below are evaluated with eval() over string literals that
# live in this file.  Nothing is read from input; evaluating the source text is
# what keeps each printed label and the expression that produced it from
# drifting apart, and exec/eval are themselves part of what the suite covers.
def show(label, fn):
    try:
        print(label, "=", repr(fn()))
    except BaseException as exc:
        print(label, "->", type(exc).__name__)


def show_approx(label, fn):
    """For a result that goes through libm rather than through arithmetic.

    A complex power with a non-integral exponent is exp/log/atan2 underneath,
    and those differ in the last ulp between glibc versions -- CI and this
    machine disagreed about (1+1j) ** 1j by exactly one.  The path is still
    worth exercising; the sixteenth digit of it is not.
    """
    try:
        z = fn()
        print(label, "=", "%.12g%+.12gj" % (z.real, z.imag))
    except BaseException as exc:
        print(label, "->", type(exc).__name__)


print("--- construction ---")
for expr in ("complex()", "complex(3)", "complex(2.5)", "complex(1, 2)",
             "complex(True)", "complex(1 << 70)", "complex(complex(1, 2))",
             "complex(1j, 1)", "complex(1, 1j)", "complex(0, -0.0)"):
    show(expr, lambda e=expr: eval(e))

print("--- repr ---")
# A real part of exactly +0.0 is not printed; a NEGATIVE zero is.
for expr in ("2j", "-2j", "0j", "-0j", "1+2j", "1-2j", "complex(-0.0, 2)",
             "complex(100)", "complex(2.5)", "complex(1e16)", "complex(1e-5)",
             "complex(0, 1)", "complex(float('inf'), 1)",
             "complex(1, float('inf'))", "complex(float('nan'), 0)"):
    show(expr, lambda e=expr: eval(e))

print("--- arithmetic, both operand orders ---")
for expr in ("1 + 2j", "2j + 1", "1.5 * (2+3j)", "(2+3j) * 1.5",
             "(1+2j) - 1", "1 - (1+2j)", "(1+2j) / 2", "2 / (1+2j)",
             "True + (1+1j)", "(1 << 70) + 1j",
             "(1+2j) + (3+4j)", "(1+2j) * (3+4j)", "(1+2j) / (3-4j)",
             "-(1+2j)", "+(1+2j)", "abs(3+4j)", "abs(0j)"):
    show(expr, lambda e=expr: eval(e))

print("--- power ---")
# The integer fast path runs for a real, integral exponent within +-100.
for expr in ("(1+2j) ** 0", "(1+2j) ** 1", "(1+2j) ** 2", "(1+2j) ** 3",
             "(1+2j) ** -1", "(1+2j) ** 100", "(0j) ** 0", "(0j) ** -1",
             "(1e300+0j) ** 2"):
    show(expr, lambda e=expr: eval(e))
# The general path: a non-integral exponent, which is libm's rather than ours.
for expr in ("(2+0j) ** 0.5", "(1+1j) ** 1j"):
    show_approx(expr, lambda e=expr: eval(e))

print("--- what complex does not support ---")
for expr in ("(1+2j) // 2", "(1+2j) % 2", "divmod(1+2j, 2)", "int(1+0j)",
             "float(1+0j)", "(1+2j) < (1+3j)", "(1+2j) > 1", "(1+2j) / 0",
             "(1+2j) + 'x'", "(1+2j) + [1]", "(1+2j) & 1", "~(1+2j)"):
    show(expr, lambda e=expr: eval(e))

print("--- equality and truth ---")
print((1+2j) == (1+2j), (1+2j) == (1+3j), (1+0j) == 1, 1 == (1+0j))
print((1+0j) == 1.0, 1.0 == (1+0j), (1+2j) == 1, (1+2j) == "x")
print((1+2j) != (1+3j), bool(0j), bool(1j), bool(complex(-0.0, 0.0)))

print("--- hash ---")
# hash(complex(x, 0)) must equal hash(x), or an int and an equal complex land
# in different dict slots.  No NaN here: CPython hashes a NaN by identity.
print(hash(complex(2, 0)) == hash(2), hash(complex(1.5, 0)) == hash(1.5))
print(hash(1+2j), hash(1-2j), hash(0j))
print(len({1, 1+0j, 1.0}), {1+0j: "a"}[1])

print("--- attributes and methods ---")
z = 1+2j
print(z.real, z.imag, z.conjugate(), (0j).conjugate())
print(z.__complex__() == z, z.__getnewargs__())
print(type(z).__name__, repr(complex))
