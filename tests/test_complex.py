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


# The binary family went through the SLOTS and nothing else, so
# `complex(1,2).__add__` did not exist.  A slot with no matching entry in
# tp_dict answers the wrong thing to everything that asks by name -- and the
# numeric tower asks by name, because a class that dispatches on
# NotImplemented cannot ask a type that has no __add__ to try.
print("=== the dunders exist ===")
z = complex(3, 4)
print([n for n in ("__add__", "__sub__", "__mul__", "__truediv__", "__pow__",
                   "__radd__", "__rsub__", "__rmul__", "__rtruediv__",
                   "__rpow__", "__neg__", "__pos__", "__abs__", "__bool__",
                   "__eq__", "__hash__") if hasattr(z, n)])

print("=== and answer what the operators do ===")
print(z.__add__(1), z.__add__(1.5), z.__add__(complex(1, 1)))
print(z.__radd__(1), z.__rsub__(1), z.__rmul__(2), z.__rtruediv__(1))
print(z.__neg__(), z.__pos__(), z.__abs__(), z.__bool__())
print(z.__pow__(2), z.__rpow__(2), complex(0).__bool__())

# ...and decline what the operator would hand on to the other side.
print("=== NotImplemented, not a wrong answer ===")
for bad in ("x", None, [], b"x"):
    print("%-6r %r %r %r" % (bad, z.__add__(bad), z.__mul__(bad),
                             z.__radd__(bad)))

print("=== called by name off the type ===")
print(complex.__add__(z, 1), complex.__neg__(z))
try:
    complex.__add__(1, z)
except TypeError as e:
    print("wrong self ->", type(e).__name__)

print("=== pow with a modulus ===")
# complex is decided in pow() itself and not by its __pow__: CPython's answer
# for a modulus is ValueError, and complex.__pow__ is the generated
# three-argument wrapper, which calls pow() -- so looking it up would be a
# recursion with no floor.
for args in (("x", 0), (2, 3), (2,)):
    try:
        print(args, complex(1, 2).__pow__(*args))
    except Exception as e:
        print(args, type(e).__name__, e)
for args in ((complex(1, 2), 2, 3), (complex(1, 2), 2)):
    try:
        print(len(args), pow(*args))
    except Exception as e:
        print(len(args), type(e).__name__, e)
