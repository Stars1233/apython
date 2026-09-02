# real, imag, numerator and denominator on the numeric builtins.
#
# Every one of these was an AttributeError on a plain int or float.  bool and
# complex had a tp_getattr chain for real/imag; int and float had none at all,
# so `True.real` worked and `(5).real` did not -- and numbers.py and
# fractions.py ask for all four.
#
# The immediate half of op_load_attr had never called a tp_getattr, because no
# type reachable as an immediate had one.  Two things were wrong with it and
# neither could show: it passed the raw payload rather than the Value, so one
# tp_getattr could not serve an int in all three of its shapes; and it treated
# a NULL return as "no such attribute" instead of falling through to the
# tp_dicts, which would have hidden bit_length() and __eq__ the moment int
# acquired a chain.
#
# int.real on a subclass hands back a plain int, and float.real a plain float,
# which is what CPython does -- type(I(5).real) is int, not I.


def check(label, fn):
    try:
        v = fn()
        print("%-28s %-24r %s" % (label, v, type(v).__name__))
    except BaseException as e:
        print("%-28s %s" % (label, type(e).__name__))


class I(int):
    pass


class F(float):
    pass


class M(complex):
    pass


print("--- int ---")
check("(5).real", lambda: (5).real)
check("(5).imag", lambda: (5).imag)
check("(5).numerator", lambda: (5).numerator)
check("(5).denominator", lambda: (5).denominator)
check("(0).real", lambda: (0).real)
check("(0).denominator", lambda: (0).denominator)
check("(-5).real", lambda: (-5).real)
check("(-5).imag", lambda: (-5).imag)
check("(-5).numerator", lambda: (-5).numerator)

print()
print("--- int, past the immediate range ---")
check("(10**30).real", lambda: (10 ** 30).real)
check("(10**30).numerator", lambda: (10 ** 30).numerator)
check("(10**30).denominator", lambda: (10 ** 30).denominator)
check("(-(2**70)).real", lambda: (-(2 ** 70)).real)
check("(2**70).imag", lambda: (2 ** 70).imag)

print()
print("--- an int subclass answers with a plain int ---")
check("I(5).real", lambda: I(5).real)
check("I(5).imag", lambda: I(5).imag)
check("I(5).numerator", lambda: I(5).numerator)
check("I(5).denominator", lambda: I(5).denominator)
check("I(10**30).real", lambda: I(10 ** 30).real)

print()
print("--- float ---")
check("(1.5).real", lambda: (1.5).real)
check("(1.5).imag", lambda: (1.5).imag)
check("(-0.25).real", lambda: (-0.25).real)
check("(0.0).real", lambda: (0.0).real)
check("(-0.0).real", lambda: (-0.0).real)
check("(1e300).real", lambda: (1e300).real)
# float has neither of these, and must still say so.
check("(1.5).numerator", lambda: (1.5).numerator)
check("(1.5).denominator", lambda: (1.5).denominator)

print()
print("--- a float subclass answers with a plain float ---")
check("F(1.5).real", lambda: F(1.5).real)
check("F(1.5).imag", lambda: F(1.5).imag)
check("F(-2.5).real", lambda: F(-2.5).real)

print()
print("--- bool, which had real and imag but not the other two ---")
check("True.real", lambda: True.real)
check("True.imag", lambda: True.imag)
check("True.numerator", lambda: True.numerator)
check("True.denominator", lambda: True.denominator)
check("False.real", lambda: False.real)
check("False.numerator", lambda: False.numerator)
check("False.denominator", lambda: False.denominator)

print()
print("--- complex, which already had its two ---")
check("(1+2j).real", lambda: (1 + 2j).real)
check("(1+2j).imag", lambda: (1 + 2j).imag)
check("M(1, 2).real", lambda: M(1, 2).real)
check("M(1, 2).imag", lambda: M(1, 2).imag)
check("(1+2j).numerator", lambda: (1 + 2j).numerator)

print()
print("--- the chain must not shadow what tp_dict holds ---")
# Each of these lives in a tp_dict, and a tp_getattr that answered NULL
# without falling through would have hidden every one of them.
check("(5).bit_length()", lambda: (5).bit_length())
check("(5).bit_count()", lambda: (5).bit_count())
check("(5).conjugate()", lambda: (5).conjugate())
check("(5).to_bytes(2, 'big')", lambda: (5).to_bytes(2, 'big'))
check("(5).__eq__(5)", lambda: (5).__eq__(5))
check("(5).__class__", lambda: (5).__class__)
check("(1.5).is_integer()", lambda: (1.5).is_integer())
check("(1.5).conjugate()", lambda: (1.5).conjugate())
check("(1.5).hex()", lambda: (1.5).hex())
check("(1.5).as_integer_ratio()", lambda: (1.5).as_integer_ratio())
check("(1.5).__class__", lambda: (1.5).__class__)
check("True.bit_length()", lambda: True.bit_length())
check("(1+2j).conjugate()", lambda: (1 + 2j).conjugate())

print()
print("--- and must still refuse what nothing has ---")
check("(5).nope", lambda: (5).nope)
check("(1.5).nope", lambda: (1.5).nope)
check("True.nope", lambda: True.nope)
check("(1+2j).nope", lambda: (1 + 2j).nope)
print("getattr default   :", getattr(5, 'nope', 'fallback'))
print("hasattr int real  :", hasattr(5, 'real'))
print("hasattr float real:", hasattr(1.5, 'real'))
print("hasattr float num :", hasattr(1.5, 'numerator'))

print()
print("--- they compose the way numbers.py uses them ---")
print("real of a sum     :", (2 + 3).real)
print("ratio             :", (7).numerator, "/", (7).denominator)
print("as a complex part :", complex((5).real, (5).imag))
print("sum of reals      :", sum(x.real for x in [1, 2.5, 3]))
print("all imag zero     :", all(x.imag == 0 for x in [1, 2.5, True, 10 ** 30]))
