# complex() from a string, and the three conversion protocols.
#
# complex("1+2j") was never implemented: builtin_complex handed the str to
# complex_to_parts, which classifies and does not parse, so every string was
# "complex() argument must be a number".  tests/test_complex.py had no string
# case at all, which is why it shipped.
#
# The grammar is CPython's, from complex_from_string_inner:
#
#     <float> | <float>j | <float><signed-float>j
#
# plus three compatibility forms -- <float><sign>j, <sign>j, j -- optionally
# in one bracket pair, optionally surrounded by whitespace but never
# containing any.  Underscores follow PEP 515 and are a different error from
# a malformed number, which is why both messages appear below.
#
# glibc's strtod accepts two things CPython's does not, and both are refused
# before the call: a hex float ("0x10" is malformed, not 16) and a nan
# payload.
#
# The protocols are __complex__, then __float__, then __index__ -- and only
# for the FIRST argument.  CPython calls try_complex_special_method on r and
# never on i, so complex(1, C()) is a TypeError where complex(C(), 1) is not.


def check(label, fn):
    try:
        print("%-26s %r" % (label, fn()))
    except BaseException as e:
        print("%-26s %s: %s" % (label, type(e).__name__, e))


print("--- the three ordinary forms ---")
for src in ['1+2j', '1-2j', '3', '3j', '1e3+2e-3j', '1.5+2.5j', '-1.5-2.5j',
            '.5j', '5.j', '1.5', '-1.5', '1e-3', '2+3J', '1J', '00', '007']:
    check("complex(%r)" % src, lambda s=src: complex(s))

print()
print("--- the compatibility forms ---")
for src in ['j', '+j', '-j', 'J', '+3j', '-3j', '1+j', '1-j', '+1.5j']:
    check("complex(%r)" % src, lambda s=src: complex(s))

print()
print("--- brackets and whitespace, outside only ---")
for src in [' 1+2j ', '(1+2j)', '( 1+2j )', '(1+2j) ', '\t1+2j\n',
            '  \t\n j \r ', '(j)', '(-j)', '(1.5+2.5j)']:
    check("complex(%r)" % src, lambda s=src: complex(s))

print()
print("--- inf and nan ---")
for src in ['inf', '-inf', 'infj', 'Infinity', 'nan', 'nanj', 'nan+nanj',
            'nan-nanj', 'inf+infj', '-inf-infj', '1e400', '1e400j']:
    check("complex(%r)" % src, lambda s=src: complex(s))

print()
print("--- underscores, PEP 515 ---")
for src in ['1_0', '1_0+2j', '1_000.5', '1.5_0j', '1_0j']:
    check("complex(%r)" % src, lambda s=src: complex(s))
for src in ['1_+2j', '_1+2j', '1__0+2j']:
    check("complex(%r)" % src, lambda s=src: complex(s))

print()
print("--- malformed ---")
for src in ['1 + 2j', '1j+1', '', '   ', '(1+2j', '1+2j)', 'abc', '1+2i',
            '0x10', '0X1P3', '0b101', '0o17', '++1j', '--1j', '1+-2j',
            '1-+2j', '1+2j+3j', 'j1', '1+', '+', '-', '()', '( )', '1e',
            '1e+', '1.5e', '2j3', '.j', '.', '+.j', '  (  1 + 2j )  ',
            'nan(1)']:
    check("complex(%r)" % src, lambda s=src: complex(s))

print()
print("--- a str subclass parses too ---")


class S(str):
    pass


check("complex(S('1+2j'))", lambda: complex(S('1+2j')))

print()
print("--- a string may not have a companion ---")
check("complex('1', 2)", lambda: complex('1', 2))
check("complex(1, '2')", lambda: complex(1, '2'))
check("complex('1', '2')", lambda: complex('1', '2'))


class WithComplex:
    def __complex__(self):
        return complex(3, 4)


class WithFloat:
    def __float__(self):
        return 2.5


class WithIndex:
    def __index__(self):
        return 7


class BadComplex:
    def __complex__(self):
        return "not a complex"


class RaisingComplex:
    def __complex__(self):
        raise RuntimeError("__complex__ raised")


class RaisingFloat:
    def __float__(self):
        raise RuntimeError("__float__ raised")


class Plain:
    pass


print()
print("--- the conversion protocols ---")
check("complex(WithComplex())", lambda: complex(WithComplex()))
check("complex(WithFloat())", lambda: complex(WithFloat()))
check("complex(WithIndex())", lambda: complex(WithIndex()))
# The coerced value's shape decides the two-argument arithmetic, not the
# argument's: 3+4j with an imaginary 1 gives (3+5j), not (3+1j).
check("complex(WithComplex(), 1)", lambda: complex(WithComplex(), 1))
check("complex(WithFloat(), 1)", lambda: complex(WithFloat(), 1))
check("complex(WithIndex(), 2)", lambda: complex(WithIndex(), 2))
# ...but the second argument is never offered __complex__.
check("complex(1, WithComplex())", lambda: complex(1, WithComplex()))
check("complex(1, WithFloat())", lambda: complex(1, WithFloat()))
check("complex(1, WithIndex())", lambda: complex(1, WithIndex()))

print()
print("--- and their failures ---")
check("complex(BadComplex())", lambda: complex(BadComplex()))
check("complex(RaisingComplex())", lambda: complex(RaisingComplex()))
check("complex(RaisingFloat())", lambda: complex(RaisingFloat()))
check("complex(Plain())", lambda: complex(Plain()))
check("complex(1, Plain())", lambda: complex(1, Plain()))
check("complex(None)", lambda: complex(None))
check("complex(1, None)", lambda: complex(1, None))
check("complex([1])", lambda: complex([1]))
check("complex({})", lambda: complex({}))

print()
print("--- the ordinary numeric cases are untouched ---")
check("complex()", lambda: complex())
check("complex(3)", lambda: complex(3))
check("complex(3.5)", lambda: complex(3.5))
check("complex(True)", lambda: complex(True))
check("complex(1, 2)", lambda: complex(1, 2))
check("complex(1j, 1)", lambda: complex(1j, 1))
check("complex(1, 1j)", lambda: complex(1, 1j))
check("complex(1+2j)", lambda: complex(1 + 2j))
check("complex(0, -0.0)", lambda: complex(0, -0.0))
# complex(10**30) is left out: float(10**30) already rounds
# differently from CPython here, which is a GMP-to-double gap in
# float_to_f64 and nothing to do with the constructor.

print()
print("--- a complex subclass argument yields a plain complex ---")


class Sub(complex):
    pass


check("complex(Sub(1, 2))", lambda: complex(Sub(1, 2)))
check("type(complex(Sub(1,2)))", lambda: type(complex(Sub(1, 2))).__name__)
check("complex(Sub(1, 2), 1)", lambda: complex(Sub(1, 2), 1))

print()
print("--- the parse allocates, so churn afterwards ---")
kept = [complex("%d-%dj" % (i, i)) for i in range(40)]
kept += [complex("1_%d.5j" % i) for i in range(10)]
print("churn  :", len([[i, i] for i in range(3000)]))
print("kept   :", kept[3], kept[41])
print("sum    :", sum(kept[:5]))
