# int and float answer their unary operators by name.
#
# Neither type had a single operator dunder in its tp_dict, which cost two
# separate things.  `(-5).__abs__()` was an AttributeError, and the stdlib asks
# by name -- operator.index goes through __index__.  And an MRO name lookup
# could not prefer int's operator over a later base's, because int had nothing
# to find: `class I(int, M)` with an M defining __invert__ installed M's over
# the nb_invert the class had already inherited, so ~I(3) was M's answer where
# CPython gives -4.
#
# The thunks call the defining type's slot, not the argument's, which is what
# keeps a subclass out of its own recursion.

class M:
    def __invert__(self):
        return "M's invert"
    def __neg__(self):
        return "M's neg"

class I(int, M):
    pass

print(~I(3), -I(3), +I(3), abs(I(-3)))
print(type(~I(3)).__name__, ~I(0), ~I(-1))

# The other order is CPython's too: M comes first, so M's wins.
class J(M, int):
    pass
print(~J(3), -J(3))

for n in (0, 1, -1, 5, -5, 255, 2 ** 70, -(2 ** 70)):
    print(n, n.__abs__(), n.__neg__(), n.__pos__(), n.__invert__())
    print(n, n.__int__(), n.__index__(), n.__trunc__(), n.__bool__(), n.__float__())

for f in (0.0, -0.0, 1.5, -1.5, 2.0, -3.75, 1e300):
    print(f, f.__abs__(), f.__neg__(), f.__pos__(), f.__bool__())
    print(f, f.__int__(), f.__trunc__(), f.__float__())

print(True.__abs__(), True.__int__(), True.__index__(), False.__bool__())

# A subclass answers with the plain base type, as CPython does.
i = I(-5)
print(i.__abs__(), type(i.__abs__()).__name__, i.__index__(), type(i.__int__()).__name__)
class F(float):
    pass
f = F(-1.5)
print(f.__abs__(), type(f.__abs__()).__name__, f.__int__(), f.__float__())

# The slots these fill are reached by the builtins too.
import operator
print(operator.index(5), operator.index(True), operator.index(I(7)))
print(abs(-5), abs(-(2 ** 70)), abs(-1.5), int(5.9), int(-5.9), float(5))
print(int(2.0 ** 70) == 2 ** 70, int(1e300) == 10 ** 300 or int(1e300) > 10 ** 299)
print(bool(0), bool(5), bool(0.0), bool(1.5))

# Errors keep their shape.
for expr in ("int('x')", "int(1.5, 2)", "(1.5).__int__(2)", "float('nan').__int__()"):
    try:
        eval(expr)
        print(expr, "=> no error")
    except (TypeError, ValueError) as e:
        print(expr, "=>", type(e).__name__)
