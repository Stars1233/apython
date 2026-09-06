# COMPARE_OP on two float immediates used to fall through to the general
# protocol -- float_binop_accepts, then float_compare, which calls
# float_binop_accepts twice more, fc_wide_int twice and float_to_f64 twice.
# About nine calls to reach one ucomisd, and the only float operation that was
# not already ahead of CPython.  Opcodes 223/224/225 specialize it, the second
# two fused with the jump that follows.
#
# NaN is what the integer forms never had to think about.  ucomisd reports
# unordered as ZF=1, PF=1, CF=1, so seta/setae are already false for it while
# setb/setbe/sete would each be wrongly true and setne wrongly false.  Every
# operator against every combination of specials is here for that reason.
#
# The loops matter as much as the table: a site only specializes once it has
# run, so the answers have to stay right after it does, and a site that then
# sees a str has to deopt rather than answer from the float path.

import math
nan = float('nan'); inf = float('inf')
vals = [0.0, -0.0, 1.0, -1.0, 0.5, 2.0, inf, -inf, nan, 1e300, -1e300,
        5e-324, -5e-324, 1e16, 1.5]
ops = [("<", lambda a,b: a<b), ("<=", lambda a,b: a<=b), ("==", lambda a,b: a==b),
       ("!=", lambda a,b: a!=b), (">", lambda a,b: a>b), (">=", lambda a,b: a>=b)]
out = []
for a in vals:
    for b in vals:
        for name, fn in ops:
            out.append("%r %s %r = %r" % (a, name, b, fn(a, b)))
# in a loop, so the site specializes, and both branches of the fused forms
def loops():
    r = []
    for a in vals:
        for b in vals:
            c1 = c2 = c3 = c4 = 0
            for _ in range(3):
                if a < b: c1 += 1
                if a >= b: c2 += 1
                if a == b: c3 += 1
                if a != b: c4 += 1
            r.append((c1,c2,c3,c4))
    return r
out.append(repr(loops()))
# mixed operands must still work after the site has specialized
def mixed():
    r = []
    for _ in range(3):
        r.append(1.5 < 2)
        r.append(2 < 1.5)
        r.append(1.5 == 1)
        r.append(1.0 == 1)
        r.append(1.5 < "a" if False else None)
    return r
out.append(repr(mixed()))
# a site that sees floats then something else: it must deopt, not answer wrong
def poly():
    r = []
    for v in [1.0, 2.0, 3.0, "a", "b", 1.0, 2.0]:
        for w in [1.0, "a"]:
            try:
                r.append(v < w)
            except TypeError:
                r.append("TypeError")
    return r
out.append(repr(poly()))
print("\n".join(out))
