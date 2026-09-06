# Mixed int/float arithmetic had no fast arm anywhere: `x * 2` and `2 * x`
# both took the whole generic protocol on EVERY execution, with nothing ever
# rewritten -- binop_left_wrapper, binop_subclass_first, binop_is_number, and
# then float_mul, which runs float_binop_accepts and float_to_f64 twice each.
# CPython does not specialize it either; its binary-op specializer rejects
# operands of different types before it looks at float at all.
#
# The float superinstructions now take an integer immediate on either side.
# The three things that has to keep right, and which this file checks:
#
#   - TWO INTEGERS must not reach it.  That pair is the integer opcode's, and
#     answering it here would give a float where Python gives an int.
#   - A float or int SUBCLASS with its own __mul__/__rmul__ must not be
#     bypassed.  It is not, because neither a TAG_FLOAT nor a TAG_SMALLINT
#     immediate can be a heaptype instance -- but that is the invariant the
#     specialization rests on, so it is tested rather than argued.
#   - A specialized site that then sees a str has to deopt, not answer.
#
# The loops are the point: a site only specializes once it has run.

import math
vals_f = [0.0, -0.0, 1.5, -2.5, 1e300, 1e-300, float('inf'), float('-inf'), float('nan')]
vals_i = [0, 1, -1, 2, -3, 7, 10**6, -(10**6), 2**40, -(2**40)]
out = []
for f in vals_f:
    for i in vals_i:
        for name, fn in (("+", lambda a,b: a+b), ("-", lambda a,b: a-b),
                         ("*", lambda a,b: a*b), ("/", lambda a,b: a/b)):
            for a, b in ((f, i), (i, f)):
                try:
                    out.append("%r %s %r = %r" % (a, name, b, fn(a,b)))
                except ZeroDivisionError:
                    out.append("%r %s %r = ZeroDivisionError" % (a, name, b))
# in loops so the sites specialize, then feed them ints only and strs
def loop1():
    r = []
    x = 1.0
    for _ in range(5):
        x = x * 2 + 1
        x = x / 2 - 1
        r.append(x)
    return r
def loop2():
    r = []
    for a in [1.0, 2.0, 3, 4, 1.5, "a"]:
        for b in [2.0, 3, "b"]:
            try: r.append(a * b)
            except TypeError: r.append("TypeError")
    return r
def intonly():
    r = []
    for _ in range(5):
        r.append(3 * 4)
        r.append(7 // 2)
        r.append(7 / 2)
    return r
out.append(repr(loop1())); out.append(repr(loop2())); out.append(repr(intonly()))
# int subclass and float subclass must not be bypassed
class MyF(float):
    def __mul__(self, o): return "MyF.__mul__"
    def __rmul__(self, o): return "MyF.__rmul__"
class MyI(int):
    def __mul__(self, o): return "MyI.__mul__"
    def __rmul__(self, o): return "MyI.__rmul__"
def subs():
    r = []
    for _ in range(4):
        r.append(MyF(1.5) * 2); r.append(2 * MyF(1.5))
        r.append(MyI(3) * 1.5); r.append(1.5 * MyI(3))
        r.append(MyF(1.5) * 2.0); r.append(2.0 * MyF(1.5))
    return r
out.append(repr(subs()))
print("\n".join(out))
