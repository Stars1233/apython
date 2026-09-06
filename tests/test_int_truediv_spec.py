# `int / int` answers a float, and it had no fast arm: op_binary_op's ladder
# did not carry NB_TRUE_DIVIDE at all, so `i / 7` took the whole generic
# protocol on every execution -- 23% of the loop spent getting to a divsd.
# Opcode 233 specializes it.
#
# The arm is only correct because both operands are immediates.  Each is
# inside +-2^50, so each converts to a double EXACTLY, and IEEE division of
# two exact doubles is correctly rounded -- the same answer CPython's
# long_true_divide reaches by scaling and rounding the integers by hand.
# That argument fails the moment either side is wide: converting a wide int to
# a double rounds once and the division rounds again, and the two roundings do
# not compose.  A wide operand therefore deopts, and the wide cases are in
# tests/test_int_truediv_wide.py rather than here.
#
# Neither an overflow nor a subnormal is reachable: the quotient's magnitude
# is between 2^-50 and 2^50.
#
# Each site is run repeatedly, because a site only specializes once it has run.

V = [0, 1, -1, 2, -2, 3, -3, 7, -7, 10, 100, 255, 999983,
     2 ** 20, -(2 ** 20), 2 ** 30, 2 ** 49, -(2 ** 49),
     2 ** 50 - 1, -(2 ** 50), 2 ** 50, 2 ** 60, -(2 ** 60)]
def d(a,b): return a/b
out=[]
for a in V:
    for b in V:
        for _ in range(3):
            try: r=repr(d(a,b))
            except ZeroDivisionError: r="ZeroDivisionError"
        out.append("%d / %d = %s"%(a,b,r))
print("\n".join(out))
def idiv(a,b):
    a /= b; return a
for a in V[:12]:
    for b in (1,2,3,7,-3):
        print("i", a, b, repr(idiv(a,b)))
def mix(xs):
    r=[]
    for x in xs:
        try: r.append(x/3)
        except TypeError: r.append("TE")
        try: r.append(3/x)
        except (TypeError,ZeroDivisionError) as e: r.append(type(e).__name__)
    return r
print(mix([1, 2, 3, 2 ** 60, 0, 1.5, True, False, "x", 7]))
print(mix([1,2,3]))
class D(int):
    def __truediv__(self,o): return "D.__truediv__"
    def __rtruediv__(self,o): return "D.__rtruediv__"
for _ in range(4): print(d(D(3),2), d(2,D(3)))
