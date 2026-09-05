# `&`, `|`, `^` and `%` had no fast arm in op_binary_op.  The bitwise three had
# int64 arms in int_and/int_or/int_xor all along but were reached only through
# the whole generic protocol -- binop_left_wrapper, binop_subclass_first,
# binop_is_number -- and `%` was not in the NB ladder at all, so it went
# straight to .binop_generic.  Opcodes 226/227/228/229 specialize them.
#
# Three things this has to keep right:
#
#   - `%` takes the sign of the DIVISOR, where idiv gives the dividend's.
#     Every sign combination is here.
#   - a zero divisor must still raise, after the site has specialized.
#   - an int subclass with its own __and__/__mod__ must not be bypassed, and
#     neither must bool, which is a heap singleton and not an immediate.
#
# The loops matter: a site only specializes once it has run.

def band(a,b): return a & b
def bor(a,b):  return a | b
def bxor(a,b): return a ^ b
def bmod(a,b): return a % b
def bfd(a,b):  return a // b
V = [0,1,2,3,7,255,-1,-2,-3,-7,-255, 2**20,-(2**20), 2**49,-(2**49),
     2**50, 2**50-1, -(2**50), 2**62,-(2**62), 2**63, -(2**63), 10**25,-(10**25)]
out=[]
for a in V:
    for b in V:
        out.append("%d&%d=%d" % (a,b,band(a,b)))
        out.append("%d|%d=%d" % (a,b,bor(a,b)))
        out.append("%d^%d=%d" % (a,b,bxor(a,b)))
        if b != 0:
            out.append("%d%%%d=%d" % (a,b,bmod(a,b)))
            out.append("%d//%d=%d" % (a,b,bfd(a,b)))
            out.append("dm=%r" % (divmod(a,b),))
# in loops so the sites specialize, then feed them other types
def loops():
    r=[]
    for _ in range(4):
        for a in [5, -5, 2**55, True, False]:
            for b in [3, -3, 2**55]:
                r.append((a & b, a | b, a ^ b, a % b, a // b))
    return r
out.append(repr(loops()))
# zero divisor still raises after specialization
def zdiv():
    r=[]
    for _ in range(4):
        try: r.append(7 % 0)
        except ZeroDivisionError as e: r.append(str(e))
        try: r.append(7 // 0)
        except ZeroDivisionError as e: r.append(str(e))
    return r
out.append(repr(zdiv()))
# in-place forms
def inplace():
    a=0xF0F0; b=0x0F0F
    a &= b; a |= 0x1234; a ^= 0x5678; a %= 1000; a //= 7
    return a
out.append(repr(inplace()))
# a subclass must not be bypassed
class MyI(int):
    def __and__(self,o): return "MyI.__and__"
    def __mod__(self,o): return "MyI.__mod__"
def subs():
    r=[]
    for _ in range(4):
        r.append(MyI(6) & 3); r.append(MyI(7) % 3)
        r.append(6 & MyI(3)); r.append(7 % MyI(3))
    return r
out.append(repr(subs()))
print("\n".join(out)); print(len(out))
