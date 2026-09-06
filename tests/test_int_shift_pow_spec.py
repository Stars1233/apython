# `<<`, `>>` and `**` reached their int64 arms through the whole generic
# protocol, and getting there was 45% of a shift loop and 33% of a power loop.
# Opcodes 230/231/232 specialize them.
#
# Each declines rather than specializing when the answer will not fit an
# immediate, so a site that overflows once does not rewrite itself into an
# opcode that must immediately deopt.  The values below straddle every place
# it can decline:
#
#   - a shift count of 51 or more, which cannot fit even a left operand of 1
#   - a count of 64 or more, past what `sar` and `shl` will take
#   - a negative count, which raises ValueError
#   - bits falling off the top of a left shift, checked by shifting back
#   - a product that overflows int64 in the middle of the repeated squaring
#   - a result that fits an int64 but not +-2^50
#   - a negative exponent, which answers a float
#
# Each site is run repeatedly, because a site only specializes once it has run.

V = [0,1,2,3,7,-1,-2,-7,255,-255,2**20,-(2**20),2**30,2**49,-(2**49),
     2**50-1,-(2**50),2**50,2**63,-(2**63),10**30,-(10**30),2**62]
S = list(range(0,66))+[100,200,-1,-2]
def lsh(a,b): return a<<b
def rsh(a,b): return a>>b
def pw(a,b): return a**b
out=[]
for a in V:
    for b in S:
        for _ in range(3):
            try: r=lsh(a,b)
            except (ValueError,OverflowError) as e: r=type(e).__name__
        out.append("L %d %d %s"%(a,b,r))
        for _ in range(3):
            try: r=rsh(a,b)
            except (ValueError,OverflowError) as e: r=type(e).__name__
        out.append("R %d %d %s"%(a,b,r))
E=[0,1,2,3,4,5,6,7,8,10,16,20,31,32,33,50,62,63,64,65,100,-1,-2,-3]
for a in V:
    for b in E:
        for _ in range(3):
            try: r=repr(pw(a,b))
            except ZeroDivisionError: r="ZeroDivisionError"
        out.append("P %d %d %s"%(a,b,r))
# in-place forms
def ipl(a,b):
    a <<= b; return a
def ipr(a,b):
    a >>= b; return a
def ipp(a,b):
    a **= b; return a
for a in V[:12]:
    for b in (0,1,2,3,10,49,50,51,63):
        out.append("il %d %d %s"%(a,b,ipl(a,b)))
        out.append("ir %d %d %s"%(a,b,ipr(a,b)))
    for b in (0,1,2,3,10,20):
        out.append("ip %d %d %s"%(a,b,ipp(a,b)))
# deopt: non-int operands after specializing
def mixed(xs):
    r=[]
    for x in xs:
        try: r.append(x<<2)
        except TypeError: r.append("TE")
        try: r.append(x>>2)
        except TypeError: r.append("TE")
        try: r.append(x**2)
        except TypeError: r.append("TE")
    return r
print("\n".join(out))
print(mixed([1,2,3,2**60,4,True,False,5]))
print(mixed([1,2,3]))
try: print(1.5 << 2)
except TypeError as e: print("TypeError:", e)
print(pow(2,10,1000), pow(3,100,7), pow(-3,5,7))
class B(int):
    def __lshift__(self,o): return "B.__lshift__"
    def __pow__(self,o): return "B.__pow__"
    def __rshift__(self,o): return "B.__rshift__"
for _ in range(4): print(lsh(B(3),2), rsh(B(3),2), pw(B(3),2))
