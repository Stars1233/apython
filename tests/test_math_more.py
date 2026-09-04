# The five math functions the module was short of: perm, ulp, prod, isclose
# and dist.  comb was there and perm was not, which is the odd half of that
# pair to be missing.
#
# dist hands its coordinate differences to the same routine hypot uses, so
# the two always agree with each other.
import math
def t(l,f):
    try: print(l,"=>",repr(f()))
    except BaseException as e: print(l,"!!",type(e).__name__,e)
t("perm(5,2)", lambda: math.perm(5,2))
t("perm(5)", lambda: math.perm(5))
t("perm(5,None)", lambda: math.perm(5,None))
t("perm(5,0)", lambda: math.perm(5,0))
t("perm(5,6)", lambda: math.perm(5,6))
t("perm(0,0)", lambda: math.perm(0,0))
t("perm(30,10)", lambda: math.perm(30,10))
t("perm big", lambda: math.perm(100,50))
t("perm neg", lambda: math.perm(-1,2))
t("perm neg k", lambda: math.perm(5,-1))
t("perm float", lambda: math.perm(5.0,2))
t("ulp(1)", lambda: math.ulp(1.0))
t("ulp(0)", lambda: math.ulp(0.0))
t("ulp(-1)", lambda: math.ulp(-1.0))
t("ulp(nan)", lambda: math.isnan(math.ulp(float('nan'))))
t("ulp(inf)", lambda: math.ulp(float('inf')))
t("ulp(max)", lambda: math.ulp(1.7976931348623157e308))
t("ulp(2)", lambda: math.ulp(2.0))
t("ulp int", lambda: math.ulp(5))
t("prod", lambda: math.prod([1,2,3,4]))
t("prod start", lambda: math.prod([2,3], start=10))
t("prod empty", lambda: math.prod([]))
t("prod empty start", lambda: math.prod([], start=7))
t("prod floats", lambda: math.prod([1.5,2.0]))
t("prod big", lambda: math.prod(range(1,30)))
t("prod gen", lambda: math.prod(x for x in (2,3,4)))
t("prod noniter", lambda: math.prod(5))
t("isclose eq", lambda: math.isclose(1.0,1.0))
t("isclose near", lambda: math.isclose(1.0,1.0+1e-10))
t("isclose far", lambda: math.isclose(1.0,1.2))
t("isclose rel", lambda: math.isclose(1.0,1.05,rel_tol=0.1))
t("isclose abs", lambda: math.isclose(0.0,1e-10,abs_tol=1e-9))
t("isclose both", lambda: math.isclose(0.0,1e-10,rel_tol=0.0,abs_tol=1e-11))
t("isclose inf", lambda: (math.isclose(float('inf'),float('inf')), math.isclose(float('inf'),1.0)))
t("isclose nan", lambda: math.isclose(float('nan'),float('nan')))
t("isclose ints", lambda: math.isclose(1,1))
t("dist", lambda: math.dist((1,2),(4,6)))
t("dist 1d", lambda: math.dist([1],[4]))
t("dist 0d", lambda: math.dist([],[]))
t("dist 3d", lambda: math.dist((1,2,3),(4,6,8)))
t("dist lists", lambda: math.dist([0.0,0.0],[3.0,4.0]))
t("dist mismatch", lambda: math.dist((1,),(1,2)))
t("dist neg", lambda: math.dist((-1,-2),(2,2)))
t("dist ints", lambda: math.dist((0,0),(1,1)))

# The scale in isclose is max(|a|, |b|), and the difference has to survive the
# conversion of an explicit rel_tol without landing in the slot `a` is in --
# it did, so the scale became max(|b|, |a - b|) and this was False.  The
# default-tolerance form never went through that path, which is why every case
# above passed.
t("isclose rel scale", lambda: math.isclose(100.0, 99.0, rel_tol=0.01))
t("isclose rel scale2", lambda: math.isclose(1000.0, 999.0, rel_tol=0.01))
t("isclose rel just under", lambda: math.isclose(100.0, 98.0, rel_tol=0.01))
t("isclose both given", lambda: math.isclose(1.0, 1.05, rel_tol=0.01, abs_tol=0.1))
t("isclose both order", lambda: math.isclose(1.0, 1.05, abs_tol=0.1, rel_tol=0.01))
t("isclose abs wins", lambda: math.isclose(100.0, 99.0, rel_tol=0.0, abs_tol=2.0))
t("isclose rel zero", lambda: math.isclose(0.0, 0.0, rel_tol=0.5))
t("isclose negatives", lambda: math.isclose(-100.0, -99.0, rel_tol=0.01))
t("isclose swapped", lambda: math.isclose(99.0, 100.0, rel_tol=0.01))
t("isclose rel bad", lambda: math.isclose(1.0, 1.0, rel_tol="x"))
t("isclose abs bad", lambda: math.isclose(1.0, 1.0, abs_tol=None))
t("isclose rel neg", lambda: math.isclose(1.0, 1.0, rel_tol=-1.0))

# ulp's type error names the argument, which means keeping it: the message
# used to read the slot the converted double goes in, before anything had
# been written there.
t("ulp str", lambda: math.ulp("abc"))
t("ulp none", lambda: math.ulp(None))
t("ulp list", lambda: math.ulp([1.0]))
t("ulp int", lambda: math.ulp(1))
t("ulp bool", lambda: math.ulp(True))
