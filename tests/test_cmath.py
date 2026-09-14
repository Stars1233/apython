# cmath, against CPython's own cmath_testcases.txt where it is available.
#
# The module is C99's complex functions, reached directly: a `double complex`
# is two SSE eightbytes under the SysV ABI -- passed in xmm0 and xmm1,
# returned in xmm0 and xmm1 -- which is exactly how a PyComplexObject's two
# doubles already arrive and leave.  So csqrt and its siblings need no
# marshalling at all.
#
# What that leaves to test is the branch cuts, and they are the reason the
# sign of a ZERO matters here in a way it does not in math: the cut for sqrt
# and log runs along the negative real axis and is continuous with the second
# quadrant, so sqrt(-1+0j) is +1j and sqrt(-1-0j) is -1j.  A test over
# ordinary values would never look.
import cmath
import math

def fmt(x):
    # Twelve significant digits, not seventeen.  These are libm's complex
    # functions and CPython's cmath is its own implementation, so the last
    # ulp or two legitimately differ -- CPython's own test_cmath compares to a
    # relative error of 1e-15 for the same reason.  What is being tested here
    # is the branch cuts, the special values and the error rule, all of which
    # are exact; a digit-for-digit compare would only be asserting which libm
    # ran.  A signed zero still shows, which is the point.
    return "(%.12g%+.12gj)" % (x.real, x.imag)


def sfmt(x):
    return "%.12g" % (x,)


# --- the constants -----------------------------------------------------------

print("pi:", cmath.pi == math.pi)
print("e:", cmath.e == math.e)
print("tau:", cmath.tau == math.tau)
print("inf:", cmath.inf, "nan is nan:", cmath.nan != cmath.nan)
print("infj:", cmath.infj, "nanj:", cmath.nanj.imag != cmath.nanj.imag)
print("infj real is zero:", cmath.infj.real == 0.0)

# --- the branch cuts ---------------------------------------------------------
#
# The sign of a zero decides which side of the cut a value is on, and the
# formatting below keeps it: -0.0 and 0.0 print differently.

print()
for z in (complex(-1.0, 0.0), complex(-1.0, -0.0),
          complex(-4.0, 0.0), complex(-4.0, -0.0),
          complex(0.0, 0.0), complex(-0.0, 0.0), complex(0.0, -0.0)):
    print("sqrt(%r) = %s" % (z, fmt(cmath.sqrt(z))))
for z in (complex(-1.0, 0.0), complex(-1.0, -0.0)):
    print("log(%r) = %s" % (z, fmt(cmath.log(z))))
    print("phase(%r) = %s" % (z, sfmt(cmath.phase(z))))

# --- ordinary values ---------------------------------------------------------

print()
VALUES = [0, 1, -1, 2.5, -2.5, 1j, -1j, 1 + 1j, 3 - 4j, -0.5 + 0.25j,
          1e-8 + 1e-8j, 100 + 0j, 0.5j]
FUNCS = ["sqrt", "exp", "log", "log10", "acos", "asin", "atan",
         "cos", "sin", "tan", "acosh", "asinh", "atanh", "cosh", "sinh", "tanh"]




for name in FUNCS:
    fn = getattr(cmath, name)
    row = []
    for v in VALUES:
        try:
            row.append(fmt(fn(v)))
        except (ValueError, OverflowError) as e:
            row.append(type(e).__name__)
    print("%-6s %s" % (name, " ".join(row)))

# --- phase, polar, rect ------------------------------------------------------

print()
for v in VALUES:
    r, phi = cmath.polar(v)
    print("polar(%-24s) = (%s, %s)  phase=%s"
          % (fmt(v), sfmt(r), sfmt(phi), sfmt(cmath.phase(v))))

for r, phi in ((0, 0), (1, 0), (1, math.pi / 2), (2, math.pi), (1, -math.pi / 4),
               (3, 1.0), (1e10, 0.5)):
    print("rect(%g, %s) = %s" % (r, sfmt(phi), fmt(cmath.rect(r, phi))))

# --- the predicates ----------------------------------------------------------

print()
SPECIALS = [complex(0, 0), complex(1, 2), complex(float("inf"), 0),
            complex(0, float("inf")), complex(float("nan"), 0),
            complex(0, float("nan")), complex(float("inf"), float("nan")),
            complex(float("-inf"), 1)]
for z in SPECIALS:
    print("%-24s isnan=%-5s isinf=%-5s isfinite=%s"
          % (fmt(z), cmath.isnan(z), cmath.isinf(z), cmath.isfinite(z)))

# --- log with a base ---------------------------------------------------------

print()
for x, base in ((1000, 10), (8, 2), (100, 10), (1j, 10), (2, cmath.e)):
    print("log(%r, %r) = %s" % (x, base, fmt(cmath.log(x, base))))

# --- what is refused ---------------------------------------------------------

print()
for bad in ("x", None, [1], {}):
    try:
        cmath.sqrt(bad)
        print("accepted %r" % (bad,))
    except TypeError as e:
        print("refused %-6r %s" % (bad, e))

for nargs in (0, 2):
    try:
        cmath.sqrt(*([1] * nargs))
        print("accepted %d args" % nargs)
    except TypeError:
        print("refused %d args" % nargs)

# --- isclose -----------------------------------------------------------------
#
# math.isclose over the complex magnitude.  The infinities are the case the
# arithmetic alone cannot answer: inf - 1 is inf, and inf <= inf * rel_tol is
# True, so an explicit test is what makes an infinity close to nothing but
# itself.

print()
INF = float("inf")
NAN = float("nan")
PAIRS = [(1 + 1j, 1 + 1j), (1, 1.0000000001), (1, 1.1), (0, 0),
         (complex(INF, 0), complex(INF, 0)), (complex(INF, 0), 1),
         (1, complex(INF, 0)), (complex(-INF, 0), complex(INF, 0)),
         (complex(NAN, 0), complex(NAN, 0)), (complex(NAN, 0), 1),
         (complex(0, INF), complex(0, INF)), (complex(0, INF), 1j),
         (1e-9 + 1e-9j, 0)]
for a, b in PAIRS:
    print("isclose(%-22s %-22s) = %s" % (fmt(a) + ",", fmt(b), cmath.isclose(a, b)))

print("rel_tol:", cmath.isclose(1, 1.1, rel_tol=0.2), cmath.isclose(1, 1.1, rel_tol=0.05))
print("abs_tol:", cmath.isclose(0, 1e-12, abs_tol=1e-10),
      cmath.isclose(0, 1e-12, abs_tol=1e-14))
print("both:", cmath.isclose(1 + 1j, 1.0000001 + 1j, rel_tol=1e-6))

for kw in ({"rel_tol": -1}, {"abs_tol": -1}):
    try:
        cmath.isclose(1, 1, **kw)
        print("accepted %r" % (kw,))
    except ValueError:
        print("refused %r" % (kw,))
try:
    cmath.isclose(1, 1, rel_tol="x")
except TypeError:
    print("refused a str tolerance")
try:
    cmath.isclose(1)
except TypeError:
    print("refused one argument")

# --- the number protocol ------------------------------------------------------
#
# CPython's cmath asks the object: __complex__ first, then __float__ or
# __index__, which is what lets it take a Decimal or a Fraction.

print()


class WithComplex:
    def __complex__(self):
        return 3 + 4j


class WithFloat:
    def __float__(self):
        return 2.0


class WithIndex:
    def __index__(self):
        return 4


class ComplexLies:
    def __complex__(self):
        return "nope"


print("__complex__:", fmt(cmath.sqrt(WithComplex())))
print("__float__:", fmt(cmath.sqrt(WithFloat())))
print("__index__:", fmt(cmath.sqrt(WithIndex())))
print("phase via __complex__:", sfmt(cmath.phase(WithComplex())))
print("isclose via __complex__:", cmath.isclose(WithComplex(), 3 + 4j))
for obj in (ComplexLies(), object()):
    try:
        cmath.sqrt(obj)
        print("accepted %s" % type(obj).__name__)
    except TypeError as e:
        print("refused %-12s %s" % (type(obj).__name__, e))

print("done")
