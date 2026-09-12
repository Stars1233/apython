# float.fromhex's grammar, and its arithmetic.
#
# The old parser required both the `0x` prefix and the `p` exponent, checked
# neither digit count nor end of string, knew nothing of whitespace or of
# inf/nan, and accumulated the mantissa into a 64-bit register with `shl 4`
# per digit -- so anything past 16 hex digits silently WRAPPED.
# '0x1.0000000000000000000001p+0' answered 3.2311742677852644e-27 instead of
# 1.0, and '0x1p1zzz', '0x1p' and '0xp1' were accepted.
#
# CPython's grammar, in its order (Objects/floatobject.c float_fromhex):
#   [ws] [sign] ( inf | infinity | nan | [0x] hexdigits [. hexdigits]
#                 [ p [sign] decdigits ] ) [ws] EOS
# with at least one hex digit overall, at least one exponent digit after a
# `p`, and the whole string consumed.


def show(s):
    try:
        print(repr(s), "->", repr(float.fromhex(s)))
    except (ValueError, OverflowError, TypeError) as e:
        print(repr(s), "->", type(e).__name__, e)


# --- what used to be refused -----------------------------------------------
for s in ("0x1.8", "0x10", "1p3", "1", "10", ".5", "0.5", "1.",
          "inf", "-inf", "+inf", "Inf", "INF", "infinity", "-Infinity",
          "nan", "NaN", "-nan", "0x1p1  ", "  0x1p1", "  0x1p1  ",
          "\t0x1p1\n", "0X1.8", "0x.8", "0x8.", "0xA", "0xa.bp0"):
    show(s)

# --- what used to be accepted and should not be ----------------------------
for s in ("0x1p1zzz", "0x1p", "0x1p+", "0x1p-", "0xp1", "0x", "0x.", "",
          "  ", "0x1.2.3", "0x1p1p1", "1 2", "0x 1p1", "0x1 p1", "+-1",
          "--1", "inf inf", "infinit", "nana", "0xinf", "p1", "0x1p1.5"):
    show(s)

# --- the mantissa past 16 hex digits ---------------------------------------
for s in ("0x1.0000000000000000000001p+0",
          "0x1234567890abcdef123p0",
          "0x1.fffffffffffff8p+0",
          "0x1.fffffffffffff7p+0",
          "0x1.00000000000008p+0",
          "0x1.00000000000018p+0",
          "0x0000000000000000000001p+0",
          "0x1000000000000000000000000000000p-100"):
    show(s)

# --- overflow and underflow ------------------------------------------------
for s in ("0x1p1000000", "0x1.8p99999999999999999999", "-0x1p1000000",
          "0x1p1024", "0x1.fffffffffffffp+1023", "0x1p-1074", "0x1p-1075",
          "0x1p-1076", "0x0.0000000000001p-1022", "0x1p-10000000",
          "0x1p99999999999999999999999"):
    show(s)

# --- the ordinary cases, and the round trip --------------------------------
for s in ("0x0p0", "-0x0p0", "0x1p+1", "0x.8p1", "+0x1p1", "0X1P-1074",
          "0x1.999999999999ap-4", "-0x1.0000000000000p+0", "0x1P1"):
    show(s)

vals = [0.0, -0.0, 1.0, -1.0, 0.1, 1e300, 1e-300, 5e-324, 2.2250738585072014e-308,
        1.7976931348623157e308, 3.141592653589793, float("inf"), float("-inf")]
for v in vals:
    h = v.hex()
    print(h, float.fromhex(h) == v or (v != v))
print(float.fromhex(float("nan").hex()) != float.fromhex(float("nan").hex()))


# --- a subclass gets its own type -------------------------------------------
class MyFloat(float):
    pass


r = MyFloat.fromhex("0x1p+2")
print(type(r).__name__, r)
print(type(float.fromhex("0x1p+2")).__name__)


# --- the type of the argument ------------------------------------------------
for bad in (0, None, b"0x1p1", 1.5, ["0x1p1"]):
    try:
        print(repr(bad), "->", float.fromhex(bad))
    except TypeError as e:
        print(repr(bad), "-> TypeError")


# float.hex() is the other half: a subnormal has no implicit leading 1 and its
# exponent is -1022, not -1023, so 5e-324 was '0x1.0000000000001p-1023' -- a
# different number, and the round trip did not close.
for v in (5e-324, -5e-324, 1e-320, 1.1125369292536007e-308,
          2.2250738585072014e-308, 2.225073858507201e-308,
          1e-310, 4e-323, 0.0, -0.0):
    h = v.hex()
    print(h, float.fromhex(h) == v)
