# Float formatting, compared against CPython value by value.
#
# repr() of a float has to be the SHORTEST decimal string that reads back as
# the same double -- not the correctly rounded one at some precision.  The two
# are different problems and they disagree: for 6.256509672447191e-148 the
# correctly rounded sixteen-digit form is 6.25650967244719e-148, which does
# not round-trip, while the shortest sixteen-digit form ends in 1 and does.
# A repr built out of printf can only see the first, and four of the 2098
# powers of two came out with seventeen digits where CPython prints sixteen.
#
# So this file exists to compare the text, not to assert properties of it.
# Every line it prints is an answer CPython also has to produce; the harness
# diffs the two runs.  It uses no `struct` -- there is no such module here --
# and no unseeded randomness, so both interpreters walk the identical values.

import math


class Rand:
    """xorshift64: the same stream on both sides, with no random module."""

    def __init__(self, seed):
        self.s = seed

    def next(self):
        x = self.s
        x ^= (x << 13) & 0xFFFFFFFFFFFFFFFF
        x ^= x >> 7
        x ^= (x << 17) & 0xFFFFFFFFFFFFFFFF
        self.s = x
        return x


def from_bits(sign, exp, mant):
    """A normal double from its fields, spelled as a hex literal."""
    return float.fromhex('%s0x1.%013xp%+d' % ('-' if sign else '', mant, exp))


def values():
    v = [
        0.0, -0.0, 1.0, -1.0, 0.5, 2.0, 10.0, 100.0, 1e16, 1e17, 1e-5,
        0.1, 0.2, 0.3, 1.0 / 3.0, 2.0 / 3.0, math.pi, math.e,
        1e300, 1e-300, 1.7976931348623157e308, 5e-324,
        2.2250738585072014e-308, 1234567.125, 9007199254740992.0,
        1e15, 123456789012345.6, 0.30000000000000004, 1e22, 1e23,
        1.5e-323, 2.5e-323, 0.1 + 0.2, 1e16 + 2.0,
    ]

    # Decimal powers of ten across the range.
    e = -300
    while e <= 300:
        v.append(float('1e%d' % e))
        e += 3

    # Every power of two, subnormals included.  This is where the bug lived:
    # four of these printed seventeen digits instead of sixteen.
    e = -1074
    while e < 1024:
        v.append(math.ldexp(1.0, e))
        e += 1

    # Random normals over the whole exponent range.
    rng = Rand(0x2545F4914F6CDD1D)
    for _ in range(4000):
        r = rng.next()
        try:
            v.append(from_bits((r >> 63) & 1, ((r >> 52) % 700) - 350,
                               r & 0xFFFFFFFFFFFFF))
        except (ValueError, OverflowError):
            pass

    # Values with short decimal forms, where a fast path would live.
    for i in range(1, 800):
        v.append(float(i))
        v.append(i / 7.0)
        v.append(i * 1.11111)
        v.append(i * 0.001)
        v.append(-i / 3.0)
    return v


def main():
    out = []
    vals = values()

    for x in vals:
        r = repr(x)
        out.append(r)
        if str(x) != r:
            out.append("STR-DIFFERS " + str(x))
        # The property that makes a repr correct at all.
        if float(r) != x:
            out.append("ROUNDTRIP-FAIL " + r)

    for x in vals[:2500]:
        for nd in (0, 1, 2, 5, 10, 17, -1, -2):
            try:
                out.append(repr(round(x, nd)))
            except (OverflowError, ValueError) as exc:
                out.append("round-err " + type(exc).__name__)
        try:
            out.append(repr(round(x)))
        except (OverflowError, ValueError) as exc:
            out.append("round0-err " + type(exc).__name__)

    for x in vals[:1500]:
        for spec in ("", ".0f", ".2f", ".6f", ".17g", "g", "e", ".3e",
                     "%", ".0e", ".15g", "20.5f", "<20.3f", "+.4f"):
            try:
                out.append(format(x, spec))
            except (ValueError, OverflowError) as exc:
                out.append("fmt-err " + type(exc).__name__)

    # Parsing is the other half of the same machinery.
    for x in vals[:2500]:
        t = repr(x)
        out.append(repr(float(t)))
        out.append(repr(float(t.upper())))

    # The specials, which take their own paths.
    for t in ("inf", "-inf", "nan", "-nan", "Infinity", "-Infinity",
              "  1.5  ", "1_000.5", "0x1.8p+1"):
        try:
            out.append(repr(float(t)))
        except ValueError:
            out.append("parse-err " + t.strip())
    for x in (float('inf'), float('-inf'), float('nan')):
        out.append(repr(x))
        out.append(str(x))
        out.append(format(x, '.3f'))

    print("\n".join(out))
    print("VALUES", len(vals))


main()
