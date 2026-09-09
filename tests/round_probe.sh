#!/bin/bash
# round(x, ndigits), over a corpus far larger than `make check` can afford.
#
# float_round_ndigits has an exact integer fast path -- 0 <= ndigits <= 22 and
# a result numerator inside 53 bits -- and a rendering path behind it for
# everything else.  Two paths answering one question is exactly the shape that
# wants volume: a bound that is one out only shows up on the values that sit
# against it, and there is no way to reach those by writing test cases.
#
# ~1.4 million (value, ndigits) pairs.  The values are every seventh binade in
# three variants, 4,000 random bit patterns from a seeded xorshift, 16,000
# "money-shaped" decimals -- which is what round() is actually used on -- and
# the constructed traps: 2.675, 0.125, 9.995, 99.5, the subnormal floor, the
# largest finite double.  ndigits runs -30..30 plus the values around
# CPython's NDIGITS_MIN and NDIGITS_MAX, where the answer is settled without
# looking at the digits.
#
# CPython is the oracle, compared through repr() -- which tests/dtoa_probe.sh
# already gates -- because apython has no `struct` to hex a double with.  Zero
# differences are allowed.
#
# Usage: bash tests/round_probe.sh
set -u
cd "$(dirname "$0")/.."
BD=${TMPDIR:-/tmp}/apython-round-probe
mkdir -p "$BD"

cat > "$BD/corpus.py" <<'EOF'
import math


class Xorshift:
    """Seeded, so the corpus is the same on every run and on every machine."""

    def __init__(self, seed):
        self.s = seed

    def next(self):
        x = self.s
        x ^= (x << 13) & 0xFFFFFFFFFFFFFFFF
        x ^= x >> 7
        x ^= (x << 17) & 0xFFFFFFFFFFFFFFFF
        self.s = x
        return x


vals = []

# The traps.  2.675 is the one that settles whether the exact value or the
# shortest decimal decides; 0.125 and the halves are exact ties; 9.995 and
# 99.5 are the decade boundaries that a one-digit rendering gets wrong.
vals += [2.675, -2.675, 0.125, -0.125, 0.5, 1.5, 2.5, 3.5, -0.5, -2.5,
         1.005, 9.995, 99.5, 0.1, 0.2, 0.3, 0.30000000000000004,
         1.0000000000000002, 1234567.125, 123456789.987654321,
         1e-8, 1e-5, 0.0001, -0.0001, 1e16, 1e17, 1e22, 1e23,
         9007199254740993.0, 2.2250738585072014e-308, 5e-324, -5e-324,
         1.7976931348623157e308, 1e-300, 1e300, 0.0, -0.0,
         3.141592653589793, 2.718281828459045]

# Every seventh binade, and its two neighbours in the last bit -- the values
# that sit against a shift bound rather than in the middle of one.
for e in range(-1074, 1024, 7):
    v = math.ldexp(1.0, e)
    vals.append(v)
    vals.append(math.ldexp(1.5, e))
    vals.append(-v)

r = Xorshift(0x2545F4914F6CDD1D)
n = 0
while n < 4000:
    b = r.next()
    e = (b >> 52) & 0x7FF
    if e == 0x7FF or e == 0:
        continue
    vals.append(float.fromhex('%s0x1.%013xp%+d' % (
        '-' if (b >> 63) else '', b & ((1 << 52) - 1), e - 1023)))
    n += 1

# What round() is actually called on: two decimal places of money, three of a
# measurement, and values spread over the range where the fast path lives.
for i in range(4000):
    k = r.next()
    vals.append(((k % 2000000001) - 1000000000) / 100.0)
    vals.append(((k % 2000000000001) - 1000000000000) / 1000.0)
    vals.append((k % 1000000000) * 1e-3 - 500000.0)
    vals.append((k % 1000000000) * 1e-12)

digits = list(range(-30, 31)) + [-330, -309, -308, 100, 323, 324, 400]

out = []
for v in vals:
    for d in digits:
        try:
            out.append(repr(round(v, d)))
        except OverflowError:
            out.append("OVF")

print(len(out))
print("\n".join(out))
EOF

python3 -m py_compile "$BD/corpus.py"
PYC=$(ls "$BD/__pycache__/corpus."*.pyc)

python3 "$PYC" > "$BD/expected.txt" 2>&1
./apython "$PYC" > "$BD/actual.txt" 2>&1

n=$(head -1 "$BD/expected.txt")
if diff -q "$BD/expected.txt" "$BD/actual.txt" >/dev/null; then
    echo -e "round: $n cases, 0 differing  \033[0;32mPASS\033[0m"
    exit 0
fi
d=$(diff "$BD/expected.txt" "$BD/actual.txt" | grep -c '^<')
echo -e "round: $n cases, $d differing  \033[0;31mFAIL\033[0m"
diff "$BD/expected.txt" "$BD/actual.txt" | head -20
exit 1
