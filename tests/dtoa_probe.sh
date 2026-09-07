#!/bin/bash
# repr(float), over a corpus far larger than `make check` can afford.
#
# The shortest-decimal conversion in src/dtoa.asm answers a question with one
# right answer per input, and the whole of it is arithmetic on table entries --
# so the way to have confidence in it is volume, and the way to have volume is
# to keep it out of the suite that runs on every build.
#
# 141,000 doubles: every power of two from the smallest subnormal to the
# largest normal in three variants, 120,000 random bit patterns from a seeded
# xorshift, and 15,000 constructed values that concentrate on short decimals
# and on the decade boundaries.  CPython is the oracle, compared string for
# string; zero differences are allowed, on the precedent tests/re_floor.txt
# sets.
#
# Usage: bash tests/dtoa_probe.sh
set -u
cd "$(dirname "$0")/.."
BD=${TMPDIR:-/tmp}/apython-dtoa-probe
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


out = []
r = Xorshift(0x9E3779B97F4A7C15)
n = 0
while n < 120000:
    b = r.next()
    e = (b >> 52) & 0x7FF
    if e == 0x7FF:                      # the infinities and the NaNs
        continue
    if e == 0:
        continue                        # subnormals come from ldexp below
    out.append(float.fromhex('%s0x1.%013xp%+d' % (
        '-' if (b >> 63) else '', b & ((1 << 52) - 1), e - 1023)))
    n += 1

for e in range(-1074, 1024):            # every binade, and the subnormals
    out.append(math.ldexp(1.0, e))
    out.append(math.ldexp(-1.0, e))
    out.append(math.ldexp(1.5, e))

for i in range(1, 3000):                # short decimals and the decades
    out.append(float(i))
    out.append(i / 7.0)
    out.append(i * 1e-30)
    out.append(i * 1e30)
    out.append(1.0 / i)

out += [0.0, -0.0, 5e-324, -5e-324, 1.7976931348623157e308,
        2.0 ** -24, 6.256509672447191e-148, 9.995, 99.5, 0.1, 1e16, 1e-5]

print(len(out))
for x in out:
    print(repr(x))
EOF

python3 -m py_compile "$BD/corpus.py"
PYC=$(ls "$BD/__pycache__/corpus."*.pyc)

python3 "$PYC" > "$BD/expected.txt" 2>&1
./apython "$PYC" > "$BD/actual.txt" 2>&1

n=$(head -1 "$BD/expected.txt")
if diff -q "$BD/expected.txt" "$BD/actual.txt" >/dev/null; then
    echo -e "dtoa: $n values, 0 differing  \033[0;32mPASS\033[0m"
    exit 0
fi
d=$(diff "$BD/expected.txt" "$BD/actual.txt" | grep -c '^<')
echo -e "dtoa: $n values, $d differing  \033[0;31mFAIL\033[0m"
diff "$BD/expected.txt" "$BD/actual.txt" | head -20
exit 1
