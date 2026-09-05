#!/bin/bash
# Float benchmarks: apython against CPython, on the same .pyc, startup
# subtracted.  Reported, not gated -- a perf claim should be reproducible
# rather than trusted.
#
# Every loop is INSIDE A FUNCTION on purpose.  The same loop written at module
# scope measures LOAD_NAME/STORE_NAME dict lookups, not float arithmetic: at
# module level `x = x + 1.5` reads 0.79x of CPython, and in a function it reads
# 2.17x.  Both numbers are real; only the second one is about floats.
#
# Usage: bash tests/run_float_bench.sh
set -u
cd "$(dirname "$0")/.."
APYTHON=${APYTHON:-./apython}
PYTHON=${PYTHON:-python3}
BD=${TMPDIR:-/tmp}/apython-float-bench
mkdir -p "$BD"
mk() { cat > "$BD/$1.py"; }
echo "pass" > "$BD/empty.py"

mk g_add <<'EOF'
def run(n):
    x = 0.0
    i = 0
    while i < n:
        x = x + 1.5
        i += 1
    return x
print(round(run(3000000), 2))
EOF

mk g_mul <<'EOF'
def run(n):
    x = 1.0
    i = 0
    while i < n:
        x = x * 1.0000001
        i += 1
    return x
print(round(run(3000000), 6))
EOF

mk g_div <<'EOF'
def run(n):
    x = 1e300
    i = 0
    while i < n:
        x = x / 1.0000001
        i += 1
    return x
print(round(run(3000000), 2))
EOF

mk g_cmp <<'EOF'
def run(n):
    x = 0.5
    c = 0
    i = 0
    while i < n:
        if x < 1.0:
            c += 1
        i += 1
    return c
print(run(3000000))
EOF

mk g_inplace <<'EOF'
def run(n):
    x = 0.0
    i = 0
    while i < n:
        x += 1.5
        i += 1
    return x
print(round(run(3000000), 2))
EOF

mk g_neg <<'EOF'
def run(n):
    x = 1.5
    s = 0.0
    i = 0
    while i < n:
        s = s + (-x)
        i += 1
    return s
print(round(run(3000000), 2))
EOF

mk g_mixed <<'EOF'
def run(n):
    x = 1.0
    i = 0
    while i < n:
        x = x * 2 + 1
        x = x / 2 - 1
        i += 1
    return x
print(round(run(2000000), 6))
EOF

mk g_poly <<'EOF'
def poly(x):
    return ((((3.1 * x + 2.2) * x + 1.3) * x + 0.4) * x + 5.5)
def run(n):
    s = 0.0
    i = 0
    while i < n:
        s += poly(1.0001)
        i += 1
    return s
print(round(run(1000000), 2))
EOF

mk g_abs <<'EOF'
def run(n):
    s = 0.0
    i = 0
    while i < n:
        s += abs(-1.5) + abs(2.5)
        i += 1
    return s
print(round(run(2000000), 2))
EOF

mk g_sqrt <<'EOF'
import math
def run(n):
    sq = math.sqrt
    s = 0.0
    i = 1
    while i < n:
        s += sq(i)
        i += 1
    return s
print(round(run(1000000), 3))
EOF

mk g_floor <<'EOF'
import math
def run(n):
    fl = math.floor
    s = 0
    i = 0
    while i < n:
        s += fl(i * 1.5)
        i += 1
    return s
print(run(1000000))
EOF

mk g_trig <<'EOF'
import math
def run(n):
    sn = math.sin
    s = 0.0
    i = 0
    while i < n:
        s += sn(i * 0.001)
        i += 1
    return s
print(round(run(500000), 6))
EOF

mk g_round <<'EOF'
def run(n):
    s = 0.0
    i = 0
    while i < n:
        s += round(i * 1.11111, 2)
        i += 1
    return s
print(round(run(1000000), 2))
EOF

mk g_round0 <<'EOF'
def run(n):
    s = 0
    i = 0
    while i < n:
        s += round(i * 1.11111)
        i += 1
    return s
print(run(1000000))
EOF

mk g_repr <<'EOF'
def run(n):
    vals = [1.0, 0.1, 3.141592653589793, 1e300, 2.5e-8, -0.0, 1234567.125]
    t = 0
    for r in range(n):
        for v in vals:
            t += len(repr(v))
    return t
print(run(20000))
EOF

mk g_parse <<'EOF'
def run(n):
    strs = ["1.0", "0.1", "3.141592653589793", "1e300", "2.5e-8", "-0.0"]
    s = 0.0
    for r in range(n):
        for t in strs:
            s += float(t)
    return s
print(round(run(20000), 6))
EOF

mk g_tofloat <<'EOF'
def run(n):
    s = 0.0
    i = 0
    while i < n:
        s += float(i)
        i += 1
    return s
print(round(run(2000000), 1))
EOF

mk g_toint <<'EOF'
def run(n):
    s = 0
    x = 1.5
    i = 0
    while i < n:
        s += int(x)
        i += 1
    return s
print(run(2000000))
EOF

mk g_sum <<'EOF'
def run(n):
    a = [float(i) for i in range(1000)]
    s = 0.0
    for r in range(n):
        s += sum(a)
    return s
print(round(run(3000), 1))
EOF

mk g_listiter <<'EOF'
def run(n):
    a = [float(i) for i in range(1000)]
    s = 0.0
    for r in range(n):
        for v in a:
            s += v
    return s
print(round(run(2000), 1))
EOF

mk g_pow <<'EOF'
def run(n):
    s = 0.0
    i = 1
    while i < n:
        s += (i * 1.0) ** 0.5
        i += 1
    return s
print(round(run(500000), 3))
EOF

mk g_divmod <<'EOF'
def run(n):
    s = 0.0
    i = 1
    while i < n:
        s += (i * 1.5) % 3.0
        i += 1
    return s
print(round(run(1000000), 2))
EOF

BENCHES="g_add g_mul g_div g_cmp g_inplace g_neg g_mixed g_poly g_abs \
         g_sqrt g_floor g_trig g_round g_round0 g_repr g_parse \
         g_tofloat g_toint g_sum g_listiter g_pow g_divmod"

for b in $BENCHES empty; do "$PYTHON" -m py_compile "$BD/$b.py"; done

timeit() {
    local best=999999 t
    for i in 1 2 3 4 5; do
        local s=$(date +%s%N); "$@" >/dev/null 2>&1; local e=$(date +%s%N)
        t=$(( (e-s)/1000000 )); [ "$t" -lt "$best" ] && best=$t
    done
    echo "$best"
}

EPYC=$(ls "$BD/__pycache__/empty."*.pyc)
SA=$(timeit "$APYTHON" "$EPYC"); SC=$(timeit "$PYTHON" "$EPYC")

echo "=== agreement ==="
for b in $BENCHES; do
    PYC=$(ls "$BD/__pycache__/$b."*.pyc)
    e=$("$PYTHON" "$PYC" 2>&1); a=$("$APYTHON" "$PYC" 2>&1)
    [ "$e" == "$a" ] || echo "  DIFF $b: cpython=[$e] apython=[$a]"
done
echo "  (nothing above means all agree)"
echo
echo "=== float in functions (startup-subtracted ms; >1.00x = apython faster) ==="
printf "%-12s %9s %9s %8s\n" "benchmark" "apython" "cpython" "speedup"
TA=0; TC=0
for b in $BENCHES; do
    PYC=$(ls "$BD/__pycache__/$b."*.pyc)
    a=$(timeit "$APYTHON" "$PYC"); c=$(timeit "$PYTHON" "$PYC")
    a=$((a-SA)); c=$((c-SC)); [ "$a" -lt 1 ] && a=1; [ "$c" -lt 1 ] && c=1
    TA=$((TA+a)); TC=$((TC+c))
    printf "%-12s %9s %9s %8s\n" "$b" "$a" "$c" "$("$PYTHON" -c "print('%.2fx'%($c/$a))")"
done
echo
printf "TOTAL apython=%sms cpython=%sms overall=%s\n" "$TA" "$TC" \
       "$("$PYTHON" -c "print('%.2fx'%($TC/$TA))")"
