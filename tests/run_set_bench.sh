#!/bin/bash
# Set benchmarks: construction, membership, mutation, the seven operators and
# their method forms, iteration, and frozenset.  apython against CPython on
# the same .pyc, startup subtracted, best of five.  Reported, not gated.
#
# The eighth harness, after int, float, str, list, dict, core and macro.
# Between them those seven had ZERO set cases -- the seven case names that
# contain "set" are all about dict, attribute or list *setting*.  The one
# macro benchmark that leans on sets, m_nqueens, describes itself in its own
# header comment as "recursion, list append/pop and short-circuit boolean",
# so nobody was looking; callgrind puts 23% of it in set.asm, and 15% in the
# probe loop alone.  Lists were 0.61x and dicts 0.73x when their harnesses
# were first run.
#
# Same discipline as the other seven.  Every loop is INSIDE A FUNCTION: at
# module scope the same code measures LOAD_NAME's three-dict walk rather than
# the set operation it is named after.  Both interpreters run the identical
# .pyc, so the source compiler is not in the measurement.  The run diffs both
# interpreters' OUTPUT on every case before it times anything -- an operation
# that computes the wrong answer is usually faster.
#
# TWO TRAPS THAT ARE SPECIFIC TO SETS.  No case prints a hash: apython uses
# unseeded FNV-1a where CPython uses randomized siphash13.  And no case
# prints a SET -- iteration order is slot order, and the two implementations'
# tables have different capacities and different probe sequences, so even a
# correct set prints differently.  Every case prints `sorted(...)`, a length
# or a checksum instead.  The corpus comes from a seeded LCG rather than
# `random`, so it is the same on every run and on every machine.
#
# Usage: bash tests/run_set_bench.sh [case ...]
set -u
cd "$(dirname "$0")/.."
BD=${TMPDIR:-/tmp}/apython-set-bench
mkdir -p "$BD"
mk() { cat > "$BD/$1.py"; }
echo "pass" > "$BD/empty.py"

# ---- construction ----------------------------------------------------------
mk st_empty <<'EOF'
def run(n):
    c = 0
    i = 0
    while i < n:
        s = set()
        c += len(s)
        i += 1
    return c
print(run(2000000))
EOF

mk st_literal <<'EOF'
def run(n):
    c = 0
    i = 0
    while i < n:
        s = {1, 2, 3, 4, 5}
        c += len(s)
        i += 1
    return c
print(run(500000))
EOF

mk st_literal_big <<'EOF'
def run(n):
    c = 0
    i = 0
    while i < n:
        s = {0, 1, 2, 3, 4, 5, 6, 7, 8, 9,
             10, 11, 12, 13, 14, 15, 16, 17, 18, 19}
        c += len(s)
        i += 1
    return c
print(run(150000))
EOF

mk st_literal_str <<'EOF'
def run(n):
    c = 0
    i = 0
    while i < n:
        s = {"alpha", "beta", "gamma", "delta"}
        c += len(s)
        i += 1
    return c
print(run(400000))
EOF

mk st_from_list <<'EOF'
def run(n):
    src = list(range(100))
    c = 0
    for _ in range(n):
        c += len(set(src))
    return c
print(run(20000))
EOF

mk st_from_tuple <<'EOF'
def run(n):
    src = tuple(range(100))
    c = 0
    for _ in range(n):
        c += len(set(src))
    return c
print(run(20000))
EOF

mk st_from_range <<'EOF'
def run(n):
    c = 0
    for _ in range(n):
        c += len(set(range(100)))
    return c
print(run(20000))
EOF

mk st_from_str <<'EOF'
def run(n):
    s = "abcdefghij" * 10
    c = 0
    for _ in range(n):
        c += len(set(s))
    return c
print(run(40000))
EOF

mk st_from_set <<'EOF'
def run(n):
    src = set(range(100))
    c = 0
    for _ in range(n):
        c += len(set(src))
    return c
print(run(20000))
EOF

mk st_from_gen <<'EOF'
def run(n):
    c = 0
    for _ in range(n):
        c += len(set(x for x in range(100)))
    return c
print(run(10000))
EOF

mk st_comp <<'EOF'
def run(n):
    c = 0
    for _ in range(n):
        s = {i * 2 for i in range(100)}
        c += len(s)
    return c
print(run(20000))
EOF

mk st_comp_str <<'EOF'
def run(n):
    keys = ["key%03d" % i for i in range(100)]
    c = 0
    for _ in range(n):
        s = {k for k in keys}
        c += len(s)
    return c
print(run(15000))
EOF

mk st_starstar <<'EOF'
def run(n):
    a = set(range(50))
    b = set(range(50, 100))
    c = 0
    for _ in range(n):
        c += len({*a, *b})
    return c
print(run(15000))
EOF

mk st_grow <<'EOF'
def run(n):
    c = 0
    for _ in range(n):
        s = set()
        for i in range(1000):
            s.add(i)
        c += len(s)
    return c
print(run(1500))
EOF

mk st_frozen_new <<'EOF'
def run(n):
    src = list(range(50))
    c = 0
    for _ in range(n):
        c += len(frozenset(src))
    return c
print(run(30000))
EOF

# ---- membership ------------------------------------------------------------
mk st_in_hit <<'EOF'
def run(n):
    s = set(range(100))
    c = 0
    i = 0
    while i < n:
        if i % 100 in s:
            c += 1
        i += 1
    return c
print(run(2000000))
EOF

mk st_in_miss <<'EOF'
def run(n):
    s = set(range(100))
    c = 0
    i = 0
    while i < n:
        if i + 1000 in s:
            c += 1
        i += 1
    return c
print(run(2000000))
EOF

mk st_in_small <<'EOF'
def run(n):
    s = {2, 4, 6}
    c = 0
    i = 0
    while i < n:
        if i % 8 in s:
            c += 1
        i += 1
    return c
print(run(2000000))
EOF

mk st_in_str <<'EOF'
def run(n):
    s = {"alpha", "beta", "gamma", "delta", "epsilon"}
    c = 0
    i = 0
    while i < n:
        if "gamma" in s:
            c += 1
        if "zeta" in s:
            c += 1
        i += 1
    return c
print(run(1000000))
EOF

mk st_in_str_dyn <<'EOF'
def run(n):
    keys = ["key%03d" % i for i in range(100)]
    s = set(keys)
    c = 0
    for _ in range(n):
        for k in keys:
            if k in s:
                c += 1
    return c
print(run(6000))
EOF

mk st_in_tuple <<'EOF'
def run(n):
    keys = [(i, i + 1) for i in range(100)]
    s = set(keys)
    c = 0
    for _ in range(n):
        for k in keys:
            if k in s:
                c += 1
    return c
print(run(4000))
EOF

mk st_in_obj <<'EOF'
class K:
    def __init__(self, v):
        self.v = v

    def __hash__(self):
        return self.v * 2654435761 & 0xFFFFFFFF

    def __eq__(self, o):
        return isinstance(o, K) and self.v == o.v


def run(n):
    keys = [K(i) for i in range(100)]
    s = set(keys)
    c = 0
    for _ in range(n):
        for k in keys:
            if k in s:
                c += 1
    return c
print(run(2000))
EOF

mk st_in_collide <<'EOF'
def run(n):
    # Every key shares its low bits, so a table masked to a power of two
    # sends all of them to one slot and the probe sequence IS the
    # measurement.  No benchmark anywhere had a case like this for sets.
    s = set()
    for i in range(200):
        s.add(i * 4096)
    c = 0
    for _ in range(n):
        for i in range(200):
            if i * 4096 in s:
                c += 1
    return c
print(run(2000))
EOF

mk st_in_strided <<'EOF'
def run(n):
    s = set()
    for i in range(1000):
        s.add(i * 8)
    c = 0
    for _ in range(n):
        for i in range(1000):
            if i * 8 in s:
                c += 1
    return c
print(run(600))
EOF

mk st_in_bigint <<'EOF'
def run(n):
    base = 2 ** 60
    s = set(base + i for i in range(200))
    c = 0
    for _ in range(n):
        for i in range(200):
            if base + i in s:
                c += 1
    return c
print(run(2000))
EOF

mk st_in_float <<'EOF'
def run(n):
    s = set(i + 0.5 for i in range(500))
    c = 0
    for _ in range(n):
        for i in range(500):
            if i + 0.5 in s:
                c += 1
    return c
print(run(1200))
EOF

# ---- mutation --------------------------------------------------------------
mk st_add_new <<'EOF'
def run(n):
    c = 0
    for _ in range(n):
        s = set()
        for i in range(100):
            s.add(i)
        c += len(s)
    return c
print(run(20000))
EOF

mk st_add_dup <<'EOF'
def run(n):
    s = set(range(100))
    for _ in range(n):
        for i in range(100):
            s.add(i)
    return len(s)
print(run(15000))
EOF

mk st_churn <<'EOF'
def run(n):
    # add-then-discard over a narrow range: the shape m_nqueens has, and the
    # one that fills a table with tombstones.  A growth rule that sizes from
    # the capacity rather than from the live count doubles forever here.
    s = set(range(20))
    c = 0
    i = 0
    while i < n:
        k = i % 20
        s.discard(k)
        s.add(k)
        c += 1
        i += 1
    return c + len(s)
print(run(1000000))
EOF

mk st_discard <<'EOF'
def run(n):
    c = 0
    for _ in range(n):
        s = set(range(100))
        for i in range(100):
            s.discard(i)
        c += len(s)
    return c
print(run(10000))
EOF

mk st_remove <<'EOF'
def run(n):
    c = 0
    for _ in range(n):
        s = set(range(100))
        for i in range(100):
            s.remove(i)
        c += len(s)
    return c
print(run(10000))
EOF

mk st_pop <<'EOF'
def run(n):
    c = 0
    for _ in range(n):
        s = set(range(100))
        while s:
            s.pop()
        c += len(s)
    return c
print(run(8000))
EOF

mk st_clear <<'EOF'
def run(n):
    s = set(range(100))
    c = 0
    for _ in range(n):
        s.clear()
        for i in range(10):
            s.add(i)
        c += len(s)
    return c
print(run(50000))
EOF

mk st_update <<'EOF'
def run(n):
    src = set(range(100))
    c = 0
    for _ in range(n):
        s = set()
        s.update(src)
        c += len(s)
    return c
print(run(15000))
EOF

mk st_update_iter <<'EOF'
def run(n):
    src = list(range(100))
    c = 0
    for _ in range(n):
        s = set()
        s.update(src)
        c += len(s)
    return c
print(run(15000))
EOF

# ---- the operators and their method forms ---------------------------------
mk st_union <<'EOF'
def run(n):
    a = set(range(50))
    b = set(range(50, 100))
    c = 0
    for _ in range(n):
        c += len(a | b)
    return c
print(run(15000))
EOF

mk st_union_m <<'EOF'
def run(n):
    a = set(range(50))
    b = list(range(50, 100))
    c = 0
    for _ in range(n):
        c += len(a.union(b))
    return c
print(run(15000))
EOF

mk st_inter <<'EOF'
def run(n):
    a = set(range(100))
    b = set(range(50, 150))
    c = 0
    for _ in range(n):
        c += len(a & b)
    return c
print(run(15000))
EOF

mk st_inter_lopsided <<'EOF'
def run(n):
    # The case that decides whether the smaller operand is iterated.
    big = set(range(5000))
    small = set(range(3))
    c = 0
    for _ in range(n):
        c += len(big & small)
    return c
print(run(20000))
EOF

mk st_diff <<'EOF'
def run(n):
    a = set(range(100))
    b = set(range(50, 150))
    c = 0
    for _ in range(n):
        c += len(a - b)
    return c
print(run(15000))
EOF

mk st_symdiff <<'EOF'
def run(n):
    a = set(range(100))
    b = set(range(50, 150))
    c = 0
    for _ in range(n):
        c += len(a ^ b)
    return c
print(run(8000))
EOF

mk st_issubset <<'EOF'
def run(n):
    a = set(range(50))
    b = set(range(100))
    c = 0
    i = 0
    while i < n:
        if a <= b:
            c += 1
        i += 1
    return c
print(run(30000))
EOF

mk st_issubset_lopsided <<'EOF'
def run(n):
    # A size check answers this without looking at an element.
    big = set(range(5000))
    small = set(range(3))
    c = 0
    i = 0
    while i < n:
        if big <= small:
            c += 1
        i += 1
    return c
print(run(200000))
EOF

mk st_issuperset <<'EOF'
def run(n):
    a = set(range(100))
    b = set(range(50))
    c = 0
    i = 0
    while i < n:
        if a >= b:
            c += 1
        i += 1
    return c
print(run(30000))
EOF

mk st_isdisjoint <<'EOF'
def run(n):
    big = set(range(5000))
    small = set(range(-3, 0))
    c = 0
    i = 0
    while i < n:
        if big.isdisjoint(small):
            c += 1
        i += 1
    return c
print(run(20000))
EOF

mk st_eq <<'EOF'
def run(n):
    a = set(range(100))
    b = set(range(100))
    c = 0
    i = 0
    while i < n:
        if a == b:
            c += 1
        i += 1
    return c
print(run(30000))
EOF

mk st_ior <<'EOF'
def run(n):
    b = set(range(50, 100))
    c = 0
    for _ in range(n):
        s = set(range(50))
        s |= b
        c += len(s)
    return c
print(run(10000))
EOF

mk st_iand <<'EOF'
def run(n):
    b = set(range(50, 150))
    c = 0
    for _ in range(n):
        s = set(range(100))
        s &= b
        c += len(s)
    return c
print(run(10000))
EOF

mk st_isub <<'EOF'
def run(n):
    b = set(range(50, 150))
    c = 0
    for _ in range(n):
        s = set(range(100))
        s -= b
        c += len(s)
    return c
print(run(10000))
EOF

# ---- iteration and the rest ------------------------------------------------
mk st_iter <<'EOF'
def run(n):
    s = set(range(1000))
    t = 0
    for _ in range(n):
        for x in s:
            t += x
    return t
print(run(1500))
EOF

mk st_iter_small <<'EOF'
def run(n):
    s = {1, 2, 3}
    t = 0
    i = 0
    while i < n:
        for x in s:
            t += x
        i += 1
    return t
print(run(300000))
EOF

mk st_len <<'EOF'
def run(n):
    s = set(range(100))
    t = 0
    i = 0
    while i < n:
        t += len(s)
        i += 1
    return t
print(run(2000000))
EOF

mk st_copy <<'EOF'
def run(n):
    s = set(range(100))
    c = 0
    for _ in range(n):
        c += len(s.copy())
    return c
print(run(20000))
EOF

mk st_repr <<'EOF'
def run(n):
    s = set(range(20))
    c = 0
    i = 0
    while i < n:
        c += len(repr(s))
        i += 1
    return c
print(run(60000))
EOF

mk st_sorted <<'EOF'
def run(n):
    s = set((i * 37) % 200 for i in range(200))
    c = 0
    for _ in range(n):
        c += len(sorted(s))
    return c
print(run(4000))
EOF

mk st_sum <<'EOF'
def run(n):
    s = set(range(1000))
    t = 0
    for _ in range(n):
        t += sum(s)
    return t
print(run(1500))
EOF

mk st_frozen_hash <<'EOF'
def run(n):
    f = frozenset(range(100))
    c = 0
    i = 0
    while i < n:
        c += 1 if hash(f) else 1
        i += 1
    return c
print(run(200000))
EOF

mk st_frozen_key <<'EOF'
def run(n):
    keys = [frozenset(range(i, i + 5)) for i in range(50)]
    d = {}
    for k in keys:
        d[k] = 1
    c = 0
    for _ in range(n):
        for k in keys:
            c += d[k]
    return c
print(run(3000))
EOF

mk st_frozen_in <<'EOF'
def run(n):
    f = frozenset(range(100))
    c = 0
    i = 0
    while i < n:
        if i % 100 in f:
            c += 1
        i += 1
    return c
print(run(2000000))
EOF

BENCHES="st_empty st_literal st_literal_big st_literal_str st_from_list \
         st_from_tuple st_from_range st_from_str st_from_set st_from_gen \
         st_comp st_comp_str st_starstar st_grow st_frozen_new \
         st_in_hit st_in_miss st_in_small st_in_str st_in_str_dyn \
         st_in_tuple st_in_obj st_in_collide st_in_strided st_in_bigint \
         st_in_float \
         st_add_new st_add_dup st_churn st_discard st_remove st_pop \
         st_clear st_update st_update_iter \
         st_union st_union_m st_inter st_inter_lopsided st_diff st_symdiff \
         st_issubset st_issubset_lopsided st_issuperset st_isdisjoint st_eq \
         st_ior st_iand st_isub \
         st_iter st_iter_small st_len st_copy st_repr st_sorted st_sum \
         st_frozen_hash st_frozen_key st_frozen_in"

[ $# -gt 0 ] && BENCHES="$*"

for b in $BENCHES empty; do python3 -m py_compile "$BD/$b.py"; done

timeit() {
    local best=999999 t i s e
    for i in 1 2 3 4 5; do
        s=$(date +%s%N); "$@" >/dev/null 2>&1; e=$(date +%s%N)
        t=$(( (e-s)/1000000 )); [ "$t" -lt "$best" ] && best=$t
    done
    echo "$best"
}

EPYC=$(ls "$BD/__pycache__/empty."*.pyc)
SA=$(timeit ./apython "$EPYC"); SC=$(timeit python3 "$EPYC")

echo "=== agreement ==="
bad=0
for b in $BENCHES; do
    PYC=$(ls "$BD/__pycache__/$b."*.pyc)
    e=$(python3 "$PYC" 2>&1); a=$(./apython "$PYC" 2>&1)
    if [ "$e" != "$a" ]; then echo "  DIFF $b: cpython=[$e] apython=[$a]"; bad=1; fi
done
[ "$bad" -eq 0 ] && echo "  (nothing above means all agree)"
echo
echo "=== sets (startup-subtracted ms; >1.00x = apython faster) ==="
printf "%-20s %9s %9s %8s\n" "benchmark" "apython" "cpython" "speedup"
TA=0; TC=0
for b in $BENCHES; do
    PYC=$(ls "$BD/__pycache__/$b."*.pyc)
    a=$(timeit ./apython "$PYC"); c=$(timeit python3 "$PYC")
    a=$((a-SA)); c=$((c-SC)); [ "$a" -lt 1 ] && a=1; [ "$c" -lt 1 ] && c=1
    TA=$((TA+a)); TC=$((TC+c))
    printf "%-20s %9s %9s %8s\n" "$b" "$a" "$c" "$(python3 -c "print('%.2fx'%($c/$a))")"
done
echo
printf "TOTAL apython=%sms cpython=%sms overall=%s\n" "$TA" "$TC" \
       "$(python3 -c "print('%.2fx'%($TC/$TA))")"
