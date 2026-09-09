#!/bin/bash
# Dict benchmarks: construction, lookup, mutation, deletion, the views, and
# the interpreter machinery dicts are the substrate for.  apython against
# CPython on the same .pyc, startup subtracted, best of five.  Reported, not
# gated.
#
# The seventh harness, after int, float, str, list, core and macro.  Between
# them those six had seven dict cases, five of which read 1.4x or better, so
# dicts looked fine -- which is exactly how lists looked before the list
# harness found 0.61x.  All seven measure a warm read of a table keyed by
# contiguous integers, which is the one shape linear probing makes
# collision-free.  Nothing measured deletion, the views, copy, update, merge,
# tuple or object keys, a table under collision pressure, or **kwargs.
#
# Same discipline as the other six.  Every loop is INSIDE A FUNCTION: at
# module scope the same code measures LOAD_NAME's three-dict walk rather than
# the dict operation it is named after.  Both interpreters run the identical
# .pyc, so the source compiler is not in the measurement.  The run diffs both
# interpreters' OUTPUT on every case before it times anything -- an operation
# that computes the wrong answer is usually faster.
#
# NO CASE PRINTS A HASH VALUE.  apython uses unseeded FNV-1a where CPython
# uses randomized siphash13, so a hash is the one dict-derived number that
# legitimately differs.  A dict's ITERATION ORDER is safe to print: both
# implementations keep the compact layout's insertion order.  The keys are
# built from a seeded LCG rather than `random`, so the corpus is the same on
# every run and on every machine.
#
# Usage: bash tests/run_dict_bench.sh [case ...]
set -u
cd "$(dirname "$0")/.."
BD=${TMPDIR:-/tmp}/apython-dict-bench
mkdir -p "$BD"
mk() { cat > "$BD/$1.py"; }
echo "pass" > "$BD/empty.py"

# ---- construction ----------------------------------------------------------
mk d_empty <<'EOF'
def run(n):
    c = 0
    i = 0
    while i < n:
        d = {}
        c += len(d)
        i += 1
    return c
print(run(2000000))
EOF

mk d_literal <<'EOF'
def run(n):
    c = 0
    i = 0
    while i < n:
        d = {"alpha": 1, "beta": 2, "gamma": 3, "delta": 4}
        c += d["alpha"] + d["delta"]
        i += 1
    return c
print(run(500000))
EOF

mk d_literal_big <<'EOF'
def run(n):
    c = 0
    i = 0
    while i < n:
        d = {"k00": 0, "k01": 1, "k02": 2, "k03": 3, "k04": 4,
             "k05": 5, "k06": 6, "k07": 7, "k08": 8, "k09": 9,
             "k10": 10, "k11": 11, "k12": 12, "k13": 13, "k14": 14,
             "k15": 15, "k16": 16, "k17": 17, "k18": 18, "k19": 19}
        c += d["k00"] + d["k19"]
        i += 1
    return c
print(run(150000))
EOF

mk d_literal_var <<'EOF'
def run(n):
    a = "one"
    b = "two"
    c = 0
    i = 0
    while i < n:
        d = {a: i, b: i + 1}
        c += d[a]
        i += 1
    return c
print(run(500000))
EOF

mk d_comp <<'EOF'
def run(n):
    s = 0
    for _ in range(n):
        d = {i: i * 2 for i in range(100)}
        s += d[50]
    return s
print(run(20000))
EOF

mk d_comp_str <<'EOF'
def run(n):
    keys = ["key%03d" % i for i in range(100)]
    s = 0
    for _ in range(n):
        d = {k: 1 for k in keys}
        s += len(d)
    return s
print(run(20000))
EOF

mk d_grow <<'EOF'
def run(n):
    c = 0
    for _ in range(n):
        d = {}
        for i in range(1000):
            d[i] = i
        c += len(d)
    return c
print(run(1500))
EOF

mk d_from_pairs <<'EOF'
def run(n):
    pairs = [(i, i * 2) for i in range(100)]
    c = 0
    for _ in range(n):
        d = dict(pairs)
        c += len(d)
    return c
print(run(20000))
EOF

mk d_from_dict <<'EOF'
def run(n):
    src = {i: i * 2 for i in range(100)}
    c = 0
    for _ in range(n):
        d = dict(src)
        c += len(d)
    return c
print(run(20000))
EOF

mk d_from_kwargs <<'EOF'
def run(n):
    c = 0
    i = 0
    while i < n:
        d = dict(alpha=1, beta=2, gamma=3, delta=4)
        c += d["beta"]
        i += 1
    return c
print(run(300000))
EOF

mk d_fromkeys <<'EOF'
def run(n):
    keys = list(range(100))
    c = 0
    for _ in range(n):
        d = dict.fromkeys(keys, 0)
        c += len(d)
    return c
print(run(20000))
EOF

# ---- lookup ----------------------------------------------------------------
mk d_get_int <<'EOF'
def run(n):
    d = {i: i * 2 for i in range(1000)}
    s = 0
    for _ in range(n):
        for i in range(1000):
            s += d[i]
    return s
print(run(1500))
EOF

mk d_get_str <<'EOF'
def run(n):
    d = {"alpha": 1, "beta": 2, "gamma": 3, "delta": 4, "epsilon": 5}
    s = 0
    i = 0
    while i < n:
        s += d["alpha"] + d["gamma"] + d["epsilon"]
        i += 1
    return s
print(run(1000000))
EOF

mk d_get_str_dyn <<'EOF'
def run(n):
    keys = ["key%03d" % i for i in range(100)]
    d = {}
    for k in keys:
        d[k] = 1
    c = 0
    for _ in range(n):
        for k in keys:
            c += d[k]
    return c
print(run(6000))
EOF

mk d_get_miss <<'EOF'
def run(n):
    d = {i: i for i in range(100)}
    c = 0
    i = 0
    while i < n:
        if d.get(i + 1000) is None:
            c += 1
        i += 1
    return c
print(run(1000000))
EOF

mk d_method_get <<'EOF'
def run(n):
    d = {i: i for i in range(100)}
    g = d.get
    s = 0
    i = 0
    while i < n:
        s += g(i % 100, 0)
        i += 1
    return s
print(run(1000000))
EOF

mk d_get_strided <<'EOF'
def run(n):
    d = {}
    for i in range(1000):
        d[i * 8] = i
    s = 0
    for _ in range(n):
        for i in range(1000):
            s += d[i * 8]
    return s
print(run(600))
EOF

mk d_get_collide <<'EOF'
def run(n):
    # Every key has the same low bits, so a table masked to a power of two
    # sends all of them to one slot and the probe sequence is the whole
    # measurement.  No current case has any collision pressure at all.
    d = {}
    for i in range(200):
        d[i * 4096] = i
    s = 0
    for _ in range(n):
        for i in range(200):
            s += d[i * 4096]
    return s
print(run(2000))
EOF

mk d_get_tuple <<'EOF'
def run(n):
    keys = [(i, i + 1) for i in range(100)]
    d = {}
    for k in keys:
        d[k] = 1
    c = 0
    for _ in range(n):
        for k in keys:
            c += d[k]
    return c
print(run(4000))
EOF

mk d_get_obj <<'EOF'
class K:
    def __init__(self, v):
        self.v = v

    def __hash__(self):
        return self.v * 2654435761 & 0xFFFFFFFF

    def __eq__(self, o):
        return isinstance(o, K) and self.v == o.v


def run(n):
    keys = [K(i) for i in range(100)]
    d = {}
    for k in keys:
        d[k] = 1
    c = 0
    for _ in range(n):
        for k in keys:
            c += d[k]
    return c
print(run(2000))
EOF

mk d_get_float <<'EOF'
def run(n):
    d = {i + 0.5: i for i in range(1000)}
    s = 0
    for _ in range(n):
        for i in range(1000):
            s += d[i + 0.5]
    return s
print(run(600))
EOF

mk d_get_bigint <<'EOF'
def run(n):
    base = 2 ** 60
    d = {base + i: i for i in range(200)}
    s = 0
    for _ in range(n):
        for i in range(200):
            s += d[base + i]
    return s
print(run(2000))
EOF

mk d_in_hit <<'EOF'
def run(n):
    d = {i: i for i in range(100)}
    c = 0
    i = 0
    while i < n:
        if i % 100 in d:
            c += 1
        i += 1
    return c
print(run(1000000))
EOF

mk d_in_miss <<'EOF'
def run(n):
    d = {i: i for i in range(100)}
    c = 0
    i = 0
    while i < n:
        if i + 1000 in d:
            c += 1
        i += 1
    return c
print(run(1000000))
EOF

mk d_setdefault <<'EOF'
def run(n):
    c = 0
    for _ in range(n):
        d = {}
        for i in range(100):
            d.setdefault(i % 50, []).append(i)
        c += len(d)
    return c
print(run(10000))
EOF

mk d_count_idiom <<'EOF'
def run(n):
    words = ["w%02d" % (i % 40) for i in range(200)]
    c = 0
    for _ in range(n):
        d = {}
        for w in words:
            d[w] = d.get(w, 0) + 1
        c += d["w00"]
    return c
print(run(4000))
EOF

# ---- mutation --------------------------------------------------------------
mk d_set_new <<'EOF'
def run(n):
    c = 0
    for _ in range(n):
        d = {}
        for i in range(100):
            d[i] = i
        c += len(d)
    return c
print(run(20000))
EOF

mk d_set_over <<'EOF'
def run(n):
    d = {i: 0 for i in range(1000)}
    for _ in range(n):
        for i in range(1000):
            d[i] = i
    return len(d)
print(run(1500))
EOF

mk d_del <<'EOF'
def run(n):
    c = 0
    for _ in range(n):
        d = {i: i for i in range(100)}
        for i in range(100):
            del d[i]
        c += len(d)
    return c
print(run(10000))
EOF

mk d_del_churn <<'EOF'
def run(n):
    # Delete and reinsert forever: the table fills with tombstones and the
    # probe path lengthens until a resize compacts it.
    d = {i: i for i in range(100)}
    for i in range(n):
        k = i % 100
        del d[k]
        d[k] = i
    return len(d)
print(run(500000))
EOF

mk d_pop <<'EOF'
def run(n):
    c = 0
    for _ in range(n):
        d = {i: i for i in range(100)}
        for i in range(100):
            c += d.pop(i)
    return c
print(run(8000))
EOF

mk d_popitem <<'EOF'
def run(n):
    c = 0
    for _ in range(n):
        d = {i: i for i in range(100)}
        while d:
            k, v = d.popitem()
            c += v
    return c
print(run(6000))
EOF

mk d_clear <<'EOF'
def run(n):
    d = {i: i for i in range(100)}
    c = 0
    for _ in range(n):
        d.clear()
        for i in range(10):
            d[i] = i
        c += len(d)
    return c
print(run(60000))
EOF

mk d_update <<'EOF'
def run(n):
    src = {i: i for i in range(100)}
    c = 0
    for _ in range(n):
        d = {}
        d.update(src)
        c += len(d)
    return c
print(run(15000))
EOF

mk d_merge <<'EOF'
def run(n):
    a = {i: i for i in range(50)}
    b = {i + 50: i for i in range(50)}
    c = 0
    for _ in range(n):
        d = a | b
        c += len(d)
    return c
print(run(15000))
EOF

mk d_imerge <<'EOF'
def run(n):
    b = {i + 50: i for i in range(50)}
    c = 0
    for _ in range(n):
        d = {i: i for i in range(50)}
        d |= b
        c += len(d)
    return c
print(run(10000))
EOF

mk d_starstar <<'EOF'
def run(n):
    a = {"k%02d" % i: i for i in range(20)}
    b = {"j%02d" % i: i for i in range(20)}
    c = 0
    for _ in range(n):
        d = {**a, **b}
        c += len(d)
    return c
print(run(30000))
EOF

# ---- views and iteration ---------------------------------------------------
mk d_iter_keys <<'EOF'
def run(n):
    d = {i: i * 2 for i in range(1000)}
    s = 0
    for _ in range(n):
        for k in d:
            s += k
    return s
print(run(1500))
EOF

mk d_iter_values <<'EOF'
def run(n):
    d = {i: i * 2 for i in range(1000)}
    s = 0
    for _ in range(n):
        for v in d.values():
            s += v
    return s
print(run(1500))
EOF

mk d_iter_items <<'EOF'
def run(n):
    d = {i: i * 2 for i in range(1000)}
    s = 0
    for _ in range(n):
        for k, v in d.items():
            s += k + v
    return s
print(run(1000))
EOF

mk d_keys_in <<'EOF'
def run(n):
    d = {i: i for i in range(100)}
    ks = d.keys()
    c = 0
    i = 0
    while i < n:
        if i % 200 in ks:
            c += 1
        i += 1
    return c
print(run(500000))
EOF

mk d_items_in <<'EOF'
def run(n):
    d = {i: i for i in range(20)}
    it = d.items()
    c = 0
    i = 0
    while i < n:
        if (i % 40, i % 40) in it:
            c += 1
        i += 1
    return c
print(run(60000))
EOF

mk d_len <<'EOF'
def run(n):
    d = {i: i for i in range(100)}
    s = 0
    i = 0
    while i < n:
        s += len(d)
        i += 1
    return s
print(run(2000000))
EOF

mk d_copy <<'EOF'
def run(n):
    d = {i: i * 2 for i in range(100)}
    c = 0
    for _ in range(n):
        c += len(d.copy())
    return c
print(run(20000))
EOF

mk d_eq <<'EOF'
def run(n):
    a = {i: i for i in range(100)}
    b = {i: i for i in range(100)}
    c = 0
    i = 0
    while i < n:
        if a == b:
            c += 1
        i += 1
    return c
print(run(30000))
EOF

mk d_repr <<'EOF'
def run(n):
    d = {"k%02d" % i: i for i in range(20)}
    c = 0
    i = 0
    while i < n:
        c += len(repr(d))
        i += 1
    return c
print(run(60000))
EOF

mk d_sorted <<'EOF'
def run(n):
    d = {"k%03d" % ((i * 37) % 200): i for i in range(200)}
    c = 0
    for _ in range(n):
        c += len(sorted(d))
    return c
print(run(4000))
EOF

mk d_sum_values <<'EOF'
def run(n):
    d = {i: i for i in range(1000)}
    s = 0
    for _ in range(n):
        s += sum(d.values())
    return s
print(run(2000))
EOF

# ---- the machinery dicts are the substrate for -----------------------------
mk d_kwargs <<'EOF'
def f(a, **kw):
    return a + len(kw)


def run(n):
    s = 0
    i = 0
    while i < n:
        s += f(1, x=2, y=3)
        i += 1
    return s
print(run(1000000))
EOF

mk d_kwargs_none <<'EOF'
def f(a, **kw):
    return a


def run(n):
    s = 0
    i = 0
    while i < n:
        s += f(1)
        i += 1
    return s
print(run(2000000))
EOF

mk d_callex <<'EOF'
def f(a, b, c):
    return a + b + c


def run(n):
    kw = {"a": 1, "b": 2, "c": 3}
    s = 0
    i = 0
    while i < n:
        s += f(**kw)
        i += 1
    return s
print(run(500000))
EOF

mk d_module_attr <<'EOF'
import math


def run(n):
    s = 0.0
    i = 0
    while i < n:
        s += math.floor(2.5)
        i += 1
    return s
print(run(1000000))
EOF

mk d_class_attr <<'EOF'
class C:
    LIMIT = 7

    def __init__(self, v):
        self.v = v


def run(n):
    s = 0
    i = 0
    while i < n:
        s += C.LIMIT
        i += 1
    return s
print(run(1000000))
EOF

mk d_dunder <<'EOF'
class V:
    def __init__(self, v):
        self.v = v

    def __eq__(self, o):
        return isinstance(o, V) and self.v == o.v

    def __len__(self):
        return self.v


def run(n):
    a = V(3)
    b = V(3)
    s = 0
    i = 0
    while i < n:
        if a == b:
            s += len(a)
        i += 1
    return s
print(run(500000))
EOF

mk d_instance_attr <<'EOF'
class C:
    def __init__(self):
        self.a = 1
        self.b = 2
        self.c = 3


def run(n):
    o = C()
    s = 0
    i = 0
    while i < n:
        s += o.a + o.b + o.c
        i += 1
    return s
print(run(1000000))
EOF

BENCHES="d_empty d_literal d_literal_big d_literal_var d_comp d_comp_str \
         d_grow d_from_pairs d_from_dict d_from_kwargs d_fromkeys \
         d_get_int d_get_str d_get_str_dyn d_get_miss d_method_get \
         d_get_strided d_get_collide d_get_tuple d_get_obj d_get_float \
         d_get_bigint d_in_hit d_in_miss d_setdefault d_count_idiom \
         d_set_new d_set_over d_del d_del_churn d_pop d_popitem d_clear \
         d_update d_merge d_imerge d_starstar \
         d_iter_keys d_iter_values d_iter_items d_keys_in d_items_in d_len \
         d_copy d_eq d_repr d_sorted d_sum_values \
         d_kwargs d_kwargs_none d_callex d_module_attr d_class_attr \
         d_dunder d_instance_attr"

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
echo "=== dicts (startup-subtracted ms; >1.00x = apython faster) ==="
printf "%-18s %9s %9s %8s\n" "benchmark" "apython" "cpython" "speedup"
TA=0; TC=0
for b in $BENCHES; do
    PYC=$(ls "$BD/__pycache__/$b."*.pyc)
    a=$(timeit ./apython "$PYC"); c=$(timeit python3 "$PYC")
    a=$((a-SA)); c=$((c-SC)); [ "$a" -lt 1 ] && a=1; [ "$c" -lt 1 ] && c=1
    TA=$((TA+a)); TC=$((TC+c))
    printf "%-18s %9s %9s %8s\n" "$b" "$a" "$c" "$(python3 -c "print('%.2fx'%($c/$a))")"
done
echo
printf "TOTAL apython=%sms cpython=%sms overall=%s\n" "$TA" "$TC" \
       "$(python3 -c "print('%.2fx'%($TC/$TA))")"
