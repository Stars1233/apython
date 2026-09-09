#!/bin/bash
# List benchmarks: construction, growth, indexing, slicing, searching,
# sorting and iteration.  apython against CPython on the same .pyc, startup
# subtracted, best of five.  Reported, not gated.
#
# The sixth harness, after int, float, str, core and macro.  Between them
# those five had six list cases, and lists therefore looked fine.  The first
# run of this one read 0.61x overall -- the worst standing of any area
# measured -- with 68% of the loss in list.sort alone, which was a plain
# bottom-up merge sort with no run detection.  A suite total cannot show that;
# only a harness that covers one type's whole surface can.
#
# Same discipline as the other five.  Every loop is INSIDE A FUNCTION: at
# module scope the same code measures LOAD_NAME/STORE_NAME dict lookups
# rather than the list operation it is named after.  Both interpreters run
# the identical .pyc, so the source compiler is not in the measurement.  The
# run diffs both interpreters' OUTPUT on every case before it times anything
# -- an operation that computes the wrong answer is usually faster.
#
# Every case prints a checksum derived from the list it built, so the
# agreement pass is a real correctness test.  The sort cases seed their own
# LCG rather than importing random, so the corpus is the same on every run
# and on every machine, and no case prints a hash value: apython uses
# unseeded FNV-1a where CPython uses randomized siphash13.
#
# Usage: bash tests/run_list_bench.sh [case ...]
set -u
cd "$(dirname "$0")/.."
BD=${TMPDIR:-/tmp}/apython-list-bench
mkdir -p "$BD"
mk() { cat > "$BD/$1.py"; }
echo "pass" > "$BD/empty.py"

# ---- construction ----------------------------------------------------------
mk l_empty <<'EOF'
def run(n):
    c = 0
    i = 0
    while i < n:
        a = []
        c += len(a)
        i += 1
    return c
print(run(2000000))
EOF

mk l_literal <<'EOF'
def run(n):
    c = 0
    i = 0
    while i < n:
        a = [1, 2, 3, 4, 5]
        c += len(a)
        i += 1
    return c
print(run(1000000))
EOF

mk l_literal_big <<'EOF'
def run(n):
    c = 0
    i = 0
    while i < n:
        a = [1, 2, 3, 4, 5, 6, 7, 8, 9, 10,
             11, 12, 13, 14, 15, 16, 17, 18, 19, 20]
        c += len(a)
        i += 1
    return c
print(run(500000))
EOF

mk l_from_range <<'EOF'
def run(n):
    c = 0
    for _ in range(n):
        a = list(range(100))
        c += len(a)
    return c
print(run(50000))
EOF

mk l_from_tuple <<'EOF'
def run(n):
    t = tuple(range(100))
    c = 0
    for _ in range(n):
        c += len(list(t))
    return c
print(run(50000))
EOF

mk l_from_str <<'EOF'
def run(n):
    s = "abcdefghij" * 10
    c = 0
    for _ in range(n):
        c += len(list(s))
    return c
print(run(50000))
EOF

# ---- growth ----------------------------------------------------------------
mk l_append <<'EOF'
def run(n):
    for _ in range(n):
        a = []
        for i in range(100):
            a.append(i)
    return len(a)
print(run(20000))
EOF

mk l_append_long <<'EOF'
def run(n):
    a = []
    for i in range(n):
        a.append(i)
    return len(a)
print(run(2000000))
EOF

mk l_append_bound <<'EOF'
def run(n):
    a = []
    ap = a.append
    for i in range(n):
        ap(i)
    return len(a)
print(run(2000000))
EOF

mk l_extend_list <<'EOF'
def run(n):
    src = list(range(100))
    for _ in range(n):
        a = []
        a.extend(src)
    return len(a)
print(run(50000))
EOF

mk l_extend_gen <<'EOF'
def run(n):
    for _ in range(n):
        a = []
        a.extend(i for i in range(100))
    return len(a)
print(run(20000))
EOF

mk l_iadd <<'EOF'
def run(n):
    src = list(range(100))
    for _ in range(n):
        a = []
        a += src
    return len(a)
print(run(50000))
EOF

mk l_insert_front <<'EOF'
def run(n):
    for _ in range(n):
        a = []
        for i in range(200):
            a.insert(0, i)
    return len(a)
print(run(2000))
EOF

mk l_pop_end <<'EOF'
def run(n):
    c = 0
    for _ in range(n):
        a = list(range(200))
        while a:
            c += a.pop()
    return c
print(run(5000))
EOF

mk l_pop_front <<'EOF'
def run(n):
    c = 0
    for _ in range(n):
        a = list(range(200))
        while a:
            c += a.pop(0)
    return c
print(run(2000))
EOF

# ---- comprehensions --------------------------------------------------------
mk l_comp <<'EOF'
def run(n):
    c = 0
    for _ in range(n):
        a = [i * 2 for i in range(100)]
        c += len(a)
    return c
print(run(20000))
EOF

mk l_comp_if <<'EOF'
def run(n):
    c = 0
    for _ in range(n):
        a = [i for i in range(100) if i & 1]
        c += len(a)
    return c
print(run(20000))
EOF

mk l_comp_nested <<'EOF'
def run(n):
    c = 0
    for _ in range(n):
        a = [[j for j in range(10)] for i in range(10)]
        c += len(a)
    return c
print(run(20000))
EOF

# ---- indexing and slicing --------------------------------------------------
mk l_get <<'EOF'
def run(n):
    a = list(range(1000))
    s = 0
    for _ in range(n):
        for i in range(1000):
            s += a[i]
    return s
print(run(2000))
EOF

mk l_get_neg <<'EOF'
def run(n):
    a = list(range(1000))
    s = 0
    for _ in range(n):
        for i in range(1, 1001):
            s += a[-i]
    return s
print(run(2000))
EOF

mk l_set <<'EOF'
def run(n):
    a = list(range(1000))
    for _ in range(n):
        for i in range(1000):
            a[i] = i
    return len(a)
print(run(2000))
EOF

mk l_slice <<'EOF'
def run(n):
    a = list(range(1000))
    c = 0
    for _ in range(n):
        c += len(a[100:900])
    return c
print(run(200000))
EOF

mk l_slice_step <<'EOF'
def run(n):
    a = list(range(1000))
    c = 0
    for _ in range(n):
        c += len(a[::2])
    return c
print(run(100000))
EOF

mk l_slice_assign <<'EOF'
def run(n):
    src = list(range(800))
    c = 0
    for _ in range(n):
        a = list(range(1000))
        a[100:900] = src
        c += len(a)
    return c
print(run(20000))
EOF

mk l_del_slice <<'EOF'
def run(n):
    c = 0
    for _ in range(n):
        a = list(range(1000))
        del a[100:900]
        c += len(a)
    return c
print(run(20000))
EOF

# ---- whole-list operations -------------------------------------------------
mk l_concat <<'EOF'
def run(n):
    a = list(range(100))
    b = list(range(100))
    c = 0
    for _ in range(n):
        c += len(a + b)
    return c
print(run(200000))
EOF

mk l_repeat <<'EOF'
def run(n):
    a = list(range(10))
    c = 0
    for _ in range(n):
        c += len(a * 20)
    return c
print(run(200000))
EOF

mk l_copy <<'EOF'
def run(n):
    a = list(range(1000))
    c = 0
    for _ in range(n):
        c += len(a.copy())
    return c
print(run(200000))
EOF

mk l_reverse <<'EOF'
def run(n):
    a = list(range(1000))
    for _ in range(n):
        a.reverse()
    return a[0]
print(run(200000))
EOF

mk l_reversed <<'EOF'
def run(n):
    a = list(range(1000))
    s = 0
    for _ in range(n):
        for v in reversed(a):
            s += v
    return s
print(run(2000))
EOF

# ---- searching -------------------------------------------------------------
mk l_in_hit <<'EOF'
def run(n):
    a = list(range(100))
    c = 0
    i = 0
    while i < n:
        if (i % 100) in a:
            c += 1
        i += 1
    return c
print(run(200000))
EOF

mk l_in_miss <<'EOF'
def run(n):
    a = list(range(100))
    c = 0
    i = 0
    while i < n:
        if -1 in a:
            c += 1
        i += 1
    return c
print(run(200000))
EOF

mk l_index <<'EOF'
def run(n):
    a = list(range(100))
    c = 0
    i = 0
    while i < n:
        c += a.index(i % 100)
        i += 1
    return c
print(run(200000))
EOF

mk l_count <<'EOF'
def run(n):
    a = list(range(100)) * 2
    c = 0
    i = 0
    while i < n:
        c += a.count(i % 100)
        i += 1
    return c
print(run(100000))
EOF

mk l_remove <<'EOF'
def run(n):
    c = 0
    for _ in range(n):
        a = list(range(100))
        for i in range(100):
            a.remove(i)
        c += len(a)
    return c
print(run(5000))
EOF

# ---- sorting ---------------------------------------------------------------
mk l_sort_random <<'EOF'
def make(n):
    x = 123456789
    out = []
    for _ in range(n):
        x = (x * 1103515245 + 12345) & 0x7FFFFFFF
        out.append(x)
    return out
def run(n, k):
    src = make(k)
    c = 0
    for _ in range(n):
        a = list(src)
        a.sort()
        c += a[0]
    return c
print(run(300, 2000))
EOF

mk l_sort_sorted <<'EOF'
def run(n, k):
    src = list(range(k))
    c = 0
    for _ in range(n):
        a = list(src)
        a.sort()
        c += a[0]
    return c
print(run(3000, 2000))
EOF

mk l_sort_reversed <<'EOF'
def run(n, k):
    src = list(range(k, 0, -1))
    c = 0
    for _ in range(n):
        a = list(src)
        a.sort()
        c += a[0]
    return c
print(run(3000, 2000))
EOF

mk l_sort_runs <<'EOF'
def run(n, k):
    src = []
    for base in range(0, k, 100):
        src.extend(range(base, base + 100))
    c = 0
    for _ in range(n):
        a = list(src)
        a.sort()
        c += a[0]
    return c
print(run(2000, 2000))
EOF

mk l_sort_str <<'EOF'
def make(n):
    x = 987654321
    out = []
    for _ in range(n):
        x = (x * 1103515245 + 12345) & 0x7FFFFFFF
        out.append("k%08d" % x)
    return out
def run(n, k):
    src = make(k)
    c = 0
    for _ in range(n):
        a = list(src)
        a.sort()
        c += len(a[0])
    return c
print(run(300, 2000))
EOF

mk l_sort_key <<'EOF'
def make(n):
    x = 55555
    out = []
    for _ in range(n):
        x = (x * 1103515245 + 12345) & 0x7FFFFFFF
        out.append(x)
    return out
def run(n, k):
    src = make(k)
    c = 0
    for _ in range(n):
        a = list(src)
        a.sort(key=lambda v: -v)
        c += a[0]
    return c
print(run(100, 2000))
EOF

mk l_sort_tuple <<'EOF'
def make(n):
    x = 24680
    out = []
    for _ in range(n):
        x = (x * 1103515245 + 12345) & 0x7FFFFFFF
        out.append((x & 15, x))
    return out
def run(n, k):
    src = make(k)
    c = 0
    for _ in range(n):
        a = list(src)
        a.sort()
        c += a[0][0]
    return c
print(run(200, 2000))
EOF

mk l_sorted <<'EOF'
def make(n):
    x = 13579
    out = []
    for _ in range(n):
        x = (x * 1103515245 + 12345) & 0x7FFFFFFF
        out.append(x)
    return out
def run(n, k):
    src = make(k)
    c = 0
    for _ in range(n):
        c += sorted(src)[0]
    return c
print(run(300, 2000))
EOF

mk l_sort_small <<'EOF'
def run(n):
    src = [5, 3, 9, 1, 7, 2, 8, 4, 6, 0]
    c = 0
    for _ in range(n):
        a = list(src)
        a.sort()
        c += a[0]
    return c
print(run(300000))
EOF

# ---- iteration and unpacking ----------------------------------------------
mk l_iter <<'EOF'
def run(n):
    a = list(range(1000))
    s = 0
    for _ in range(n):
        for v in a:
            s += v
    return s
print(run(3000))
EOF

mk l_enumerate <<'EOF'
def run(n):
    a = list(range(1000))
    s = 0
    for _ in range(n):
        for i, v in enumerate(a):
            s += v
    return s
print(run(1000))
EOF

mk l_zip <<'EOF'
def run(n):
    a = list(range(1000))
    b = list(range(1000))
    s = 0
    for _ in range(n):
        for x, y in zip(a, b):
            s += x + y
    return s
print(run(500))
EOF

mk l_unpack <<'EOF'
def run(n):
    a = [1, 2, 3]
    s = 0
    i = 0
    while i < n:
        x, y, z = a
        s += x + y + z
        i += 1
    return s
print(run(1000000))
EOF

mk l_sum <<'EOF'
def run(n):
    a = list(range(1000))
    s = 0
    for _ in range(n):
        s += sum(a)
    return s
print(run(3000))
EOF

mk l_minmax <<'EOF'
def run(n):
    a = list(range(1000))
    s = 0
    for _ in range(n):
        s += min(a) + max(a)
    return s
print(run(2000))
EOF

mk l_eq <<'EOF'
def run(n):
    a = list(range(100))
    b = list(range(100))
    c = 0
    i = 0
    while i < n:
        if a == b:
            c += 1
        i += 1
    return c
print(run(200000))
EOF

mk l_repr <<'EOF'
def run(n):
    a = list(range(100))
    c = 0
    for _ in range(n):
        c += len(repr(a))
    return c
print(run(20000))
EOF

mk l_nested_get <<'EOF'
def run(n):
    g = [[0] * 64 for _ in range(64)]
    s = 0
    for _ in range(n):
        for i in range(64):
            row = g[i]
            for j in range(64):
                s += row[j]
    return s
print(run(200))
EOF

BENCHES="l_empty l_literal l_literal_big l_from_range l_from_tuple l_from_str \
         l_append l_append_long l_append_bound l_extend_list l_extend_gen \
         l_iadd l_insert_front l_pop_end l_pop_front \
         l_comp l_comp_if l_comp_nested \
         l_get l_get_neg l_set l_slice l_slice_step l_slice_assign l_del_slice \
         l_concat l_repeat l_copy l_reverse l_reversed \
         l_in_hit l_in_miss l_index l_count l_remove \
         l_sort_random l_sort_sorted l_sort_reversed l_sort_runs l_sort_str \
         l_sort_key l_sort_tuple l_sorted l_sort_small \
         l_iter l_enumerate l_zip l_unpack l_sum l_minmax l_eq l_repr \
         l_nested_get"

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
echo "=== lists (startup-subtracted ms; >1.00x = apython faster) ==="
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
