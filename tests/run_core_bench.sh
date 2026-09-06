#!/bin/bash
# Core-interpreter benchmarks: the paths that are neither integer nor float
# arithmetic -- conditionals, iteration, name loads, subscript, calls,
# attributes, strings, comprehensions.  apython against CPython on the same
# .pyc, startup subtracted, best of five.  Reported, not gated.
#
# This exists because run_int_bench.sh and run_float_bench.sh between them
# measure almost none of the interpreter.  A change to `if x:` or to FOR_ITER
# moves nothing either of them reports, so without this harness there is no
# way to tell a win from a wish.
#
# Every loop is INSIDE A FUNCTION, for the same reason as in the other two: at
# module scope the same loop measures LOAD_NAME/STORE_NAME dict lookups rather
# than the thing it is named after, and the two answers differ by more than
# the thing being measured.
#
# The run diffs both interpreters' OUTPUT on every case before it times
# anything.  A handler that computes the wrong answer is usually faster.
#
# Usage: bash tests/run_core_bench.sh
set -u
cd "$(dirname "$0")/.."
BD=${TMPDIR:-/tmp}/apython-core-bench
mkdir -p "$BD"
mk() { cat > "$BD/$1.py"; }
echo "pass" > "$BD/empty.py"

# ---- conditionals: every `if` and `while` in every program -----------------
mk c_if_bool <<'EOF'
def run(n):
    c = 0
    i = 0
    while i < n:
        f = (i & 7) == 0          # a real bool
        if f:
            c += 1
        i += 1
    return c
print(run(2000000))
EOF

mk c_if_int <<'EOF'
def run(n):
    c = 0
    i = 0
    while i < n:
        if i & 7:                 # an int immediate, not a bool
            c += 1
        i += 1
    return c
print(run(2000000))
EOF

mk c_if_obj <<'EOF'
def run(n):
    a = []
    b = [1]
    c = 0
    i = 0
    while i < n:
        if a:                     # a container: reaches nb_bool/sq_length
            c += 1
        if b:
            c += 1
        i += 1
    return c
print(run(1000000))
EOF

mk c_not <<'EOF'
def run(n):
    c = 0
    i = 0
    while i < n:
        if not (i & 7):
            c += 1
        i += 1
    return c
print(run(2000000))
EOF

mk c_and_or <<'EOF'
def run(n):
    c = 0
    i = 0
    while i < n:
        if (i & 1) and (i & 2):
            c += 1
        if (i & 4) or (i & 8):
            c += 1
        i += 1
    return c
print(run(1000000))
EOF

# ---- iteration -------------------------------------------------------------
mk c_for_list <<'EOF'
def run(n):
    a = list(range(1000))
    s = 0
    for _ in range(n):
        for v in a:
            s += v
    return s
print(run(2000))
EOF

mk c_for_range <<'EOF'
def run(n):
    s = 0
    for i in range(n):
        s += i
    return s
print(run(5000000))
EOF

mk c_for_tuple <<'EOF'
def run(n):
    a = tuple(range(1000))
    s = 0
    for _ in range(n):
        for v in a:
            s += v
    return s
print(run(2000))
EOF

mk c_for_dict <<'EOF'
def run(n):
    d = {i: i * 2 for i in range(1000)}
    s = 0
    for _ in range(n):
        for k in d:
            s += k
    return s
print(run(1000))
EOF

mk c_for_str <<'EOF'
def run(n):
    s = "abcdefghij" * 100
    c = 0
    for _ in range(n):
        for ch in s:
            c += 1
    return c
print(run(2000))
EOF

mk c_genexp <<'EOF'
def run(n):
    a = list(range(1000))
    s = 0
    for _ in range(n):
        s += sum(v for v in a)
    return s
print(run(500))
EOF

# ---- name loads ------------------------------------------------------------
mk c_global <<'EOF'
G = 3
def run(n):
    s = 0
    i = 0
    while i < n:
        s += G                    # LOAD_GLOBAL_MODULE
        i += 1
    return s
print(run(3000000))
EOF

mk c_builtin <<'EOF'
def run(n):
    s = 0
    i = 0
    while i < n:
        s += len                  # LOAD_GLOBAL_BUILTIN, no call
        i += 1
    return 0
def run2(n):
    s = 0
    i = 0
    while i < n:
        s += len(_L)              # LOAD_GLOBAL_BUILTIN + call
        i += 1
    return s
_L = [1, 2, 3]
print(run2(1000000))
EOF

# ---- identity and membership ----------------------------------------------
mk c_is_none <<'EOF'
def run(n):
    a = [None, 1, None, 2]
    c = 0
    for _ in range(n):
        for v in a:
            if v is None:
                c += 1
    return c
print(run(500000))
EOF

mk c_in_dict <<'EOF'
def run(n):
    d = {i: i for i in range(100)}
    c = 0
    i = 0
    while i < n:
        if i % 200 in d:
            c += 1
        i += 1
    return c
print(run(1000000))
EOF

mk c_in_list <<'EOF'
def run(n):
    a = list(range(20))
    c = 0
    i = 0
    while i < n:
        if (i % 40) in a:
            c += 1
        i += 1
    return c
print(run(200000))
EOF

# ---- subscript -------------------------------------------------------------
mk c_list_get <<'EOF'
def run(n):
    a = list(range(1000))
    s = 0
    for _ in range(n):
        for i in range(1000):
            s += a[i]
    return s
print(run(2000))
EOF

mk c_list_set <<'EOF'
def run(n):
    a = list(range(1000))
    for _ in range(n):
        for i in range(1000):
            a[i] = i
    return a[500]
print(run(2000))
EOF

mk c_dict_get <<'EOF'
def run(n):
    d = {i: i * 2 for i in range(1000)}
    s = 0
    for _ in range(n):
        for i in range(1000):
            s += d[i]
    return s
print(run(1500))
EOF

mk c_dict_set <<'EOF'
def run(n):
    d = {}
    for _ in range(n):
        for i in range(1000):
            d[i] = i
    return len(d)
print(run(1500))
EOF

mk c_tuple_get <<'EOF'
def run(n):
    a = tuple(range(1000))
    s = 0
    for _ in range(n):
        for i in range(1000):
            s += a[i]
    return s
print(run(2000))
EOF

mk c_str_get <<'EOF'
def run(n):
    s = "abcdefghij" * 100
    c = 0
    for _ in range(n):
        for i in range(1000):
            c += 1 if s[i] == "a" else 0
    return c
print(run(500))
EOF

# ---- calls -----------------------------------------------------------------
mk c_call <<'EOF'
def f(a, b):
    return a + b
def run(n):
    s = 0
    i = 0
    while i < n:
        s = f(s, 1)
        i += 1
    return s
print(run(1000000))
EOF

mk c_call_method <<'EOF'
class C:
    def __init__(self):
        self.x = 0
    def bump(self, v):
        return self.x + v
def run(n):
    c = C()
    s = 0
    i = 0
    while i < n:
        s = c.bump(1)
        i += 1
    return s
print(run(1000000))
EOF

mk c_call_builtin <<'EOF'
def run(n):
    a = [1, 2, 3]
    s = 0
    i = 0
    while i < n:
        s += len(a)
        i += 1
    return s
print(run(2000000))
EOF

mk c_append <<'EOF'
def run(n):
    for _ in range(n):
        a = []
        for i in range(100):
            a.append(i)
    return len(a)
print(run(20000))
EOF

# ---- attributes ------------------------------------------------------------
mk c_attr_get <<'EOF'
class C:
    def __init__(self):
        self.x = 1
def run(n):
    c = C()
    s = 0
    i = 0
    while i < n:
        s += c.x
        i += 1
    return s
print(run(2000000))
EOF

mk c_attr_set <<'EOF'
class C:
    def __init__(self):
        self.x = 1
def run(n):
    c = C()
    i = 0
    while i < n:
        c.x = i
        i += 1
    return c.x
print(run(2000000))
EOF

# ---- strings ---------------------------------------------------------------
mk c_fstring <<'EOF'
def run(n):
    s = 0
    i = 0
    while i < n:
        t = f"a{i}b{i}c{i}d"
        s += len(t)
        i += 1
    return s
print(run(300000))
EOF

mk c_fstring_wide <<'EOF'
def run(n):
    a, b, c, d, e = 1, 2, 3, 4, 5
    s = 0
    i = 0
    while i < n:
        t = f"{a}-{b}-{c}-{d}-{e}-{a}-{b}-{c}-{d}-{e}"
        s += len(t)
        i += 1
    return s
print(run(200000))
EOF

mk c_concat <<'EOF'
def run(n):
    s = 0
    i = 0
    while i < n:
        t = "abc" + "def" + str(i & 7)
        s += len(t)
        i += 1
    return s
print(run(300000))
EOF

mk c_str_eq <<'EOF'
def run(n):
    a = ["alpha", "beta", "gamma", "alpha"]
    c = 0
    for _ in range(n):
        for v in a:
            if v == "alpha":
                c += 1
    return c
print(run(500000))
EOF

# ---- comprehensions and unpacking ------------------------------------------
mk c_listcomp <<'EOF'
def run(n):
    s = 0
    for _ in range(n):
        a = [i * 2 for i in range(100)]
        s += a[50]
    return s
print(run(20000))
EOF

mk c_dictcomp <<'EOF'
def run(n):
    s = 0
    for _ in range(n):
        d = {i: i * 2 for i in range(100)}
        s += d[50]
    return s
print(run(10000))
EOF

mk c_unpack <<'EOF'
def run(n):
    t = (1, 2, 3)
    s = 0
    i = 0
    while i < n:
        a, b, c = t
        s += a + b + c
        i += 1
    return s
print(run(1000000))
EOF

mk c_unary <<'EOF'
def run(n):
    s = 0
    i = 0
    while i < n:
        s += -i + (~i)
        i += 1
    return s
print(run(2000000))
EOF

BENCHES="c_if_bool c_if_int c_if_obj c_not c_and_or \
         c_for_list c_for_range c_for_tuple c_for_dict c_for_str c_genexp \
         c_global c_builtin c_is_none c_in_dict c_in_list \
         c_list_get c_list_set c_dict_get c_dict_set c_tuple_get c_str_get \
         c_call c_call_method c_call_builtin c_append \
         c_attr_get c_attr_set \
         c_fstring c_fstring_wide c_concat c_str_eq \
         c_listcomp c_dictcomp c_unpack c_unary"

for b in $BENCHES empty; do python3 -m py_compile "$BD/$b.py"; done

timeit() {
    local best=999999 t
    for i in 1 2 3 4 5; do
        local s=$(date +%s%N); "$@" >/dev/null 2>&1; local e=$(date +%s%N)
        t=$(( (e-s)/1000000 )); [ "$t" -lt "$best" ] && best=$t
    done
    echo "$best"
}

EPYC=$(ls "$BD/__pycache__/empty."*.pyc)
SA=$(timeit ./apython "$EPYC"); SC=$(timeit python3 "$EPYC")

echo "=== agreement ==="
for b in $BENCHES; do
    PYC=$(ls "$BD/__pycache__/$b."*.pyc)
    e=$(python3 "$PYC" 2>&1); a=$(./apython "$PYC" 2>&1)
    [ "$e" == "$a" ] || echo "  DIFF $b: cpython=[$e] apython=[$a]"
done
echo "  (nothing above means all agree)"
echo
echo "=== core paths (startup-subtracted ms; >1.00x = apython faster) ==="
printf "%-16s %9s %9s %8s\n" "benchmark" "apython" "cpython" "speedup"
TA=0; TC=0
for b in $BENCHES; do
    PYC=$(ls "$BD/__pycache__/$b."*.pyc)
    a=$(timeit ./apython "$PYC"); c=$(timeit python3 "$PYC")
    a=$((a-SA)); c=$((c-SC)); [ "$a" -lt 1 ] && a=1; [ "$c" -lt 1 ] && c=1
    TA=$((TA+a)); TC=$((TC+c))
    printf "%-16s %9s %9s %8s\n" "$b" "$a" "$c" "$(python3 -c "print('%.2fx'%($c/$a))")"
done
echo
printf "TOTAL apython=%sms cpython=%sms overall=%s\n" "$TA" "$TC" \
       "$(python3 -c "print('%.2fx'%($TC/$TA))")"
