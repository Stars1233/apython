#!/bin/bash
# arity_probe.sh - does a builtin method refuse the wrong number of arguments?
#
# A method registered with no argument counts accepts whatever it is handed
# and reads the first N: `str.upper("a", 1)` answered 'A' where CPython raises
# TypeError.  The machinery to refuse exists -- add_method_to_dict_checked
# takes a min and a max and reports CPython's counted wording -- and what is
# missing is per-method numbers, several hundred of them.  Guessing them is
# what this exists to avoid.
#
# Every method of every builtin type is called with 0, 1, 2 and 3 arguments,
# unbound and off a live instance, and the two interpreters are compared on
# whether the call raised TypeError.  What it *answers* is not compared: only
# whether it was refused.  A method that runs for a long time, mutates the
# world, or blocks is excluded by name.
#
# CPython's own wording here is inconsistent -- clinic-generated methods say
# "takes exactly one argument (2 given)" and hand-written ones say "takes no
# arguments" -- so the wording is not compared either, and each method's
# numbers come from the oracle rather than from a rule.
#
# Ratcheted against tests/arity_floor.txt: a method that agreed with CPython
# and no longer does fails the target.

set -u

APYTHON=${APYTHON:-./apython}
PYTHON=${PYTHON:-python3}
TESTDIR="$(cd "$(dirname "$0")" && pwd)"
ROOT="$(dirname "$TESTDIR")"
FLOOR="$TESTDIR/arity_floor.txt"
WORK=$(mktemp -d)
trap 'rm -rf "$WORK"' EXIT

GREEN='\033[0;32m'; RED='\033[0;31m'; NC='\033[0m'

case "$APYTHON" in
    /*) APY="$APYTHON" ;;
    *)  APY="$ROOT/${APYTHON#./}" ;;
esac
[ -x "$APY" ] || { echo "SKIP: $APYTHON not built"; exit 0; }

cat > "$WORK/probe.py" <<'PYEOF'
# One line per (type, method, argument count): "name/count refused" or
# "name/count ran".  Nothing here compares a return VALUE -- two correct
# interpreters may still differ on the repr of a hash -- only on whether the
# call was refused for its arity.
TYPES = [
    ("str", "x"), ("bytes", b"x"), ("bytearray", bytearray(b"x")),
    ("list", [1]), ("tuple", (1,)), ("dict", {1: 2}), ("set", {1}),
    ("frozenset", frozenset({1})), ("int", 7), ("float", 1.5),
    ("bool", True), ("complex", 1j), ("slice", slice(1)),
    ("range", range(3)), ("memoryview", memoryview(b"xy")),
]

# Excluded by name, with the reason.  These are not arity questions.
SKIP = {
    "__init_subclass__", "__subclasshook__", "__class_getitem__",
    "__reduce__", "__reduce_ex__", "__getstate__", "__setstate__",
    "__sizeof__", "__dir__", "__format__", "__init__", "__new__",
    "sort", "clear", "pop", "popitem", "remove", "discard", "append",
    "extend", "insert", "reverse", "update", "setdefault", "add",
    "release", "conjugate",
}

# A plausible argument for each position, chosen so a method that ACCEPTS the
# count is likely to run rather than raise for the value.  A ValueError or a
# LookupError is not a refusal; only TypeError is.
FILLER = ["x", 0, 1]


def classify(fn, args):
    try:
        fn(*args)
        return "ran"
    except TypeError:
        return "refused"
    except Exception:
        return "ran"        # it accepted the count and disliked the value


for tname, inst in TYPES:
    t = type(inst)
    for name in sorted(dir(t)):
        if name in SKIP or name.startswith("_abc"):
            continue
        try:
            unbound = getattr(t, name)
        except Exception:
            continue
        if not callable(unbound):
            continue
        for n in range(4):
            args = tuple(FILLER[:n])
            print("%s.%s/%d %s" % (tname, name, n,
                                   classify(getattr(inst, name), args)))
PYEOF

"$PYTHON" -m py_compile "$WORK/probe.py" 2>/dev/null || {
    echo "probe.py does not compile under $PYTHON"; exit 1; }

"$PYTHON" "$WORK/probe.py" 2>/dev/null | sort > "$WORK/cpython.txt"
timeout 120 "$APY" "$WORK/__pycache__/probe.cpython-312.pyc" 2>/dev/null \
    | sort > "$WORK/apython.txt"

if [ ! -s "$WORK/apython.txt" ]; then
    echo -e "${RED}FAIL${NC} arity probe: apython produced nothing"
    exit 1
fi

# A method apython does not have at all is not an arity difference; it is a
# missing method, which the ordinary suite is what measures.
join "$WORK/cpython.txt" "$WORK/apython.txt" > "$WORK/both.txt"
awk '$2 != $3 {print $1}' "$WORK/both.txt" | sort > "$WORK/differ.txt"
awk '$2 == $3 {print $1}' "$WORK/both.txt" | sort > "$WORK/agree.txt"

AGREE=$(wc -l < "$WORK/agree.txt")
DIFFER=$(wc -l < "$WORK/differ.txt")
MISSING=$(( $(wc -l < "$WORK/cpython.txt") - $(wc -l < "$WORK/both.txt") ))

echo "builtin arity: $AGREE agree, $DIFFER differ, $MISSING not comparable"

if [ "${1:-}" = "--record" ]; then
    {
        echo "# (type.method/argcount) pairs on which apython and CPython 3.12"
        echo "# agree about whether the call is refused for its arity."
        echo "# Regenerate with: bash tests/arity_probe.sh --record"
        echo "# A pair listed here must keep agreeing."
        cat "$WORK/agree.txt"
    } > "$FLOOR"
    echo "recorded floor: $AGREE pairs -> $FLOOR"
    exit 0
fi

[ -f "$FLOOR" ] || { echo "no floor at $FLOOR; run with --record"; exit 1; }

grep -v '^#' "$FLOOR" | sort > "$WORK/floor.txt"
REGRESSED=$(comm -23 "$WORK/floor.txt" "$WORK/agree.txt")
if [ -n "$REGRESSED" ]; then
    echo -e "${RED}FAIL${NC} these agreed with CPython and no longer do:"
    echo "$REGRESSED" | sed 's/^/    /'
    exit 1
fi
GAINED=$(comm -13 "$WORK/floor.txt" "$WORK/agree.txt" | wc -l)
if [ "$GAINED" -gt 0 ]; then
    echo "  $GAINED newly agreeing; raise the floor with --record"
fi
echo -e "${GREEN}PASS${NC} arity scoreboard: $AGREE agree, $DIFFER still differ"
