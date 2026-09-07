#!/bin/bash
# typefuzz_probe.sh - what does a builtin do with an argument of the wrong type?
#
# A Value is one machine word.  A pointer is stored raw, but an int inside
# +-2^50 and every float are IMMEDIATES with no ob_type to read, so a type
# check written as `mov rax, [rbx + PyObject.ob_type]` dereferences the
# NUMBER.  A first run of this probe found sixteen of them -- exec(1), eval(1),
# compile(1,...), __import__(1), map(1,[1]), staticmethod(1).__func__ and
# classmethod(1).__func__, each in its int and its float form -- and none was
# a TypeError.  Every one was a SIGSEGV.
#
# None is here for the opposite reason: it IS a pointer, so it survives the
# dereference and reads a heap singleton's bytes as a type.  __import__(None)
# answered "No module named 'plemented'" -- a fragment of unrelated .rodata.
#
# So this is two gates in one file:
#
#   * CRASHES MUST BE ZERO.  Not a ratchet, a wall, the way pyc_probe.sh
#     treats an exit status of 139.  A wrong answer is a bug; a signal is a
#     memory-safety failure.
#   * The answers are compared against CPython 3.12 and ratcheted against
#     tests/typefuzz_floor.txt.  A case that agreed and no longer does fails
#     the target.  Wording is compared, because a program that catches
#     TypeError and prints it should read the same either way.
#
# Finding the crashes is the awkward part: a segfault loses whatever is still
# in the output buffer, so a batch run cannot say which case died.  Each case
# therefore prints with flush=True, and the harness restarts the batch after
# the last line it saw -- O(crashes) runs rather than O(cases).

set -u

APYTHON=${APYTHON:-./apython}
PYTHON=${PYTHON:-python3}
TESTDIR="$(cd "$(dirname "$0")" && pwd)"
ROOT="$(dirname "$TESTDIR")"
FLOOR="$TESTDIR/typefuzz_floor.txt"
WORK=$(mktemp -d)
trap 'rm -rf "$WORK"' EXIT

GREEN='\033[0;32m'; RED='\033[0;31m'; YELLOW='\033[0;33m'; NC='\033[0m'

case "$APYTHON" in
    /*) APY="$APYTHON" ;;
    *)  APY="$ROOT/${APYTHON#./}" ;;
esac
[ -x "$APY" ] || { echo "SKIP: $APYTHON not built"; exit 0; }

# ---------------------------------------------------------------------------
# The corpus.  One line per case, and the line is the case's name -- so a
# regression names the expression that caused it and nothing has to be looked
# up.  Braces are doubled where the case text needs a literal one.
# ---------------------------------------------------------------------------
cat > "$WORK/gen.py" <<'PYEOF'
import sys

# The values that have no ob_type, plus the two that do and are still wrong to
# dereference as one.
ARGS = ["1", "1.5", "10**60", "True", "None", "'s'", "[]", "object"]

# Every one of these takes an object somewhere and does something with its
# type.  The ones that reach a stored slot and hand it back later -- the two
# method wrappers, property -- are the ones the constructor cannot check.
CALLS = [
    "exec({0})", "eval({0})", "compile({0},'a','exec')",
    "compile('1',{0},'exec')", "compile('1','a',{0})",
    "__import__({0})", "list(map({0},[1]))", "list(filter({0},[1]))",
    "staticmethod({0}).__func__", "classmethod({0}).__func__",
    "staticmethod({0}).__wrapped__", "property({0}).fget",
    "len({0})", "iter({0})", "next({0})", "reversed({0})", "sorted({0})",
    "sum({0})", "min({0})", "max({0})", "abs({0})", "round({0})",
    # hash() is absent: CPython randomises the str hash per process, so the
    # case could not agree twice in a row even with an identical algorithm.
    "repr({0})", "ascii({0})", "format({0})", "format(1,{0})",
    "int({0})", "float({0})", "complex({0})", "bool({0})",
    "list({0})", "tuple({0})", "dict({0})", "set({0})", "frozenset({0})",
    "bytes({0})", "bytearray({0})", "memoryview({0})",
    "getattr({0},'x')", "setattr({0},'x',1)", "hasattr({0},'x')",
    "delattr({0},'x')", "isinstance(1,{0})", "issubclass(int,{0})",
    "callable({0})", "dir({0})", "vars({0})", "type({0},(),{{}})",
    # open() is deliberately absent: open(True) opens file descriptor 1, and
    # the file object closing it takes stdout down with it -- under CPython as
    # readily as here.  A probe must not break the thing it reports through.
    "chr({0})", "ord({0})", "bin({0})", "oct({0})", "hex({0})",
    "divmod({0},2)", "divmod(2,{0})", "pow({0},2)", "pow(2,{0},7)",
    "range({0})", "range(1,{0})", "slice({0}).indices(3)",
    "enumerate([],{0})", "zip([],strict={0})",
    "'abc'[{0}]", "[1,2][{0}]", "(1,2)[{0}]", "{{1:2}}[{0}]",
    "'a'*{0}", "[1]*{0}", "'%s'%{0}", "b'%s'%{0}",
    "'abc'.translate({0})", "'abc'.center({0})", "'abc'.expandtabs({0})",
    "'abc'.zfill({0})", "'abc'.count({0})", "'abc'.find({0})",
    "'abc'.replace({0},'x')", "'abc'.strip({0})", "'abc'.ljust({0})",
    "'abc'.split({0})", "'abc'.join({0})", "'abc'.startswith({0})",
    "'abc'.encode({0})", "b'abc'.decode({0})", "b'a'.hex({0})",
    "[1].index({0})", "[1].count({0})", "[1].insert({0},1)",
    "[1].remove({0})", "sorted([1],key={0})",
    "{{}}.get({0})", "{{}}.setdefault({0})", "{{}}.update({0})",
    "set().union({0})", "set().add({0})",
    "(1).to_bytes({0})", "int.from_bytes({0})", "(1).__round__({0})",
    "print(1,sep={0})", "print(1,end={0})", "print(1,file={0})",
]

cases = [c.format(a) for c in CALLS for a in ARGS]
out = []
for i, c in enumerate(cases):
    out.append(
        "try:\n"
        "    _r = repr(eval(%r))\n"
        "except BaseException as _e:\n"
        "    _r = '!! ' + type(_e).__name__ + ': ' + str(_e)\n"
        "print(%r, _r, sep='\\t', flush=True)\n" % (c, c))
sys.stdout.write("import sys\n" + "".join(out))
PYEOF

"$PYTHON" "$WORK/gen.py" > "$WORK/cases.py" || {
    echo "typefuzz: could not generate the corpus"; exit 1; }
TOTAL=$(grep -c '^print(' "$WORK/cases.py")

"$PYTHON" -m py_compile "$WORK/cases.py" 2>/dev/null || {
    echo "typefuzz: cases.py does not compile under $PYTHON"; exit 1; }

# ---------------------------------------------------------------------------
# CPython first.  A case that makes CPython itself die is a bad case, not a
# finding, so its own exit status is checked too.
# ---------------------------------------------------------------------------
"$PYTHON" "$WORK/cases.py" > "$WORK/cpython.txt" 2>/dev/null
if [ $? -ne 0 ]; then
    echo "typefuzz: $PYTHON did not survive the corpus; fix the corpus"
    exit 1
fi

# ---------------------------------------------------------------------------
# apython, restarting past each crash.  `skip` is how many cases to drop, and
# the crashing case is the one after the last line that reached the pipe.
# ---------------------------------------------------------------------------
PYC="$WORK/__pycache__/cases.cpython-312.pyc"
: > "$WORK/apython.txt"
: > "$WORK/crashes.txt"
skip=0
guard=0
while : ; do
    guard=$((guard + 1))
    [ "$guard" -gt 200 ] && { echo "typefuzz: too many restarts"; break; }

    if [ "$skip" -eq 0 ]; then
        cp "$WORK/cases.py" "$WORK/run.py"
    else
        # Keep the import line, drop the first $skip four-statement blocks.
        "$PYTHON" - "$WORK/cases.py" "$skip" > "$WORK/run.py" <<'PYEOF'
import sys
src = open(sys.argv[1]).read().split("\n")
head, body = src[0], src[1:]
blocks, cur = [], []
for line in body:
    cur.append(line)
    if line.startswith("print("):
        blocks.append(cur); cur = []
print("\n".join([head] + [l for b in blocks[int(sys.argv[2]):] for l in b]))
PYEOF
        "$PYTHON" -m py_compile "$WORK/run.py" 2>/dev/null
        PYC="$WORK/__pycache__/run.cpython-312.pyc"
    fi

    timeout 120 "$APY" "$PYC" > "$WORK/chunk.txt" 2>/dev/null
    rc=$?
    cat "$WORK/chunk.txt" >> "$WORK/apython.txt"
    [ "$rc" -eq 0 ] && break

    ran=$(wc -l < "$WORK/chunk.txt")
    idx=$((skip + ran))
    if [ "$idx" -ge "$TOTAL" ]; then break; fi
    sed -n "$((idx + 1))p" "$WORK/cpython.txt" | cut -f1 >> "$WORK/crashes.txt"
    skip=$((idx + 1))
done

if [ ! -s "$WORK/apython.txt" ] && [ ! -s "$WORK/crashes.txt" ]; then
    echo -e "${RED}FAIL${NC} typefuzz: apython produced nothing"
    exit 1
fi

NCRASH=$(wc -l < "$WORK/crashes.txt")

# ---------------------------------------------------------------------------
# Compare.  The key is the case expression, which is the first field, and the
# answer is the rest of the line.  An address in a repr is not a difference.
# ---------------------------------------------------------------------------
scrub() { sed 's/0x[0-9a-f]*/0xADDR/g' "$1" | sed 's/[[:space:]]*$//' | sort; }
scrub "$WORK/cpython.txt" > "$WORK/c.txt"
scrub "$WORK/apython.txt" > "$WORK/a.txt"
comm -12 "$WORK/c.txt" "$WORK/a.txt" | cut -f1 | sort > "$WORK/agree.txt"

AGREE=$(wc -l < "$WORK/agree.txt")
DIFFER=$((TOTAL - AGREE - NCRASH))

echo "builtin type checks: $AGREE agree, $DIFFER differ, $NCRASH crash (of $TOTAL)"

if [ "${1:-}" = "--show" ]; then
    # Every case that differs, with both answers.  This is how a finding is
    # read: the case expression, then CPython's answer, then apython's.
    "$PYTHON" - "$WORK/c.txt" "$WORK/a.txt" <<'SHOWEOF'
import sys

def load(path):
    d = {}
    # bytes(1.5) and friends put raw bytes in a repr; errors="replace"
    # keeps the report readable without changing what was compared.
    for line in open(path, errors="replace"):
        line = line.rstrip("\n")
        key, tab, rest = line.partition("\t")
        if not tab:
            continue
        d[key] = rest
    return d

cpy, apy = load(sys.argv[1]), load(sys.argv[2])
for k in sorted(cpy):
    if k not in apy:
        print("%s\n  cpy: %s\n  apy: <no answer>" % (k, cpy[k]))
    elif apy[k] != cpy[k]:
        print("%s\n  cpy: %s\n  apy: %s" % (k, cpy[k], apy[k]))
SHOWEOF
    exit 0
fi

if [ "${1:-}" = "--record" ]; then
    {
        echo "# Expressions on which apython and CPython 3.12 answer the same"
        echo "# thing when a builtin is handed an argument of the wrong type."
        echo "# Regenerate with: bash tests/typefuzz_probe.sh --record"
        echo "# A case listed here must keep agreeing, and NOTHING may crash."
        cat "$WORK/agree.txt"
    } > "$FLOOR"
    echo "recorded floor: $AGREE cases -> $FLOOR"
    exit 0
fi

FAILED=0
if [ "$NCRASH" -gt 0 ]; then
    echo -e "${RED}FAIL${NC} these were killed by a signal, not refused:"
    sed 's/^/    /' "$WORK/crashes.txt"
    FAILED=1
fi

if [ -f "$FLOOR" ]; then
    grep -v '^#' "$FLOOR" | sort > "$WORK/floor.txt"
    REGRESSED=$(comm -23 "$WORK/floor.txt" "$WORK/agree.txt")
    if [ -n "$REGRESSED" ]; then
        echo -e "${RED}FAIL${NC} these agreed with CPython and no longer do:"
        echo "$REGRESSED" | sed 's/^/    /'
        FAILED=1
    fi
    GAINED=$(comm -13 "$WORK/floor.txt" "$WORK/agree.txt" | wc -l)
    if [ "$GAINED" -gt 0 ]; then
        echo "  $GAINED newly agreeing; raise the floor with --record"
    fi
else
    echo "no floor at $FLOOR; run with --record"
    FAILED=1
fi

[ "$FAILED" -eq 0 ] || exit 1
echo -e "${GREEN}PASS${NC} typefuzz scoreboard: $AGREE agree, $DIFFER still differ, 0 crash"
