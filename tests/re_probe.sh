#!/bin/bash
# re_probe.sh - a differential test of the regex engine against CPython.
#
# tests/re_differential.py runs several hundred patterns and prints only
# span() and groups() -- never a Match object, whose repr differs.  This runs
# it under both interpreters and diffs.
#
# It is not part of `make check`: `re` is a PYTHON module, and lib/README.md's
# rule is that lib/ stands in for CPython's C modules, not its Python ones --
# so `import re` needs a real stdlib on $CPYTHON_LIB, exactly as `os` does.
#
# It is deliberately NOT named test_*.py: run_tests.sh auto-discovers those,
# and this one needs $CPYTHON_LIB.  tests/test_re_opcodes.py is the version
# that runs in the ordinary gate, driving _sre directly.
#
# The score ratchets against tests/re_floor.txt, the way stdlib_probe.sh and
# source_probe.sh do.  Raise it with --record in the commit that earns it.
#
#   CPYTHON_LIB=~/tmp/repo/cpython/Lib bash tests/re_probe.sh
#   bash tests/re_probe.sh --record

set -u
cd "$(dirname "$0")/.."

CPYTHON_LIB="${CPYTHON_LIB:-$HOME/tmp/repo/cpython/Lib}"
TEST=tests/re_differential.py
FLOOR=tests/re_floor.txt
RECORD=""
[ "${1:-}" = "--record" ] && RECORD=1

GREEN=$'\033[0;32m'; RED=$'\033[0;31m'; YELLOW=$'\033[0;33m'; OFF=$'\033[0m'

if [ ! -d "$CPYTHON_LIB/re" ]; then
    echo "${YELLOW}SKIP${OFF} re differential: no CPython Lib at $CPYTHON_LIB"
    echo "     (set CPYTHON_LIB to a CPython 3.12 checkout's Lib/)"
    exit 0
fi

W=$(mktemp -d)
trap 'rm -rf "$W"' EXIT

python3 -m py_compile "$TEST" 2>/dev/null || { echo "${RED}FAIL${OFF} cannot compile $TEST"; exit 1; }
PYC="tests/__pycache__/$(basename "$TEST" .py).cpython-312.pyc"

PYTHONPATH="$CPYTHON_LIB" python3 "$TEST" > "$W/expected" 2>&1
PYTHONPATH="$CPYTHON_LIB" timeout 300 ./apython "$PYC" > "$W/actual" 2>&1
rc=$?
if [ $rc -ge 128 ]; then
    echo "${RED}CRASH${OFF} re differential: apython died with signal $((rc - 128))"
    exit 1
fi

total=$(wc -l < "$W/expected")
# Count lines present in expected that the actual output does not reproduce.
differing=$(diff "$W/expected" "$W/actual" | grep -c '^<')
matching=$((total - differing))

echo "re differential: $matching matching, $differing differing, $total total"

if [ -n "$RECORD" ]; then
    printf '%s\n' "$matching" > "$FLOOR"
    echo "recorded floor: $matching matching lines -> $FLOOR"
    exit 0
fi

if [ ! -f "$FLOOR" ]; then
    echo "${YELLOW}NOTE${OFF} no floor recorded; run 'bash tests/re_probe.sh --record'"
    exit 0
fi

floor=$(head -1 "$FLOOR")
if [ "$matching" -lt "$floor" ]; then
    echo "${RED}FAIL${OFF} re scoreboard: $matching matching, below the floor of $floor"
    diff "$W/expected" "$W/actual" | head -40
    exit 1
fi
if [ "$matching" -gt "$floor" ]; then
    echo "${GREEN}PASS${OFF} re scoreboard: $matching matching (floor $floor -- raise it with --record)"
    exit 0
fi
echo "${GREEN}PASS${OFF} re scoreboard: $matching matching"
