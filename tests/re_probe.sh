#!/bin/bash
# re_probe.sh - a differential test of the regex engine against CPython.
#
# tests/re_differential.py runs several hundred patterns and prints only
# span() and groups() -- never a Match object, whose repr differs.  This runs
# it under both interpreters and diffs.
#
# It is not part of `make check`: `re` is a PYTHON module, and lib/README.md's
# rule is that lib/ stands in for CPython's C modules, not its Python ones --
# so `import re` used to need a real stdlib on $CPYTHON_LIB.  apython ships
# `re` itself now, and only the oracle needs one -- which python3 has.
#
# It is deliberately NOT named test_*.py: run_tests.sh auto-discovers those,
# and this one needs $CPYTHON_LIB.  tests/test_re_opcodes.py is the version
# that runs in the ordinary gate, driving _sre directly.
#
# The floor in tests/re_floor.txt is the set of lines that are ALLOWED to
# differ, one per line, not a count.  A count cannot tell an improvement from
# a regression that an improvement elsewhere happens to offset -- and this
# engine is being fixed a family at a time, so that is the normal case rather
# than a corner one.  Record it with --record in the commit that earns it.
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

# apython ships its own `re` now, so only the ORACLE needs a real stdlib --
# and python3 has one.  $CPYTHON_LIB is still honoured, and still puts the
# same library in front of both sides, but it is no longer required.

W=$(mktemp -d)
trap 'rm -rf "$W"' EXIT

python3 -m py_compile "$TEST" 2>/dev/null || { echo "${RED}FAIL${OFF} cannot compile $TEST"; exit 1; }
PYC="tests/__pycache__/$(basename "$TEST" .py).cpython-312.pyc"

if [ -d "$CPYTHON_LIB/re" ]; then
    export PYTHONPATH="$CPYTHON_LIB"
fi
python3 "$TEST" > "$W/expected" 2>&1
timeout 300 ./apython "$PYC" > "$W/actual" 2>&1
rc=$?
if [ $rc -ge 128 ]; then
    echo "${RED}CRASH${OFF} re differential: apython died with signal $((rc - 128))"
    exit 1
fi

total=$(wc -l < "$W/expected")
# The expected-side lines the actual output does not reproduce.  Sorted, so
# that the floor file is stable against a reordering of the differential.
diff "$W/expected" "$W/actual" | sed -n 's/^< //p' | sort > "$W/differing"
differing=$(wc -l < "$W/differing")
matching=$((total - differing))

echo "re differential: $matching matching, $differing differing, $total total"

if [ -n "$RECORD" ]; then
    cp "$W/differing" "$FLOOR"
    echo "recorded floor: $differing lines allowed to differ -> $FLOOR"
    exit 0
fi

if [ ! -f "$FLOOR" ]; then
    echo "${YELLOW}NOTE${OFF} no floor recorded; run 'bash tests/re_probe.sh --record'"
    exit 0
fi

sort "$FLOOR" > "$W/floor"
# A line that differs now and was not allowed to is a regression, whatever
# the totals say.
comm -23 "$W/differing" "$W/floor" > "$W/new"
fixed=$(comm -13 "$W/differing" "$W/floor" | wc -l)
if [ -s "$W/new" ]; then
    echo "${RED}FAIL${OFF} re scoreboard: $(wc -l < "$W/new") answer(s) that used to match now differ"
    head -20 "$W/new"
    exit 1
fi
if [ "$fixed" -gt 0 ]; then
    echo "${GREEN}PASS${OFF} re scoreboard: $matching matching, $fixed newly fixed (record it with --record)"
    exit 0
fi
echo "${GREEN}PASS${OFF} re scoreboard: $matching matching, $differing differing"
