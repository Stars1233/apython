#!/bin/bash
# stdlib_probe.sh - can apython import the CPython 3.12 standard library?
#
# The goal is that apython is a drop-in replacement for CPython 3.12, so the
# blunt measure of progress is how much of CPython's own Lib/ it can import.
# Each module is imported in a fresh process and classified:
#
#   OK     the module imported and `print("OK")` ran
#   FAIL   it raised -- an honest error, the interpreter survived
#   CRASH  the process died on a signal, or hung
#
# The result is compared against tests/stdlib_floor.txt, which records the OK
# set and the crash count at the last commit that moved them.  A module that
# was OK and is no longer, or a rise in crashes, fails the target: this is a
# ratchet, not a report.
#
# Needs CPython's Lib/ (a source checkout, not an installed python).  Point
# $CPYTHON_LIB at it; it reaches the interpreter as PYTHONPATH.  The target
# skips cleanly when it is not there, so a build never depends on an
# out-of-tree checkout.

set -u

APYTHON=${APYTHON:-./apython}
PYTHON=${PYTHON:-python3}
TESTDIR="$(cd "$(dirname "$0")" && pwd)"
ROOT="$(dirname "$TESTDIR")"
FLOOR="$TESTDIR/stdlib_floor.txt"
CPYTHON_LIB=${CPYTHON_LIB:-$HOME/tmp/repo/cpython/Lib}
WORK=$(mktemp -d)
trap 'rm -rf "$WORK"' EXIT

GREEN='\033[0;32m'; RED='\033[0;31m'; YELLOW='\033[0;33m'; NC='\033[0m'

if [ ! -d "$CPYTHON_LIB" ]; then
    echo "SKIP: no CPython Lib/ at $CPYTHON_LIB (set CPYTHON_LIB to override)"
    exit 0
fi
if [ ! -x "$ROOT/$APYTHON" ] && [ ! -x "$APYTHON" ]; then
    echo "SKIP: $APYTHON not built"
    exit 0
fi
case "$APYTHON" in
    /*) APY="$APYTHON" ;;
    *)  APY="$ROOT/${APYTHON#./}" ;;
esac

# ---------------------------------------------------------------------------
# The module list is derived from Lib/ rather than checked in, so it tracks
# whichever CPython the developer has.  Everything under test/ is excluded --
# that is CPython's own test suite, not the library.
# ---------------------------------------------------------------------------
list_modules() {
    ( cd "$CPYTHON_LIB" || exit
      ls *.py 2>/dev/null | sed 's/\.py$//'
      for d in */; do
          d=${d%/}
          [ "$d" = "test" ] && continue
          [ "$d" = "__pycache__" ] && continue
          [ -f "$d/__init__.py" ] && echo "$d"
      done ) | sort -u | grep -v '^test'
}

# apython reads .pyc, never .py, so the reference tree has to be byte-compiled
# before anything can be found in it.  A source checkout is not, and CPython
# gitignores __pycache__, so doing it in place is invisible.  Skip CPython's
# own test suite -- it is large and nothing here imports it.
if [ ! -f "$CPYTHON_LIB/__pycache__/os.cpython-312.pyc" ]; then
    echo "byte-compiling $CPYTHON_LIB (one time)..."
    $PYTHON -m compileall -q -j0 -x '[/\\]test[/\\]' "$CPYTHON_LIB" >/dev/null 2>&1
fi

RESULTS="$WORK/results.txt"
: > "$RESULTS"

probe_one() {
    local m="$1"
    printf 'import %s\nprint("OK")\n' "$m" > "$WORK/probe.py"
    if ! $PYTHON -m py_compile "$WORK/probe.py" 2>/dev/null; then
        echo "$m SYNTAX" >> "$RESULTS"
        return
    fi
    local out rc
    # Run from inside Lib/ so apython's own lib/ and tests/cpython shims --
    # both *relative* sys.path entries -- do not resolve and shadow the real
    # stdlib.  What is being measured is CPython's library, not our stand-ins.
    out=$(cd "$CPYTHON_LIB" && PYTHONPATH="$CPYTHON_LIB" timeout 20 "$APY" \
          "$WORK/__pycache__/probe.cpython-312.pyc" 2>&1)
    rc=$?
    if [ "$out" = "OK" ]; then
        echo "$m OK" >> "$RESULTS"
    elif [ $rc -ge 132 ] && [ $rc -le 139 ]; then
        echo "$m CRASH" >> "$RESULTS"
    elif [ $rc -eq 124 ]; then
        echo "$m HANG" >> "$RESULTS"
    else
        echo "$m FAIL" >> "$RESULTS"
    fi
}

echo "Probing $(list_modules | wc -l) stdlib modules from $CPYTHON_LIB"
while read -r m; do probe_one "$m"; done < <(list_modules)

OK=$(grep -c ' OK$'    "$RESULTS" || true)
FAIL=$(grep -c ' FAIL$'  "$RESULTS" || true)
CRASH=$(( $(grep -c ' CRASH$' "$RESULTS" || true) + $(grep -c ' HANG$' "$RESULTS" || true) ))
TOTAL=$(wc -l < "$RESULTS")

echo
echo "  OK    $OK"
echo "  FAIL  $FAIL"
echo "  CRASH $CRASH"
echo "  total $TOTAL"

# --record rewrites the floor instead of checking against it.
if [ "${1:-}" = "--record" ]; then
    {
        echo "# Modules that import under apython, and the crash count."
        echo "# Regenerate with: bash tests/stdlib_probe.sh --record"
        echo "# A module listed here must keep importing; crashes must not rise."
        echo "crash_ceiling $CRASH"
        grep -E ' (CRASH|HANG)$' "$RESULTS" | awk '{print "crash " $1}' | sort
        grep ' OK$' "$RESULTS" | awk '{print "ok " $1}' | sort
    } > "$FLOOR"
    echo
    echo "recorded floor: $OK OK, crash ceiling $CRASH -> $FLOOR"
    exit 0
fi

if [ ! -f "$FLOOR" ]; then
    echo "no floor file at $FLOOR; run with --record to create it"
    exit 1
fi

CEILING=$(awk '/^crash_ceiling /{print $2}' "$FLOOR")
awk '/^ok /{print $2}' "$FLOOR" | sort > "$WORK/floor_ok.txt"
grep ' OK$' "$RESULTS" | awk '{print $1}' | sort > "$WORK/now_ok.txt"

REGRESSED=$(comm -23 "$WORK/floor_ok.txt" "$WORK/now_ok.txt")
GAINED=$(comm -13 "$WORK/floor_ok.txt" "$WORK/now_ok.txt")

rc=0
if [ -n "$GAINED" ]; then
    echo
    echo -e "${GREEN}newly importing${NC} ($(echo "$GAINED" | wc -w)):"
    echo "$GAINED" | tr '\n' ' ' | fold -s -w 76 | sed 's/^/  /'
    echo
    echo "  (run 'bash tests/stdlib_probe.sh --record' to raise the floor)"
fi
if [ -n "$REGRESSED" ]; then
    echo
    echo -e "${RED}REGRESSED${NC} — these imported before and no longer do:"
    echo "$REGRESSED" | tr '\n' ' ' | fold -s -w 76 | sed 's/^/  /'
    echo
    rc=1
fi
# The crash *set* is recorded, not just the count: a module that starts
# crashing must be caught even if another stopped.
awk '/^crash /{print $2}' "$FLOOR" | sort > "$WORK/floor_crash.txt"
grep -E ' (CRASH|HANG)$' "$RESULTS" | awk '{print $1}' | sort > "$WORK/now_crash.txt"
NEWCRASH=$(comm -13 "$WORK/floor_crash.txt" "$WORK/now_crash.txt")
FIXEDCRASH=$(comm -23 "$WORK/floor_crash.txt" "$WORK/now_crash.txt")
if [ -n "$NEWCRASH" ]; then
    echo
    echo -e "${RED}NEW CRASHES${NC} — these did not crash before:"
    echo "$NEWCRASH" | tr '\n' ' ' | fold -s -w 76 | sed 's/^/  /'
    echo
    rc=1
fi
if [ -n "$FIXEDCRASH" ]; then
    echo
    echo -e "${GREEN}no longer crashing${NC}:"
    echo "$FIXEDCRASH" | tr '\n' ' ' | fold -s -w 76 | sed 's/^/  /'
    echo
    echo "  (record to tighten)"
fi

if [ $rc -eq 0 ]; then
    echo
    echo -e "${GREEN}PASS${NC} stdlib scoreboard: $OK importing, $CRASH crashing"
else
    echo
    echo -e "${RED}FAIL${NC} stdlib scoreboard"
fi
exit $rc
