#!/bin/bash
# cpython_source_probe.sh - run the CPython-derived corpus through our compiler.
#
# make check-cpython hands apython a .pyc that CPython produced; this hands it
# the .py.  The corpus is CPython's own and written to be adversarial, so it
# reaches corners tests/ does not: it is where the ternary's associativity, the
# lambda body's binding power, the starred loop target, `yield a, b` and the
# class-body closure were all found.
#
# Ratchets against tests/cpython_source_floor.txt.  Raise the floor with
#
#     bash tests/cpython_source_probe.sh --record
#
# in the commit that earns it.
set -u

APYTHON=./apython
FLOOR=tests/cpython_source_floor.txt
RECORD=0
[ "${1:-}" = "--record" ] && RECORD=1

RED='\033[0;31m'; GREEN='\033[0;32m'; YELLOW='\033[0;33m'; NC='\033[0m'

# One list, shared with the Makefile.
TESTS=$(sed -n '/^CPYTHON_TESTS = /,/^$/p' Makefile | tr -d '\\' | sed 's/CPYTHON_TESTS =//')

ok=""
bad=""
for t in $TESTS; do
    if timeout 60 $APYTHON "tests/cpython/$t.py" > /dev/null 2>&1; then
        ok="$ok $t"
    else
        bad="$bad $t"
    fi
done

n_ok=$(echo $ok | wc -w)
n_bad=$(echo $bad | wc -w)

if [ $RECORD -eq 1 ]; then
    {
        echo "# CPython-corpus files that run under our own compiler."
        echo "# Recorded by: bash tests/cpython_source_probe.sh --record"
        echo "# A name here that stops running is a regression."
        for t in $ok; do echo "$t"; done
    } > "$FLOOR"
    echo "recorded $n_ok passing files to $FLOOR"
    exit 0
fi

if [ ! -f "$FLOOR" ]; then
    echo "no $FLOOR; run: bash tests/cpython_source_probe.sh --record"
    exit 1
fi

regressed=""
for t in $(grep -v '^#' "$FLOOR" | grep -v '^$'); do
    case " $ok " in
        *" $t "*) ;;
        *) regressed="$regressed $t" ;;
    esac
done

gained=""
for t in $ok; do
    grep -qx "$t" "$FLOOR" || gained="$gained $t"
done

echo ""
echo "cpython corpus from source: $n_ok passing, $n_bad failing"
[ -n "$bad" ] && echo "  failing:$bad"
if [ -n "$regressed" ]; then
    echo -e "${RED}REGRESSED${NC}$regressed"
    exit 1
fi
[ -n "$gained" ] && echo -e "${YELLOW}NEW${NC}$gained (raise the floor: bash tests/cpython_source_probe.sh --record)"
echo -e "${GREEN}PASS${NC} cpython-source scoreboard: $n_ok passing"
exit 0
