#!/bin/bash
# cpython_source_probe.sh - run the CPython-derived corpus through our compiler.
#
# make check-cpython hands apython a .pyc that CPython produced; this hands it
# the .py.  The corpus is CPython's own and written to be adversarial, so it
# reaches corners tests/ does not: it is where the ternary's associativity, the
# lambda body's binding power, the starred loop target, `yield a, b` and the
# class-body closure were all found.
#
# Until 2026-08-31 it only checked the exit status, so a file that ran to
# completion while reporting a different set of results counted as passing --
# which is most of what a differential gate is supposed to catch.
#
# The oracle is the *same file run from its .pyc*, which is what make
# check-cpython already does.  Both sides are then the same interpreter and the
# same lib/unittest.py, and the only thing that differs is which compiler
# produced the bytecode: CPython's, or ours.  A difference is therefore
# attributable to our compiler, which is the whole point of this target.
#
# python3 cannot be the oracle here the way it is in source_probe.sh.  This
# corpus drives our own lib/unittest.py, whose per-test progress output differs
# from CPython's by design, and two of its files import test.seq_tests /
# test.test_grammar -- support modules that ship in lib/ and that a system
# CPython cannot find at all.
#
# What this gate does NOT compare is the bytecode.  Two compilers may fold,
# order and lay out differently and both be right, so diffing their output
# would be measuring style, not correctness.  Diffing the *behaviour* of the
# programs they produce is immune to that -- with a narrow and known band of
# exceptions, where a compiler's choices are legitimately observable:
#
#   - Identity of constants one compiler folds and the other does not.
#     `"ab" * 3 is "ababab"` is True from CPython's .pyc and False from ours;
#     equal constants are not guaranteed to be the same object, so neither
#     answer is wrong.
#   - Code-object introspection: co_consts contents and order, co_stacksize,
#     co_flags, co_varnames ordering.
#   - Traceback text, where the line and column tables differ.
#   - The wording and position of a compile-time error.
#   - A compile-time WARNING.  CPython emits SyntaxWarnings while compiling,
#     so its .pyc was built with them already reported and the run is silent;
#     compiling the same file here reports them now.  Both are right, and
#     summarize() drops them for the same reason it drops elapsed times.
#
# A newly differing file must be triaged against that list before it is treated
# as a regression.  If it turns out to be one of these, leave it off the floor
# with a comment saying which -- do not "fix" the compiler to match CPython's
# choices.  As of 2026-08-31 all 64 files match and the floor has no such
# exceptions.
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

# The comparable part of a unittest run: the test count, the verdict line with
# its counts, and which tests failed or errored.  Progress characters, per-test
# tracing and the elapsed time are dropped -- they differ between the two
# runners for reasons that are not divergence.
# Elapsed times legitimately differ between two runs of the same code.
summarize() {
    sed 's/ in [0-9.]*s$//' \
        | awk '/: SyntaxWarning: /{skip=1; next} skip{skip=0; next} {print}'
}

ok=""
bad=""
crashed=""
for t in $TESTS; do
    src="tests/cpython/$t.py"
    pyc="tests/cpython/__pycache__/$t.cpython-312.pyc"

    if [ ! -f "$pyc" ]; then
        echo "missing $pyc; run: make gen-cpython-tests"
        exit 1
    fi

    expected=$(timeout 60 $APYTHON "$pyc" 2>&1 | summarize) || true
    actual=$(timeout 60 $APYTHON "$src" 2>&1 | summarize)
    status=${PIPESTATUS[0]}

    if [ $status -ge 128 ]; then
        crashed="$crashed $t"
        bad="$bad $t"
    elif [ "$expected" = "$actual" ]; then
        ok="$ok $t"
    else
        bad="$bad $t"
    fi
done

n_ok=$(echo $ok | wc -w)
n_bad=$(echo $bad | wc -w)
n_crash=$(echo $crashed | wc -w)

if [ $RECORD -eq 1 ]; then
    {
        echo "# CPython-corpus files whose output under our own compiler"
        echo "# matches the same file run from CPython's .pyc."
        echo "# Recorded by: bash tests/cpython_source_probe.sh --record"
        echo "# A name here that stops matching is a regression."
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
echo "cpython corpus from source: $n_ok matching, $n_bad differing, $n_crash crashing"
[ -n "$bad" ] && echo "  differing:$bad"
[ -n "$crashed" ] && echo "  crashing:$crashed"
if [ -n "$regressed" ]; then
    echo -e "${RED}REGRESSED${NC}$regressed"
    exit 1
fi
[ -n "$gained" ] && echo -e "${YELLOW}NEW${NC}$gained (raise the floor: bash tests/cpython_source_probe.sh --record)"
echo -e "${GREEN}PASS${NC} cpython-source scoreboard: $n_ok matching, $n_crash crashing"
exit 0
