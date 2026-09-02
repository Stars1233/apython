#!/bin/bash
# run_tests.sh - Test runner for apython
# Compiles .py to .pyc, runs both python3 and ./apython, diffs output

set -e

APYTHON=./apython
PYTHON=python3
TESTDIR=tests
TIMEOUT=60          # no test should take anywhere near this; a hang must not
                    # wedge the run until CI's own job limit kills it
PASS=0
FAIL=0
SKIP=0
ERRORS=""

# Private scratch, so two concurrent runs cannot clobber each other's output
# and a leftover file owned by someone else cannot make a selftest "fail".
WORK=$(mktemp -d)
trap 'rm -rf "$WORK"' EXIT

# Colors
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[0;33m'
NC='\033[0m'

# Value encoding self-test: verifies the NaN-box boundaries directly, before
# any Python-level test can be misled by a mis-encoded value.
printf "%-40s " "value encoding selftest"
if $APYTHON --selftest-value > "$WORK/selftest.out" 2>&1; then
    printf "${GREEN}PASS${NC}\n"
    PASS=$((PASS + 1))
else
    printf "${RED}FAIL${NC}\n"
    cat "$WORK/selftest.out"
    FAIL=$((FAIL + 1))
    ERRORS="$ERRORS value-selftest"
fi

# Static checks over the assembly: 64-bit reads of 4-byte struct fields, and
# calls made with a misaligned rsp.  Both assemble cleanly and fail at runtime
# far from the cause, so they are checked here rather than discovered.
printf "%-40s " "compiler lint"
if $PYTHON src/compiler/lint.py > "$WORK/lint.out" 2>&1; then
    printf "${GREEN}PASS${NC}\n"
    PASS=$((PASS + 1))
else
    printf "${RED}FAIL${NC}\n"
    cat "$WORK/lint.out"
    FAIL=$((FAIL + 1))
    ERRORS="$ERRORS compiler-lint"
fi

# Source-compiler self-test: checks the compiler's encoders directly, against
# the decoders the interpreter will actually use.  Runs before any Python-level
# test, because an encoding bug produces symptoms that look nothing like their
# cause.
printf "%-40s " "compiler selftest"
if $APYTHON --selftest-compile > "$WORK/comptest.out" 2>&1; then
    printf "${GREEN}PASS${NC}\n"
    PASS=$((PASS + 1))
else
    printf "${RED}FAIL${NC}\n"
    cat "$WORK/comptest.out"
    FAIL=$((FAIL + 1))
    ERRORS="$ERRORS compile-selftest"
fi

# Pre-compile all non-test .py files (helper modules)
for helper_py in "$TESTDIR"/*.py; do
    case "$(basename "$helper_py")" in
        test_*) continue ;;
    esac
    $PYTHON -m py_compile "$helper_py" 2>/dev/null || true
done

for test_py in "$TESTDIR"/test_*.py; do
    test_name=$(basename "$test_py" .py)

    # Compile to .pyc.  Guarded: under `set -e` an unguarded failure here
    # aborts the whole run mid-loop, before the summary, which makes the SKIP
    # branch just below unreachable and the symptom a run that simply stops.
    $PYTHON -m py_compile "$test_py" 2>/dev/null || true
    pyc_file="$TESTDIR/__pycache__/${test_name}.cpython-312.pyc"

    if [ ! -f "$pyc_file" ]; then
        echo -e "${YELLOW}SKIP${NC} $test_name (no .pyc generated)"
        SKIP=$((SKIP + 1))
        continue
    fi

    # Oracle: normally CPython itself.  A handful of tests exercise private
    # APIs that CPython does not validate and that crash it outright — those
    # record their expected output in tests/expected/ instead.  Such a test
    # must be self-checking (assert on every step) so the recording is a
    # transcript of a verified run, not the definition of correct.
    expected_file="$TESTDIR/expected/${test_name}.txt"
    if [ -f "$expected_file" ]; then
        expected=$(cat "$expected_file")
    else
        expected=$(timeout $TIMEOUT $PYTHON "$test_py" 2>&1) || true
    fi

    # tests/srcpkg exists to be imported from source.  Producing the expected
    # output above ran CPython over it, which left a __pycache__ behind -- and
    # with one there apython reads the .pyc and the test proves nothing.
    rm -rf "$TESTDIR"/srcpkg/__pycache__ "$TESTDIR"/srcpkg/*/__pycache__

    # Run with apython
    actual=$(timeout $TIMEOUT $APYTHON "$pyc_file" 2>&1) || true

    # Compare
    if [ "$expected" = "$actual" ]; then
        echo -e "${GREEN}PASS${NC} $test_name"
        PASS=$((PASS + 1))
    else
        echo -e "${RED}FAIL${NC} $test_name"
        FAIL=$((FAIL + 1))
        ERRORS="$ERRORS\n--- $test_name ---\nExpected:\n$expected\nActual:\n$actual\n"
    fi

    # Dual-backend testing for async tests
    if [[ "$test_name" == test_async_* ]]; then
        # Test with poll backend
        actual_poll=$(APYTHON_IO_BACKEND=poll timeout $TIMEOUT $APYTHON "$pyc_file" 2>&1) || true
        if [ "$expected" != "$actual_poll" ]; then
            echo -e "${RED}FAIL${NC} $test_name (poll backend)"
            FAIL=$((FAIL + 1))
            ERRORS="$ERRORS\n--- $test_name (poll) ---\nExpected:\n$expected\nActual:\n$actual_poll\n"
        else
            echo -e "${GREEN}PASS${NC} $test_name (poll)"
            PASS=$((PASS + 1))
        fi
        # Test with iouring backend (Linux only)
        if [ "$(uname)" = "Linux" ]; then
            actual_uring=$(APYTHON_IO_BACKEND=iouring timeout $TIMEOUT $APYTHON "$pyc_file" 2>&1) || true
            if [ "$expected" != "$actual_uring" ]; then
                echo -e "${YELLOW}SKIP${NC} $test_name (iouring — may need newer kernel)"
                SKIP=$((SKIP + 1))
            else
                echo -e "${GREEN}PASS${NC} $test_name (iouring)"
                PASS=$((PASS + 1))
            fi
        fi
    fi
done

echo ""
echo "Results: $PASS passed, $FAIL failed, $SKIP skipped"

if [ -n "$ERRORS" ]; then
    echo -e "\nFailure details:$ERRORS"
    exit 1
fi
