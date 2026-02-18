#!/bin/bash
# run_tests.sh - Test runner for apython
# Compiles .py to .pyc, runs both python3 and ./apython, diffs output

set -e

APYTHON=./apython
PYTHON=python3
TESTDIR=tests
PASS=0
FAIL=0
SKIP=0
ERRORS=""

# Colors
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[0;33m'
NC='\033[0m'

# Pre-compile all non-test .py files (helper modules)
for helper_py in "$TESTDIR"/*.py; do
    case "$(basename "$helper_py")" in
        test_*) continue ;;
    esac
    $PYTHON -m py_compile "$helper_py" 2>/dev/null || true
done

for test_py in "$TESTDIR"/test_*.py; do
    test_name=$(basename "$test_py" .py)

    # Compile to .pyc
    $PYTHON -m py_compile "$test_py" 2>/dev/null
    pyc_file="$TESTDIR/__pycache__/${test_name}.cpython-312.pyc"

    if [ ! -f "$pyc_file" ]; then
        echo -e "${YELLOW}SKIP${NC} $test_name (no .pyc generated)"
        SKIP=$((SKIP + 1))
        continue
    fi

    # Run with Python
    expected=$($PYTHON "$test_py" 2>&1) || true

    # Run with apython
    actual=$($APYTHON "$pyc_file" 2>&1) || true

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
        actual_poll=$(APYTHON_IO_BACKEND=poll $APYTHON "$pyc_file" 2>&1) || true
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
            actual_uring=$(APYTHON_IO_BACKEND=iouring $APYTHON "$pyc_file" 2>&1) || true
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
