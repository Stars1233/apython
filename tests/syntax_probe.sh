#!/bin/bash
# syntax_probe.sh - does a syntax error say what CPython's says, and where?
#
# A SyntaxError carries five things a program can read: msg, lineno, offset,
# end_lineno and end_offset.  This compiler's were its own -- "expected ':'"
# where CPython says something longer, the column of the token the parser
# stopped at rather than the one CPython blames, and a span one character
# wide where CPython covers a whole token or the subexpression the message is
# about.
#
# Every snippet below is compiled by both interpreters and the five fields are
# compared.  Ratcheted against tests/syntax_floor.txt: a snippet that matched
# and no longer does fails the target.  The corpus is checked in rather than
# generated -- each line is a shape someone actually writes.

set -u

APYTHON=${APYTHON:-./apython}
PYTHON=${PYTHON:-python3}
TESTDIR="$(cd "$(dirname "$0")" && pwd)"
ROOT="$(dirname "$TESTDIR")"
FLOOR="$TESTDIR/syntax_floor.txt"
CORPUS="$TESTDIR/syntax_corpus.txt"
WORK=$(mktemp -d)
trap 'rm -rf "$WORK"' EXIT

GREEN='\033[0;32m'; RED='\033[0;31m'; NC='\033[0m'

case "$APYTHON" in
    /*) APY="$APYTHON" ;;
    *)  APY="$ROOT/${APYTHON#./}" ;;
esac
[ -x "$APY" ] || { echo "SKIP: $APYTHON not built"; exit 0; }
[ -f "$CORPUS" ] || { echo "no corpus at $CORPUS"; exit 1; }

# The corpus is one snippet per line, with \n for a newline so a multi-line
# snippet stays on one line.  A line starting with # is a comment.
cat > "$WORK/probe.py" <<PYEOF
CORPUS = "$CORPUS"

with open(CORPUS) as fh:
    lines = [L.rstrip("\n") for L in fh]

for n, raw in enumerate(lines):
    if not raw or raw.startswith("#"):
        continue
    src = raw.replace("\\\\n", "\n")
    try:
        compile(src, "<probe>", "exec")
        print("%d|ok" % n)
    except SyntaxError as e:
        print("%d|%s|%s|%s|%s|%s" % (n, e.msg, e.lineno, e.offset,
                                     e.end_lineno, e.end_offset))
    except Exception as e:
        print("%d|%s: %s" % (n, type(e).__name__, e))
PYEOF

"$PYTHON" -m py_compile "$WORK/probe.py" 2>/dev/null || {
    echo "probe.py does not compile under $PYTHON"; exit 1; }

"$PYTHON" "$WORK/probe.py" 2>/dev/null | sort -t'|' -k1,1n > "$WORK/cpython.txt"
timeout 60 "$APY" "$WORK/__pycache__/probe.cpython-312.pyc" 2>/dev/null \
    | sort -t'|' -k1,1n > "$WORK/apython.txt"

if [ ! -s "$WORK/apython.txt" ]; then
    echo -e "${RED}FAIL${NC} syntax probe: apython produced nothing"
    exit 1
fi

join -t'|' -o 0,1.2,2.2 -j1 \
     <(sed 's/|/\t/' "$WORK/cpython.txt" | awk -F'\t' '{print $1 "|" $2}') \
     <(sed 's/|/\t/' "$WORK/apython.txt" | awk -F'\t' '{print $1 "|" $2}') \
     > "$WORK/both.txt" 2>/dev/null

awk -F'|' '$2 == $3 {print $1}' "$WORK/both.txt" | sort > "$WORK/agree.txt"
awk -F'|' '$2 != $3 {print $1}' "$WORK/both.txt" | sort -n > "$WORK/differ.txt"

AGREE=$(wc -l < "$WORK/agree.txt")
DIFFER=$(wc -l < "$WORK/differ.txt")

echo "syntax errors: $AGREE identical, $DIFFER differing"

if [ "${1:-}" = "--record" ]; then
    {
        echo "# Snippets of tests/syntax_corpus.txt on which our SyntaxError's"
        echo "# msg, lineno, offset, end_lineno and end_offset are all CPython"
        echo "# 3.12's.  Regenerate with: bash tests/syntax_probe.sh --record"
        echo "# A snippet listed here must keep matching."
        sort -n "$WORK/agree.txt"
    } > "$FLOOR"
    echo "recorded floor: $AGREE snippets -> $FLOOR"
    exit 0
fi

if [ "${1:-}" = "--show" ]; then
    echo
    while read -r n; do
        printf '%3s  %s\n' "$n" "$(sed -n "$((n + 1))p" "$CORPUS")"
        printf '     cpython: %s\n' "$(grep "^$n|" "$WORK/cpython.txt" | cut -d'|' -f2-)"
        printf '     apython: %s\n' "$(grep "^$n|" "$WORK/apython.txt" | cut -d'|' -f2-)"
    done < "$WORK/differ.txt"
    exit 0
fi

[ -f "$FLOOR" ] || { echo "no floor at $FLOOR; run with --record"; exit 1; }

grep -v '^#' "$FLOOR" | sort > "$WORK/floor.txt"
REGRESSED=$(comm -23 "$WORK/floor.txt" "$WORK/agree.txt")
if [ -n "$REGRESSED" ]; then
    echo -e "${RED}FAIL${NC} these matched CPython and no longer do:"
    while read -r n; do
        [ -n "$n" ] || continue
        printf '    %3s  %s\n' "$n" "$(sed -n "$((n + 1))p" "$CORPUS")"
    done <<< "$REGRESSED"
    exit 1
fi
GAINED=$(comm -13 "$WORK/floor.txt" "$WORK/agree.txt" | wc -l)
if [ "$GAINED" -gt 0 ]; then
    echo "  $GAINED newly matching; raise the floor with --record"
fi
echo -e "${GREEN}PASS${NC} syntax scoreboard: $AGREE identical, $DIFFER still differ"
