#!/bin/bash
# pyc_probe.sh - does a malformed .pyc get refused, or run?
#
# A .pyc is a file on disk, and often not one written by the program running
# it.  The marshal reader validated offsets and lengths and never TYPES, so a
# crafted stream could hand co_names a tuple of ints and eval_frame would
# dereference one as a PyStrObject, or hand co_code an int and the dispatcher
# would jump through it.  frame_new added two 32-bit fields in 32 bits, so a
# pair near 2^31 wrapped to a small total and the frame came out too short.
#
# These cannot be a tests/test_*.py: the point is what happens when the
# interpreter is handed a file, not what a program can do from inside one.
# Each case must exit non-zero with a message, and must not be killed by a
# signal -- an exit status of 139 is the bug this exists to catch.
#
# The files are assembled here rather than checked in, because the magic
# number belongs to whichever CPython built them.

set -u

APYTHON=${APYTHON:-./apython}
PYTHON=${PYTHON:-python3}
TESTDIR="$(cd "$(dirname "$0")" && pwd)"
ROOT="$(dirname "$TESTDIR")"
WORK=$(mktemp -d)
trap 'rm -rf "$WORK"' EXIT

GREEN='\033[0;32m'; RED='\033[0;31m'; NC='\033[0m'

case "$APYTHON" in
    /*) APY="$APYTHON" ;;
    *)  APY="$ROOT/${APYTHON#./}" ;;
esac
[ -x "$APY" ] || { echo "SKIP: $APYTHON not built"; exit 0; }

"$PYTHON" - "$WORK" <<'PYEOF'
"""Assemble .pyc files by hand, so one field can be given the wrong type."""
import importlib.util
import marshal
import struct
import sys

out = sys.argv[1]


def code_record(**kw):
    f = dict(argcount=0, posonly=0, kwonly=0, stacksize=2, flags=0,
             code=b"\x97\x00\x64\x00\x53\x00", consts=(None,), names=("x",),
             localsplusnames=(), localspluskinds=b"", filename="<crafted>",
             name="<module>", qualname="<module>", firstlineno=1,
             linetable=b"", exctable=b"")
    f.update(kw)
    rec = b"c"
    for k in ("argcount", "posonly", "kwonly", "stacksize", "flags"):
        rec += struct.pack("<i", f[k])
    for k in ("code", "consts", "names", "localsplusnames", "localspluskinds",
              "filename", "name", "qualname"):
        rec += marshal.dumps(f[k], 4)
    rec += struct.pack("<i", f["firstlineno"])
    rec += marshal.dumps(f["linetable"], 4) + marshal.dumps(f["exctable"], 4)
    return rec


def write(name, record):
    with open(out + "/" + name, "wb") as fh:
        fh.write(importlib.util.MAGIC_NUMBER)
        fh.write(b"\0" * 12)
        fh.write(record)


write("ok.pyc", code_record())
write("names_of_ints.pyc", code_record(names=(1, 2)))
write("code_is_an_int.pyc", code_record(code=42))
write("filename_is_an_int.pyc", code_record(filename=7))
write("name_is_a_tuple.pyc", code_record(name=(1,)))
write("consts_is_a_str.pyc", code_record(consts="not a tuple"))
write("linetable_is_a_list.pyc", code_record(linetable=[1, 2]))
write("kinds_is_a_str.pyc", code_record(localspluskinds="ss"))
write("huge_stack.pyc", code_record(stacksize=0x7FFFFFF0))
write("huge_stack_pair.pyc", code_record(stacksize=0x7FFFFFFF,
                                         localsplusnames=("a",) * 3))
PYEOF

FAIL=0
PASS=0

expect_refused() {
    name=$1
    "$APY" "$WORK/$name" >/dev/null 2>"$WORK/$name.err"
    status=$?
    if [ $status -ge 128 ]; then
        echo -e "${RED}FAIL${NC} $name: killed by signal $((status - 128))"
        FAIL=$((FAIL + 1))
    elif [ $status -eq 0 ]; then
        echo -e "${RED}FAIL${NC} $name: accepted"
        FAIL=$((FAIL + 1))
    elif ! grep -q "marshal:" "$WORK/$name.err"; then
        echo -e "${RED}FAIL${NC} $name: refused, but not by the marshal reader"
        cat "$WORK/$name.err"
        FAIL=$((FAIL + 1))
    else
        echo -e "${GREEN}PASS${NC} $name"
        PASS=$((PASS + 1))
    fi
}

# The well-formed one has to still run, or the checks are simply too strict.
if "$APY" "$WORK/ok.pyc" >/dev/null 2>&1; then
    echo -e "${GREEN}PASS${NC} ok.pyc (a hand-built, valid .pyc still runs)"
    PASS=$((PASS + 1))
else
    echo -e "${RED}FAIL${NC} ok.pyc: a valid hand-built .pyc was refused"
    FAIL=$((FAIL + 1))
fi

for f in names_of_ints code_is_an_int filename_is_an_int name_is_a_tuple \
         consts_is_a_str linetable_is_a_list kinds_is_a_str \
         huge_stack huge_stack_pair; do
    expect_refused "$f.pyc"
done

echo
echo "crafted .pyc: $PASS refused as they should be, $FAIL wrong"
if [ $FAIL -ne 0 ]; then
    echo -e "${RED}FAIL${NC} pyc scoreboard"
    exit 1
fi
echo -e "${GREEN}PASS${NC} pyc scoreboard: $PASS cases"
