#!/bin/bash
# gmp_align_probe.sh - is every call into GMP made with rsp 16-byte aligned?
#
# The SysV ABI wants rsp % 16 == 0 at a `call`, and GMP is built with SSE.
# Getting this wrong does not fail where it happens: the callee inherits the
# bad parity, every frame under it inherits it too, and the fault arrives
# somewhere else entirely and only when something reaches an aligned store.
# check_handler_alignment in lint.py records how the class was first found --
# libz's inflate happened to have one.
#
# THIS READS THE BUILT BINARY, and that is the point.  lint.py cannot see
# this: check_alignment counts only the pushes before the first non-push
# instruction, exempts DEF_FUNC_BARE entirely, and -- fatally -- cannot see
# inside a NASM macro, which may hide both pushes and branches.  A
# source-level detector written for this produced twenty-two false positives
# for exactly that reason and was thrown away.  objdump sees the macros
# expanded, and gdb sees the register.
#
# Every `call ...@plt` to a __gmp* symbol in .text gets a breakpoint; a
# workload that exercises every big-int path runs under it; and rsp is
# sampled at each.  Anything not zero is a bug.
#
# Needs gdb, which is not a build dependency.  Without it the probe SKIPS --
# which means a machine without gdb gets no coverage here, and the lint check
# that every GMP call goes through GMP_CALL is what still runs.

set -u

APYTHON=${APYTHON:-./apython}
PYTHON=${PYTHON:-python3}
TESTDIR="$(cd "$(dirname "$0")" && pwd)"
ROOT="$(dirname "$TESTDIR")"
cd "$ROOT"

if ! command -v gdb >/dev/null 2>&1; then
    echo "gmp alignment: SKIP (no gdb)"
    exit 0
fi
if ! command -v objdump >/dev/null 2>&1; then
    echo "gmp alignment: SKIP (no objdump)"
    exit 0
fi

WORK=$(mktemp -d)
trap 'rm -rf "$WORK"' EXIT

# The workload.  Every shape that reaches GMP: the four arithmetic operators
# and the two divisions, comparison both ways, the bit operators, shifts,
# power both plain and modular, hashing, every conversion and every
# formatting path, and the boundary either side of the immediate range --
# because a value that fits an int64 takes a different arm from one that does
# not, and only the second reaches GMP at all.
cat > "$WORK/gmpwork.py" <<'PYEOF'
B = 2 ** 70
S = 2 ** 49
vals = [B, -B, B + 1, 3 ** 90, 10 ** 40, -(7 ** 60), S, -S, S + 1, 0, 1, -1, 255]
acc = 0
for a in vals:
    for b in vals:
        if b != 0:
            acc ^= (a // b) + (a % b) + divmod(a, b)[0]
        for r in (a + b, a - b, a * b, a & b, a | b, a ^ b):
            acc ^= r
        acc ^= (a < b) + (a == b) + (a >= b) + (a != b) + (a > b) + (a <= b)
for a in vals:
    str(a); repr(a); hex(a); oct(a); bin(a); hash(a); abs(a); -a; ~a
    a >> 3; a << 3; int(str(a)); "%d" % a; f"{a}"; "{}".format(a)
    int(str(a), 10); int(hex(a), 16); a.bit_length(); bool(a)
print(pow(3, 200, 1000003), pow(2, 100), 2 ** 200 // 3 ** 50)
print(len(str(2 ** 4000)), int("9" * 60) % 7, (10 ** 50).bit_length())
print(sum(divmod(10 ** 30, 7)), max(vals), min(vals), len(sorted(vals)))
import math
print(math.gcd(2 ** 80, 3 ** 40 * 2 ** 12), math.isqrt(10 ** 40), math.factorial(30))
PYEOF
$PYTHON -m py_compile "$WORK/gmpwork.py" 2>/dev/null || {
    echo "gmp alignment: SKIP (cannot compile the workload)"; exit 0; }
PYC="$WORK/__pycache__/gmpwork.cpython-312.pyc"

# Every GMP call site in .text, with the symbol it sits in.
objdump -d --no-show-raw-insn "$APYTHON" | $PYTHON -c '
import re, sys
cur = None
for line in sys.stdin:
    m = re.match(r"^0[0-9a-f]+ <([^>]+)>:", line)
    if m:
        cur = m.group(1); continue
    m = re.match(r"^\s+([0-9a-f]+):\s+call\s+[0-9a-f]+ <(__gmp[a-z_0-9]+)@plt>", line)
    if m and cur:
        print(m.group(1), m.group(2), cur)
' > "$WORK/sites.txt"

NSITES=$(wc -l < "$WORK/sites.txt")
if [ "$NSITES" -eq 0 ]; then
    echo "gmp alignment: SKIP (no GMP call sites found in $APYTHON)"
    exit 0
fi

{
    echo "set confirm off"
    echo "set pagination off"
    echo "set height 0"
    while read -r addr callee fn; do
        echo "break *0x$addr"
        echo "commands"
        echo "silent"
        printf 'printf "%%ld|%s|%s|0x%s\\n", ((long)$rsp) %% 16\n' "$callee" "$fn" "$addr"
        echo "continue"
        echo "end"
    done < "$WORK/sites.txt"
    echo "run"
} > "$WORK/probe.gdb"

# LD_BIND_NOW: lazy PLT resolution and a breakpoint on the call that triggers
# it do not mix -- the first resolution runs _dl_fixup with the breakpoint
# already planted, and gdb has been seen to lose the inferior there.
env -i LD_BIND_NOW=1 gdb -batch -x "$WORK/probe.gdb" --args "$APYTHON" "$PYC" \
    2>/dev/null | grep -E '^[0-9]+\|' > "$WORK/samples.txt" || true

TOTAL=$(wc -l < "$WORK/samples.txt")
if [ "$TOTAL" -eq 0 ]; then
    echo "gmp alignment: SKIP (the probe collected no samples)"
    exit 0
fi

BAD=$(awk -F'|' '$1 != 0' "$WORK/samples.txt" | wc -l)
if [ "$BAD" -eq 0 ]; then
    echo "gmp alignment: $TOTAL calls at $NSITES sites, all 16-byte aligned"
    exit 0
fi

echo "gmp alignment: $BAD of $TOTAL calls made with rsp misaligned"
awk -F'|' '$1 != 0 {print "    " $3 "  " $2 "  (" $4 ")"}' "$WORK/samples.txt" \
    | sort | uniq -c | sort -rn
echo "    the ABI wants rsp % 16 == 0 at a call.  Either the function's own"
echo "    push discipline is wrong, or it was ENTERED wrong and inherited it --"
echo "    run this under gdb with a backtrace at the site to tell which."
exit 1
