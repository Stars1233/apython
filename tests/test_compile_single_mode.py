# compile(src, name, "single") displays what its expressions evaluate to.
#
# "single" is the REPL's mode, and the difference from "exec" is one opcode: a
# bare expression at module level is followed by CALL_INTRINSIC_1
# INTRINSIC_PRINT before the POP_TOP, which hands the value to
# sys.displayhook.  That is what makes an interactive line echo -- and it is
# where doctest reads every "expected output" from, since a doctest example is
# compiled in single mode and its expected text compared against what the hook
# printed.
#
# This tree parsed "single" (par_single_check enforces its one-statement rule)
# and then compiled it exactly as "exec": the value was discarded and nothing
# was displayed, so every doctest in the standard library's own test suite
# reported "Got nothing".  INTRINSIC_PRINT was not implemented either.

import io
import sys


def run(src, mode="single"):
    """Compile and run, capturing whatever the display writes."""
    buf = io.StringIO()
    saved = sys.stdout
    sys.stdout = buf
    try:
        exec(compile(src, "<t>", mode), {})
    finally:
        sys.stdout = saved
    return buf.getvalue()


print(repr(run("2 + 2")), "an expression is displayed")
print(repr(run("'text'")), "a str is displayed as its repr")
print(repr(run("None")), "None displays as nothing at all")
print(repr(run("x = 5")), "an assignment displays nothing")
print(repr(run("2 + 2", "exec")), "exec mode still discards")

# Inside a function the statement is an ordinary discard, in single mode too:
# only a module-level expression echoes.  A def is one statement, so it is a
# legal single-mode compilation, and its body must not have picked the
# intrinsic up.
ns = {}
buf = io.StringIO()
saved = sys.stdout
sys.stdout = buf
try:
    exec(compile("def f():\n    2 + 2\n", "<t>", "single"), ns)
    ns["f"]()
finally:
    sys.stdout = saved
print(repr(buf.getvalue()), "a function body does not display")

# The bytecode difference itself.
single = compile("2+2", "<t>", "single")
plain = compile("2+2", "<t>", "exec")
print(single.co_code != plain.co_code, "single and exec differ in bytecode")

# The hook is looked up, not hardwired: replacing it takes the echo over.
seen = []
saved_hook = sys.displayhook
sys.displayhook = seen.append
try:
    exec(compile("7 * 6", "<t>", "single"), {})
finally:
    sys.displayhook = saved_hook
print(seen, "a replaced displayhook receives the value")

# An exception from the hook reaches the caller rather than being swallowed.
def angry(value):
    raise ValueError("no display")


saved_hook = sys.displayhook
sys.displayhook = angry
try:
    exec(compile("1", "<t>", "single"), {})
    print(False, "a raising displayhook must propagate")
except ValueError:
    print(True, "a raising displayhook propagates")
finally:
    sys.displayhook = saved_hook

# And the mode still refuses what it always refused.
try:
    compile("x = 1\ny = 2\n", "<t>", "single")
    print(False, "single mode takes only one statement")
except SyntaxError:
    print(True, "single mode still refuses two statements")
