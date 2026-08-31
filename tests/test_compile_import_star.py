# `from m import *` leaves nothing on the stack: INTRINSIC_IMPORT_STAR is net
# zero, so the module it was handed has to be popped.  Without the POP_TOP the
# statement grew the stack by one each time it ran -- and inside a loop the
# assembler's depth worklist grew without bound and walked off its allocation,
# so the *compiler* segfaulted on valid Python.
import sys
sys.path.insert(0, "tests")

for i in range(3):
    from starmod import *

print(A, B)
print(i, "_hidden" in globals())


def f():
    total = 0
    for j in range(4):
        total += j
    return total


print(f())

# Repeated at module level, outside a loop.
from starmod import *
from starmod import *

print(A, B)


# And through our own compiler at run time, in a loop again.
ns = {"__builtins__": __builtins__}
exec("for k in range(2):\n    from starmod import *\nout = (A, B, k)\n", ns)
print(ns["out"])
