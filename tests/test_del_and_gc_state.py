# A __del__ that raises used to leave the exception pending.  Nothing was
# going to receive it -- the object is being freed -- so it sat in
# current_exception until the next raise silently discarded it, or, if the
# dealloc came from the unwinder dropping the value stack, until a handler
# received the wrong exception object.
#
# The same longjmp out of a collection left gc_collecting latched at 1, after
# which the reentrancy guard made the collector a permanent no-op and cyclic
# garbage accumulated silently for the rest of the run.
#
# The reports themselves cannot be diffed: the default one names the object,
# and naming an object means printing its address.  sys.unraisablehook is the
# way round that -- and it is the way a program is meant to collect these --
# so the hook below counts them and the count is what is compared.  It used to
# be a recorded transcript, which stopped working the moment the reports
# started carrying an address.

import sys

ignored = []


def collect_unraisable(unraisable):
    ignored.append((unraisable.exc_type.__name__,
                    str(unraisable.exc_value),
                    unraisable.exc_traceback is not None,
                    type(unraisable.object).__name__))


sys.unraisablehook = collect_unraisable


class Boom:
    def __del__(self):
        raise ValueError("delfail")


for _ in range(5):
    b = Boom()
    b = None

assert True, "reaching here at all is the point"

# The next real exception must be the one the handler sees
try:
    raise KeyError("real")
except KeyError as e:
    assert type(e).__name__ == "KeyError", type(e).__name__
    assert e.args[0] == "real" or str(e) in ("real", "'real'"), e.args


# A raising __del__ inside a collection cycle must not stop the collector
class Node:
    def __init__(self, name):
        self.name = name
        self.peer = None

    def __del__(self):
        raise RuntimeError("node del")


for _ in range(20):
    a = Node("a")
    c = Node("c")
    a.peer = c
    c.peer = a
    del a, c

pass

# Allocation still works afterwards, which it would not if the collector had
# latched off and memory were exhausted
big = [list(range(100)) for _ in range(200)]
assert len(big) == 200 and len(big[0]) == 100


# A non-raising __del__ still runs, and exactly once
log = []


class Quiet:
    def __del__(self):
        log.append("gone")


q = Quiet()
q = None
assert log == ["gone"], log

# Every ignored exception reached the hook, with its type, its message, a
# traceback and the object it came out of.  The collection is explicit so the
# count does not depend on when a threshold happens to fire: five Booms, and
# forty Nodes in twenty cycles.
import gc

gc.collect()
print(len(ignored))
print(sorted(set(ignored)))

print("PASS: del and gc state")
