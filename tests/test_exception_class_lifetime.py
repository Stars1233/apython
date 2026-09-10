# An exception instance holds a reference to its class, like every other
# instance does.
#
# exc_new stamped ob_type without counting it, on the reasoning that exception
# types are immortal.  That is true of the sixty-nine builtin ones and false of
# every class a program writes: a user exception class whose last other
# reference has gone is kept alive by nothing, so the collector frees it out
# from under live instances and the next `type(e)` reads a freed class.  A
# class makes a cycle with its own MRO tuple, so it is the COLLECTOR that frees
# one -- which is why gc.collect() is what makes this appear.

import gc
import sys


def make(i):
    class E(Exception):
        pass
    E.__name__ = "E%d" % i
    return E("boom-%d" % i)


# Each class is now referenced only by the instance it built.
live = [make(i) for i in range(20)]
gc.collect()
gc.collect()
for e in live:
    print(type(e).__name__, e.args, isinstance(e, Exception))

# Still raisable, catchable and re-raisable afterwards.
try:
    raise live[0]
except Exception as x:
    print("re-raised", type(x).__name__, x.args, x is live[0])

try:
    raise live[1] from live[2]
except Exception as x:
    print("chained", type(x).__name__, type(x.__cause__).__name__)

# The count itself: an instance is worth exactly one reference to its class,
# and gives it back.
class Held(Exception):
    pass

gc.collect()
base = sys.getrefcount(Held)
inst = Held("x")
print("alive delta", sys.getrefcount(Held) - base)
del inst
gc.collect()
print("released delta", sys.getrefcount(Held) - base)

# A group is an exception and answers the same way.
class HeldEG(ExceptionGroup):
    pass

gc.collect()
base = sys.getrefcount(HeldEG)
g = HeldEG("m", [ValueError(1)])
print("group alive delta", sys.getrefcount(HeldEG) - base)
del g
gc.collect()
print("group released delta", sys.getrefcount(HeldEG) - base)

# And a group whose class survives only through a live instance.
def make_group(i):
    class G(ExceptionGroup):
        pass
    G.__name__ = "G%d" % i
    return G("g", [ValueError(i)])

groups = [make_group(i) for i in range(10)]
gc.collect()
for g in groups:
    print(type(g).__name__, g.message, [repr(x) for x in g.exceptions])

del live, groups
gc.collect()
print("done")
