# A __del__ that runs while an exception is unwinding must not destroy it.
#
# instance_dealloc snapshots current_exception before calling __del__ and then
# asks whether it changed.  A bare inequality is the wrong question: the
# global also holds the exception being *handled*, and a __del__ that raises
# and catches internally leaves it at 0 -- a change, but not a raise.  The
# "it raised" arm then cleared the global, throwing away the exception the
# interpreter was carrying, and the enclosing except never ran.
#
# Only __del__s that catch what they raise are here.  One that lets an
# exception escape prints a traceback with an object address in it, which
# cannot be diffed against CPython's.

print("=== __del__ that catches internally, during an unwind ===")

class Quiet:
    def __del__(self):
        try:
            raise ValueError("swallowed")
        except ValueError:
            pass

def raises():
    q = Quiet()
    raise KeyError("outer")

try:
    raises()
except KeyError as e:
    print("caught", type(e).__name__, e.args)
else:
    print("LOST THE EXCEPTION")

print("=== the same, two frames down ===")

def outer():
    inner()

def inner():
    q = Quiet()
    raise IndexError("deep")

try:
    outer()
except IndexError as e:
    print("caught", type(e).__name__, e.args)
else:
    print("LOST THE EXCEPTION")

print("=== a quiet __del__ during an unwind ===")

class Plain:
    def __del__(self):
        self.x = 1

def raises3():
    p = Plain()
    raise AttributeError("outer3")

try:
    raises3()
except AttributeError as e:
    print("caught", type(e).__name__, e.args)
else:
    print("LOST THE EXCEPTION")

print("=== a __del__ inside an except block, which is also a live exception ===")
try:
    raise KeyError("first")
except KeyError as e:
    q = Quiet()
    del q
    print("still bound:", type(e).__name__, e.args)

print("=== and the exception still propagates from the handler ===")
try:
    try:
        raise KeyError("first")
    except KeyError:
        q = Quiet()
        del q
        raise
except KeyError as e:
    print("re-raised", type(e).__name__, e.args)

print("=== the ordinary case: no exception in flight ===")
q = Quiet()
del q
print("done")

print("=== a chain of them, freed by one collection ===")
class Chain:
    def __init__(self, n):
        self.n = n
    def __del__(self):
        try:
            raise ValueError(self.n)
        except ValueError:
            pass

def many():
    keep = [Chain(i) for i in range(20)]
    raise RuntimeError("with twenty pending")

try:
    many()
except RuntimeError as e:
    print("caught", type(e).__name__, e.args)
else:
    print("LOST THE EXCEPTION")
