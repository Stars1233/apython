# A frame object is reachable from its own f_locals: `f = sys._getframe()`
# binds it to a fast local, and frameobj_detach copies the fast locals into
# f_locals when the frame is recycled -- so the object refers to itself and
# its refcount never reaches zero.  frame_object_type has had tp_traverse and
# tp_clear all along, but nothing tracked an instance, so the collector never
# saw one: every such frame, its locals dict, and every local in it leaked.
#
# Nothing about that is visible from Python except by counting: the objects
# are simply still alive after gc.collect().

import gc
import sys


class Tracked:
    live = 0

    def __init__(self):
        Tracked.live += 1

    def __del__(self):
        Tracked.live -= 1


def leaked(fn, n=100):
    gc.collect()
    before = Tracked.live
    for _ in range(n):
        fn(Tracked())
    gc.collect()
    return Tracked.live - before


def discarded(x):
    sys._getframe()
    return 1


def bound_to_a_local(x):
    f = sys._getframe()          # the object lands in its own f_locals
    return 1


def read_back(x):
    f = sys._getframe()
    return len(f.f_locals)


def walked_outward(x):
    f = sys._getframe()
    return f.f_back is not None


def returned_from_the_frame(x):
    f = sys._getframe()
    return f


def kept_in_a_list(x, out):
    f = sys._getframe()
    out.append(f)
    return 1


def two_deep(x):
    def inner():
        return sys._getframe().f_back

    g = inner()
    return g is not None


def a_traceback_instead(x):
    """The other way to get a frame object, which never leaked: a traceback
    builds its own snapshot."""
    try:
        raise ValueError("x")
    except ValueError:
        tb = sys.exc_info()[2]
        return tb is not None


print("discarded          ", leaked(discarded))
print("bound to a local   ", leaked(bound_to_a_local))
print("read back          ", leaked(read_back))
print("walked outward     ", leaked(walked_outward))
print("returned           ", leaked(returned_from_the_frame))
print("two deep           ", leaked(two_deep))
print("a traceback instead", leaked(a_traceback_instead))


def kept_then_dropped():
    gc.collect()
    before = Tracked.live
    out = []
    for _ in range(100):
        kept_in_a_list(Tracked(), out)
    held = Tracked.live - before      # every frame is still referenced
    del out
    gc.collect()
    return held, Tracked.live - before


print("kept then dropped  ", kept_then_dropped())


def the_snapshot_still_reads():
    """Collectable is not the same as broken: everything a detached frame
    object answers has to keep answering."""
    def inner(a, b):
        return sys._getframe()

    f = inner(1, 2)
    return (sorted(f.f_locals), f.f_code.co_name, f.f_lineno > 0,
            f.f_back is not None)


print("snapshot           ", the_snapshot_still_reads())
