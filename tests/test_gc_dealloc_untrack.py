# A dying object leaves the collector's lists before it is taken apart.
#
# The untrack happened last, inside gc_dealloc, so for the whole of a
# container's teardown -- every field released, every __del__ those releases
# fire -- the object was still in a generation list with a refcount of zero.
# Anything that walked the generations from inside one of those finalizers
# found it: gc.get_objects() took a reference to a dead object and freed it a
# second time when the list went, and gc.collect() computed gc_refs = 0 for it,
# cleared it and left the freed block in its own young list.  CPython untracks
# at the top of every tp_dealloc for exactly this reason.
#
# The window that matters is the one AFTER the finalizer.  During __del__ the
# object is tracked and held at a refcount of one, as CPython's subtype_dealloc
# arranges, so a finalizer can still see itself.

import gc

gc.disable()


def deep_chain(cls, n):
    t = []
    for i in range(n):
        t = [t, cls()]
    return t


# --- gc.collect() from inside a finalizer, deep enough for the trashcan ----
class Collector:
    n = 0

    def __del__(self):
        Collector.n += 1
        gc.collect()


t = deep_chain(Collector, 60)
del t
print("collect in __del__:", Collector.n)

# --- gc.get_objects() from inside a finalizer ------------------------------
class Walker:
    n = 0

    def __del__(self):
        Walker.n += 1
        gc.get_objects()


t = deep_chain(Walker, 60)
del t
print("get_objects in __del__:", Walker.n)

# --- both, over dicts and tuples as well as lists --------------------------
class Both:
    n = 0

    def __del__(self):
        Both.n += 1
        if Both.n % 7 == 0:
            gc.collect()
        else:
            gc.get_objects()


v = {}
for i in range(60):
    v = {1: v, 2: Both()}
del v
u = ()
for i in range(60):
    u = (u, Both())
del u
print("mixed:", Both.n)

# --- CPython's own test_gc test_trashcan, which is where this came from ----
class Ouch:
    n = 0

    def __del__(self):
        Ouch.n += 1
        if Ouch.n % 17 == 0:
            gc.collect()


gc.enable()
N = 150
for count in range(2):
    t = []
    for i in range(N):
        t = [t, Ouch()]
    u = []
    for i in range(N):
        u = [u, Ouch()]
    v = {}
    for i in range(N):
        v = {1: v, 2: Ouch()}
gc.disable()
print("trashcan:", Ouch.n)

# --- a finalizer can still find ITSELF, which is what the tracked window is -
found = []


class SeesItself:
    def __del__(self):
        found.append(any(o is self for o in gc.get_objects()))


s = SeesItself()
s.me = s          # a cycle, so only the collector frees it
del s
gc.collect()
print("finalizer sees itself:", found)

gc.collect()
print("done")
