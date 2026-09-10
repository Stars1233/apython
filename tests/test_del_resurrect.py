# A __del__ that stores `self` somewhere brings the object back, and nothing
# may free it afterwards.
#
# The finalizer was called with the refcount temporarily at 1 and then simply
# decremented again, and instance_dealloc went on to release the fields and
# free the object whatever the finalizer had done.  A resurrected object was
# therefore freed with a live reference to it still in the list its own
# __del__ had put it in -- which the collector then walked.  CPython's
# PyObject_CallFinalizerFromDealloc answers "resurrected" and its caller
# returns without freeing anything.
#
# The weak reference has to survive too: the finalizer runs BEFORE the
# weakrefs are cleared, so an object that comes back still has the ones that
# pointed at it.

import gc
import _weakref

survivors = []
del_calls = []


class R:
    def __init__(self, n):
        self.n = n
        self.tag = "intact-%d" % n

    def __del__(self):
        del_calls.append(self.n)
        survivors.append(self)


gc.collect()

# --- resurrection out of an ordinary refcount dealloc ----------------------
r = R(1)
wr = _weakref.ref(r)
del r
print("del ran:", del_calls)
print("survived:", len(survivors))
print("intact:", survivors[0].tag, survivors[0].n)
print("weakref alive:", wr() is not None, wr() is survivors[0])

# Dropping it again finalizes nothing a second time, and now it really goes.
survivors.clear()
gc.collect()
print("after drop, del ran:", del_calls)
print("weakref now:", wr())

# --- the same object resurrected repeatedly --------------------------------
del_calls.clear()
survivors.clear()


class Sticky:
    count = 0

    def __del__(self):
        Sticky.count += 1
        survivors.append(self)


s = Sticky()
del s
print("sticky first:", Sticky.count, len(survivors))
survivors.clear()
gc.collect()
print("sticky after drop:", Sticky.count, len(survivors))

# --- an ordinary non-resurrecting __del__ still frees ----------------------
freed = []


class Plain:
    def __del__(self):
        freed.append(1)


for i in range(20):
    p = Plain()
    del p
gc.collect()
print("plain freed:", len(freed))

# --- a resurrected object is still usable and can be collected later -------
survivors.clear()
del_calls.clear()
holder = []


class Cycle:
    def __init__(self):
        self.me = self
        self.data = [1, 2, 3]

    def __del__(self):
        holder.append(self)


c = Cycle()
del c
gc.collect()
print("cycle resurrected:", len(holder), holder[0].data if holder else None)
holder.clear()
gc.collect()
gc.collect()
print("done")
