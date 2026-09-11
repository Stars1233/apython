# A finalizer that RAISES must not reopen the door a finalizer that merely
# collects had closed.
#
# tests/test_gc_reentrant_collect.py pins the rule: gc.collect() from inside a
# __del__ answers 0 and starts nothing, because two collections cannot overlap
# -- gc_collect_gen keeps its young, unreachable and finalize sentinels in its
# OWN stack frame, so a nested pass rebuilds the generation lists around a
# second frame and leaves the outer pass walking links into a frame that has
# moved on.  gc_track and gc_collect_gen both enforce it by testing
# gc_collecting.
#
# eval_exception_unwind cleared that flag unconditionally (a blanket reset,
# added so a raise that escapes the collector entirely could not leave it set
# for ever and make the collector a permanent no-op).  But a __del__ that
# raises and is merely reported unwinds only as far as its own frame and
# returns to the collector, which is still running -- with the guard now
# switched off behind it.  The very next gc.collect(), or simply enough
# allocation to cross the gen0 threshold, then started the nested pass the
# guard exists to refuse.
#
# It surfaced as a segfault in gc_list_remove under gc_visit_reachable, with
# the node's gc_next holding a pointer whose low half had been overwritten --
# pages away from anything the program did wrong.
import gc

order = []


class Junk:
    """Cyclic garbage with no finalizer: only the collector frees it."""

    def __init__(self):
        self.self_ref = self


class RaisesThenCollects:
    """The shape that reopened the door: raise first, then re-enter."""

    def __init__(self):
        self.self_ref = self

    def __del__(self):
        # The raise is the point.  It is caught here, so nothing escapes the
        # collector -- but the unwind ran, and the unwind is what cleared the
        # flag that protects the lists the outer collection is holding.
        try:
            {}["missing"]
        except KeyError:
            pass
        # Make garbage first, so "collected nothing" and "refused to start"
        # are distinguishable: a pass that really ran would find these.
        for _ in range(20):
            Junk()
        order.append(("nested collect returned", gc.collect()))


for _ in range(20):
    Junk()
RaisesThenCollects()

# Everything the nested pass would corrupt is downstream of this line.
gc.collect()
print("outer collection completed")
print("nested collect refused:", order)

# The generations must still be walkable and self-consistent.
print("get_objects() works:", len(gc.get_objects()) > 0)
print("second collection completes:", gc.collect() >= 0)
print("third collection completes:", gc.collect() >= 0)

# And new cyclic garbage must still be reachable by the collector: if the
# generations were rebuilt around a dead frame, it is not.
seen = []


class Counted:
    def __init__(self):
        self.self_ref = self

    def __del__(self):
        seen.append(1)


for _ in range(30):
    Counted()
gc.collect()
print("new cyclic garbage still collected:", len(seen) == 30)

keep = [Junk() for _ in range(4)]
gc.collect()
print("live objects survive:", all(o.self_ref is o for o in keep))
