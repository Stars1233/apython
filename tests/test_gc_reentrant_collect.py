# A __del__ that calls gc.collect() must not re-enter the collector.
#
# gc_collect_gen SET gc_collecting on entry and never tested it, and
# gc.collect() called it unconditionally.  So a finalizer run from the
# collector's own finalize phase could start a second collection over the same
# generation lists -- whose sentinels are local to the OUTER gc_collect_gen
# frame.  The inner pass rebuilt those lists around stack addresses belonging
# to a frame that was still live; when the outer pass resumed, its links
# pointed into dead stack, and the next gc_list_remove wrote through them.
#
# It surfaced as a segfault in gc_list_remove under gc_visit_reachable, pages
# away from anything the program had done wrong, and it is what stood between
# this tree and CPython's test_contextlib_async and test_asyncgen -- the
# asyncio test suite calls support.gc_collect() from tearDown constantly, and
# an event loop's __del__ is exactly the finalizer that runs underneath one.
#
# CPython returns 0 from gc.collect() when a collection is already running
# (Modules/gcmodule.c: "if (gcstate->collecting) return 0"), which is what
# this now does.
import gc

order = []


class Junk:
    """Cyclic garbage with no finalizer: only the collector frees it."""

    def __init__(self):
        self.self_ref = self


class Nested:
    """A cycle whose finalizer collects."""

    def __init__(self, tag):
        self.tag = tag
        self.self_ref = self

    def __del__(self):
        order.append(self.tag)
        # Make some cyclic garbage FIRST, so that a collector which really
        # re-entered would have something to report.  CPython refuses and
        # answers 0; a collector that runs would answer at least 1, and would
        # be rebuilding the generation lists the outer pass is walking.
        junk = Junk()
        junk = None
        n = gc.collect()
        order.append(("reentrant returned", n))


gc.collect()
for i in range(5):
    Nested(i)
gc.collect()
nested = [x for x in order if isinstance(x, tuple)]
print("finalizers ran:", sorted(x for x in order if isinstance(x, int)))
print("re-entrant calls:", len(nested))
print("every re-entrant collect answered 0:", all(n == 0 for _, n in nested))

# The collector still works afterwards: the lists were not left corrupt.
class Plain:
    def __init__(self):
        self.self_ref = self

for i in range(200):
    Plain()
print("collected after:", gc.collect() > 0)
print("still collecting nothing:", gc.collect())

# And a finalizer that collects while ANOTHER finalizer is pending -- two
# levels of the same shape, which is what an event loop teardown looks like.
deep = []


class Outer:
    def __init__(self):
        self.self_ref = self

    def __del__(self):
        deep.append("outer")
        gc.collect()


class Inner:
    def __init__(self):
        self.self_ref = self

    def __del__(self):
        deep.append("inner")
        gc.collect()


Outer()
Inner()
gc.collect()
print("both ran:", sorted(deep))

# A large allocation run afterwards, which is what actually tripped the
# corrupt links: gc_track triggers a collection on the threshold.
acc = []
for i in range(120000):
    acc.append((i, str(i)))
print("survived allocation:", len(acc))
print("gc counts sane:", all(isinstance(c, int) for c in gc.get_count()))
