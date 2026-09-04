# The gc module.
#
# There was none at all: gc_collect_gen had three callers and every one of
# them was inside gc.asm, so nothing could ask for a collection.  gc.collect()
# is what a test for a reference cycle is written around, which is why this
# had to come before the cycles.

import gc
import sys

print("=== it is a module, and a builtin one ===")
print(type(gc).__name__, "gc" in sys.builtin_module_names)

print("=== enable / disable / isenabled ===")
print(gc.isenabled())
gc.disable()
print(gc.isenabled())
gc.enable()
print(gc.isenabled())

print("=== the counts and the thresholds ===")
# get_count() is (allocations since the last gen0 pass, gen0 collections
# since the last gen1 pass, gen1 collections since the last gen2 pass).  The
# first depends on how much the interpreter itself has allocated, so only its
# type is compared; a collect() moves the second, which is compared.
c = gc.get_count()
print(type(c).__name__, len(c), all(isinstance(x, int) for x in c))
gc.collect(2)
before = gc.get_count()[1]
gc.collect(0)
print(gc.get_count()[1] - before)
t = gc.get_threshold()
print(type(t).__name__, len(t), t)
gc.set_threshold(123, 4, 5)
print(gc.get_threshold())
gc.set_threshold(50)
print(gc.get_threshold()[0])
gc.set_threshold(700, 10, 10)
print(gc.get_threshold())

print("=== the two lists ===")
print(gc.garbage, gc.callbacks)

print("=== collect() answers with a count ===")
n = gc.collect()
print(type(n).__name__, n >= 0)
for gen in (0, 1, 2):
    print(gen, gc.collect(gen) >= 0)

print("=== and it actually collects a cycle ===")
class Node:
    def __init__(self):
        self.other = None

def make_cycle():
    a = Node()
    b = Node()
    a.other = b
    b.other = a

# An instance cycle's count is not compared: CPython 3.12 keeps an
# instance's __dict__ in a managed slot that is not a tracked object of its
# own, so it counts two here where this collector counts four -- two
# instances and their two dicts.  See bugs.md.  Container cycles, below, are
# compared exactly.
gc.collect()
make_cycle()
print(gc.collect() > 0)

def make_self_cycle():
    a = []
    a.append(a)

gc.collect()
make_self_cycle()
print(gc.collect())

def make_dict_cycle():
    d = {}
    d["self"] = d

gc.collect()
make_dict_cycle()
print(gc.collect())

print("=== the count is the size of the unreachable set ===")
# Every object in the cycle, not just the one the sweep happened to reach
# first.  This used to answer 1 for all of them.
gc.collect()
a, b = [], []
a.append(b)
b.append(a)
del a, b
print(gc.collect())

gc.collect()
ring = [[] for _ in range(5)]
for i in range(5):
    ring[i].append(ring[(i + 1) % 5])
del ring
print(gc.collect())

gc.collect()
d, e = {}, {}
d["x"] = e
e["x"] = d
del d, e
print(gc.collect())

print("=== a chain of cycles ===")
gc.collect()
def make_many(n):
    for i in range(n):
        a = []
        b = [a]
        a.append(b)

make_many(50)
print(gc.collect() > 0)

print("=== nothing to collect answers zero ===")
gc.collect()
print(gc.collect())

print("=== the refusals ===")
for bad in (3, -1, 99):
    try:
        gc.collect(bad)
        print(bad, "NO ERROR")
    except ValueError:
        print(bad, "ValueError")
try:
    gc.collect(1, 2)
except TypeError:
    print("TypeError")
try:
    gc.get_count(1)
except TypeError:
    print("TypeError")

print("=== is_tracked ===")
# An immediate -- an int, a float -- is not an object at all here, so it is
# never tracked.  An empty dict is not compared: CPython untracks one whose
# contents are all untrackable, which is an optimization this collector does
# not have.
print(gc.is_tracked([]), gc.is_tracked({"a": []}), gc.is_tracked([1, 2]))
print(gc.is_tracked(1), gc.is_tracked(1.5), gc.is_tracked("abc"), gc.is_tracked(None))


class Tracked:
    pass


print(gc.is_tracked(Tracked()), gc.is_tracked(Tracked))
try:
    gc.is_tracked()
except TypeError:
    print("TypeError")

print("=== get_referents ===")
# The dict case visits keys as well as values, which CPython's split-key
# dicts do not, so only a list and a tuple are compared exactly.
print(sorted(gc.get_referents(["a", "b"])))
print(sorted(map(repr, gc.get_referents(("a", ("b",))))))
inner = ["deep"]
outer = [inner]
print(gc.get_referents(outer)[0] is inner)
# Nothing to traverse, and no arguments at all, both answer an empty list.
print(gc.get_referents(), gc.get_referents(1, "s", 2.5))

print("=== get_objects ===")
mine = ["a marker this test can find"]
objs = gc.get_objects()
print(type(objs).__name__, len(objs) > 0)
print(any(o is mine for o in objs))
# The result list is tracked itself, and must not contain itself.
print(objs in objs)
print(type(gc.get_objects(0)).__name__, type(gc.get_objects(None)).__name__)
try:
    gc.get_objects(3)
except ValueError as e:
    print("ValueError", e)
try:
    gc.get_objects(0, 1)
except TypeError:
    print("TypeError")

print("=== the debug flags ===")
print(gc.DEBUG_STATS, gc.DEBUG_COLLECTABLE, gc.DEBUG_UNCOLLECTABLE)
print(gc.DEBUG_SAVEALL, gc.DEBUG_LEAK)
print(gc.get_debug())
gc.set_debug(gc.DEBUG_SAVEALL)
print(gc.get_debug() == gc.DEBUG_SAVEALL)

# SAVEALL keeps the unreachable set instead of clearing it: it goes into
# gc.garbage, still intact.  DEBUG_STATS and DEBUG_COLLECTABLE are not
# exercised here because they write to stderr, which this harness merges into
# the output it compares -- and one of the two prints addresses.
gc.collect()
del gc.garbage[:]
saved_a, saved_b = [], []
saved_a.append(saved_b)
saved_b.append(saved_a)
del saved_a, saved_b
print(gc.collect())
print(len(gc.garbage), [len(x) for x in gc.garbage])
del gc.garbage[:]
gc.set_debug(0)
print(gc.get_debug())
print(gc.collect())

print("=== disabled means the thresholds do not fire ===")
gc.disable()
make_cycle()
gc.enable()
print(gc.collect() > 0)
print("done")
