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
c = gc.get_count()
print(type(c).__name__, len(c), all(isinstance(x, int) for x in c))
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

# The COUNT is not compared: CPython counts both instances of a two-object
# cycle and this collector counts one, because clearing the first drops the
# second by refcount before the sweep reaches it.  That something was
# collected is the property that matters.
gc.collect()
make_cycle()
print(gc.collect() > 0)

def make_self_cycle():
    a = []
    a.append(a)

gc.collect()
make_self_cycle()
print(gc.collect() > 0)

def make_dict_cycle():
    d = {}
    d["self"] = d

gc.collect()
make_dict_cycle()
print(gc.collect() > 0)

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

print("=== disabled means the thresholds do not fire ===")
gc.disable()
make_cycle()
gc.enable()
print(gc.collect() > 0)
print("done")
