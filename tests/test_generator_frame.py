# A suspended generator's frame owns everything on its VALUE STACK as well as
# in its locals -- the iterator a `for` is walking above all -- and both
# frame_free and gen_traverse walked the locals and stopped there.  So an
# abandoned generator released its locals and leaked the rest, and a cycle
# through one was invisible to the collector.
#
# What the probe below asks, and why it does not count.  `gc.collect() > 0`
# was the obvious question and it is not portable: the return value is a
# number of unreachable objects, and CPython builds disagree about it -- this
# file passed against 3.12.3 and failed against the 3.12 CI installs, on the
# three cases below, with apython answering the same thing in both places.
# CLAUDE.md already records that the collector's counts are not comparable.
#
# So each case takes a weak reference and asks two questions instead, which
# together say what "a cycle the collector can see" actually means:
#
#   alive after make() returned   -- refcounting alone did NOT free it, so
#                                    there is a cycle
#   dead after gc.collect()       -- and the collector could see it
#
# Both are facts about the object, not about the collector's bookkeeping, and
# an exhausted generator -- which forms no cycle at all, because its frame is
# already gone -- still answers False on the first, which is the distinction
# the count was being used for.

import gc
import _weakref


def cycle(make):
    gc.collect()
    ref = make()
    cyclic = ref() is not None
    gc.collect()
    return cyclic and ref() is None


print("=== a generator suspended inside a for, in a cycle ===")

class Marker:
    pass


def walking(seq):
    for x in seq:
        yield x
    yield "end"


def through_locals():
    m = Marker()
    g = walking([m])
    next(g)
    m.g = g
    return _weakref.ref(m)


print("locals", cycle(through_locals))


def through_the_stack():
    a = []
    def g():
        for x in a:
            yield x
        yield None
    it = g()
    next(it)
    a.append(it)
    return _weakref.ref(it)


print("stack", cycle(through_the_stack))


def nested_for():
    a = [[1, 2], [3]]
    def g():
        for row in a:
            for x in row:
                yield x
    it = g()
    next(it)
    a.append(it)
    return _weakref.ref(it)


print("nested", cycle(nested_for))

print("=== and one that was never started ===")

def never_started():
    a = []
    def g():
        for x in a:
            yield x
    it = g()
    a.append(it)
    return _weakref.ref(it)


print("unstarted", cycle(never_started))

print("=== one run to exhaustion is not walked twice ===")

def exhausted():
    a = [1, 2]
    def g():
        for x in a:
            yield x
    it = g()
    for _ in it:
        pass
    a.append(it)
    return _weakref.ref(it)


print("exhausted", cycle(exhausted))

print("=== generators still work ===")

def counter(n):
    total = 0
    for i in range(n):
        total += i
        yield total

print(list(counter(5)))
g = counter(3)
print(next(g), next(g), next(g))
try:
    next(g)
except StopIteration:
    print("StopIteration")

def with_send():
    got = []
    while True:
        v = yield len(got)
        if v is None:
            break
        got.append(v)
    return got

g = with_send()
print(next(g), g.send("a"), g.send("b"))

def delegating():
    yield from counter(3)
    yield "after"

print(list(delegating()))

print("=== the values on the stack are released ===")
freed = []

class Loud:
    def __init__(self, n):
        self.n = n
    def __del__(self):
        freed.append(self.n)

def drop_suspended():
    def g():
        for x in [Loud(1), Loud(2)]:
            yield x
    it = g()
    next(it)

drop_suspended()
gc.collect()
print(sorted(freed))
print("done")

# gen_traverse walks the frame's value stack, but stack_ptr is written by
# YIELD_VALUE and by nothing else -- in a RUNNING generator it records the
# depth of the previous suspension, so the walk visited slots already popped
# and released.  A running generator's stack needs no visiting: it holds
# owned references no tp_traverse accounts for, which is what makes the
# interpreter stack a root.
def collecting(*a):
    gc.collect()
    return sum(a)


def deep():
    x = [[1], (yield 1), [2], [3], [4]]
    del x
    gc.collect()
    yield collecting(10, 20, 30, 40)


d = deep()
print(next(d))
print(d.send([9, 9]))

# and a cycle held only by a SUSPENDED generator is still reclaimed
class Node:
    pass


def holds_cycle(box):
    a = Node()
    b = Node()
    a.b = b
    b.a = a
    box.append(_weakref.ref(a))
    yield 1
    yield a


box = []
it = holds_cycle(box)
next(it)
del it
gc.collect()
print("suspended cycle reclaimed:", box[0]() is None)
