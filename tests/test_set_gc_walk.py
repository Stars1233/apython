# The collector's three walks over a set -- traverse, clear and dealloc --
# stop when the live count is exhausted rather than at the end of the table.
# A set is a quarter full at most, so most of the array is empty; but the
# count and the occupied slots agree only if every tombstone, every resize
# and every mutation has kept them in step, and if they ever disagree the
# symptom is a reference dropped or double-dropped by the COLLECTOR, which
# is a crash somewhere else entirely.
#
# So: cycles through sets, sets punched full of holes, sets that shrank, and
# a __del__ that touches the set being torn down.
import gc


class Node:
    def __init__(self, tag):
        self.tag = tag
        self.peers = set()


def cycle(n, holes):
    nodes = [Node(i) for i in range(n)]
    for a in nodes:
        for b in nodes:
            a.peers.add(b)
        if holes:
            for b in nodes[::2]:
                a.peers.discard(b)
    return nodes


for n in (1, 2, 5, 17, 40):
    for holes in (False, True):
        ns = cycle(n, holes)
        print(n, holes, len(ns[0].peers), gc.is_tracked(ns[0].peers))
        del ns
        print(gc.collect() >= 0)

# a set that grew, drained and regrew, then died in a cycle
class Holder:
    pass


h = Holder()
h.s = set()
for i in range(500):
    h.s.add(Node(i))
for x in list(h.s)[:490]:
    h.s.discard(x)
h.s.add(h)
print(len(h.s), gc.is_tracked(h.s))
del h, x
print(gc.collect() >= 0)

# frozensets in a cycle, which cannot be cleared in place
class FHolder:
    pass


f = FHolder()
f.f = frozenset([Node(i) for i in range(50)] + [f])
print(len(f.f))
del f
print(gc.collect() >= 0)

# a __del__ that reads the set losing its last reference
seen = []


class Loud:
    def __init__(self, tag, back):
        self.tag = tag
        self.back = back

    def __del__(self):
        seen.append(len(self.back) if self.back is not None else -1)


def teardown():
    s = set()
    for i in range(30):
        s.add(Loud(i, None))
    for x in list(s)[:20]:
        s.discard(x)
    del s


teardown()
print(len(seen))
gc.collect()
print(len(seen))

# clear() on a set full of holes, then reuse
c = set(range(300))
for i in range(300):
    if i % 7:
        c.discard(i)
print(len(c))
c.clear()
print(len(c), 0 in c, sorted(c))
for i in range(10):
    c.add(i)
print(sorted(c))
print(gc.collect() >= 0)
