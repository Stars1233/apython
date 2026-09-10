# A set subclass whose table is the shared empty one must not free it.
#
# set.clear() releases the entries array and installs set_empty_entries, a
# static table in .rodata that every empty set points at; set_dealloc checks
# for it before freeing.  instance_dealloc -- which is what runs for a
# SUBCLASS of set -- did not, so an instance that had been cleared handed
# free() a static address: "free(): invalid pointer", and the process aborted.
#
# CPython's test_set reaches it through bpo-46615's TestBinaryOpsMutating,
# whose __eq__ clears both operands at random while a set operation walks
# them.

import gc


class S(set):
    pass


# The plain shape: clear it, then let it go.
for i in range(200):
    s = S(range(i % 30))
    s.clear()
    del s
gc.collect()
print("cleared and released")

# Cleared, then used again, then released.
for i in range(50):
    s = S(range(20))
    s.clear()
    s.add(1)
    s.clear()
    s |= {2, 3}
    s.clear()
    del s
gc.collect()
print("reused")

# Never populated at all: the table is the shared one from birth.
for i in range(200):
    del_me = S()
    del del_me
gc.collect()
print("born empty")

# Freed by the COLLECTOR rather than by a refcount, through a cycle.
for i in range(100):
    s = S(range(10))
    s.clear()
    holder = []
    holder.append(holder)
    holder.append(s)
    del s, holder
gc.collect()
print("through a cycle")

# frozenset subclass, and one with __slots__, take the same route out.
class F(frozenset):
    pass


class WithSlots(set):
    __slots__ = ("x",)


for i in range(100):
    f = F(range(5))
    w = WithSlots(range(5))
    w.x = i
    w.clear()
    del f, w
gc.collect()
print("frozenset and slots")

# And an __eq__ that empties the operands while a set operation walks them,
# which is what CPython's own test does.
_state = [12345]


def rnd(n):
    _state[0] = (_state[0] * 1103515245 + 12345) & 0x7FFFFFFF
    return (_state[0] >> 7) % n


def build(c1, c2):
    class Bad:
        def __eq__(self, other):
            if not enabled:
                return False
            if rnd(20) == 0:
                a.clear()
            if rnd(20) == 0:
                b.clear()
            return bool(rnd(2))

        def __hash__(self):
            return rnd(2)

    enabled = False
    a = c1(Bad() for _ in range(rnd(50)))
    b = c2(Bad() for _ in range(rnd(50)))
    enabled = True
    return a, b


for c1, c2 in ((set, S), (S, set), (S, S), (set, set)):
    for op in (lambda x, y: x & y,
               lambda x, y: x | y,
               lambda x, y: x - y,
               lambda x, y: x ^ y,
               lambda x, y: x == y,
               lambda x, y: x <= y):
        for _ in range(40):
            a, b = build(c1, c2)
            try:
                op(a, b)
            except RuntimeError:
                pass
    gc.collect()

print("mutating operands survived")
print("done")
