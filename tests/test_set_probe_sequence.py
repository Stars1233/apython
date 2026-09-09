# A key is findable only if the walk that PLACED it is the walk that goes
# looking.  Three pieces of code walk a set's table: set_find_slot (which
# answers lookups and hands insert its slot), set_remove, and the rehash
# inside set_resize_to.  They have to agree exactly, and nothing about a
# disagreement shows until keys collide -- with room to spare every key sits
# in its own slot and any sequence finds it on the first try.
#
# So every case here is built out of keys that COLLIDE, and each one crosses
# at least one resize, at least one tombstone, or both.

MASK_BITS = 4096            # far above any capacity these sets reach


def collide(n, stride=MASK_BITS):
    # i * 4096 shares its low bits with every other, so a table masked to any
    # power of two up to 4096 sends all of them to the same starting slot
    return [i * stride for i in range(n)]


class Z:
    # the same thing for objects, where the hash is not the identity
    def __init__(self, v):
        self.v = v

    def __hash__(self):
        return 0

    def __eq__(self, o):
        return isinstance(o, Z) and self.v == o.v

    def __lt__(self, o):
        return self.v < o.v

    def __repr__(self):
        return "Z(%d)" % self.v


# --- every key placed is every key found, across every resize on the way ---
for n in (1, 2, 8, 9, 10, 11, 20, 50, 200, 500):
    keys = collide(n)
    s = set()
    for k in keys:
        s.add(k)
    print(n, len(s), all(k in s for k in keys),
          any(k + 1 in s for k in keys))

# --- and built in one step, so the constructor's path is exercised too ----
for n in (10, 11, 200):
    keys = collide(n)
    s = set(keys)
    f = frozenset(keys)
    print(n, len(s), len(f), all(k in s for k in keys),
          all(k in f for k in keys), s == f)

# --- remove walks its own probe: what it tombstones must be what lookup
#     finds, and the keys BEYOND it must stay reachable -----------------
for n in (11, 40, 300):
    keys = collide(n)
    s = set(keys)
    for k in keys[::3]:
        s.remove(k)
    gone = keys[::3]
    left = [k for k in keys if k not in gone]
    print(n, len(s), all(k in s for k in left), any(k in s for k in gone))
    for k in gone:
        s.add(k)
    print(len(s), all(k in s for k in keys))

# --- and discard, which is the same probe with a different answer ---------
s = set(collide(100))
for k in collide(100):
    s.discard(k)
    s.discard(k)
print(len(s), sorted(s))

# --- a tombstone-heavy table crossing a resize ---------------------------
# The rehash drops the tombstones and re-places every live key, so if its
# walk differed from the lookup's this is where it would show.
s = set()
for round_ in range(6):
    for k in collide(60):
        s.add(k + round_)
    for k in collide(60):
        s.discard(k + round_)
    s.add(-round_)
print(len(s), sorted(s), all(-r in s for r in range(6)))

# --- objects whose __hash__ is a constant, which is the worst case -------
for n in (1, 9, 10, 60, 200):
    z = set()
    for i in range(n):
        z.add(Z(i))
    print(n, len(z), all(Z(i) in z for i in range(n)), Z(n) in z)
    for i in range(0, n, 2):
        z.discard(Z(i))
    print(len(z), all(Z(i) in z for i in range(1, n, 2)),
          any(Z(i) in z for i in range(0, n, 2)))
    for i in range(n, n + 40):
        z.add(Z(i))
    print(len(z), all(Z(i) in z for i in range(n, n + 40)))

# --- pop drains a colliding table -----------------------------------------
p = set(collide(150))
got = []
while p:
    got.append(p.pop())
print(len(got), sorted(got) == sorted(collide(150)), len(p))

# --- and the operators over colliding operands ---------------------------
a = set(collide(80))
b = set(collide(120))
print(len(a | b), len(a & b), len(a - b), len(b - a), len(a ^ b),
      a <= b, b <= a, a.isdisjoint(b), a == b)
c = {Z(i) for i in range(30)}
d = {Z(i) for i in range(15, 45)}
print(len(c | d), len(c & d), len(c - d), len(c ^ d), c <= d, c.isdisjoint(d))
print(sorted(c & d) == [Z(i) for i in range(15, 30)])
