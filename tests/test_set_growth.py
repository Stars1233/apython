# The table a set rebuilds into is sized from the LIVE count, not from the
# capacity it had: a set full of tombstones SHRINKS, and the rehash that
# does it drops every tombstone on the way.  So a resize is the one moment
# a set can lose a key without any wrong answer being printed first -- the
# key is dropped, the length is right, and only a later lookup notices.
#
# Everything below asks the table for a key that had to survive a rebuild:
# after growth, after a shrink, and with keys whose hashes collide so that
# the rebuild has to reconstruct a probe run rather than land each key in
# its own slot.


class Z:
    # every instance hashes to the same slot, so the whole set is one run
    def __init__(self, v):
        self.v = v

    def __hash__(self):
        return 0

    def __eq__(self, o):
        return isinstance(o, Z) and self.v == o.v

    def __repr__(self):
        return "Z(%d)" % self.v


# --- growth: every key put in is still there -------------------------------
s = set()
for i in range(500):
    s.add(i)
print(len(s), all(i in s for i in range(500)), any(i in s for i in (-1, 500)))

# --- and with a hash that gives no help ------------------------------------
z = set()
for i in range(200):
    z.add(Z(i))
print(len(z), all(Z(i) in z for i in range(200)), Z(200) in z)

# --- shrink: fill, empty, and the survivors are still findable -------------
# The discards leave the table dense with tombstones.  The next add trips
# the load factor, and the rebuild is the one that shrinks.
t = set(range(1000))
for i in range(1000):
    if i % 4:
        t.discard(i)
print(len(t), sorted(t)[:5], sorted(t)[-3:])
for i in range(1000, 1200):
    t.add(i)
print(len(t), all(i in t for i in range(0, 1000, 4)),
      all(i in t for i in range(1000, 1200)),
      any(i in t for i in range(1, 1000, 4)))

# --- the same drain, then refill from scratch ------------------------------
d = set(range(300))
for i in range(300):
    d.remove(i)
print(len(d), 0 in d, sorted(d))
for i in range(300):
    d.add(i)
print(len(d), all(i in d for i in range(300)))

# --- churn at a fixed size, which is what used to double forever -----------
# ob_size stays at 64 while thousands of tombstones pass through, so a set
# sized from its capacity grows without bound and one sized from its live
# count does not.  What is checked here is only that the answers stay right.
c = set(range(64))
for i in range(64, 20000):
    c.add(i)
    c.discard(i - 64)
print(len(c), sorted(c)[0], sorted(c)[-1], 19936 in c, 19935 in c)

# --- the collision run, drained and refilled ------------------------------
zz = set(Z(i) for i in range(100))
for i in range(0, 100, 2):
    zz.discard(Z(i))
print(len(zz), all(Z(i) in zz for i in range(1, 100, 2)),
      any(Z(i) in zz for i in range(0, 100, 2)))
for i in range(100, 300):
    zz.add(Z(i))
print(len(zz), all(Z(i) in zz for i in range(1, 100, 2)),
      all(Z(i) in zz for i in range(100, 300)))
zz.clear()
print(len(zz), Z(1) in zz)

# --- pop drains a table that is shrinking under it -------------------------
p = set(range(400))
got = []
while p:
    got.append(p.pop())
print(len(got), sorted(got) == list(range(400)), len(p))

# --- frozensets take the same path through the constructor -----------------
f = frozenset(range(500))
print(len(f), all(i in f for i in range(500)), 500 in f)
fz = frozenset(Z(i) for i in range(200))
print(len(fz), all(Z(i) in fz for i in range(200)), Z(200) in fz)
