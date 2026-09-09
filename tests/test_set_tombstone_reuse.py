# A set that is added to and removed from repeatedly must reuse the slots the
# removals freed.  set_find_slot used to skip a tombstone and keep probing, so
# every add consumed a fresh EMPTY slot and the table only ever filled up:
# repeatedly adding and discarding a small fixed range of keys grew the
# capacity without bound and made each probe walk an ever-longer run of
# tombstones.  dict never had the bug -- its probe remembers the first
# reusable slot -- and this is set catching up.
#
# The keys here are deliberately few and small, so they all hash into a narrow
# band of slots.  That is what turns "never reuse a tombstone" from a leak
# into a quadratic: the tombstones pile up in one linear-probe run, and every
# later add walks the whole run.  Spread-out keys hide it completely.
#
# This is a PERFORMANCE regression test, so it asserts by finishing: without
# the fix the churn below takes minutes and run_tests.sh's 60s timeout fires.
# The correctness assertions are real too -- a tombstone reused too eagerly
# would lose a live key, which is the failure mode of getting the fix wrong.


def churn(distinct, iters):
    """Add and remove keys from a narrow range; return the survivors."""
    s = set()
    for i in range(iters):
        k = i % distinct
        s.add(k)
        s.discard(k)
    return len(s)


def churn_resident(distinct, iters):
    """The same, but the set is never empty -- one key stays in throughout."""
    s = set()
    s.add(-1)
    for i in range(iters):
        k = i % distinct
        s.add(k)
        s.discard(k)
    return sorted(s)


def stack_churn(depth, rounds):
    """Add/remove in stack order, the shape a backtracking search produces."""
    s = set()
    total = 0
    for _ in range(rounds):
        for d in range(depth):
            s.add(d)
        total += len(s)
        for d in range(depth - 1, -1, -1):
            s.discard(d)
    return total


print(churn(8, 400000))
print(churn_resident(8, 400000))
print(stack_churn(12, 20000))

# Interleaved add/remove must still answer membership correctly.
s = set()
for i in range(500):
    s.add(i % 16)
    if i % 3 == 0:
        s.discard(i % 16)
print(sorted(s))

# A removed key is gone; a re-added one is back exactly once.
t = set()
t.add(1); t.add(2); t.add(3)
t.discard(2)
print(sorted(t), len(t), 2 in t, 1 in t)
t.add(2)
print(sorted(t), len(t), 2 in t)
t.add(2)
print(sorted(t), len(t))

# Removing every key and refilling must not grow the set's answer set.
u = set(range(32))
for r in range(200):
    for k in range(32):
        u.discard(k)
    for k in range(32):
        u.add(k)
print(len(u), sorted(u) == list(range(32)))

# frozenset shares the entry layout; building one from a churned set works.
f = frozenset(churn_resident(6, 5000))
print(sorted(f))

# --- pop() must leave a TOMBSTONE, not an empty slot -----------------------
# It wrote only the key and left the hash behind, which SET_ENTRY_CLASSIFY
# reads as EMPTY -- so any probe run passing through the popped slot stopped
# there.  A colliding key further along the run became unreachable to `in`,
# to discard() and to remove(), while still being visible to iteration and
# still counted by len().  The set was permanently inconsistent with itself.
#
# Every key here shares one slot, so the survivors are exactly the ones that
# sit beyond the popped entry.


class OneSlot:
    def __init__(self, v):
        self.v = v

    def __hash__(self):
        return 0

    def __eq__(self, o):
        return isinstance(o, OneSlot) and self.v == o.v


for n in (2, 3, 4, 8, 20):
    s = {OneSlot(i) for i in range(n)}
    s.pop()
    by_iteration = sorted(x.v for x in s)
    by_lookup = sorted(i for i in range(n) if OneSlot(i) in s)
    print(n, len(s), by_iteration == by_lookup, len(by_iteration) == n - 1)

# ...and the survivors must still be removable, which is the same probe
drained = {OneSlot(i) for i in range(10)}
drained.pop()
for i in range(10):
    drained.discard(OneSlot(i))
print(len(drained), drained == set())

# a full drain, which is also what the pop cursor is for: without it the scan
# restarts at slot zero every time and the drain is quadratic
big = set(range(500))
out = []
while big:
    out.append(big.pop())
print(len(out), sorted(out) == list(range(500)), len(big))

# the cursor is only a hint: it must survive a resize, a clear and a swap
grown = set(range(4))
grown.pop()
for i in range(100, 400):
    grown.add(i)
print(len(grown), all(i in grown for i in range(100, 400)))
grown.clear()
grown.add(1)
print(sorted(grown), grown.pop(), len(grown))

swapped = set(range(20))
swapped.pop()
swapped &= set(range(10))
print(sorted(swapped) == sorted(x for x in range(10) if x in swapped))

mixed = {1, "a", (2, 3), 4.5, frozenset({1})}
popped = mixed.pop()
print(len(mixed), sorted(str(x) for x in mixed) ==
      sorted(str(x) for x in mixed if x in mixed))

try:
    set().pop()
except KeyError as e:
    print("KeyError", e)
print(hasattr(frozenset(), "pop"))
