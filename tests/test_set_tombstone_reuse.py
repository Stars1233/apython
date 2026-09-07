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
