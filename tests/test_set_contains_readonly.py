# `x in s` must not touch the table.
#
# A set iterator is a cursor into the entry array -- slot 0, then 1, then 2 --
# so anything that rehashes the set moves every element to a different slot
# and the walk in progress silently starts reporting some elements twice and
# others not at all.  set_add makes its room before it probes, and the probe
# is shared with set_contains, so a lookup that inherited the room-making
# would rehash a set that is only being ASKED about.
#
# Nothing about that shows in a single lookup's answer, which is why the
# tests below all read the set while something else is walking it.

# --- a lookup inside an iteration over the same set ------------------------
# A table only sits one element below its growth trigger at particular
# sizes, and which sizes those are depends on how the set was BUILT: a
# constructor takes its room in one step, repeated add() climbs through
# every capacity on the way.  So both, over every size up to a hundred --
# whichever n is the boundary, the lookups inside the for land on it.
# The keys are STRINGS, not range(n).  An int hashes to itself, so in any
# table big enough the key i sits in slot i and a rehash puts it back where
# it was -- a resize under an index-based iterator is then invisible, and a
# test built on ints proves nothing.  str hashes scatter, so a rehash really
# does move things.
def by_ctor(keys):
    return set(keys)


def by_add(keys):
    s = set()
    for k in keys:
        s.add(k)
    return s


def by_churn(keys):
    # the same climb, but leaving tombstones behind, because it is
    # ob_size + tombstones that the growth trigger actually counts
    s = set()
    for k in keys:
        s.add(k)
        s.add(k + "!")
        s.discard(k + "!")
    return s


for build in (by_ctor, by_add, by_churn):
    bad = []
    for n in range(1, 101):
        keys = ["k%d" % i for i in range(n)]
        s = build(keys)
        want = sorted(s)
        seen = []
        for x in s:
            for probe in (x, x + "z", "nope", keys[0]):
                probe in s
            seen.append(x)
        if sorted(seen) != want or len(seen) != len(want):
            bad.append(n)
    print(build.__name__, bad)

# --- and with tombstones, which are what the load factor actually counts ---
# Discarding leaves dead slots; fill climbs back to the trigger without
# ob_size moving, so a lookup that tested (fill + 1) would fire here.
t = set(range(64))
for i in range(0, 64, 2):
    t.discard(i)
seen = []
for x in t:
    for probe in range(0, 64):
        probe in t
    seen.append(x)
print(len(seen), sorted(seen) == list(range(1, 64, 2)))

# --- a frozenset is read-only, so a lookup on one had better be too --------
f = frozenset(range(50))
acc = []
for x in f:
    if x in f:
        acc.append(x)
print(len(acc), sorted(acc) == list(range(50)))

# --- and the set-as-key path, which builds a frozenset to ask with ---------
# (the walk order is slot order and the two tables differ, so only the
# collected answers are printed)
g = {frozenset({1, 2}), frozenset({3})}
walked = []
answers = []
for x in g:
    answers.append(({1, 2} in g, {3} in g, {9} in g))
    walked.append(sorted(x))
print(sorted(walked), sorted(set(answers)))

# --- a lookup during a comprehension over the same set --------------------
u = set(range(30))
print(sorted(y for y in u if y in u and y % 3 == 0))
