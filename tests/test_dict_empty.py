# Every empty dict points at ONE shared, read-only table instead of
# allocating its own.  A dict used to malloc twice and `rep stosq` twice in
# dict_new -- 192 bytes of entries and 64 of indices -- for every dict ever
# made, including `{}`, every **kwargs frame dict whether or not a keyword
# arrives, and every instance __dict__ that never gets an attribute.
#
# The table's capacity is ONE, which is what makes it safe rather than a
# special case: a read masks the hash to the single slot, finds it empty and
# answers miss, and a write cannot reach it because dict_set's room test is
# `dk_nentries + 1 > capacity * 3/4` and three quarters of one is zero, so
# the first insert resizes to a real table before storing anything.  The
# table is in .rodata, so a stray write is a fault rather than corruption.
#
# What this file is for: everything an empty dict can be asked, every way one
# can stop being empty, and every way one can become empty again.
import gc

d = {}
print(d, len(d), bool(d), list(d), list(d.keys()), list(d.values()), list(d.items()))
print("a" in d, 1 in d, d.get("a"), d.get("a", 7), d == {}, d != {}, d == {"a": 1})
print(repr(d), sorted(d), list(reversed(d)), d.copy(), dict(d), {**d})
print(d.keys() == set(), d.items() == set(), len(d.keys()), len(d.items()))
try:
    d["missing"]
except KeyError as e:
    print("KeyError", e)
try:
    del d["missing"]
except KeyError as e:
    print("KeyError", e)
try:
    d.pop("missing")
except KeyError as e:
    print("KeyError", e)
print(d.pop("missing", 5), d.setdefault("x", 1), d)

# Two empty dicts share a table but are still two dicts.
a, b = {}, {}
a["k"] = 1
print(a, b, len(a), len(b), a == b)
lots = [{} for _ in range(50)]
lots[7]["x"] = 1
print(len(lots), lots[7], lots[8], sum(len(x) for x in lots))

# Growing away from the shared table, at every size the resize rule can
# notice.  The first insert must land on a real table, not on a two-slot one
# that immediately resizes again.
for n in (0, 1, 2, 5, 6, 7, 8, 9, 16, 17, 100, 1000):
    e = {}
    for i in range(n):
        e[i] = i * 2
    assert len(e) == n and list(e) == list(range(n)), n
    assert all(e[i] == i * 2 for i in range(n)), n
print("growth ok")

# ... and back to empty, then away again.
c = {i: i for i in range(50)}
c.clear()
print(c, len(c), list(c), "a" in c, c == {})
c["z"] = 1
c[1] = 2
print(c, len(c))
c.clear()
c.clear()
print(c, len(c), c.get(1), 1 in c)

f = {"a": 1, "b": 2}
del f["a"]
del f["b"]
print(f, len(f), f == {})
f["c"] = 3
print(f)

g = {1: 1}
g.pop(1)
g[2] = 2
print(g)

# The bulk builders over an empty source and an empty destination.
h = {}
h.update({1: 2})
print(h)
i2 = {}
i2 |= {3: 4}
print(i2, {} | {5: 6}, {} | {}, {**{}, **{}})
print(dict(), dict([]), dict({}), dict(**{}), dict.fromkeys([]), dict.fromkeys([], 0))
print({}.copy(), dict({}.items()), list({}.items()))

# A dict subclass shares the machinery, and its instance is freed through
# instance_dealloc rather than dict_dealloc.
class D(dict):
    pass


x = D()
print(x, len(x), x == {})
x["k"] = 1
print(x, len(x))
del x

# An instance __dict__ that never receives an attribute, and one that does.
class C:
    pass


o = C()
print(o.__dict__, len(o.__dict__))
o.v = 1
print(o.__dict__)
p = C()
print(p.__dict__)


# A **kwargs function called with no keywords builds an empty dict per call.
def kw(a, **k):
    return a, k


print(kw(1), kw(1, z=2), kw(1))

# The collector must not follow the shared table, and an empty dict is not
# tracked at all.
print(gc.is_tracked({}), gc.is_tracked({1: 2}), gc.is_tracked({1: []}))
cyc = {}
cyc["self"] = cyc
del cyc
gc.collect()
print("gc ok")
