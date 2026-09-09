# d.copy() and dict(d) clone both tables rather than re-inserting every key.
#
# The copy takes the source's capacity, so the sparse index array transfers
# as it stands and no key is hashed or probed; the dense array is memcpy'd up
# to its high-water mark, HOLES AND ALL, which is what keeps the index
# array's dummies pointing at the right slots and keeps insertion order.
#
# So the cases that matter are the ones where the two arrays disagree with a
# naive walk: a table with holes, a table that is all holes, a table churned
# by delete-and-reinsert until the tombstones outnumber the entries, and a
# table whose live count no longer matches its high-water mark.  A copy that
# got the index array or the high-water mark wrong would still answer every
# key correctly right up until a probe ran off the end of a chain.
import gc


def shapes():
    yield {}
    yield {1: 2}
    yield {i: i * 2 for i in range(7)}
    yield {i: i for i in range(1000)}
    yield {"a": 1, "b": 2, "c": 3}
    holed = {i: i for i in range(20)}
    for i in range(0, 20, 2):
        del holed[i]
    yield holed
    emptied = {i: i for i in range(20)}
    for i in range(20):
        del emptied[i]
    yield emptied
    churned = {i: i for i in range(50)}
    for i in range(50):
        del churned[i]
        churned[i + 100] = i
    yield churned
    yield {(1, 2): [3], "k": {4: 5}, 6.5: None, 2 ** 70: True}


ok = True
for d in shapes():
    c = d.copy()
    c2 = dict(d)
    c3 = {**d}
    if not (c == d == c2 == c3):
        ok = False
    if not (list(c) == list(d) == list(c2) == list(c3)):
        ok = False
    if list(c.values()) != list(d.values()):
        ok = False
    if not (len(c) == len(d) == len(c2)):
        ok = False
    # every key still probes to the right entry in the clone
    for k in d:
        if c[k] != d[k] or k not in c:
            ok = False
    # and the clone is independent
    c[999] = 1
    if 999 in d:
        ok = False
print("copy shapes", ok)

d = {"a": 1, "b": 2}
c = d.copy()
print(c, list(c), c == d, c is d)
c["c"] = 3
del c["a"]
print(d, c, list(c))

# shallow: the inner object is shared
inner = [1]
n = {"k": inner}
m = n.copy()
print(m["k"] is inner)
m["k"].append(2)
print(n, m)

# References: an object carried through four kinds of copy dies exactly once.
seen = []


class Watch:
    def __init__(self, tag):
        self.tag = tag

    def __del__(self):
        seen.append(self.tag)


def churn():
    w = Watch("w")
    a = {"k": w, "j": 1}
    b = a.copy()
    c2 = dict(a)
    d2 = {**a}
    e2 = {}
    e2.update(a)
    del a, b, c2, d2, e2, w


churn()
print(seen)

# A non-empty clone is collector-tracked exactly when its source is.
print(gc.is_tracked({}.copy()), gc.is_tracked({1: 2}.copy()),
      gc.is_tracked({1: []}.copy()))
print(gc.is_tracked(dict({1: []})), gc.is_tracked(dict({1: 2})))

# An EMPTY source is the case where the two come apart, because clear() does
# not untrack: this dict is still in a generation and holds nothing.  Every
# copy of it is untracked, which is what CPython answers too -- PyDict_Copy
# returns a plain PyDict_New() when ma_used is 0 -- and is right, because a
# dict holding nothing cannot be part of a cycle.
emptied = {1: []}
print(gc.is_tracked(emptied))
emptied.clear()
print(gc.is_tracked(emptied), gc.is_tracked(emptied.copy()),
      gc.is_tracked(dict(emptied)), gc.is_tracked({**emptied}))
into = {}
into.update(emptied)
print(gc.is_tracked(into))
# ...and it starts being tracked again the moment something trackable goes in
emptied["k"] = []
print(gc.is_tracked(emptied), gc.is_tracked(emptied.copy()))
emptied["self"] = emptied
del emptied, into
print(gc.collect() >= 0)
cyc = {}
cyc["s"] = cyc
cc = cyc.copy()
del cyc, cc
gc.collect()
print("gc ok")

print(dict({"a": 1}, b=2), dict({"a": 1}, a=9))


class DSub(dict):
    pass


sub = DSub({"x": 1})
print(dict(sub), sub.copy(), type(sub.copy()).__name__)

# type_from_parts copies the namespace, so the class does not alias it
ns = {"v": 1}
T = type("T", (), ns)
ns["v"] = 2
print(T.v, ns["v"])

# --- presized bulk inserts, and dense-array walks --------------------------
# update, |, |=, {**a}, dict(pairs) and the two BUILD_MAP opcodes now take
# the room once instead of rebuilding the table on the way, and they walk the
# source's dense array up to dk_nentries rather than every slot of it.  A
# wrong bound shows as a missing or duplicated entry, so every size around
# the resize boundaries is checked, and so is a source full of holes.
sizes_ok = True
for n in (0, 1, 5, 6, 7, 8, 9, 16, 17, 100, 1000):
    src = {i: i * 2 for i in range(n)}
    a = {}
    a.update(src)
    b = {"pre": 1}
    b.update(src)
    e = {"pre": 1}
    e |= src
    for got in (a, dict(src), {} | src, {**src}, dict(list(src.items()))):
        if got != src or list(got) != list(src):
            sizes_ok = False
    if len(b) != n + 1 or list(b)[0] != "pre" or len(e) != n + 1:
        sizes_ok = False
print("bulk sizes", sizes_ok)

holed = {i: i for i in range(50)}
for i in range(0, 50, 3):
    del holed[i]
target = {}
target.update(holed)
print(len(holed), ({} | holed) == holed, dict(holed) == holed,
      list({**holed}) == list(holed), target == holed, list(target) == list(holed))

selfup = {1: 1, 2: 2}
selfup.update(selfup)
selfup |= selfup
print(selfup)

print({"a": 1, "b": 2} | {"b": 9, "c": 3})
over = {"a": 1}
over.update({"a": 2}, a=3)
print(over, {**{"a": 1}, **{"a": 2}})

print({"k00": 0, "k01": 1, "k02": 2, "k03": 3, "k04": 4, "k05": 5, "k06": 6,
       "k07": 7, "k08": 8, "k09": 9, "k10": 10, "k11": 11, "k12": 12,
       "k13": 13, "k14": 14, "k15": 15, "k16": 16, "k17": 17, "k18": 18,
       "k19": 19})
kk = "z"
print({kk: 1, "y": 2}, {kk: 1, kk: 2})


def kwf(**kw):
    return sorted(kw)


print(kwf(**{"a": 1}, **{"b": 2}))
try:
    {}.update(5)
except TypeError:
    print("TypeError")
try:
    {}.update([(1,)])
except ValueError as e:
    print("ValueError", e)
try:
    {}.update([1, 2])
except TypeError as e:
    print("TypeError", e)
