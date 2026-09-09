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

# The clone is collector-tracked exactly when the source is.
print(gc.is_tracked({}.copy()), gc.is_tracked({1: 2}.copy()),
      gc.is_tracked({1: []}.copy()))
print(gc.is_tracked(dict({1: []})), gc.is_tracked(dict({1: 2})))
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
