# collections.defaultdict is a dict subclass.
#
# lib/_collections.py stands in for CPython's C module, and its defaultdict
# was a plain class holding a dict in self._data.  That is not a small
# difference: isinstance(d, dict) is False, dict(d) does not work, and every
# method the class did not hand-delegate -- setdefault, clear, copy, popitem,
# fromkeys, |, |=, ==, reversed, the reduce protocol -- simply was not there.
# CPython's own test_defaultdict reports 988 errors against it, and the file
# wins even when a real stdlib is on the path, because collections/__init__.py
# imports the name from here.
#
# The machinery for a real subclass was already in place: dict carries no
# TYPE_FLAG_FINAL, and dict_subscript already consults __missing__ on any
# subclass.
import collections

dd = collections.defaultdict

# --- it IS a dict -----------------------------------------------------------

d = dd(int)
print("isinstance:", isinstance(d, dict))
print("issubclass:", issubclass(dd, dict))
d["a"] += 1
d["b"] += 2
print("dict(d):", dict(d))
print("type:", type(d).__name__)

# --- the factory ------------------------------------------------------------

d = dd(list)
d["x"].append(1)
d["x"].append(2)
print("list factory:", dict(d))

d = dd(None)
try:
    d["missing"]
except KeyError as e:
    print("no factory:", type(e).__name__, e)

print("default_factory readable:", dd(int).default_factory)
d = dd(int)
d.default_factory = list
d["k"].append(9)
print("default_factory writable:", dict(d))
d.default_factory = None
try:
    d["nope"]
except KeyError:
    print("cleared factory raises")

# get() must NOT call the factory, and neither must `in`.
d = dd(int)
print("get:", d.get("absent"), "get default:", d.get("absent", 7))
print("contains:", "absent" in d)
print("untouched:", dict(d))

# __missing__ is the hook, and it is callable directly.
d = dd(int)
print("__missing__:", d.__missing__("z"), dict(d))

# --- the dict methods that were absent --------------------------------------

d = dd(int, {"a": 1})
print("setdefault:", d.setdefault("b", 5), d.setdefault("a", 99), dict(d))
print("pop:", d.pop("a"), d.pop("gone", "dflt"))
print("popitem:", dd(int, {"only": 1}).popitem())
print("keys/values/items:", sorted(d.keys()), sorted(d.values()), sorted(d.items()))
print("len:", len(d), "iter:", sorted(iter(d)))
print("reversed:", list(reversed(dd(int, {"a": 1, "b": 2}))))

d = dd(int, {"a": 1})
c = d.copy()
print("copy:", dict(c), type(c).__name__, c.default_factory)
c["new"] += 1
print("copy independent:", dict(d), dict(c))

d.update({"b": 2}, c=3)
print("update:", dict(d))
d.clear()
print("clear:", dict(d), d.default_factory)

print("fromkeys:", dict(dd.fromkeys("ab", 0)), type(dd.fromkeys("ab", 0)).__name__)

# --- equality and the operators ---------------------------------------------

print("eq plain dict:", dd(int, {"a": 1}) == {"a": 1})
print("eq other factory:", dd(int, {"a": 1}) == dd(list, {"a": 1}))
print("ne:", dd(int, {"a": 1}) != {"a": 2})

merged = dd(int, {"a": 1}) | {"b": 2}
print("or:", dict(merged), type(merged).__name__)
merged = {"b": 2} | dd(int, {"a": 1})
print("ror:", dict(merged), type(merged).__name__)
d = dd(int, {"a": 1})
d |= {"b": 2}
print("ior:", dict(d), type(d).__name__)

# --- repr, copy and pickling ------------------------------------------------

print("repr empty:", repr(dd(int)))
print("repr with items:", repr(dd(int, {"a": 1})))
print("repr no factory:", repr(dd(None)))
print("repr nested:", repr(dd(list, {"a": [1]})))

import copy as copymod

d = dd(list, {"a": [1, 2]})
sh = copymod.copy(d)
dp = copymod.deepcopy(d)
print("copy.copy:", dict(sh), type(sh).__name__, sh.default_factory is list)
print("copy.deepcopy:", dict(dp), type(dp).__name__, dp.default_factory is list)
dp["a"].append(3)
print("deepcopy independent:", dict(d), dict(dp))

r = d.__reduce__()
print("reduce callable:", r[0] is dd, "args:", r[1])

# --- subclassing it ---------------------------------------------------------

class Counter2(dd):
    def __init__(self, *a, **k):
        super().__init__(int, *a, **k)


c2 = Counter2({"x": 1})
c2["y"] += 5
print("subclass:", dict(c2), type(c2).__name__, isinstance(c2, dict))

# --- the constructor's own rules --------------------------------------------

try:
    dd(5)
except TypeError as e:
    print("non-callable factory:", "must be callable" in str(e) or "callable" in str(e))

print("from pairs:", dict(dd(int, [("a", 1), ("b", 2)])))
print("from kwargs:", dict(dd(int, a=1, b=2)))
print("no args:", dict(dd()), dd().default_factory)

print("done")
