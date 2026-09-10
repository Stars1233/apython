# str.format_map() takes any mapping, not only a dict.
#
# CPython's is PyObject_GetItem, and its own test suite calls
# `'{a1}'.format_map(match_object)`.  This one read whatever it was handed as
# a PyDictObject and probed the object's header as a hash table -- a segfault
# in dict_lookup, which is where CPython's test_re died.
#
# The two lookups also differ in what a miss means and in what is owned: a
# dict miss is a NULL with nothing pending and the KeyError is ours to raise,
# where mp_subscript has already raised and hands back an owned reference.

import gc
import re


class Mapping:
    def __init__(self, extra=None):
        self.asked = []
        self.extra = extra or {}

    def __getitem__(self, key):
        self.asked.append(key)
        if key in self.extra:
            return self.extra[key]
        if key == "boom":
            raise KeyError("nope")
        if key == "other":
            raise ValueError("not a KeyError at all")
        return "".join(["<", key, ">"])


m = Mapping()
print("{x} {y}".format_map(m))
print(m.asked)

try:
    "{boom}".format_map(Mapping())
except KeyError as e:
    print("KeyError", e)

try:
    "{other}".format_map(Mapping())
except ValueError as e:
    print("ValueError", e)

# A dict miss still raises, and a dict still works.
print("{k} {j}".format_map({"k": 1, "j": [2]}))
try:
    "{z}".format_map({})
except KeyError as e:
    print("dict KeyError")

# A dict SUBCLASS takes the direct path.
class D(dict):
    pass


print("{a}".format_map(D(a="sub")))

# A match object, which is what CPython's test_re does.
pat = re.compile("(?:(?P<a1>a)|(?P<b2>b))(?P<c3>c)?")
mt = pat.match("a")
print("a1={a1} b2={b2} c3={c3}".format_map(mt))
mt2 = pat.match("ac")
print("a1={a1} b2={b2} c3={c3}".format_map(mt2))
try:
    "a1={a2}".format_map(mt)
except IndexError as e:
    print("IndexError", e)

# The owned reference the mapping hands back is released: a leak here shows
# as growth, an over-release as a crash in the collector.
class Counting:
    def __getitem__(self, key):
        return "".join(["v", key, "-" * 20])


c = Counting()
for i in range(500):
    "{a}{b}{c}".format_map(c)
    if i % 100 == 0:
        gc.collect()
gc.collect()
print("hammered")

# Suffixes -- {k.attr} and {k[0]} -- go through the same lookup.
class Holder:
    def __init__(self, v):
        self.v = v


print("{h.v}".format_map({"h": Holder("attr")}))
print("{s[1]}".format_map({"s": ["zero", "one"]}))
print("{h.v}".format_map(Mapping(extra={"h": Holder("via mapping")})))

# Something that is not a mapping at all.
try:
    "{a}".format_map(5)
except (TypeError, KeyError) as e:
    print(type(e).__name__)
try:
    "{a}".format_map(object())
except (TypeError, KeyError) as e:
    print(type(e).__name__)
print("done")
