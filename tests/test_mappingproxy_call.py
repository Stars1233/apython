# mappingproxy exists so that types.py can do
# `MappingProxyType = type(type.__dict__)`, but it had no tp_call -- so
# `MappingProxyType(d)`, which enum uses for Enum.__members__, went through
# the ordinary class-construction path, allocated a proxy with a garbage
# mapping pointer and crashed on the first read.
MappingProxyType = type(type.__dict__)
print(MappingProxyType.__name__)

d = {"a": 1, "b": 2}
p = MappingProxyType(d)
print(type(p).__name__, len(p), p["a"], "b" in p, "z" in p)
print(sorted(p), sorted(p.keys()), sorted(p.values()), sorted(p.items()))
print(p)

# It is a window, not a copy.
d["c"] = 3
print(len(p), p["c"])

# Read-only.
try:
    p["d"] = 4
except TypeError as e:
    print("setitem refused")

# A proxy of a proxy, and of an empty dict.
q = MappingProxyType(p)
print(len(q), q["a"])
print(len(MappingProxyType({})))

# get() and iteration order.
print(p.get("a"), p.get("zz"), p.get("zz", 9))
print([k for k in p])

# The wrong argument type is a TypeError, not a crash.
try:
    MappingProxyType(1)
except TypeError:
    print("int refused")
try:
    MappingProxyType()
except TypeError:
    print("no-arg refused")

# A dict SUBCLASS is a mapping.  The check was `ob_type is dict`, so
# OrderedDict, defaultdict, Counter and any user subclass were refused with
# the message meant for a sequence -- which is what stopped inspect.signature,
# whose Signature.parameters wraps an OrderedDict in one.
class MyDict(dict):
    pass


sub = MyDict(a=1, b=2)
sp = MappingProxyType(sub)
print(sp["a"], len(sp), "b" in sp, sorted(sp.keys()))
print(sp.get("a"), sp.get("zz", 9), sorted(sp.items()))

import collections
od = MappingProxyType(collections.OrderedDict([("x", 1), ("y", 2)]))
print(list(od), od["y"])

# The proxy is a view: a write through the underlying dict shows, and a write
# through the proxy is still refused.
sub["c"] = 3
print(len(sp), sp["c"])
try:
    sp["d"] = 4
except TypeError:
    print("still read-only")

# A list is refused by both, and with the same wording -- this is the message
# the dict-subclass cases above used to get.
try:
    MappingProxyType([1, 2])
except TypeError as e:
    print("list refused:", e)
