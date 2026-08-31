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
