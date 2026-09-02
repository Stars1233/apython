# Test dict view objects

# dict.keys() returns a view
d = {"a": 1, "b": 2, "c": 3}
k = d.keys()
print(len(k))

# dict.values() returns a view
v = d.values()
print(len(v))

# dict.items() returns a view
items = d.items()
print(len(items))

# Iteration over keys
d2 = {"x": 10, "y": 20}
result = []
for key in d2.keys():
    result.append(key)
result.sort()
print(result)

# Iteration over values
result = []
for val in d2.values():
    result.append(val)
result.sort()
print(result)

# Iteration over items — collect as dict to avoid tuple sort issue
result = {}
for k, v in d2.items():
    result[k] = v
# Verify we got all items
print(len(result))
print(result["x"])
print(result["y"])

# Direct iteration over dict (keys by default)
result = []
for key in d2:
    result.append(key)
result.sort()
print(result)

# Views reflect mutations
d3 = {"a": 1}
k3 = d3.keys()
print(len(k3))
d3["b"] = 2
print(len(k3))

# Multiple iterations over same view
d4 = {"p": 1, "q": 2}
v4 = d4.values()
s1 = 0
for val in v4:
    s1 = s1 + val
s2 = 0
for val in v4:
    s2 = s2 + val
print(s1 == s2)

# Empty dict views
d5 = {}
print(len(d5.keys()))
print(len(d5.values()))
print(len(d5.items()))
for x in d5.keys():
    print("should not print")

# --- The repr, which the three view types did not have -----------------------
#
# All three had tp_repr 0, and obj_repr answers a NULL Value for that with no
# exception pending -- so print(d.keys()) printed nothing at all.  The text is
# the type's own name around the repr of a list of the view's contents, which
# is what CPython writes and lets list_repr do the work, recursion guard
# included.

d = {"a": 1, "b": 2}
print(repr(d.keys()), repr(d.values()), repr(d.items()))
print(d.keys(), d.values(), d.items())
print(str(d.keys()), "%s" % (d.items(),))

e = {}
print(repr(e.keys()), repr(e.values()), repr(e.items()))

f = {1: "x", (2, 3): [4]}
print(repr(f.keys()), repr(f.values()), repr(f.items()))

# The contents were always there; only the repr was missing.
print(list(d.keys()), list(d.values()), list(d.items()))
print(len(d.keys()), len(d.values()), len(d.items()))
print("a" in d.keys(), "z" in d.keys())
print(sorted(d.keys()), sorted(d.values()))
for k, v in d.items():
    print(k, v)

# A view reflects later changes, and its repr with it.
g = {"x": 1}
kv = g.keys()
print(repr(kv))
g["y"] = 2
print(repr(kv), len(kv))

# In a container, and as a dict value.
print([d.keys()], {"v": d.values()})
