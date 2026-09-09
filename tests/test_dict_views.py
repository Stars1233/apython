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

# dict_iter_next hands back an OWNED reference and list_append takes its own,
# so collecting a view's elements for its repr leaked one per element -- for
# an items view a freshly built tuple, so a loop grew without bound: 7.4MB
# live after 500 reprs of a 200-entry dict, against 151KB now.
big = {i: (i, i) for i in range(60)}
for _ in range(120):
    text = repr(big.items()) + repr(big.keys()) + repr(big.values())
print(len(text) > 0, text[:11])

# A view can reach itself, and the repr had no cycle guard, so it recursed to
# the depth limit.  It shares the stack list and tuple use; the marker is a
# bare ellipsis, because the enclosing level supplies the name.
d = {}
d["k"] = d.values()
print(d.values())
print(d.items())
e = {}
e["x"] = e.keys()
print(e.keys())
print({1: 2}.keys(), {1: 2}.values(), {1: 2}.items())

# --- `(k, v) in d.items()` ------------------------------------------------
# The items view answers this with one lookup and one comparison, where it
# used to fall to the generic protocol and walk the whole view.  What the
# shortcut must NOT change: a probe that is not a two-element tuple is False
# rather than an error, a tuple SUBCLASS is accepted, the value comparison
# runs __eq__ on the object the dict holds, and an exception from either the
# key's __hash__ or the value's __eq__ propagates instead of answering False.
w = {"a": 1, "b": 2, 3: "x", (1, 2): 9}
print(("a", 1) in w.items(), ("a", 2) in w.items(), ("z", 1) in w.items())
print((3, "x") in w.items(), ((1, 2), 9) in w.items())
print(("a", 1) not in w.items(), ("a", 9) not in w.items())
for probe in ("a", 1, None, (), ("a",), ("a", 1, 2), ["a", 1], 1.5, w):
    print(repr(probe), probe in w.items())


class TSub(tuple):
    pass


print(TSub(("a", 1)) in w.items(), TSub(("a", 9)) in w.items())

# equality across the three encodings, exactly as a plain lookup gives
one = {1: 1}
print((True, 1) in one.items(), (1, True) in one.items(),
      (1.0, 1.0) in one.items(), (1, 2) in one.items())


class Eq:
    def __init__(self, v):
        self.v = v

    def __eq__(self, o):
        return isinstance(o, Eq) and self.v == o.v

    def __hash__(self):
        return hash(self.v)


byval = {"k": Eq(1)}
print(("k", Eq(1)) in byval.items(), ("k", Eq(2)) in byval.items())
bykey = {Eq(1): "v"}
print((Eq(1), "v") in bykey.items(), (Eq(2), "v") in bykey.items())


class RaisesEq:
    def __eq__(self, o):
        raise ZeroDivisionError("eq")

    def __hash__(self):
        return 7


try:
    print(("k", 1) in {"k": RaisesEq()}.items())
except ZeroDivisionError as ex:
    print("ZeroDivisionError", ex)


class RaisesHash:
    def __hash__(self):
        raise KeyError("h")


try:
    print((RaisesHash(), 1) in w.items())
except KeyError as ex:
    print("KeyError", ex)

print(("a", 1) in {}.items(), len({}.items()))
# the other two views are unchanged: keys goes to the dict, values walks
print("a" in w.keys(), "z" in w.keys(), 1 in w.values(), 99 in w.values())
