# Every container publishes __repr__ by name, and pprint depends on it.
#
# `list.__repr__ is object.__repr__` was True here and is False in CPython.
# pprint keys its dispatch table on the UNBOUND repr --
# `_dispatch[list.__repr__] = _pprint_list`, and eight more lines like it --
# so list, tuple, dict, set, frozenset, bytearray, mappingproxy and
# SimpleNamespace all collapsed onto one key, and the table ended up holding
# whichever line ran last: _pprint_simplenamespace.  pprint then handed a list
# to the SimpleNamespace printer, which does `object.__dict__.items()`.
#
# That is where every one of the 105 `'list' object has no attribute
# '__dict__'` errors in the sweep came from -- not from __dict__ at all.
#
# Like the str/repr thunks already there, these read the DEFINING type's slot,
# so a subclass inherits the base's behaviour instead of re-dispatching into
# itself.

import types

builtins_with_repr = [
    ("list", list, [1, 2]),
    ("tuple", tuple, (1, 2)),
    ("dict", dict, {"a": 1}),
    ("set", set, {1}),
    ("frozenset", frozenset, frozenset({1})),
    ("bytearray", bytearray, bytearray(b"ab")),
    ("str", str, "s"),
    ("bytes", bytes, b"b"),
    ("int", int, 1),
    ("float", float, 1.0),
    ("complex", complex, 1j),
    ("bool", bool, True),
    ("range", range, range(2)),
    ("slice", slice, slice(1, 2)),
]

# Each type's __repr__ is its own, not object's -- which is what makes them
# usable as dispatch keys at all.
for name, t, value in builtins_with_repr:
    print(name, t.__repr__ is not object.__repr__, hasattr(t, "__repr__"))

# ...and they are distinct from each other, which is the property pprint
# actually relies on.
keys = {}
for name, t, value in builtins_with_repr:
    keys.setdefault(t.__repr__, []).append(name)
print("distinct keys:", len(keys) == len(builtins_with_repr))
for k, names in keys.items():
    if len(names) > 1:
        print("COLLISION:", sorted(names))

# Calling it, bound and unbound, gives the ordinary repr.
for name, t, value in builtins_with_repr:
    print(name, value.__repr__(), t.__repr__(value), repr(value))

# SimpleNamespace, which is the type whose printer was winning the collision.
ns = types.SimpleNamespace(a=1)
print("namespace:", ns.__repr__(), type(ns).__repr__ is not object.__repr__)

# mappingproxy
mp = type.__dict__
print("mappingproxy:", type(mp).__repr__ is not object.__repr__)

# type itself: its __repr__ is a descriptor, so it only works unbound here.
print("type:", type.__repr__ is not object.__repr__, type.__repr__(int), repr(int))

# A subclass with no __repr__ of its own inherits the base's, and one with a
# __repr__ of its own wins -- the thunk must not re-dispatch into itself.
class L(list):
    pass


class L2(list):
    def __repr__(self):
        return "<L2>"


print(repr(L([1, 2])), repr(L2([1, 2])), L.__repr__ is list.__repr__)
print(list.__repr__(L2([1, 2])))


class D(dict):
    pass


class S(set):
    pass


class T(tuple):
    pass


print(repr(D(a=1)), repr(S({1})), repr(T((1,))))

# Arity is checked, and so is the receiver.
try:
    list.__repr__([1], 2)
except TypeError:
    print("arity refused")
for wrong in ((1, 2), "x", 5, None):
    try:
        list.__repr__(wrong)
    except TypeError:
        print("TypeError for", type(wrong).__name__)
    else:
        print("no TypeError for", type(wrong).__name__)

# __str__ where the type defines one distinct from __repr__.
print(str([1, 2]), str({1: 2}), str((1,)), str(bytearray(b"a")))
print(type([]).__str__ is object.__str__)
print("done")
