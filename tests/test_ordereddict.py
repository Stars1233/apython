# OrderedDict was `OrderedDict = dict`, exported from lib/_collections.py.
#
# That alias shadowed the complete pure-Python class CPython's own
# collections/__init__.py defines: CPython defines the class there and only
# THEN tries to override it from _collections, so anything we export under
# that name wins over the real one wherever a real stdlib is on the path.
# deque and defaultdict have no such fallback and must stay in _collections;
# only OrderedDict had to move the other way.
#
# Two interpreter bugs turned up under it and are checked here too:
#   - dict_richcompare tested the RIGHT operand for the exact dict type, so
#     `D(x) == D(x)` for any dict subclass answered NotImplemented, fell
#     through to identity, and came out False.
#   - vars() read PyInstanceObject.inst_dict at a fixed offset, which for a
#     dict subclass lands inside the base's own header.  Empty it read as 0
#     and invented a dict; populated it read a live pointer and segfaulted.

from collections import OrderedDict, deque, defaultdict

d = OrderedDict()
d["a"] = 1
d["b"] = 2
d["c"] = 3
print(list(d), d["b"], len(d))

d.move_to_end("a")
print(list(d))
d.move_to_end("c", last=False)
print(list(d))

print(d.popitem(last=False))
print(d.popitem())
print(list(d.items()))

print(repr(OrderedDict()), repr(OrderedDict([("x", 1)])))
print(repr(OrderedDict(a=1, b=2)))

# __eq__ is order-sensitive against another OrderedDict and order-insensitive
# against a plain dict.  That is CPython's rule, not a shortcut.
a = OrderedDict([("x", 1), ("y", 2)])
b = OrderedDict([("y", 2), ("x", 1)])
print(a == b, b == a, a != b)
print(a == OrderedDict([("x", 1), ("y", 2)]))
print(a == {"x": 1, "y": 2}, {"x": 1, "y": 2} == a)
print(a == {"x": 1}, a == 5, a == "x")

print(OrderedDict.fromkeys("abc", 0))
print(OrderedDict.fromkeys([]))

c = a.copy()
print(type(c).__name__, list(c), c == a)

for f in (lambda: OrderedDict().popitem(), lambda: a.move_to_end("zzz")):
    try:
        f()
    except KeyError as e:
        print("KeyError", e)

print(isinstance(a, dict), a.get("x"), "x" in a, sorted(a.keys()))
a.setdefault("z", 9)
a.update({"w": 0})
print(list(a.items()))
del a["w"]
print(list(a.items()))

# The one-argument constructor and the ordinary dict protocol
print(OrderedDict({"p": 1}), OrderedDict([("q", 2)]), OrderedDict(r=3))
print(list(OrderedDict([("a", 1), ("b", 2)]).values()))

# deque and defaultdict still come from _collections.  Whether the module
# also exports OrderedDict is not comparable against CPython: there it is a C
# module with no __all__ at all, and here it is lib/_collections.py.
print(type(deque()).__name__, type(defaultdict()).__name__)

# --- the two interpreter bugs, directly ---


class Sub(dict):
    pass


s1 = Sub([("k", 1)])
s2 = Sub([("k", 1)])
print(s1 == s2, s2 == s1, s1 == {"k": 1}, {"k": 1} == s1)
print(s1 != s2, s1 == Sub([("k", 2)]))

print(vars(Sub()))
s3 = Sub()
s3["k"] = "v"
print(vars(s3))
s3.attr = 7
print(vars(s3), s3["k"], s3.attr)

for base in (dict, list, tuple, str, int, set, bytes, float, object):
    class B(base):
        pass

    obj = B()
    obj.tag = base.__name__
    print(base.__name__, vars(obj))
