# `collections`, CPython's own -- and `keyword`, which it imports.
#
# Ours was the two names _collections supplies plus a hand-written
# OrderedDict.  Counter, namedtuple, ChainMap, UserDict, UserList and
# UserString were simply absent, and the first two of those are in most
# Python programs that count anything or return a record.
#
# The two hazards our stand-in's header named are both satisfied and worth
# restating, because they are the reason it was written that way:
#
#   - CPython's file does `from _collections import deque` and exports the
#     name from __all__ whether or not the import succeeded, so a missing
#     _collections makes `from collections import deque` an ImportError under
#     a real stdlib rather than merely a slower deque.  lib/_collections.py
#     supplies it.
#   - It defines the complete pure-Python OrderedDict HERE and only then
#     tries to override it with the C one, so an `OrderedDict = dict` alias
#     exported from _collections would shadow the real class.
#     lib/_collections.py exports exactly ["deque", "defaultdict"], so there
#     is nothing to shadow it with.
#
# namedtuple needs `keyword.iskeyword` to reject a field named `class`, and
# keyword.py was not here either.  It is generated from CPython's grammar and
# comes over as it is.
import collections
import keyword
import pickle

print("__all__ complete:",
      [n for n in collections.__all__ if not hasattr(collections, n)])
print("__all__:", sorted(collections.__all__))

# --- deque, which stays ours ------------------------------------------
d = collections.deque([1, 2, 3], maxlen=5)
d.appendleft(0)
d.append(4)
d.append(5)
print("deque:", list(d), d.maxlen)
print("deque module:", collections.deque.__module__)
print("deque rotate/extendleft:", end=" ")
d2 = collections.deque([1, 2, 3])
d2.rotate(1)
d2.extendleft([0])
print(list(d2), d2.count(1), d2.index(3))

# --- OrderedDict ------------------------------------------------------
od = collections.OrderedDict(a=1, b=2, c=3)
od.move_to_end("a")
print("OrderedDict:", list(od))
print("popitem(last=False):", od.popitem(last=False))
print("order-sensitive eq:",
      collections.OrderedDict(a=1, b=2) == collections.OrderedDict(b=2, a=1),
      collections.OrderedDict(a=1, b=2) == {"b": 2, "a": 1})
print("repr:", repr(collections.OrderedDict(a=1)))
print("fromkeys/setdefault:",
      collections.OrderedDict.fromkeys("ab"),
      collections.OrderedDict().setdefault("k", 1))

# --- Counter ----------------------------------------------------------
c = collections.Counter("abracadabra")
print("Counter:", c.most_common(3), c["z"])
print("arithmetic:", collections.Counter(a=3, b=1) - collections.Counter(a=1),
      collections.Counter(a=1) + collections.Counter(a=2))
print("and/or:", collections.Counter(a=3, b=1) & collections.Counter(a=1, b=2),
      collections.Counter(a=3) | collections.Counter(a=1, b=2))
print("unary:", +collections.Counter(a=1, b=-1), -collections.Counter(a=1, b=-1))
print("total/elements:", c.total(), sorted(collections.Counter(a=2, b=1).elements()))
print("subtract/update:", end=" ")
c2 = collections.Counter(a=3)
c2.subtract(a=1)
c2.update(b=2)
print(c2)
print("Counter from a mapping:", collections.Counter({"x": 2}))

# --- namedtuple -------------------------------------------------------
Point = collections.namedtuple("Point", "x y")
p = Point(1, 2)
print("namedtuple:", p, p.x, p[1], tuple(p))
print("_fields/_asdict/_replace:", Point._fields, p._asdict(), p._replace(x=9))
print("_make:", Point._make([3, 4]))
print("defaults:", collections.namedtuple("D", "a b", defaults=[9])(1))
print("rename:", collections.namedtuple("R", ["a", "a", "def"], rename=True)._fields)
print("module/doc:", Point.__module__ is not None, isinstance(Point.__doc__, str))
for bad, what in ((("T", ["class"]), "a keyword field"),
                  (("T", ["1x"]), "a field starting with a digit"),
                  (("T", ["a", "a"]), "a duplicate field"),
                  (("class", ["a"]), "a keyword type name")):
    try:
        collections.namedtuple(*bad)
        print("%-32s NOT REFUSED" % what)
    except ValueError:
        print("%-32s ValueError" % what)

# --- ChainMap ---------------------------------------------------------
cm = collections.ChainMap({"a": 1}, {"a": 2, "b": 3})
print("ChainMap:", cm["a"], cm["b"], sorted(cm), len(cm))
print("new_child/parents:", cm.new_child({"c": 4})["c"], dict(cm.parents))
cm["a"] = 9
print("writes hit the first map:", cm.maps[0], cm.maps[1])

# --- the three User* wrappers ----------------------------------------
ud = collections.UserDict({"a": 1})
ud["b"] = 2
print("UserDict:", ud, ud.data, sorted(ud.keys()))
ul = collections.UserList([1, 2])
ul.append(3)
print("UserList:", ul, ul + [4], ul[1:], len(ul))
us = collections.UserString("ab")
print("UserString:", us, us.upper(), us + "c", us * 2, us.startswith("a"))


class CountingDict(collections.UserDict):
    def __setitem__(self, key, value):
        self.data[key] = value * 2


cd = CountingDict()
cd["k"] = 3
print("UserDict subclass:", cd["k"])

# --- pickling, which is where a rebuilt class shows up ---------------
print("pickle namedtuple:", pickle.loads(pickle.dumps(p)) == p)
print("pickle Counter:", pickle.loads(pickle.dumps(collections.Counter("ab"))))
print("pickle OrderedDict:",
      pickle.loads(pickle.dumps(collections.OrderedDict(a=1, b=2))))
print("pickle deque:",
      list(pickle.loads(pickle.dumps(collections.deque([1, 2], maxlen=4)))),
      pickle.loads(pickle.dumps(collections.deque([1], maxlen=4))).maxlen)
print("pickle defaultdict:",
      pickle.loads(pickle.dumps(collections.defaultdict(list, a=[1]))))
print("pickle UserDict/UserList/UserString:",
      pickle.loads(pickle.dumps(ud)), pickle.loads(pickle.dumps(ul)),
      pickle.loads(pickle.dumps(us)))

# --- collections.abc is a submodule, not an attribute ---------------
# CPython's __getattr__ answers only the ABC names deprecated in 3.3, and
# `abc` is not one of them, so a bare `collections.abc` is an AttributeError
# there as well until the submodule is imported.
try:
    collections.abc
    print("collections.abc without importing it: present")
except AttributeError:
    print("collections.abc without importing it: AttributeError")
import collections.abc
print("after importing it:", collections.abc.Sequence.__name__)
try:
    collections.no_such_name
    print("an unknown name: NOT REFUSED")
except AttributeError as exc:
    print("an unknown name: AttributeError")

# --- heapq, which Counter.most_common needs and which was not here ---
import heapq

h = [5, 1, 4]
heapq.heapify(h)
print("heapify/heappop:", h[0], heapq.heappop(h), h)
heapq.heappush(h, 0)
print("heappush:", h[0])
print("heappushpop/heapreplace:",
      heapq.heappushpop([1, 3], 2), heapq.heapreplace([1, 3], 2))
print("nlargest/nsmallest:",
      heapq.nlargest(2, [3, 1, 4, 1, 5]), heapq.nsmallest(2, [3, 1, 4, 1, 5]))
print("with a key:", heapq.nlargest(2, ["bb", "a", "cccc"], key=len))
print("merge:", list(heapq.merge([1, 4], [2, 3])),
      list(heapq.merge([4, 1], [3, 2], reverse=True)))
print("heapq __all__:", [n for n in heapq.__all__ if not hasattr(heapq, n)])
# A sorted drain is the property the whole module exists for.  The data is
# from a fixed LCG rather than `random`, so the test is reproducible and does
# not depend on a module lib/ does not carry.
seed = 12345
data = []
for _ in range(200):
    seed = (seed * 1103515245 + 12345) % (1 << 31)
    data.append(seed % 1000)
hp = list(data)
heapq.heapify(hp)
print("drains sorted:",
      [heapq.heappop(hp) for _ in range(len(data))] == sorted(data))

# --- keyword ---------------------------------------------------------
print("iskeyword:", keyword.iskeyword("class"), keyword.iskeyword("x"))
print("issoftkeyword:", keyword.issoftkeyword("match"), keyword.issoftkeyword("x"))
print("kwlist:", len(keyword.kwlist), keyword.kwlist[:4])
print("softkwlist:", sorted(keyword.softkwlist))
print("survived")
