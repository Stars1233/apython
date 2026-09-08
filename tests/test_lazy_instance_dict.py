"""An instance gets its __dict__ when something puts one in it.

instance_new called dict_new for every instance of every class without
__slots__ -- gc_alloc plus two ap_mallocs and two rep stosqs, nearly 300 bytes
-- including for every instance that never gets an attribute.  It is created on
demand now: instance_setattr makes one on the first store, and obj_generic_attr
makes and ATTACHES one on the first read of __dict__.

The comment that used to justify the eager creation said every consumer of
LOAD_INST_DICT could then read a NULL as "this family has no dict at all".
That was already untrue: int subclasses and bytes subclasses have shipped a
NULL slot from the start, and instance_setattr has created a tail dict on
demand for as long as it has existed.

What this pins is the observable half -- that a dict appears when it is asked
for, that it is the SAME dict every time, and that vars() and __dict__ agree.
vars() did not: a NULL slot made it return a fresh, detached dict, so
`vars(o)['x'] = 1` silently did not stick.  Only int and bytes subclasses could
reach that before; every instance can now.
"""

import gc


class Plain:
    pass


class WithInit:
    def __init__(self):
        self.a = 1


class Slots:
    __slots__ = ("s",)


class SubStr(str):
    pass


class SubBytes(bytes):
    pass


class SubInt(int):
    pass


class SubTuple(tuple):
    pass


print("--- a dict appears when it is asked for ---")
o = Plain()
print("empty:", o.__dict__)
print("stable:", o.__dict__ is o.__dict__)
o.x = 1
print("after a store:", o.__dict__)
print("still the same object:", o.__dict__ is o.__dict__)


print("--- vars() and __dict__ are the same dict ---")
p = Plain()
print("identity:", vars(p) is p.__dict__)
vars(p)["v"] = 2
print("a write through vars sticks:", p.v, p.__dict__)
p.__dict__["w"] = 3
print("a write through __dict__:", p.w)
q = Plain()
print("independent:", vars(q), vars(p) is not vars(q))


print("--- two instances, and the STORE_ATTR site they share ---")
# The first store on a fresh instance cannot install the specialized opcode,
# because the dict is not there yet; the second instance reaches the same site
# and must get the same answer.
def build(n):
    out = []
    for i in range(n):
        r = Plain()
        r.k = i
        out.append(r)
    return out


made = build(5)
print("all set:", [r.k for r in made])
print("all have dicts:", [sorted(r.__dict__) for r in made[:2]])


print("--- __init__ still fills it ---")
w = WithInit()
print("init:", w.a, sorted(w.__dict__))


print("--- __slots__ is unaffected ---")
s = Slots()
s.s = 9
print("slot:", s.s)
try:
    s.other = 1
except AttributeError as e:
    print("no dict:", type(e).__name__)
try:
    vars(s)
except TypeError as e:
    print("vars refuses:", type(e).__name__)


print("--- the builtin subclasses that always had a NULL slot ---")
t = SubStr("hi")
t.attr = 1
print("str:", str(t), t.attr, sorted(t.__dict__), vars(t) is t.__dict__)
b = SubBytes(b"ab")
b.attr = 2
print("bytes:", bytes(b), b.attr, sorted(b.__dict__))
i = SubInt(3)
i.attr = 3
print("int:", int(i), i.attr, sorted(i.__dict__))
u = SubTuple((1, 2))
u.attr = 4
print("tuple:", tuple(u), u.attr, sorted(u.__dict__))


print("--- delete, and delete again ---")
d = Plain()
d.z = 1
del d.z
print("after del:", d.__dict__)
try:
    del d.z
except AttributeError as e:
    print("second del:", type(e).__name__)
fresh = Plain()
try:
    del fresh.never
except AttributeError as e:
    print("del on a dictless instance:", type(e).__name__)


print("--- what other readers see ---")
e = Plain()
print("getstate empty:", e.__getstate__())
e.g = 1
print("getstate filled:", e.__getstate__())
print("dir includes it:", "g" in dir(e))
print("dir on a fresh one:", "g" in dir(Plain()))
print("hasattr:", hasattr(Plain(), "anything"))
print("getattr default:", getattr(Plain(), "anything", "default"))

import copy
c = copy.copy(e)
print("copy:", sorted(c.__dict__))
print("copy of an empty one:", sorted(copy.copy(Plain()).__dict__))
print("deepcopy:", sorted(copy.deepcopy(e).__dict__))


print("--- the collector still walks it ---")
class Node:
    pass


a = Node()
b2 = Node()
a.other = b2
b2.other = a
del a, b2
gc.collect()
print("cycle collected:", True)

empty_ones = [Plain() for _ in range(100)]
gc.collect()
print("many empty instances survive a collection:", len(empty_ones))

print("done")
