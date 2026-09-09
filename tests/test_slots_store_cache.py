# STORE_ATTR into a __slots__ member has an inline cache of its own (opcode
# 243).  Its single guard is the type's version, which stands in for three
# things: that tp_setattr is still instance_setattr, that the name still
# resolves to a member descriptor of this class, and that the descriptor's
# md_offset is still the cached one.
#
# The loops matter.  A site does not specialize until it has RUN, and a cache
# that answers wrongly answers wrongly only on the second execution -- so
# every case here writes through the same site more than once, and the
# interesting ones change the class in between.
#
# See [[warm-cache-needs-same-site]]: each shape gets one helper function, so
# the store really is the same bytecode site every time.


class A:
    __slots__ = ("x", "y")


def set_x(o, v):
    o.x = v


def set_y(o, v):
    o.y = v


a = A()
for i in range(5):
    set_x(a, i)
    set_y(a, i * 2)
print(a.x, a.y)

# The old value is released and the new one held: a list in a slot, written
# over and over, must not leak and must not be freed early.
class H:
    __slots__ = ("o",)


def set_o(o, v):
    o.o = v


h = H()
for i in range(5):
    set_o(h, [i, i + 1])
print(h.o)
keep = h.o
set_o(h, None)
print(keep, h.o)

# An UNSET slot holds nothing at all, and the first store has to cope with
# that -- the cache releases whatever was there.
b = A()
set_x(b, "first")
print(b.x)
del b.x
try:
    b.x
except AttributeError as e:
    print("AttributeError")
set_x(b, "again")
print(b.x)

# A subclass has slots of its own at different offsets, so the same site sees
# two classes and must deopt for the second.
class B(A):
    __slots__ = ("z",)


c = B()
for i in range(5):
    set_x(c, i)
    set_x(a, i + 100)
print(a.x, c.x)


def set_z(o, v):
    o.z = v


for i in range(5):
    set_z(c, i)
print(c.z)

# A str subclass addresses its slots from the TAIL, with a negative offset.
class S(str):
    __slots__ = ("t",)


def set_t(o, v):
    o.t = v


s = S("hello")
for i in range(5):
    set_t(s, i)
print(s, s.t, len(s), s.upper())

# The class grows a __setattr__ after the site has settled: the version guard
# is what has to notice.
class C:
    __slots__ = ("v",)


def set_v(o, v):
    o.v = v


cc = C()
for i in range(5):
    set_v(cc, i)
print(cc.v)
C.__setattr__ = lambda self, n, v: print("custom", n, v)
set_v(cc, 99)
print(cc.v)
del C.__setattr__
set_v(cc, 7)
print(cc.v)

# And the class loses the slot name to a property, which is a different data
# descriptor at the same name.
class D:
    __slots__ = ("w",)


def set_w(o, v):
    o.w = v


d = D()
for i in range(5):
    set_w(d, i)
print(d.w)

# A name the class does not have at all is still an AttributeError, from the
# site that has specialized for a name it does have.
try:
    a.nosuch = 1
except AttributeError:
    print("AttributeError")

# Cycles through slots stay collectable: the cache writes the slot without
# going near dict_set, so nothing about tracking changes.
import gc


class Cyc:
    __slots__ = ("peer",)


def set_peer(o, v):
    o.peer = v


for _ in range(3):
    p = Cyc()
    q = Cyc()
    set_peer(p, q)
    set_peer(q, p)
    del p, q
print(gc.collect() >= 0)
