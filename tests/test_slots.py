# Test __slots__

# Basic slots
class Point:
    __slots__ = ('x', 'y')

p = Point()
p.x = 10
p.y = 20
print(p.x)
print(p.y)

# Slots with different types
class Mixed:
    __slots__ = ('name', 'value', 'flag')

m = Mixed()
m.name = "hello"
m.value = 42
m.flag = True
print(m.name)
print(m.value)
print(m.flag)

# AttributeError for non-slot attribute
class Restricted:
    __slots__ = ('a',)

r = Restricted()
r.a = 1
try:
    r.b = 2
except AttributeError:
    print("AttributeError raised for non-slot")

# Unset slot raises AttributeError
class Unset:
    __slots__ = ('x',)

u = Unset()
try:
    _ = u.x
except AttributeError:
    print("AttributeError raised for unset slot")

# Overwrite slot value
class Overwrite:
    __slots__ = ('val',)

o = Overwrite()
o.val = "first"
print(o.val)
o.val = "second"
print(o.val)

# Multiple instances have independent slots
class Pair:
    __slots__ = ('a', 'b')

p1 = Pair()
p2 = Pair()
p1.a = 1
p1.b = 2
p2.a = 10
p2.b = 20
print(p1.a, p1.b)
print(p2.a, p2.b)

# Slots with __init__
class WithInit:
    __slots__ = ('x', 'y')
    def __init__(self, x, y):
        self.x = x
        self.y = y
    def sum(self):
        return self.x + self.y

w = WithInit(3, 4)
print(w.sum())

# Slots with methods
class Counter:
    __slots__ = ('count',)
    def __init__(self):
        self.count = 0
    def increment(self):
        self.count = self.count + 1
    def get(self):
        return self.count

c = Counter()
c.increment()
c.increment()
c.increment()
print(c.get())

# object.__getstate__ answered self.__dict__ or None, and a __slots__ class
# has no instance dict -- so every one of them answered None and lost every
# slot it had.  CPython 3.11+ gives a two-tuple, (None, {name: value}).
#
# There is no name-carrying slot walk anywhere else: instance_traverse and
# instance_dealloc walk the same words by OFFSET, which is all they need.
# The names come from where they are actually recorded -- the member
# descriptors in each type's dict, along the MRO, most derived first.


class Plain:
    pass


class One:
    __slots__ = ('a', 'b')


class NoSlots:
    __slots__ = ()


class Derived(One):
    __slots__ = ('c',)


p = Plain()
print(p.__getstate__())
p.q = 1
print(p.__getstate__())

print(One().__getstate__())          # nothing assigned yet: None
o = One()
o.a = 1
print(o.__getstate__())
o.b = 2
print(o.__getstate__())

print(NoSlots().__getstate__())

d = Derived()
d.a = 1
d.c = 3
print(d.__getstate__())
d2 = Derived()
d2.c = 9
print(d2.__getstate__())

# The values are the objects themselves, not copies.
marker = [1, 2]
o2 = One()
o2.a = marker
state = o2.__getstate__()
print(state[0], state[1]['a'] is marker, sorted(state[1]))

# Deleting a slot takes it back out of the state.
o3 = One()
o3.a = 1
o3.b = 2
del o3.a
print(o3.__getstate__())

# It is still a method on every object, and still takes no arguments.
try:
    o.__getstate__(1)
except TypeError:
    print("TypeError")
print(object().__getstate__())

# __slots__ suppressed the instance dict unconditionally.  Python's rule is
# that it suppresses one only when NO BASE already provides it -- `class C(A)`
# with a plain A inherits A's __dict__, so `c.z = 1` is ordinary and was an
# AttributeError here.  TYPE_FLAG_HAS_SLOTS is what four sites read as "this
# class has no dict": instance_new, instance_setattr's fallback,
# obj_generic_attr's __dict__ arm and vars().
#
# The slots are unaffected either way -- they are laid out past the header,
# and instance_dealloc's and instance_traverse's walks derive the header end
# from tp_dictoffset, which is untouched.
#
# This is also what makes object.__getstate__'s (dict, slots) pair form
# reachable; it could only ever answer (None, {...}) before.


class NoBase:
    pass


class WithDict(NoBase):
    __slots__ = ("y",)


class PureSlots:
    __slots__ = ("a",)


class EmptySlotsOnDict(NoBase):
    __slots__ = ()


class EmptySlotsPure:
    __slots__ = ()


class SlotsOnSlots(PureSlots):
    __slots__ = ("b",)


def show(label, fn):
    try:
        return "%s => %r" % (label, fn())
    except BaseException as e:
        return "%s !! %s: %s" % (label, type(e).__name__, e)


# A base with a dict keeps it.
w = WithDict()
w.y = 5
w.z = 6
print(w.y, w.z, sorted(vars(w)), w.__getstate__())

e = EmptySlotsOnDict()
e.anything = 1
print(vars(e), show("getstate", e.__getstate__))

# A base without one still refuses.
p = PureSlots()
p.a = 1
print(show("pure setattr", lambda: setattr(p, "zz", 1)))
print(show("pure vars", lambda: vars(p)))
print(p.__getstate__())

ep = EmptySlotsPure()
print(show("empty pure setattr", lambda: setattr(ep, "zz", 1)))
print(show("empty pure getstate", ep.__getstate__))

# Slots on top of slots: still no dict, and both levels' slots are present.
ss = SlotsOnSlots()
ss.a = 1
ss.b = 2
print(ss.a, ss.b, show("setattr", lambda: setattr(ss, "zz", 1)))
print(ss.__getstate__())

# A subclass of a __slots__ class that declares none regains a dict, which is
# the behaviour that was already right and must stay.
class Regains(PureSlots):
    pass


r = Regains()
r.a = 1
r.q = 2
print(r.a, r.q, sorted(vars(r)), r.__getstate__())

# Deleting and re-setting through both storage kinds.
w2 = WithDict()
w2.y = 1
w2.z = 2
del w2.z
print(show("deleted dict attr", lambda: w2.z))
del w2.y
print(show("deleted slot", lambda: w2.y))
w2.y = 3
print(w2.y, w2.__getstate__())

# `del obj.attr` stored a NULL into the instance dict instead of removing the
# key.  instance_setattr's dict fallback called dict_set either way, and a
# NULL value means DELETE -- so vars(obj) could not be repr'd, len(vars(obj))
# still counted the deleted name, and deleting a second time succeeded.
# Nothing to do with __slots__; found while testing it.
class Plain:
    pass


d = Plain()
d.z = 2
d.k = 1
del d.z
print(sorted(vars(d)), len(vars(d)), vars(d))
print(show("read deleted", lambda: d.z))
print(show("hasattr", lambda: hasattr(d, "z")))
print(show("delete twice", lambda: delattr(d, "z")))
print(show("delete never-set", lambda: delattr(d, "never")))
d.z = 9
print(d.z, d.k, sorted(vars(d)))
