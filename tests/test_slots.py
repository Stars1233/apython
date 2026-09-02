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
