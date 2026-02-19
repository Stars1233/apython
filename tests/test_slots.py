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
