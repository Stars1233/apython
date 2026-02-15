# Test general descriptor protocol (__get__, __set__, __delete__)

# === Data descriptor (has __get__ and __set__) ===
class Validated:
    def __init__(self, name):
        self.name = name

    def __get__(self, obj, objtype=None):
        if obj is None:
            return self
        return getattr(obj, "_" + self.name)

    def __set__(self, obj, value):
        if value < 0:
            raise ValueError("negative")
        setattr(obj, "_" + self.name, value)

class Point:
    x = Validated("x")
    y = Validated("y")

    def __init__(self, x, y):
        self.x = x
        self.y = y

p = Point(3, 7)
print(p.x)
print(p.y)
p.x = 10
print(p.x)

# === Non-data descriptor (only __get__) ===
class LazyAttr:
    def __init__(self, val):
        self.val = val

    def __get__(self, obj, objtype=None):
        return self.val

class Bag:
    answer = LazyAttr(42)

b = Bag()
print(b.answer)

# === Inherited descriptor ===
class Base:
    x = Validated("x")

class Child(Base):
    def __init__(self, x):
        self.x = x

c = Child(99)
print(c.x)
