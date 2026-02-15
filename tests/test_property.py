# Test property descriptor

class Circle:
    def __init__(self, radius):
        self._radius = radius

    @property
    def radius(self):
        return self._radius

    @radius.setter
    def radius(self, value):
        if value < 0:
            raise ValueError("negative radius")
        self._radius = value

    @property
    def area(self):
        return 3 * self._radius * self._radius

c = Circle(5)
print(c.radius)
print(c.area)

c.radius = 10
print(c.radius)
print(c.area)

# Test property with no setter (read-only)
class ReadOnly:
    @property
    def x(self):
        return 42

r = ReadOnly()
print(r.x)

# Test property inherited
class Shape:
    def __init__(self, name):
        self._name = name

    @property
    def name(self):
        return self._name

class Square(Shape):
    def __init__(self, side):
        super().__init__("square")
        self._side = side

    @property
    def area(self):
        return self._side * self._side

s = Square(4)
print(s.name)
print(s.area)
