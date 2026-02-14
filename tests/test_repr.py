# Test container repr and class variables
print([1, 2, 3])
print([])
print((1, 2, 3))
print((42,))
print(())
print({'a': 1, 'b': 2})
print({})
print([1, [2, 3], 4])
print((1, (2,), 3))

# Class variable access
x = 10
class Foo:
    y = 42
    z = x
print(Foo.y)
print(Foo.z)

# Class with annotations
class Point:
    x: int
    y: int
    def __init__(self, x, y):
        self.x = x
        self.y = y

p = Point(3, 4)
print(p.x)
print(p.y)
