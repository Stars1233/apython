# Test class inheritance and super()

# Basic inheritance - method lookup
class A:
    def greet(self):
        return "hello"

class B(A):
    pass

b = B()
print(b.greet())

# Inherited __init__
class Base:
    def __init__(self, val):
        self.val = val

class Child(Base):
    pass

c = Child(10)
print(c.val)

# super().__init__()
class Animal:
    def __init__(self, name):
        self.name = name

class Dog(Animal):
    def __init__(self, name, breed):
        super().__init__(name)
        self.breed = breed

d = Dog("Rex", "Lab")
print(d.name)
print(d.breed)

# super().method()
class X:
    def foo(self):
        return "X.foo"

class Y(X):
    def foo(self):
        return super().foo() + "+Y.foo"

y = Y()
print(y.foo())

# isinstance with inheritance
print(isinstance(d, Dog))
print(isinstance(d, Animal))
print(isinstance(b, A))
print(isinstance(b, B))

# Three-level inheritance
class Top:
    def who(self):
        return "top"

class Mid(Top):
    pass

class Bot(Mid):
    pass

bot = Bot()
print(bot.who())

# Override at each level
class L1:
    def val(self):
        return 1

class L2(L1):
    def val(self):
        return 2

class L3(L2):
    pass

print(L3().val())
print(L2().val())
print(L1().val())
