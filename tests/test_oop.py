# Phase 8: OOP completeness tests

# === staticmethod ===
print("--- staticmethod ---")

class Calc:
    @staticmethod
    def add(x, y):
        return x + y

    @staticmethod
    def zero():
        return 0

print(Calc.add(1, 2))
c = Calc()
print(c.add(10, 20))
print(Calc.zero())
print(c.zero())

# === classmethod ===
print("--- classmethod ---")

class Greeter:
    @classmethod
    def name(cls):
        return cls.__name__

print(Greeter.name())
g = Greeter()
print(g.name())

# classmethod with inheritance
class Base:
    @classmethod
    def kind(cls):
        return cls.__name__

class Child(Base):
    pass

print(Base.kind())
print(Child.kind())
ch = Child()
print(ch.kind())
