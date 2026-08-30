# super() must resolve descriptors the same way attribute access does:
# a staticmethod is called unbound, a classmethod binds to the derived
# class, an ordinary method binds to self, and a plain value comes back
# as itself.

class A:
    plain = 7
    zero = 0
    def __new__(cls, *a, **k):
        return super().__new__(cls)
    def __init__(self):
        self.a = 1
    def meth(self, n):
        return "A.meth " + str(n)
    @staticmethod
    def stat(x):
        return "A.stat " + str(x)
    @classmethod
    def clsm(cls, x):
        return "A.clsm " + cls.__name__ + " " + str(x)


class B(A):
    def __new__(cls, *a, **k):
        return super().__new__(cls)
    def __init__(self):
        super().__init__()
        self.b = 2
    def meth(self, n):
        return "B->" + super().meth(n)
    def use_stat(self):
        return super().stat(5)
    def use_clsm(self):
        return super().clsm(6)
    def use_plain(self):
        return super().plain
    def use_zero(self):
        return super().zero
    @classmethod
    def from_cls(cls):
        return super().clsm(9)


class C(B):
    pass


b = B()
print(b.meth(1))
print(b.a, b.b)
print(b.use_stat())
print(b.use_clsm())
print(b.use_plain(), b.use_zero())
print(type(b).__name__)

# The classmethod binds to the derived class, not the defining one
c = C()
print(c.use_clsm())
print(C.from_cls())

# Bound results are reusable as first-class objects
f = b.meth
print(f(2))

# super() inside __new__ produces an instance of the most derived class
print(type(C()).__name__, isinstance(C(), A))
