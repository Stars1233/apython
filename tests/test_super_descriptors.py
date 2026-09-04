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


# LOAD_SUPER_ATTR's low bit says whether the compiler wanted a method or a
# value, and the two forms have different stack effects: the method form
# pushes a callable and a receiver, the value form pushes one value.  Pushing
# two either way is invisible in `return super().x` -- the extra word dies
# with the frame -- and shows up as an extra argument as soon as the result is
# used in the middle of an expression.

def two(a, b):
    return "two(%r, %r)" % (a, b)


class D(A):
    prop_base = 10

    @property
    def value(self):
        return 42

    def as_argument(self):
        return two(super().plain, "K")

    def in_a_tuple(self):
        return (super().plain, super().zero, "end")

    def bound_then_called(self):
        f = super().meth
        return f(3) + " via " + str(super().plain)

    def stat_as_value(self):
        f = super().stat
        return f(7)

    def clsm_as_value(self):
        f = super().clsm
        return f(8)

    def arithmetic(self):
        return super().plain + super().zero + 1


class E(D):
    @property
    def value(self):
        return super().value + 1

    def value_as_argument(self):
        return two(super().value, "P")

    def value_in_a_list(self):
        return [super().value, 0]


d = D()
print(d.as_argument())
print(d.in_a_tuple())
print(d.bound_then_called())
print(d.stat_as_value())
print(d.clsm_as_value())
print(d.arithmetic())

e = E()
print(e.value)
print(e.value_as_argument())
print(e.value_in_a_list())
print(sorted([e.value, d.prop_base]))


# A super() lookup that fails, or a getter reached through one that raises,
# has already taken its three operands off the value stack -- and DISPATCH
# saved the stack top as it was before that.  Raising without republishing it
# hands those three slots to the unwinder a second time: `free(): invalid
# pointer` from the first of these, and a use-after-free from the second.

class F(A):
    def missing(self):
        try:
            return super().nosuchattribute
        except AttributeError:
            return "no attribute"

    def missing_mid_expression(self, x, y):
        try:
            return (x, y, super().nosuchattribute)
        except AttributeError:
            return "no attribute, mid expression"


class G(A):
    @property
    def raises(self):
        raise ValueError("from the getter")


class H(G):
    def read(self):
        try:
            return super().raises
        except ValueError as e:
            return "getter raised: " + str(e)

    def read_mid_expression(self, x, y):
        try:
            return (x, y, super().raises)
        except ValueError:
            return "getter raised, mid expression"


f = F()
h = H()
for _ in range(4):
    print(f.missing(), "|", f.missing_mid_expression(1, 2))
    print(h.read(), "|", h.read_mid_expression(3, 4))
print("still alive")
