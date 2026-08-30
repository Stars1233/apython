# A metaclass's __prepare__ supplies the mapping the class body executes in.
#
# Ignoring it and handing the body a plain dict is invisible until the body
# does something only the prepared mapping supports.  enum is built on that:
# EnumType.__prepare__ returns an _EnumDict whose __setitem__ records each
# member in _member_names, so with a plain dict every enum class raises
# `'dict' object has no attribute '_member_names'` -- and enum is under re,
# dataclasses, inspect and textwrap.
class Recording(dict):
    def __init__(self):
        super().__init__()
        self.order = []

    def __setitem__(self, key, value):
        self.order.append(key)
        super().__setitem__(key, value)


class Meta(type):
    prepared = []

    @classmethod
    def __prepare__(mcls, name, bases, **kwds):
        Meta.prepared.append((name, tuple(sorted(kwds))))
        return Recording()

    def __new__(mcls, name, bases, ns, **kwds):
        cls = super().__new__(mcls, name, bases, dict(ns))
        cls.order = list(ns.order)
        return cls


class C(metaclass=Meta):
    a = 1
    b = 2

    def m(self):
        return "m"


print(Meta.prepared)
print(C.order)
print(C.a, C.b, C().m())


class D(C, metaclass=Meta, extra="kw"):
    z = 3


print(Meta.prepared[-1])
print(D.order, D.z, D.a)


# A metaclass without __prepare__ still gets a plain dict, and a plain class
# is unaffected.
class Plain(type):
    def __new__(mcls, name, bases, ns):
        return super().__new__(mcls, name, bases, ns)


class E(metaclass=Plain):
    v = 9


print(E.v, type(E).__name__)


class F:
    w = 8


print(F.w, type(F).__name__)

# The prepared mapping is what the body sees, so a name defined and then read
# inside the body goes through it.
class G(metaclass=Meta):
    x = 5
    y = x + 1


print(G.y, G.order)
