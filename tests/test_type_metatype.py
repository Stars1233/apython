# type(name, bases, ns) builds the class the *bases* call for: the metatype is
# the most derived of the bases' metatypes, not `type`.  Calling type_from_parts
# directly ignored both that and any class keywords, so enum's _simple_enum
# decorator -- which is written as type(name, (etype,), body, boundary=..., ) --
# reported "type.__new__() takes at least 3 arguments".
class Meta(type):
    def __new__(mcls, name, bases, ns, **kw):
        cls = super().__new__(mcls, name, bases, ns)
        cls.made_by = mcls.__name__
        cls.kw = sorted(kw.items())
        return cls

    def __init__(cls, name, bases, ns, **kw):
        super().__init__(name, bases, ns)


class Base(metaclass=Meta):
    pass


print(type(Base).__name__, Base.made_by, Base.kw)

C = type('C', (Base,), {'x': 1})
print(type(C).__name__, C.made_by, C.kw, C.x)

D = type('D', (Base,), {'y': 2}, flag=True, n=3)
print(type(D).__name__, D.made_by, D.kw, D.y)

# Plain type() is unchanged.
E = type('E', (), {'z': 4})
print(type(E).__name__, E.z, E.__bases__)
print(type(1).__name__, type('s').__name__, type(E()).__name__)

# The winner is the most derived when several metatypes are in play.
class Sub(Meta):
    pass


class B2(metaclass=Sub):
    pass


F = type('F', (Base, B2), {})
print(type(F).__name__, F.made_by)

# type.__new__ itself has to tolerate the keywords it is handed.
G = type.__new__(Meta, 'G', (Base,), {'w': 5})
print(type(G).__name__, G.w)

# And a subclass built this way behaves like one written out.
inst = C()
print(isinstance(inst, Base), isinstance(inst, object), C.__name__)
