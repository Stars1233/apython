# A class with no bases inherits object, however it is built.  `type(n, (), d)`
# already substituted it; the metaclass paths did not, so `class E(metaclass=M)`
# got an MRO of just [E].  That is invisible until something merges it: enum's
# StrEnum(str, ReprEnum) linearised to [StrEnum, str, object, ReprEnum, Enum]
# because ReprEnum's own MRO had lost the object that anchors it last.
class M(type):
    pass

class V(metaclass=M):
    pass

print(V.__bases__, [c.__name__ for c in V.__mro__])
print(isinstance(V(), object), issubclass(V, object))

Z = type.__new__(M, 'Z', (), {})
print(Z.__bases__, [c.__name__ for c in Z.__mro__])

W = type('W', (), {})
print(W.__bases__, [c.__name__ for c in W.__mro__])

class U(Z):
    pass

print([c.__name__ for c in U.__mro__])

# The shape that found it: a mixin whose MRO must end at object for the merge
# to put it after the builtin base.
class Rep(V):
    pass

class SE(str, Rep):
    pass

print([c.__name__ for c in SE.__mro__])
print(SE("hi").upper(), isinstance(SE("hi"), V))

# object's own methods have to be reachable through the substituted base.
print(V().__class__.__name__, repr(Z()) is not None)
print(V.__init__ is object.__init__ or V.__init__ is not None)
