# object.__lt__, __le__, __gt__ and __ge__ exist, and answer NotImplemented.
#
# They were left out because a builtin subclass looks __lt__ up in its MRO and
# would find object's NotImplemented before reaching the base type's own
# comparison -- so `sorted([L([2]), L([1])])` on a list subclass would stop
# sorting by contents.  slot_is_object_default and object_default_impls
# already solve exactly that for __eq__, __ne__ and __hash__; the four just
# had to join them.

print(object.__lt__(1, 2), object.__le__(1, 2))
print(object.__gt__(1, 2), object.__ge__(1, 2))
print(object().__lt__(object()))

for n in ("__lt__", "__le__", "__gt__", "__ge__", "__eq__", "__ne__"):
    print(n, n in dir(object), hasattr(object, n))

# The reason they were withheld: a builtin subclass must keep its base's
# comparison, not pick up object's NotImplemented.
class L(list):
    pass
print(sorted([L([2]), L([1])]))
print(L([1]) < L([2]), L([2]) < L([1]), L([1]) == L([1]))

class S(str):
    pass
print(sorted([S("b"), S("a")]), S("a") < S("b"), S("a") == "a")

class T(tuple):
    pass
print(sorted([T((2,)), T((1,))]), T((1,)) < T((2,)), T((1,)) == (1,))

class I(int):
    pass
print(sorted([I(2), I(1)]), I(1) < I(2), I(1) == 1, I(1) <= I(1))

class B(bytes):
    pass
print(sorted([B(b"b"), B(b"a")]), B(b"a") < B(b"b"))

# A plain class still gets a TypeError for an ordering it does not define,
# because NotImplemented from both sides is what produces one.
class P:
    pass
try:
    P() < P()
    print("no error")
except TypeError:
    print("TypeError")

# A class that defines one ordering keeps it, and the reflected form works.
class Q:
    def __init__(self, v):
        self.v = v
    def __lt__(self, other):
        return self.v < other.v
print(Q(1) < Q(2), Q(2) < Q(1), Q(1) > Q(2))

# functools.total_ordering-style delegation: calling up to object is legal.
class R:
    def __lt__(self, other):
        return object.__lt__(self, other)
print(R().__lt__(R()))
