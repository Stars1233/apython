# object.__eq__ defers to the type's tp_richcompare so that a builtin answers
# by contents.  For a *user* class that slot is the generic dispatcher, which
# looks __eq__ up again and finds object's -- an unbounded recursion.  Any
# class defining a comparison other than __eq__ segfaulted on == and on !=.
class OnlyLt:
    def __lt__(self, other):
        return True


a, b = OnlyLt(), OnlyLt()
print(a == b, a != b, a == a, a != a)
print(a < b)


class OnlyGt:
    def __gt__(self, other):
        return False


print(OnlyGt() == OnlyGt(), OnlyGt() != OnlyGt())


# A class that does define __eq__ still wins.
class Eq:
    def __init__(self, v):
        self.v = v

    def __eq__(self, other):
        return self.v == other.v

    def __lt__(self, other):
        return self.v < other.v


print(Eq(1) == Eq(1), Eq(1) == Eq(2), Eq(1) != Eq(2), Eq(1) < Eq(2))

# A class with no comparisons at all falls back to identity.
class Bare:
    pass


x = Bare()
print(x == x, x == Bare(), x != Bare())

# The builtin behaviour this delegation was added for is unchanged.
p, q = (1, 2), (1, 2)
print(p.__eq__(q), [1].__eq__([1]), {"k": 1}.__eq__({"k": 1}), "ab".__eq__("ab"))
print(object.__eq__(x, x), object.__eq__(x, Bare()))

# Inheritance: the base's comparison is what the subclass uses.
class SubLt(OnlyLt):
    pass


print(SubLt() == SubLt(), SubLt() != SubLt())
