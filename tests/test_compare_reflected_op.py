# COMPARE_OP holds the comparison operator in ecx, which is caller-saved, and
# the reflected-dunder path overwrote it twice: once with the other operand's
# tag, which is the argument that goes in the same register, and once by the
# call itself.
#
# The identity fallback below it reads ecx to decide whether the pair is
# orderable at all and to name the operator in its refusal.  So an ordering
# between two classes that both decline reported itself as whatever happened
# to be left there -- and when that was EQ or NE, it did not raise at all: the
# comparison silently answered an identity result.  Two of the four cases
# below produced no output whatsoever.
#
# The left-hand path has always saved the op across its own dunder call.


class L:
    def __lt__(self, o):
        return NotImplemented

    def __gt__(self, o):
        return NotImplemented

    def __le__(self, o):
        return NotImplemented

    def __ge__(self, o):
        return NotImplemented


class R:
    pass


def show(fn):
    try:
        print("no raise:", fn())
    except TypeError as e:
        print(e)


show(lambda: L() < R())
show(lambda: R() > L())
show(lambda: L() > R())
show(lambda: R() < L())
show(lambda: L() <= R())
show(lambda: R() >= L())
show(lambda: L() >= R())
show(lambda: R() <= L())

# EQ and NE fall back to identity rather than raising, in both directions.
a, b = L(), R()
print(a == b, b == a, a != b, b != a)
print(a == a, a != a)

# The same shape with a bound method on the right, which is where this was
# found: method's own richcompare declines, then object's __eq__ does.
class C:
    def m(self):
        pass


c = C()
print(c.m == c, c.m != c, c == c.m)
show(lambda: c.m < c)
show(lambda: c < c.m)

# An int against a class that declines: the left side is a static type with a
# tp_richcompare, so this is the other road into the same fallback.
show(lambda: 1 < R())
show(lambda: R() < 1)
print(1 == R(), R() == 1)

# And one that does answer, so the fallback is not reached.
class Ord:
    def __gt__(self, o):
        return "gt"

    def __lt__(self, o):
        return "lt"


print(R() < Ord(), R() > Ord(), Ord() < R(), Ord() > R())
print("done")
