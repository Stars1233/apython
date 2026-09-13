# COMPARE_OP must not swallow an exception raised by a comparison.
#
# tp_richcompare reports "no opinion" by returning NULL, and a raise arrives
# the same way.  op_compare_op told the two apart by not asking: every
# exception out of a user __eq__ was swallowed, the comparison fell through to
# the identity fallback, and `a == b` answered False while `a == a` answered
# True.  The ordering operators went one worse -- the fallback raises
# TypeError for them, so a KeyError out of __lt__ came back as
# "'<' not supported between instances of 'D' and 'D'".
#
# The swallowed exception was never cleared either, so it resurfaced at
# interpreter shutdown: the program printed its wrong answers, then a
# traceback, then exited 1.
#
# Everything reached through obj_richcompare_bool -- `in`, list.index, the
# container comparisons -- has always been guarded; only the opcode was not.
# Both halves are checked here.


def check(label, fn):
    try:
        print(label, "->", fn())
    except Exception as e:
        print(label, "->", type(e).__name__, e)


class Boom:
    def __eq__(self, other):
        raise ValueError("eq")

    def __ne__(self, other):
        raise ValueError("ne")

    def __lt__(self, other):
        raise ValueError("lt")

    def __gt__(self, other):
        raise ValueError("gt")

    def __le__(self, other):
        raise ValueError("le")

    def __ge__(self, other):
        raise ValueError("ge")

    def __hash__(self):
        return 1


b = Boom()
other = Boom()

check("eq", lambda: b == other)
check("ne", lambda: b != other)
check("lt", lambda: b < other)
check("gt", lambda: b > other)
check("le", lambda: b <= other)
check("ge", lambda: b >= other)

# The identity cases, which the fallback used to answer True/False without ever
# asking whether the call had raised.
check("eq self", lambda: b == b)
check("ne self", lambda: b != b)
check("lt self", lambda: b < b)

# A raise through tp_richcompare rather than through a dunder lookup: an int
# on the left declines, and the reflected call is the one that raises.
check("int eq", lambda: 1 == b)
check("int lt", lambda: 1 < b)
check("str eq", lambda: "x" == b)
check("none eq", lambda: None == b)

# The left operand declines by returning NotImplemented, so the RIGHT one is
# asked and it is the one that raises.
class Decline:
    def __eq__(self, other):
        return NotImplemented

    def __lt__(self, other):
        return NotImplemented

    def __hash__(self):
        return 2


check("decline eq", lambda: Decline() == b)
check("decline lt", lambda: Decline() < b)

# ...and the other way round: the left raises before the right is reached.
check("eq decline", lambda: b == Decline())

# NE derived from __eq__: a class with only __eq__ gets __ne__ from it, and the
# raise has to come out of that derivation too.
class EqOnly:
    def __eq__(self, other):
        raise ValueError("eq only")

    __hash__ = None


check("eqonly ne", lambda: EqOnly() != EqOnly())
check("eqonly eq", lambda: EqOnly() == EqOnly())

# Inside an except block there is already an exception being handled, so the
# test cannot simply be "is anything pending now".
try:
    raise KeyError("handled")
except KeyError:
    check("eq in except", lambda: b == other)
    check("lt in except", lambda: b < other)
    # ...and a comparison that does NOT raise must still answer, rather than
    # reading the handled exception as a raise of its own.
    check("ok in except", lambda: 1 == 1)
    check("ok in except 2", lambda: "a" < "b")

# A comparison whose operand comparison legitimately swallows a StopIteration
# must still answer: list() of an exhausted iterator clears one.
class Swallow:
    def __eq__(self, other):
        list(iter([]))
        return True

    def __hash__(self):
        return 3


try:
    raise KeyError("handled")
except KeyError:
    check("swallow in except", lambda: Swallow() == Swallow())

# The container paths, which were already correct and must stay so.
check("in list", lambda: b in [other])
check("in tuple", lambda: b in (other,))
check("in dict", lambda: b in {other: 1})
check("in set", lambda: b in {other})
check("list eq", lambda: [b] == [other])
check("tuple eq", lambda: (b,) == (other,))
check("index", lambda: [other].index(b))

# tuple had a comparison protocol of its OWN -- a hand-rolled element compare
# that asked the left element's tp_richcompare and nothing else.  It swallowed
# the same way, and it never tried the reflected element either, so an element
# whose left side declined and whose right side answered came out unequal.
class Decl:
    def __eq__(self, other):
        return NotImplemented

    __hash__ = None


class Answer:
    def __eq__(self, other):
        return True

    __hash__ = None


check("tuple reflected elem", lambda: (Decl(),) == (Answer(),))
check("tuple both decline", lambda: (Decl(),) == (Decl(),))
check("tuple nested raise", lambda: ((b,),) == ((other,),))
check("tuple ordering raise", lambda: (b,) < (other,))

# A self-referential __eq__ recurses until the interpreter's own limit, which
# is a RecursionError rather than False.
class Deep:
    def __eq__(self, other):
        return self.me == other.me

    __hash__ = None


p = Deep()
q = Deep()
p.me = p
q.me = q
check("recursive eq", lambda: p == q)
check("recursive eq self", lambda: p == q)

print("done")
