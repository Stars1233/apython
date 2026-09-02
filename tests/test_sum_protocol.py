# sum(), min() and max() go through the numeric and comparison protocols.
#
# All three used to hardcode a two-type ladder.  sum() picked between int_add
# and float_add on the operands' tags and never tested the result, so a slot
# that declined -- which is how every slot in the tree says "not my types" --
# left a NULL Value as the accumulator.  NULL is not an error the loop noticed:
# it was added to, DECREFed and finally returned, and the failure surfaced
# wherever the caller next touched it, as an unrelated TypeError.
#
# min()/max() had the mirror of it on the comparison side.  A declining
# tp_richcompare returns NULL, and `cmp result, bool_true` is false for NULL,
# so all three of "the ladder does not know this type", "the slot declined"
# and "the comparison raised" meant the incumbent keeps.  max([1j, 2j])
# answered 1j, and a raising __lt__ was swallowed -- while sorted() over the
# same values was already correct, because list.sort had been taught to tell
# a NULL apart from a False.
#
# The messages are not compared: the tree's own wording for an unorderable
# pair carries no operand names, so only the exception type is printed.

def check(label, fn):
    try:
        print("%-30s %r" % (label, fn()))
    except BaseException as e:
        print("%-30s %s" % (label, type(e).__name__))


class Adder:
    """A user class whose only addition is __add__ -- no nb_add slot exists
    for it, so sum() has to reach the dunder arm of the protocol."""

    def __init__(self, v):
        self.v = v

    def __add__(self, other):
        return Adder(self.v + (other.v if isinstance(other, Adder) else other))

    def __radd__(self, other):
        return Adder(self.v + (other.v if isinstance(other, Adder) else other))

    def __repr__(self):
        return "Adder(%d)" % self.v


class Ordered:
    def __init__(self, v):
        self.v = v

    def __lt__(self, other):
        return self.v < other.v

    def __gt__(self, other):
        return self.v > other.v

    def __repr__(self):
        return "Ordered(%d)" % self.v


class Raiser:
    def __lt__(self, other):
        raise RuntimeError("comparison raised")

    def __gt__(self, other):
        raise RuntimeError("comparison raised")


def raising_gen():
    yield 1
    raise ValueError("iterator raised")


print("--- sum: the ordinary cases still work ---")
check("sum([1, 2, 3])", lambda: sum([1, 2, 3]))
check("sum([1, 2, 3], 10)", lambda: sum([1, 2, 3], 10))
check("sum([])", lambda: sum([]))
check("sum([1.5, 2.5])", lambda: sum([1.5, 2.5]))
check("sum([1, 2.5])", lambda: sum([1, 2.5]))
check("sum([True, True, False])", lambda: sum([True, True, False]))
check("sum(range(101))", lambda: sum(range(101)))
check("sum(x * x for x in range(5))", lambda: sum(x * x for x in range(5)))

print()
print("--- sum: past the int/float ladder ---")
# Each of these left a NULL Value as the accumulator.
check("sum([1j, 2j])", lambda: sum([1j, 2j]))
check("sum([1j, 2j], 3j)", lambda: sum([1j, 2j], 3j))
check("sum([1, 2j])", lambda: sum([1, 2j]))
check("sum([Adder(1), Adder(2)])", lambda: sum([Adder(1), Adder(2)]))
check("sum([Adder(1)], Adder(10))", lambda: sum([Adder(1)], Adder(10)))
# The reflected half: the accumulator starts as the int 0, so the first
# addition is 0 + Adder(1) and only __radd__ can answer it.
check("sum([Adder(4), Adder(5)]) r", lambda: sum([Adder(4), Adder(5)]).v)

print()
print("--- sum: heap ints, which no immediate path covers ---")
check("sum([10**30, 10**30])", lambda: sum([10**30, 10**30]))
check("sum([2**62, 2**62])", lambda: sum([2**62, 2**62]))

print()
print("--- sum: the sequence fallback ---")
check("sum([[1], [2]], [])", lambda: sum([[1], [2]], []))
check("sum([(1,), (2,)], ())", lambda: sum([(1,), (2,)], ()))

print()
print("--- sum: the refusals ---")
check("sum(['a', 'b'], '')", lambda: sum(['a', 'b'], ''))
check("sum([b'a'], b'')", lambda: sum([b'a'], b''))
check("sum([1, 'a'])", lambda: sum([1, 'a']))
check("sum(5)", lambda: sum(5))
check("sum()", lambda: sum())
check("sum([1], 2, 3)", lambda: sum([1], 2, 3))
# An exception raised inside the iterator must propagate, not end the sum.
check("sum(raising_gen())", lambda: sum(raising_gen()))

print()
print("--- min/max: the ordinary cases still work ---")
check("min(3, 1, 2)", lambda: min(3, 1, 2))
check("max(3, 1, 2)", lambda: max(3, 1, 2))
check("min([3, 1, 2])", lambda: min([3, 1, 2]))
check("max([3, 1, 2])", lambda: max([3, 1, 2]))
check("min('b', 'a', 'c')", lambda: min('b', 'a', 'c'))
check("max(['b', 'a', 'c'])", lambda: max(['b', 'a', 'c']))
check("min([1.5, 0.5])", lambda: min([1.5, 0.5]))
check("min([1, 0.5])", lambda: min([1, 0.5]))
check("max([True, False])", lambda: max([True, False]))
check("min(range(10, 0, -1))", lambda: min(range(10, 0, -1)))
check("max([10**30, 10**31, 10**29])", lambda: max([10**30, 10**31, 10**29]))
check("max([(1, 2), (1, 3)])", lambda: max([(1, 2), (1, 3)]))
check("min([[2], [1]])", lambda: min([[2], [1]]))
check("min([Ordered(3), Ordered(1)])", lambda: min([Ordered(3), Ordered(1)]))
check("max(Ordered(3), Ordered(1))", lambda: max(Ordered(3), Ordered(1)))

print()
print("--- min/max: a declining slot is not an answer ---")
# Each of these used to return one of its arguments.
check("min([2j, 1j])", lambda: min([2j, 1j]))
check("max([1j, 2j])", lambda: max([1j, 2j]))
check("min(1j, 2j)", lambda: min(1j, 2j))
check("max(1j, 2j)", lambda: max(1j, 2j))
check("min([1, 'a'])", lambda: min([1, 'a']))
check("max('a', 1)", lambda: max('a', 1))
check("min([None, None])", lambda: min([None, None]))

print()
print("--- min/max: a raising comparison propagates ---")
check("min([Raiser(), Raiser()])", lambda: min([Raiser(), Raiser()]))
check("max(Raiser(), Raiser())", lambda: max(Raiser(), Raiser()))
check("min(raising_gen())", lambda: min(raising_gen()))

print()
print("--- min/max: the refusals ---")
check("min([])", lambda: min([]))
check("max([])", lambda: max([]))
check("min(5)", lambda: min(5))
check("min()", lambda: min())

print()
print("--- the incumbent is held, not borrowed ---")
# The winner comes out of the iterator, whose reference the loop takes over.
# Churning the allocator afterwards would reuse it if the count were short.
best = max([[i] for i in range(50)])
print("best  :", best)
print("churn :", len([[i, i] for i in range(3000)]))
print("best  :", best)

total = sum([Adder(i) for i in range(10)], Adder(0))
print("total :", total)
print("churn :", len([[i, i] for i in range(3000)]))
print("total :", total)
