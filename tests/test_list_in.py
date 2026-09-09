# `x in list` answers by identity and by int-immediate inequality before it
# reaches obj_richcompare_bool.  Both arms have to agree with the general path
# they skip, and the ones that can tell them apart are here:
#
#   - a bool is a heap singleton, not an immediate, so `1 in [True]` and
#     `True in [1]` must still go the long way and must still be True;
#   - a heap integer holds a value outside the immediate range, so it never
#     matches an immediate by identity and must be compared properly;
#   - a float that equals an int compares equal across the two encodings;
#   - a class with its own __eq__ must still have it called, and must not be
#     answered by the int arm.
#
# The loops matter for nothing here -- there is no specialization to warm --
# but the MISS half does: it is the half the identity arm cannot settle, and
# it is every element of the list when the answer is False.


class E:
    def __init__(self, v):
        self.v = v

    def __eq__(self, o):
        return isinstance(o, E) and self.v == o.v


ints = list(range(20))
print([(i % 40) in ints for i in range(0, 60, 7)])
print(ints.index(19), ints.count(7), 20 in ints, -1 in ints)

# across the encodings
print(1 in [True], True in [1], 0 in [False], False in [0])
print(1 in [1.0], 1.0 in [1], 2 in [2.0, 3], 2.5 in [2, 3])

# heap integers: outside the immediate range, so never an immediate match
BIG = 2 ** 60
print(BIG in [BIG], BIG in [BIG + 1], -BIG in [BIG], BIG in [1, 2, BIG])
print((10 ** 30) in [10 ** 30], (10 ** 30) in [10 ** 30 + 1])

# a mixed list, so the fast arms and the slow one interleave
L = [1, "a", 2.0, None, (1, 2), BIG, E(1), True]
for name, x in (("1", 1), ("2", 2), ("a", "a"), ("b", "b"), ("2.0", 2.0),
                ("None", None), ("(1,2)", (1, 2)), ("(1,3)", (1, 3)),
                ("BIG", BIG), ("E(1)", E(1)), ("E(9)", E(9)),
                ("False", False)):
    print(name, x in L, L.count(x))

# __eq__ still runs, and still on the object the list holds
class Loud:
    def __eq__(self, o):
        print("  Loud.__eq__")
        return True


print(Loud() in [1, 2, 3])
print(1 in [Loud(), 2])

# an empty list and a one-element list, the loop's own edges
print(1 in [], 1 in [1], 1 in [2])
