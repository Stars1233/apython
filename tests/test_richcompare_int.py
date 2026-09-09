# obj_richcompare_bool is what every container search asks -- `in`, index,
# count, remove, ==, min, max and the sort's general comparator -- and it now
# answers two int IMMEDIATES itself, from one unsigned compare of the two
# Values, because the encoding is monotonic.
#
# What has to keep working is everything the arm does NOT cover, and the
# boundaries of what it does:
#   - the immediate range is +-2^50.  One value inside it and one outside is
#     two different encodings, and only the general path can compare them.
#   - a bool is a heap singleton, not an immediate, though True == 1.
#   - a float equal to an int is equal across two encodings.
#   - a class with __eq__ or __lt__ must still have it called, and a subclass
#     of int must still get its own operator.
#   - all six operators, and the reflected forms.
#
# The containers are here as well as the bare operators, because the arm is
# reached through them and not through COMPARE_OP.

LIM = 2 ** 50
BIG = 2 ** 60


def ops(a, b):
    return [a < b, a <= b, a == b, a != b, a > b, a >= b]


# --- the operators, over the whole immediate range ------------------------
for a, b in ((0, 0), (0, 1), (1, 0), (-1, 1), (1, -1), (-5, -5), (-5, -4),
             (LIM - 1, LIM - 2), (-LIM + 1, -LIM + 2), (LIM - 1, -LIM + 1),
             (0, LIM - 1), (0, -LIM + 1), (7, 7)):
    print(a, b, ops(a, b))

# one inside the immediate range and one outside it
for a, b in ((0, BIG), (BIG, 0), (-BIG, 0), (LIM - 1, LIM + 1),
             (LIM + 1, LIM - 1), (BIG, BIG), (BIG, BIG + 1), (-BIG, BIG)):
    print(ops(a, b))

# --- the encodings the arm must not settle --------------------------------
print(ops(True, 1), ops(False, 0), ops(True, 0), ops(True, False))
print(ops(1.0, 1), ops(1, 1.0), ops(0.5, 1), ops(-0.5, 0))
print(ops(1, 1.5), ops(2 ** 60, 2.0 ** 60))


class Sub(int):
    def __lt__(self, o):
        return "sub-lt"

    def __eq__(self, o):
        return "sub-eq"

    def __hash__(self):
        return 0


print(Sub(3) < 4, Sub(3) == 3, 4 > Sub(3))


class E:
    def __init__(self, v):
        self.v = v

    def __eq__(self, o):
        return isinstance(o, E) and self.v == o.v

    def __lt__(self, o):
        return self.v < o.v

    def __repr__(self):
        return "E%d" % self.v


print(E(1) == E(1), E(1) == E(2), E(1) < E(2), E(2) < E(1))

# --- through the containers, which is how the arm is reached --------------
ints = list(range(20))
print(7 in ints, 20 in ints, -1 in ints, ints.index(7), ints.count(7))
print(min(ints), max(ints), min(ints, key=lambda x: -x))
mixed = [1, True, 1.0, 0, False, 0.0, BIG, -BIG]
for probe in (1, True, 1.0, 0, False, 0.0, BIG, -BIG, 2, 2 ** 51):
    print(repr(probe), probe in mixed, mixed.count(probe))
print([1, 2, 3] == [1, 2, 3], [1, 2] < [1, 2, 3], [2] > [1, 9])
print((1, 2) == (1, 2), (1, 2) < (1, 3), {1, 2} == {2, 1})
print(sorted([5, -3, 0, LIM - 1, -LIM + 1, 2]))
print(sorted([BIG, 1, -BIG, 0]))
print(min([3, 1, 2]), max([3, 1, 2]), min(3, 1, 2), max(3, 1, 2))
print(min([BIG, 1]), max([BIG, 1]), min([-BIG, 1]), max([-BIG, 1]))

d = {1: "a", BIG: "b", True: "c"}
print(sorted(d.items(), key=lambda kv: str(kv[0])))
print(1 in d, BIG in d, 2 in d)


class Raises:
    def __eq__(self, o):
        raise ZeroDivisionError("eq")


try:
    print(Raises() in [1, 2, 3])
except ZeroDivisionError as e:
    print("ZeroDivisionError", e)
try:
    print(1 in [Raises()])
except ZeroDivisionError as e:
    print("ZeroDivisionError", e)
