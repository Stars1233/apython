# The set comparison operators accept a subclass on either side.
#
# set_richcompare asked whether the right operand's type WAS set_type or
# frozenset_type, by pointer.  A subclass of either answered no, the slot
# returned NotImplemented, and `S() <= S()` for `class S(set)` came out as
# "'<=' not supported between instances of 'S' and 'S'".
#
# Both static types carry TYPE_FLAG_SET_SUBCLASS and every subclass inherits
# it, so the flag is exactly the question being asked.

class S(set):
    pass


class F(frozenset):
    pass


class S2(S):
    pass


a = S([1, 2])
b = S([1, 2, 3])
plain = {1, 2, 3}

print(a <= b, a < b, b >= a, b > a, a == b, a != b)
print(b <= a, b < a, a >= b, a > b)
print(a <= plain, plain >= a, plain <= a, a == plain, plain == a)
print(a <= S([1, 2]), a >= S([1, 2]), a == S([1, 2]), a != S([1, 2]))
print(a <= S2([1, 2, 3]), S2([1]) < a)

c, d = F([1]), F([1, 2])
print(c <= d, c < d, c == d, c != d, d > c)
print(c <= frozenset([1, 2]), frozenset([1]) <= d)
print(c <= {1, 2}, {1} <= d, c == {1}, {1} == c)
print(F([1, 2]) == S([1, 2]), S([1, 2]) == F([1, 2]))

# Disjoint sets are unordered, and every comparison says so.
e, f = S([1]), S([2])
print(e <= f, e < f, e >= f, e > f, e == f, e != f)

# A set never compares to something that is not one.
for other in ([1, 2], (1, 2), "12", 3, None, {1: 2}):
    try:
        a <= other
    except TypeError as t:
        print("TypeError", type(other).__name__)
    else:
        print("no TypeError", type(other).__name__)
    print(a == other, a != other)

# The set methods take a subclass too.
print(a.issubset(b), b.issuperset(a), a.isdisjoint(f))
print(sorted(a & b), sorted(a | f), sorted(b - a), sorted(a ^ b))
print(type(a & b).__name__, type(a | b).__name__)
print(sorted(a.union(b)), sorted(b.intersection(a)), sorted(b.difference(a)))

# A subclass that defines its own __eq__ still wins.
class Eq(set):
    def __eq__(self, other):
        return "mine"

    def __hash__(self):
        return 1


print(Eq([1]) == S([1]), Eq([1]) == {1})
print("done")
