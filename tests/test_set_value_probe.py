# set's probe compares the stored key against the lookup key as a VALUE.  The
# encoding is a bijection, so equal Values are the same key and two equal
# small ints have bit-identical words -- but the converse does not hold, and
# every case where two DIFFERENT Values are still equal keys has to reach
# obj_richcompare_bool.  Those are the cases here:
#
#   1.0 and 1          a float immediate and an int immediate
#   True and 1         a heap singleton and an int immediate
#   a boxed int        outside +-2^50, where the Value is a pointer
#   two str objects    equal contents, different objects
#   a user __eq__      which decides for itself
#
# The hash is also taken inline for an int immediate now, so hash(-1) == -2
# and the boxed boundary matter as much as the comparison does.


def numeric_cross_type():
    s = {1, 2, 3}
    return (1.0 in s, 2.0 in s, True in s, False in s, 2.5 in s,
            0 in s, 1 in {1.0}, True in {1}, 1 in {True})


def bools_and_ints_are_one_key():
    s = set()
    s.add(1)
    s.add(True)
    s.add(1.0)
    out = [len(s), sorted(map(repr, s))]
    s.discard(True)
    out.append((len(s), 1 in s))
    return out


def big_ints_are_boxed():
    """Outside +-2^50 an int is a heap object, so two equal keys are two
    different pointers and the identity compare cannot answer."""
    big = 1 << 60
    s = {big, -big, 0}
    same = (1 << 60)
    out = [same in s, (-(1 << 60)) in s, ((1 << 60) + 1) in s, len(s)]
    s.discard(1 << 60)
    out.append((len(s), same in s))
    return out


def the_boundary_of_the_immediate_range():
    lo, hi = -(1 << 50), (1 << 50)
    vals = [lo - 1, lo, lo + 1, -2, -1, 0, 1, hi - 1, hi, hi + 1]
    s = set(vals)
    out = [len(s), all(v in s for v in vals), sorted(s) == sorted(vals)]
    for v in vals:
        s.discard(v)
    out.append(len(s))
    return out


def minus_one_hashes_to_minus_two():
    s = {-1, -2}
    return (len(s), -1 in s, -2 in s, hash(-1), hash(-2), hash(-1) == hash(-2))


def strings_by_content_not_identity():
    s = {"alpha", "beta"}
    built = "".join(["al", "pha"])
    out = [built in s, "gamma" in s]
    s.discard(built)
    out.append((len(s), "alpha" in s))
    return out


def a_user_eq_decides():
    class K:
        def __init__(self, v):
            self.v = v

        def __hash__(self):
            return hash(self.v) if self.v != 7 else 0

        def __eq__(self, o):
            return isinstance(o, K) and self.v == o.v

        def __repr__(self):
            return "K(%d)" % self.v

    s = {K(1), K(2), K(7)}
    out = [len(s), K(1) in s, K(3) in s, K(7) in s]
    s.discard(K(1))
    out.append((len(s), K(1) in s, K(2) in s))
    s.add(K(1))
    s.add(K(1))
    out.append(len(s))
    return out


def collisions_share_one_run():
    """Every key hashes to the same slot, so the probe walks a long run and
    the compare is what ends it."""
    class Same:
        def __init__(self, v):
            self.v = v

        def __hash__(self):
            return 0

        def __eq__(self, o):
            return isinstance(o, Same) and self.v == o.v

    s = set()
    for i in range(20):
        s.add(Same(i))
    out = [len(s), Same(19) in s, Same(20) in s]
    for i in range(0, 20, 2):
        s.discard(Same(i))
    out.append((len(s), Same(1) in s, Same(0) in s))
    return out


def tombstones_are_reused():
    s = set()
    for round_ in range(50):
        for i in range(12):
            s.add(i)
        for i in range(12):
            s.discard(i)
    s.add(1)
    return len(s), 1 in s, 0 in s


def frozensets_too():
    f = frozenset([1, 2, 3])
    g = frozenset([3, 2, 1])
    return (f == g, hash(f) == hash(g), 2 in f, 1.0 in f, 4 in f,
            f in {frozenset([1, 2, 3])}, set() in {frozenset()})


def a_set_key_becomes_a_frozenset():
    return set() in {frozenset()}, {1} in {frozenset([1])}, {9} in {frozenset()}


def set_operations_still_agree():
    a = {1, 2, 3, 4}
    b = {3, 4, 5, 6}
    return (sorted(a | b), sorted(a & b), sorted(a - b), sorted(a ^ b),
            a <= (a | b), a.isdisjoint({9}), sorted(set("abcab")))


print(numeric_cross_type())
print(bools_and_ints_are_one_key())
print(big_ints_are_boxed())
print(the_boundary_of_the_immediate_range())
print(minus_one_hashes_to_minus_two())
print(strings_by_content_not_identity())
print(a_user_eq_decides())
print(collisions_share_one_run())
print(tombstones_are_reused())
print(frozensets_too())
print(a_set_key_becomes_a_frozenset())
print(set_operations_still_agree())
