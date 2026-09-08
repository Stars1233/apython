# A set probe holds the entry array's mask, its remaining budget and a pointer
# into it in registers across set_keys_equal -- which runs __eq__, which is
# arbitrary Python and may add to the very set being probed.  A resize frees
# that array and rehashes into a new one, leaving every one of those stale.
#
# The probe restarts when the array moves.  Before it did, a set whose keys
# collide and whose __eq__ grows it walked the freed array and ended at
# fatal_error("set: hash table full") -- the interpreter aborted, so there was
# no exception to catch.

S = set()
R = set()


class Grows:
    """Every instance hashes to the same slot, so the probe has to walk, and
    the first one's __eq__ grows the set past its load factor."""
    busy = False

    def __init__(self, v, target):
        self.v = v
        self.target = target

    def __hash__(self):
        return 0

    def __eq__(self, o):
        if self.v == 0 and not Grows.busy:
            Grows.busy = True
            for i in range(400):
                self.target.add(i)
            Grows.busy = False
        return isinstance(o, Grows) and self.v == o.v

    def __repr__(self):
        return "G(%d)" % self.v


def a_lookup_that_grows_the_set():
    for i in range(5):
        S.add(Grows(i, S))
    hit = Grows(4, S) in S
    miss = Grows(9, S) in S
    return hit, miss, len(S) > 100, all(i in S for i in range(400))


def a_removal_that_grows_the_set():
    for i in range(5):
        R.add(Grows(i, R))
    R.discard(Grows(4, R))
    return Grows(4, R) in R, Grows(3, R) in R, len(R) > 100


def an_add_that_grows_the_set():
    T = set()
    for i in range(5):
        T.add(Grows(i, T))
    T.add(Grows(2, T))              # already there; the probe must find it
    return sum(1 for x in T if isinstance(x, Grows)), len(T) > 100


def the_same_for_a_dict():
    D = {}

    class J:
        busy = False

        def __init__(self, v):
            self.v = v

        def __hash__(self):
            return 0

        def __eq__(self, o):
            if self.v == 0 and not J.busy:
                J.busy = True
                for i in range(400):
                    D[i] = i
                J.busy = False
            return isinstance(o, J) and self.v == o.v

    for i in range(5):
        D[J(i)] = i
    return D.get(J(4)), D.get(J(9), "absent"), len(D) > 100


print(a_lookup_that_grows_the_set())
print(a_removal_that_grows_the_set())
print(an_add_that_grows_the_set())
print(the_same_for_a_dict())
