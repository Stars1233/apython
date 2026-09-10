# A set operation whose keys mutate the operands while it runs.
#
# set_add makes room BEFORE it probes, so that by the time a free slot has
# been handed out the table under it is the set's own and no write path has to
# test for the shared read-only empty table.  set_find_slot runs the keys'
# __eq__, though, and that is arbitrary Python: `a.clear()` frees the entries
# array and installs that shared table.  set_find_slot restarts its own probe
# when the table moves, and then answers about the NEW one -- so the free slot
# it handed back was in .rodata and the store faulted on a read-only page.
#
# Making the room again is what gives the set a table of its own, so set_add
# starts over when the table moved under it, exactly as CPython's
# set_add_entry does with `goto restart`.
#
# This is CPython's bpo-46615 suite, TestOperationsMutating, with a
# deterministic generator in place of random.randrange.

_state = [12345]


def rnd(n):
    _state[0] = (_state[0] * 1103515245 + 12345) & 0x7FFFFFFF
    return (_state[0] >> 7) % n


class SetSubclass(set):
    pass


def make(c1, c2):
    class Bad:
        def __eq__(self, other):
            if not enabled:
                return False
            if rnd(20) == 0:
                a.clear()
            if rnd(20) == 0:
                b.clear()
            return bool(rnd(2))

        def __hash__(self):
            return rnd(2)

    enabled = False
    a = c1(Bad() for _ in range(rnd(50)))
    b = c2(Bad() for _ in range(rnd(50)))
    enabled = True
    return a, b


def run(name, fn):
    crashes = 0
    for c1, c2 in ((set, set),
                   (SetSubclass, SetSubclass),
                   (set, SetSubclass),
                   (SetSubclass, set),
                   (set, dict.fromkeys),
                   (set, list)):
        for _ in range(30):
            a, b = make(c1, c2)
            try:
                fn(a, b)
            except RuntimeError as e:
                assert "changed size during iteration" in str(e), e
                crashes += 1
            except TypeError:
                # set.union(a, tuple) is fine; set & tuple is not.
                pass
    print(name, "survived")


run("issubset", set.issubset)
run("issuperset", set.issuperset)
run("intersection", set.intersection)
run("union", set.union)
run("difference", set.difference)
run("symmetric_difference", set.symmetric_difference)
run("isdisjoint", set.isdisjoint)
run("difference_update", set.difference_update)
run("intersection_update", set.intersection_update)
run("symmetric_difference_update", set.symmetric_difference_update)
run("update", set.update)

run("eq", lambda x, y: x == y)
run("ne", lambda x, y: x != y)
run("lt", lambda x, y: x < y)
run("le", lambda x, y: x <= y)
run("gt", lambda x, y: x > y)
run("ge", lambda x, y: x >= y)
run("and", lambda x, y: x & y)
run("or", lambda x, y: x | y)
run("sub", lambda x, y: x - y)
run("xor", lambda x, y: x ^ y)


def iand(x, y):
    x &= y


def ior(x, y):
    x |= y


def isub(x, y):
    x -= y


def ixor(x, y):
    x ^= y


run("iand", iand)
run("ior", ior)
run("isub", isub)
run("ixor", ixor)


def iterate(x, y):
    for _ in x:
        pass
    for _ in y:
        pass


run("iterate", iterate)

# The narrow shape on its own: a set that is cleared partway through an add,
# so the next insert lands on the shared empty table.
class Clears:
    def __init__(self, target, when):
        self.target = target
        self.when = when

    def __hash__(self):
        return 0            # every key collides, so every add compares

    def __eq__(self, other):
        if self.when:
            self.target.clear()
        return False


s = set()
keep = [Clears(s, False) for _ in range(8)]
for k in keep:
    s.add(k)
s.add(Clears(s, True))
print("cleared mid-add", len(s))
for i in range(200):
    s.add(Clears(s, i % 17 == 0))
print("repeated", len(s) <= 200)
print("done")
