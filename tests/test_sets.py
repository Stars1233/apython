# Set literal
s = {1, 2, 3}
print(len(s))

# Membership
print(1 in s)
print(4 in s)
print(2 in s)

# Duplicate elimination
s2 = {1, 1, 2, 2, 3, 3}
print(len(s2))

# Iteration (collect and sort since set order is implementation-defined)
result = []
for x in {10, 20, 30}:
    result.append(x)
result.sort()
for x in result:
    print(x)

# Set comprehension
s3 = {x * x for x in range(5)}
print(len(s3))
result2 = []
for x in s3:
    result2.append(x)
result2.sort()
for x in result2:
    print(x)

print("done")

# --- construction: a set source is cloned, a sized one is presized ---------
# set(other_set) and s.copy() used to walk every slot and call set_add per
# element -- a hash, a probe, a load-factor test and a possible resize each.
# The table is copied wholesale now, TOMBSTONES INCLUDED, because in a flat
# table an entry's slot is its probe position and a tombstone is what keeps a
# chain alive.  So the shapes that matter are the ones where the source's
# table is not pristine.
import gc

for src in [set(), {1}, set(range(50)), frozenset(range(50)), frozenset(),
            [1, 2, 2, 3], (1, 2, 2, 3), [], (), "abcabc", range(10),
            {1: 0, 2: 0}, iter([1, 2]), (x for x in [1, 2, 3])]:
    made = set(src)
    print(sorted(made, key=str), len(made), type(made).__name__)
for src in [set(), {1}, set(range(20)), frozenset(range(20)), [1, 2], (1,), "ab"]:
    frz = frozenset(src)
    print(sorted(frz, key=str), len(frz), type(frz).__name__)

holed = set(range(30))
for i in range(0, 30, 2):
    holed.discard(i)
print(sorted(set(holed)) == sorted(holed), sorted(frozenset(holed)) == sorted(holed))
print(sorted(holed.copy()) == sorted(holed), len(set(holed)))


class OneSlotKey:
    def __init__(self, v):
        self.v = v

    def __hash__(self):
        return 0

    def __eq__(self, o):
        return isinstance(o, OneSlotKey) and self.v == o.v


# a source whose probe chain has a hole punched in it by pop()
punched = {OneSlotKey(i) for i in range(5)}
punched.pop()
for made in (set(punched), punched.copy(), frozenset(punched)):
    print(sorted(x.v for x in made),
          sorted(i for i in range(5) if OneSlotKey(i) in made))

independent = {1, 2, 3}
duplicate = independent.copy()
duplicate.add(9)
print(sorted(independent), sorted(duplicate))


class SetSub(set):
    pass


class FrozenSub(frozenset):
    pass


print(sorted(set(SetSub([1, 2]))), sorted(set(FrozenSub([1, 2]))))
print(type(set(SetSub([1, 2]))).__name__, type(SetSub([1, 2]).copy()).__name__)

seen_construct = []


class WatchElem:
    def __init__(self, tag):
        self.tag = tag

    def __hash__(self):
        return hash(self.tag)

    def __eq__(self, o):
        return isinstance(o, WatchElem) and self.tag == o.tag

    def __del__(self):
        seen_construct.append(self.tag)


def churn_construct():
    w = WatchElem("w")
    a = {w}
    b = set(a)
    c = a.copy()
    d = frozenset(a)
    del a, b, c, d, w


churn_construct()
print(seen_construct)
gc.collect()
print("gc ok")
