# dict_lookup is two probe loops now, chosen once on entry.
#
# The STR loop runs only when the probe key is an exact str AND the dict's
# dk_kind says every key in it is one, so it compares bytes without first
# asking what the stored key is.  Everything else takes the GENERIC loop,
# which asks obj_richcompare_bool.
#
# So the cases that matter are the ones that decide WHICH loop, and the ones
# that change the answer while a lookup is in progress:
#
#   - dk_kind starts true of an empty dict and is cleared for good by the
#     first non-str INSERT.  A lookup that misses must not clear it, and a
#     delete must not restore it.  If it were ever wrong in the "all strs"
#     direction, the str loop would read a non-str key's length field and
#     memcmp that many bytes.
#   - a str SUBCLASS is not an exact str: it may carry its own __eq__ and
#     __hash__, so it must demote the table and take the generic loop.
#   - the generic loop's comparison can run Python that resizes the dict.
#     The arrays it holds in registers are then freed, so it re-checks and
#     restarts.  (test_set_mutated_under_probe drives that one hard.)
#   - an int immediate hashes to itself inline, except that hash(-1) is -2.

BIG = 2 ** 60


def probe(d, keys):
    return [d.get(k, "-") for k in keys]


# --- all-str tables: the str loop -----------------------------------------
s = {"alpha": 1, "beta": 2, "": 3, "x": 4, "a" * 100: 5}
print(probe(s, ["alpha", "beta", "", "x", "a" * 100, "gamma", "alph", "alphaa"]))
print("alpha" in s, "gamma" in s, len(s))
# a non-str probe into an all-str table takes the generic loop and finds
# nothing, and must NOT change the table
print(s.get(1), s.get(None), s.get((1, 2)), s.get(1.5), len(s))
print(probe(s, ["alpha", "x"]))

# --- the transition: a non-str insert demotes -----------------------------
t = {"a": 1, "b": 2}
print(probe(t, ["a", "b"]))
t[3] = "three"
print(probe(t, ["a", "b", 3]), len(t))
del t[3]
print(probe(t, ["a", "b", 3]), len(t))
t["c"] = 3
print(probe(t, ["a", "b", "c"]))

# --- a str subclass must demote and keep its own __eq__ -------------------
class SKey(str):
    def __eq__(self, o):
        return str.__eq__(self, o) or o == "MAGIC"

    def __hash__(self):
        return str.__hash__(self)


u = {"plain": 1}
u[SKey("sub")] = 2
print(sorted(map(str, u)), u["plain"], u[SKey("sub")], u["sub"])
print(u.get("MAGIC", "-"))

v = {SKey("only"): 1}
print(v["only"], v.get("nope", "-"), len(v))


class SOnlyHash(str):
    pass


w = {SOnlyHash("k"): 1, "k2": 2}
print(w["k"], w["k2"], len(w))

# --- collisions, both loops ----------------------------------------------
coll = {}
for i in range(40):
    coll[i * 4096] = i
print(len(coll), coll[0], coll[39 * 4096], coll.get(1, "-"))
for i in range(40):
    if coll[i * 4096] != i:
        print("BAD", i)
print("int collisions ok")


class Same:
    def __init__(self, v):
        self.v = v

    def __hash__(self):
        return 7

    def __eq__(self, o):
        return isinstance(o, Same) and self.v == o.v

    def __repr__(self):
        return "S%d" % self.v


same = {Same(i): i for i in range(30)}
print(len(same), same[Same(0)], same[Same(29)], same.get(Same(99), "-"))
del same[Same(15)]
print(len(same), same.get(Same(15), "-"), same[Same(16)])
print("object collisions ok")

# --- the inline int hash --------------------------------------------------
ints = {}
for k in (0, 1, -1, -2, 2, 2 ** 50 - 1, -(2 ** 50), BIG, -BIG, 10 ** 30):
    ints[k] = str(k)
print(len(ints), ints[-1], ints[-2], ints[0], ints[BIG], ints[10 ** 30])
print(hash(-1) == hash(-2), -1 in ints, -2 in ints, ints[-1] != ints[-2])
# an int and a float and a bool that compare equal share a slot
mix = {}
mix[1] = "int"
mix[1.0] = "float"
mix[True] = "bool"
print(mix, len(mix))
mix2 = {}
mix2[0] = "int"
mix2[False] = "bool"
mix2[0.0] = "float"
print(mix2, len(mix2))
# a heap int equal to an immediate finds the same entry
big_eq = {2 ** 50: "a"}
print(big_eq[2 ** 50], (2 ** 25) ** 2 in big_eq)

# --- a __hash__ that raises, in both loops --------------------------------
class BadHash:
    def __hash__(self):
        raise ZeroDivisionError("hash")


for d in ({"a": 1}, {1: 2}, {}):
    try:
        d[BadHash()]
    except ZeroDivisionError as e:
        print("ZeroDivisionError", e)
    try:
        BadHash() in d
    except ZeroDivisionError as e:
        print("ZeroDivisionError", e)

# --- an __eq__ that raises, reached only through a collision --------------
class BadEq:
    def __hash__(self):
        return 7

    def __eq__(self, o):
        raise KeyError("eq")


# The STORED key is the one whose __eq__ runs, so BadEq goes in the table and
# a colliding Same probes it.  (Printing the exception is out: a KeyError
# carries the key, whose repr is an address.)
be = {BadEq(): 1}
try:
    be[Same(0)]
except KeyError as e:
    print("KeyError", e)
try:
    Same(0) in be
except KeyError as e:
    print("KeyError", e)

# --- an __eq__ that resizes the dict it is probing ------------------------
D = {}


class Grow:
    busy = False

    def __init__(self, v):
        self.v = v

    def __hash__(self):
        return 0

    def __eq__(self, o):
        if self.v == 0 and not Grow.busy:
            Grow.busy = True
            for i in range(400):
                D[i] = i
            Grow.busy = False
        return isinstance(o, Grow) and self.v == o.v


for i in range(5):
    D[Grow(i)] = i
print(Grow(4) in D, Grow(9) in D, len(D) > 100, all(i in D for i in range(400)))
