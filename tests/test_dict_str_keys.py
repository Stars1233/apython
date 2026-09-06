# dict_lookup's str fast path, and the cases that must NOT take it.
#
# The probe loop answers "are these two keys equal" inline when both the probe
# key and the resident key are exact strs.  Everything else has to keep going
# through the full rich-comparison protocol, and the interesting tests are the
# ones that mix the two -- a dict whose keys are strs except for one object
# that claims to equal a string, probed with a string.
#
# CPython can skip the resident-key check because a dict records whether every
# key in it is unicode and abandons the specialised loop as soon as one is not.
# We do not track that, so the check is per entry, and these cases are what
# says it is actually being made.


class EqualsEverything:
    """Hashes like a str and claims equality with anything."""

    def __init__(self, h):
        self.h = h

    def __hash__(self):
        return self.h

    def __eq__(self, other):
        return True


class EqualsNothing:
    def __init__(self, h):
        self.h = h

    def __hash__(self):
        return self.h

    def __eq__(self, other):
        return False


class StrKey(str):
    """A str subclass whose __eq__ is a lie; it must not be shortcut."""

    def __eq__(self, other):
        return False

    def __hash__(self):
        return str.__hash__(self)


def plain():
    d = {"alpha": 1, "beta": 2, "gamma": 3}
    print(d["alpha"], d["beta"], d["gamma"])
    print("alpha" in d, "delta" in d)
    print(d.get("beta"), d.get("delta"), d.get("delta", 9))
    # A key built at run time, so it is a different object from the constant.
    k = "al" + "pha"
    print(d[k], k in d)
    d[k] = 10
    print(len(d), d["alpha"])


def nul_keys():
    # Counted keys: a NUL is an ordinary byte and must not end the comparison.
    d = {"a\0b": 1, "a\0c": 2, "a": 3, "a\0": 4}
    print(len(d), d["a\0b"], d["a\0c"], d["a"], d["a\0"])
    print("a\0b" in d, "a\0d" in d)
    d["a\0" + "b"] = 5
    print(len(d), d["a\0b"])


def lengths():
    # Every length across the 8-byte stride ap_memcmp walks, plus a neighbour
    # that differs only in the last byte so the tail decides.
    base = "abcdefghijklmnopqrstuvwxyz0123456789"
    d = {}
    for n in range(len(base) + 1):
        d[base[:n]] = n
    print(len(d))
    ok = True
    for n in range(len(base) + 1):
        if d[base[:n]] != n:
            ok = False
    print(ok)
    # Same length, differing last byte: not present.
    print(("abcdefgi" in d), ("abcdefgh" in d))


def wide():
    d = {"été": 1, "eta": 2, "中文": 3, "\U0001f600": 4, "é": 5}
    print(d["été"], d["eta"], d["中文"], d["\U0001f600"], d["é"])
    print("éta" in d, "é" in d)
    # Built at run time from parts, so a different object with the same bytes.
    print(d["ét" + "é"])


def mixed_with_liar():
    # A key that equals everything, sharing a bucket with strings.  Probing
    # with a str must still consult its __eq__.
    h = hash("zzz")
    d = {}
    liar = EqualsEverything(h)
    d[liar] = "liar"
    # "zzz" hashes to the same bucket, and liar says it is equal, so the
    # lookup finds the liar's entry rather than inserting a new one.
    print(d["zzz"], len(d))
    d["zzz"] = "overwritten"
    print(len(d), d[liar])

    # The mirror: a key that equals nothing.  A str probe with the same hash
    # must NOT match it.
    e = {}
    never = EqualsNothing(hash("qqq"))
    e[never] = "never"
    e["qqq"] = "str"
    print(len(e), e["qqq"], e[never])
    print("qqq" in e)


def subclass_keys():
    # A str subclass with a lying __eq__ stored as the resident key: a str
    # probe must go through the protocol and get False.
    d = {}
    d[StrKey("hello")] = 1
    print("hello" in d, len(d))
    d["hello"] = 2
    print(len(d))

    # The other direction -- a subclass as the PROBE key against a str
    # resident.  This used to be wrong, for a reason that had nothing to do
    # with the fast path: the probe key is not an exact str, so the fast path
    # declines and the generic route runs, and obj_richcompare_bool did not
    # apply the subclass-first rule.  It does now, so the lying __eq__ is
    # consulted and the probe finds nothing.
    g = {"hello": 1}
    print(StrKey("hello") in g, g.get(StrKey("hello")))

    # A plain str subclass, no overrides: equal to the str, so it finds it.
    class Plain(str):
        pass

    f = {"hello": 1}
    print(Plain("hello") in f, f[Plain("hello")])
    g = {Plain("hello"): 1}
    print("hello" in g, g["hello"])


def hash_first_use():
    # A string whose hash has never been taken: the fast path has to fall back
    # to obj_hash the first time and still record that the key is a str.
    s = "".join(["u", "n", "h", "a", "s", "h", "e", "d"])
    d = {}
    d[s] = 1
    t = "".join(["u", "n", "h", "a", "s", "h", "e", "d"])
    print(d[t], len(d))
    # Hash them in the other order.
    u = "".join(["l", "a", "t", "e"])
    e = {"late": 1}
    print(e[u])


def non_str_probes():
    # Ints, floats, tuples, None and bytes probing a str-keyed dict.
    d = {"1": "str one", 1: "int one", (1, 2): "tuple", None: "none",
         b"1": "bytes"}
    print(d["1"], d[1], d[(1, 2)], d[None], d[b"1"])
    print(len(d), 1.0 in d, True in d)
    # bool is an int subclass, so True finds the 1 entry.
    print(d[True], d[1.0])


def deletion_and_dummies():
    # A tombstoned slot on the probe path must not stop the str fast path.
    d = {}
    for i in range(40):
        d["k%d" % i] = i
    for i in range(0, 40, 2):
        del d["k%d" % i]
    print(len(d), d["k1"], d["k39"], "k0" in d)
    for i in range(0, 40, 2):
        d["k%d" % i] = -i
    print(len(d), d["k0"], d["k38"], d["k1"])


def keyword_and_attr():
    # The two other big consumers of str-keyed lookup.
    def f(**kw):
        return kw["alpha"] + kw["beta"]

    print(f(alpha=1, beta=2))

    class C:
        pass

    c = C()
    setattr(c, "dyn" + "amic", 7)
    print(c.dynamic, getattr(c, "dynamic"), hasattr(c, "missing"))
    print(sorted(c.__dict__.keys()))


plain()
nul_keys()
lengths()
wide()
mixed_with_liar()
subclass_keys()
hash_first_use()
non_str_probes()
deletion_and_dummies()
keyword_and_attr()
