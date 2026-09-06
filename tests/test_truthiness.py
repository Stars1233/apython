# `if x:` used to classify the value THREE times.
#
# op_pop_jump_if_false opened with VPOP_VAL, which is V_UNPACK: classify the
# Value by its high16, subtract a bias, synthesise a tag.  It then immediately
# V_PACKed the pair back into the Value it had just taken apart, because
# obj_is_true takes a Value.  And obj_is_true opened with a V_UNPACK of its
# own, so it could compare that tag against TAG_SMALLINT and TAG_FLOAT.
#
# All three are gone.  The handler pops one word and passes it straight on;
# obj_is_true asks V_IS_INT and V_IS_FLOAT of the Value directly; and the
# integer arm needs no decoding at all, since an integer immediate is zero
# exactly when its Value IS the bias.
#
# The handlers also gained the bool fast path that a comment and a
# .pjif_bool_fast / .pjit_bool_fast label had been advertising -- nothing had
# ever jumped to either.  True and False are immortal (refcount starts at
# 2^63-1), so the release is a bare `dec` with no zero check and no call.
#
# What has to keep working is everything a conditional can be handed, and in
# particular the four shapes the fast path must NOT swallow: a non-bool that
# is truthy, one that is falsy, one whose __bool__ raises, and one whose
# __bool__ returns a non-bool.


class WithBool:
    def __init__(self, r):
        self.r = r

    def __bool__(self):
        return self.r

    def __repr__(self):
        return "WithBool(%r)" % (self.r,)


class WithLen:
    def __init__(self, n):
        self.n = n

    def __len__(self):
        return self.n

    def __repr__(self):
        return "WithLen(%d)" % (self.n,)


class Plain:
    def __repr__(self):
        return "Plain()"


VALUES = [
    True, False, None,
    0, 1, -1, 7, -7, 2 ** 49, -(2 ** 49), 2 ** 50, 2 ** 60, -(2 ** 60),
    10 ** 30, -(10 ** 30),
    0.0, -0.0, 1.5, -1.5, 1e-308, float("nan"), float("inf"), float("-inf"),
    "", "a", "0", "False",
    [], [1], [0], (), (1,), (0,), {}, {1: 2}, {0: 0}, set(), {1}, frozenset(),
    b"", b"a", bytearray(), bytearray(b"a"),
    range(0), range(1), range(0, 0),
    WithBool(True), WithBool(False), WithLen(0), WithLen(3), Plain(),
]


def branch(v):
    if v:
        return "T"
    return "F"


def branch_not(v):
    if not v:
        return "T"
    return "F"


def while_guard(v, n):
    c = 0
    i = 0
    while i < n and v:
        c += 1
        i += 1
    return c


def ternary(v):
    return "yes" if v else "no"


# Each site is run repeatedly: a conditional is where the bool fast path and
# the general path have to alternate without either poisoning the other.
for v in VALUES:
    for _ in range(4):
        a = branch(v)
        b = branch_not(v)
        t = ternary(v)
    print(repr(v), a, b, t, bool(v), not v, while_guard(v, 3))

# --- alternating bool and non-bool at the SAME site ------------------------
MIXED = [True, 1, False, 0, True, "", None, [1], False, 2.5, WithBool(True)]
for _ in range(3):
    print([branch(v) for v in MIXED])
    print([branch_not(v) for v in MIXED])

# --- and / or, which drive both opcodes --------------------------------------
for a in (0, 1, "", "x", None, [], [1], True, False, 0.0, -0.0):
    for b in (0, 1, "", "x", True, False):
        print(repr(a), repr(b), repr(a and b), repr(a or b),
              repr(not a), repr(bool(a) and bool(b)))

# --- the errors a conditional can raise --------------------------------------
class BadBool:
    def __bool__(self):
        return "nope"


class NoneBool:
    __bool__ = None


class RaisingBool:
    def __bool__(self):
        raise ValueError("boom")


class BadLen:
    def __len__(self):
        return -1


for cls in (BadBool, NoneBool, RaisingBool, BadLen):
    try:
        if cls():
            print("%s -> truthy" % cls.__name__)
        else:
            print("%s -> falsy" % cls.__name__)
    except (TypeError, ValueError) as exc:
        print("%s -> %s" % (cls.__name__, type(exc).__name__))
    try:
        while cls():
            break
        print("%s while -> ok" % cls.__name__)
    except (TypeError, ValueError) as exc:
        print("%s while -> %s" % (cls.__name__, type(exc).__name__))

# --- a long run over the bool fast path, to shake out a refcount slip -------
def spin(n):
    c = 0
    i = 0
    while i < n:
        if (i & 1) == 0:
            c += 1
        if (i & 1) != 0:
            c += 2
        if i & 3:
            c += 4
        i += 1
    return c


print(spin(20000))
print(True, False, bool(True), bool(False), True is True, False is False)
print([x for x in range(20) if x % 3], [x for x in range(20) if not x % 3])
print(any([0, 0, 1]), all([1, 1, 0]), any([]), all([]))
