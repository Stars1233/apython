# MT19937, now in assembly.
#
# lib/_random.py was the twist itself in Python -- a 624-iteration loop of
# shifts and xors per 624 outputs -- and it cost 47x CPython on random() and
# nearly two seconds per megabyte of randbytes().  That last number is why
# test_zlib timed out: CPython's check_big_compress_buffer opens with
# randbytes(10 * 1024 * 1024) whether or not -M was given.
#
# What is asserted here is the SEQUENCE, against values taken from CPython
# 3.12, because MT19937 is fully specified and every recorded seed in CPython's
# own tests depends on the exact stream.  A generator that is uniform and
# different is the failure this guards against, and no statistical test would
# see it.  The three places a rewrite gets it wrong quietly:
#
#   * random() takes 27 bits from the first word and 26 from the second, in
#     that order.  Reversing the split is just as uniform.
#   * getrandbits(k) above 32 composes little-endian words with the LAST one
#     narrowed, and the narrowing shifts RIGHT -- it drops the low bits, so a
#     one-byte tail is the word's TOP byte.
#   * seeding is init_by_array over the seed's 32-bit words, not init_genrand.
import _random

# --- the stream, against CPython's own -----------------------------------

# _random.Random(12345); random(), getrandbits(32), getrandbits(64),
# getrandbits(7), getrandbits(200), getrandbits(1) -- in that order, since
# each one consumes.
r = _random.Random(12345)
print(r.random())
print(r.getrandbits(32))
print(r.getrandbits(64))
print(r.getrandbits(7))
print(r.getrandbits(0))
print(r.getrandbits(200))
print(r.getrandbits(1))

EXPECTED = [
    0.41661987254534116,
    43676229,
    15222373233177589868,
    102,
    0,
    905599004244682168194040024353801414088091170049964748548418,
    0,
]
r = _random.Random(12345)
got = [r.random(), r.getrandbits(32), r.getrandbits(64), r.getrandbits(7),
       r.getrandbits(0), r.getrandbits(200), r.getrandbits(1)]
print("stream matches CPython:", got == EXPECTED)

# The first five words for a seed of 0, and for one wider than a word.
print(_random.Random(0).random(), _random.Random(1).random())
print(_random.Random(2 ** 70 + 7).random())
print(_random.Random(-99).random())      # seeded by absolute value

# A default-seeded generator is not the unseeded reference sequence.
print("seeded differs:", _random.Random().random() != _random.Random(5489).random())

# --- randbytes, which is getrandbits(n * 8) by construction ---------------

print()
EXPECTED_BYTES = {
    0: b"",
    1: b"j",
    2: b"\xa7j",
    3: b"\x99\xa7j",
    4: b"\x87\x99\xa7j",
    5: b"\x87\x99\xa7j\xbb",
    7: b"\x87\x99\xa7jC\x91\xbb",
    8: b"\x87\x99\xa7j:C\x91\xbb",
}
def randbytes(r, n):
    # random.Random.randbytes is written exactly this way, so this is what the
    # bulk path actually is.  _random.Random itself has no randbytes, here or
    # in CPython.
    return r.getrandbits(n * 8).to_bytes(n, "little") if n else b""


for n in sorted(EXPECTED_BYTES):
    b = randbytes(_random.Random(12345), n)
    print("%-3d %-28r %s" % (n, b, b == EXPECTED_BYTES[n]))

# It is fast enough to be used in bulk, which was the point.
big = randbytes(_random.Random(1), 1024 * 1024)
print("a megabyte:", len(big), len(set(big)) == 256)

# --- getstate / setstate --------------------------------------------------

print()
r = _random.Random(7)
state = r.getstate()
print("state length:", len(state), "cursor:", state[-1])
print("all words:", all(isinstance(v, int) and 0 <= v <= 0xffffffff
                        for v in state[:-1]))
before = [r.random() for _ in range(4)]
r.setstate(state)
print("restored:", [r.random() for _ in range(4)] == before)

# A state taken mid-block restores mid-block, cursor and all.
r = _random.Random(7)
r.getrandbits(32)
mid = r.getstate()
print("mid cursor:", mid[-1] == 1)
tail = [r.random() for _ in range(3)]
r.setstate(mid)
print("mid restored:", [r.random() for _ in range(3)] == tail)

# One generator's state moved into another.
a = _random.Random(11)
b = _random.Random(22)
b.setstate(a.getstate())
print("transplanted:", a.random() == b.random())

for bad in ([0] * 625, (0,) * 624, (0,) * 626, (0,) * 624 + (625,),
            (0,) * 624 + (-1,)):
    try:
        _random.Random(1).setstate(bad)
        print("accepted", type(bad).__name__, len(bad))
    except (TypeError, ValueError) as e:
        print("refused:", type(e).__name__)

# --- what the arguments insist on ----------------------------------------

print()
r = _random.Random(1)
for bad in (-1, -1000):
    try:
        r.getrandbits(bad)
    except ValueError as e:
        print("ValueError:", e)
try:
    r.getrandbits(1.5)
except TypeError as e:
    print("TypeError:", e)
print("bool accepted:", r.getrandbits(True) in (0, 1))

# Every width from 1 to 130 produces a value in range, which is the check the
# word-composition arithmetic fails at its boundaries -- 32, 33, 64, 65, 96.
ok = True
for k in range(1, 131):
    v = r.getrandbits(k)
    if not (0 <= v < (1 << k)):
        ok = False
        print("out of range at", k)
print("widths 1..130 in range:", ok)

# random() is in [0, 1), always.
r = _random.Random(3)
values = [r.random() for _ in range(2000)]
print("range:", all(0.0 <= v < 1.0 for v in values))
print("distinct:", len(set(values)) == len(values))

# --- the handle is not a pointer a program can forge ----------------------
#
# _randomcore takes an INDEX into a table it owns, bounds- and magic-checked,
# so a number a program invents is refused rather than dereferenced.

# CPython has no _randomcore, so the result is reduced to one line that reads
# the same from both interpreters; there is nothing here for CPython to check.

print()
try:
    import _randomcore
except ImportError:
    _randomcore = None

refused = []
if _randomcore is not None:
    for bad in (-1, 99999, 2 ** 40):
        try:
            _randomcore.random(bad)
            refused.append(False)
        except ValueError:
            refused.append(True)
    h = _randomcore.new()
    _randomcore.free(h)
    try:
        _randomcore.random(h)
        refused.append(False)
    except ValueError:
        refused.append(True)
else:
    refused = [True] * 4
print("forged handles refused:", refused == [True] * 4)

# --- subclassing, which is how random.Random uses this -------------------

print()


class Sub(_random.Random):
    def __init__(self, seed):
        super().__init__(seed)
        self.tag = "sub"


s = Sub(12345)
print("subclass:", s.tag, s.random() == EXPECTED[0])

# random.Random overrides __init__ and calls self.seed() without chaining, so
# the state has to exist before __init__ runs at all.


class NoChain(_random.Random):
    def __init__(self, seed):
        self.seed(seed)


print("no chain:", NoChain(12345).random() == EXPECTED[0])

# Many live generators at once: the handle table grows, and a freed slot is
# reused rather than leaked.
gens = [_random.Random(i) for i in range(200)]
print("independent:", len({g.random() for g in gens}) == 200)
del gens
more = [_random.Random(i) for i in range(200)]
print("after reuse:", len({g.random() for g in more}) == 200)

print("done")
