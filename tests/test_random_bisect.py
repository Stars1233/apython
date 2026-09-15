# `random` and `bisect`, CPython's own.
#
# Neither was in lib/.  src/modules/random.asm has been here all along -- the
# Mersenne Twister itself, as `_random` -- and lib/_random.py wraps it, but
# `random`, the module every caller actually imports, was missing.  bisect
# came with it: random.py does `from bisect import bisect as _bisect` at
# module scope for `choices`, so it is not optional.
#
# What makes this testable rather than merely present: the whole module is
# deterministic from a seed, and our MT19937 is CPython's, so a seeded
# sequence is comparable VALUE BY VALUE against python3 rather than only
# checked for shape.  Every number below is CPython's own output for seed 42.
import bisect
import random

random.seed(42)
print("random():", random.random())
print("randrange:", random.randrange(10), random.randrange(5, 15),
      random.randrange(0, 100, 7))
print("randint:", random.randint(1, 6), random.randint(-5, 5))
print("getrandbits:", random.getrandbits(1), random.getrandbits(16),
      random.getrandbits(200))
print("choice:", random.choice([1, 2, 3]), random.choice("abcdef"))
print("choices:", random.choices([1, 2, 3], k=5),
      random.choices("ab", weights=[1, 9], k=6),
      random.choices("ab", cum_weights=[1, 10], k=4))
values = list(range(10))
random.shuffle(values)
print("shuffle:", values)
print("sample:", random.sample(range(20), 5), random.sample("abcdef", 3))
print("sample counted:", random.sample([1, 2], 3, counts=[2, 2]))
print("uniform:", random.uniform(0, 1), random.uniform(-1, 1))
print("triangular:", random.triangular(), random.triangular(0, 10, 9))
print("gauss/normalvariate:", random.gauss(0, 1), random.normalvariate(5, 2))
print("lognormvariate:", random.lognormvariate(0, 1))
print("expovariate:", random.expovariate(1.5))
print("vonmisesvariate:", random.vonmisesvariate(0, 1))
print("gammavariate:", random.gammavariate(2, 3))
print("betavariate:", random.betavariate(2, 5))
print("paretovariate:", random.paretovariate(3))
print("weibullvariate:", random.weibullvariate(1, 2))
print("binomialvariate:", random.binomialvariate(10, 0.5))
print("randbytes:", random.randbytes(8))

# --- state ------------------------------------------------------------
state = random.getstate()
first = [random.random() for _ in range(5)]
random.setstate(state)
print("setstate replays:", first == [random.random() for _ in range(5)])
print("state shape:", type(state).__name__, len(state), state[0])

# --- a seeded instance is independent of the module's --------------------
r = random.Random(7)
print("Random(7):", r.random(), r.randint(1, 100))
print("two instances agree:", random.Random(7).random() == random.Random(7).random())
print("seed(str) and seed(bytes):",
      random.Random("abc").random(), random.Random(b"abc").random())
print("seed version 1:", random.Random(0).random(), end=" ")
r1 = random.Random()
r1.seed("abc", version=1)
print(r1.random())

# --- SystemRandom, which is urandom and so only checked for shape -------
sr = random.SystemRandom()
print("SystemRandom:", 0 <= sr.random() < 1, 1 <= sr.randint(1, 6) <= 6,
      len(sr.randbytes(4)))
try:
    sr.seed(1)
    print("SystemRandom.seed: no-op")
except Exception as exc:
    print("SystemRandom.seed:", type(exc).__name__)
try:
    sr.getstate()
    print("SystemRandom.getstate: NOT REFUSED")
except NotImplementedError:
    print("SystemRandom.getstate: NotImplementedError")

# --- what must be refused ----------------------------------------------
random.seed(1)
for call, what in ((lambda: random.randrange(0), "an empty range"),
                   (lambda: random.choice([]), "choice of nothing"),
                   (lambda: random.sample([1], 2), "a sample too large"),
                   (lambda: random.getrandbits(-1), "negative bits"),
                   (lambda: random.choices([1], weights=[1, 2]), "mismatched weights"),
                   (lambda: random.randint(5, 1), "an inverted randint")):
    try:
        call()
        print("%-24s NOT REFUSED" % what)
    except (ValueError, IndexError) as exc:
        print("%-24s %s" % (what, type(exc).__name__))

# --- a subclass overriding random(), which is the documented hook -------
class Half(random.Random):
    def random(self):
        return 0.5


h = Half()
print("subclass drives the rest:", h.random(), h.randrange(10), h.choice("abcd"))


# --- bisect -------------------------------------------------------------
data = [1, 3, 3, 5, 7]
print("bisect_left:", [bisect.bisect_left(data, v) for v in (0, 1, 3, 4, 8)])
print("bisect_right:", [bisect.bisect_right(data, v) for v in (0, 1, 3, 4, 8)])
print("bisect is bisect_right:", bisect.bisect is bisect.bisect_right)
print("lo/hi:", bisect.bisect_left(data, 3, 2), bisect.bisect_right(data, 3, 0, 2))
L = list(data)
bisect.insort_left(L, 4)
bisect.insort_right(L, 3)
print("insort:", L)
L2 = list(data)
bisect.insort(L2, 6)
print("insort is insort_right:", L2, bisect.insort is bisect.insort_right)
records = [("a", 3), ("b", 1), ("c", 2)]
records.sort(key=lambda r: r[1])
print("with a key:", bisect.bisect_left(records, 2, key=lambda r: r[1]))
K = [("b", 1), ("c", 2)]
bisect.insort(K, ("a", 3), key=lambda r: r[1])
print("insort with a key:", K)
# bisect has no __all__ -- CPython's file does not define one -- so what is
# checked is that every documented name is there.
print("bisect names:", [n for n in ("bisect", "bisect_left", "bisect_right",
                                    "insort", "insort_left", "insort_right")
                       if not hasattr(bisect, n)])
print("survived")
