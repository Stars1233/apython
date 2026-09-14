# The specialised FOR_ITER over a range has to saturate like the generic one.
#
# A `for` over a range rewrites itself to FOR_ITER_RANGE the first time it
# runs, and that handler inlines range_iter_next.  range_iter_next was taught
# to treat a signed overflow of `current += step` as the end of the run --
# without it, wrapping round to the other end put current back below stop and
# started the range again -- and the inlined copy was not.
#
# So the loop was RIGHT on its first pass and wrong on every pass after: cold,
# it called range_iter_next; warm, it ran the copy.  A test that iterates once
# proves nothing, which is why this one iterates twice and then many times.
import sys

M = sys.maxsize

CASES = [
    (1, 256, M),
    (0, 10, M),
    (1, 2, M),
    (-5, 5, M),
    (1, 256, -M),
    (0, -10, -M),
    (-1, -2, -M),
    (M - 4, M, 2),
    (-M, -M + 5, 2),
    (M - 4, M, M),
    (-M, -M + 5, -M),
    (0, 10, 3),
    (10, 0, -3),
]


def collect(args, cap=8):
    out = []
    for value in range(*args):
        out.append(value)
        if len(out) >= cap:
            break
    return out


# Cold: the site has not specialised yet.
for args in CASES:
    print("%-34s %r" % (args, collect(args)))

# Warm: the same code object, a thousand times over.  Any case that yields
# more than it should shows up here and nowhere else.
print()
seen = {}
for _ in range(1000):
    for args in CASES:
        got = tuple(collect(args))
        seen.setdefault(args, set()).add(got)
for args in CASES:
    values = seen[args]
    print("%-34s stable=%s %r"
          % (args, len(values) == 1, sorted(values)[0]))

# len() and list() agree with the loop, warm or cold.
print()
for args in CASES:
    r = range(*args)
    print("%-34s len=%-3d list=%r" % (args, len(r), list(r)[:4]))

# The same through a comprehension and through sum(), which drive the loop by
# different paths.
print()
for args in CASES[:6]:
    print("%-34s %r %r"
          % (args, [v for v in range(*args)][:4], sum(range(*args))))

# A loop that really does run to its end, so exhaustion is not the only exit
# being tested.
total = 0
for _ in range(1000):
    for v in range(0, 100, 7):
        total += v
print()
print("total:", total)

print("done")
