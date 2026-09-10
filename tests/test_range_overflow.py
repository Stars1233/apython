# A range whose next value would leave int64 is finished, not wrapped.
#
# range_iter_next advanced with a bare `add rax, rdx`.  A signed overflow put
# `current` back at the OTHER end of the number line, below `stop` again, and
# the iterator started over: `range(2**63-2, 2**63-1, 2)` yielded for ever, and
# `range(1, 2, 2**63-1)` answered [1, -2**63].  CPython's test_range hits both
# through test_iterator_pickling, and the module was killed by the OOM reaper
# rather than merely failing.

M = 2**63
N = 2**31

cases = []
for K in (N, M):
    cases += [
        (K - 3, K - 1), (4 * K, 4 * K + 2),
        (K - 2, K - 1, 2), (-K + 1, -K, -2),
        (1, 2, K - 1), (-1, -2, -K),
        (1, K - 1, K - 1), (-1, -K, -K),
    ]

for t in cases:
    r = range(*t)
    print(t, len(r), list(r))

# The step that lands exactly on the last representable value.
print(list(range(M - 4, M - 1, 2)))
print(list(range(-M, -M + 4, 3)))
print(list(range(M - 1, M - 4, -2)))
print(list(range(-M + 3, -M, -1)))

# An empty one at each end.
print(list(range(M - 1, M - 1)), list(range(-M, -M)))
print(list(range(M - 1, M - 3)), list(range(-M, -M + 1, -1)))

# Reversed, in, and index still agree.
r = range(M - 5, M - 1, 2)
print(list(r), list(reversed(r)), len(r))
print((M - 5) in r, (M - 4) in r, (M - 3) in r)
print(r[0], r[1], r[-1])

# The ordinary ranges are untouched.
print(list(range(5)), list(range(2, 9, 3)), list(range(9, 2, -3)))
print(sum(range(1000)), len(range(10**6)))
print("done")
