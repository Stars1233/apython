"""Substring search has to be linear, not just correct.

The memchr-driven scan is O(n*m) whenever the needle's prefix repeats, and
CPython's own string_tests.test_adaptive_find is built to hit exactly that:
a haystack of `A A B A A` searched for `A B B A`, with A and B runs of a
million identical characters.  That shape made test_bytes, test_unicode and
test_userstring report as HANG rather than as results.

Crochemore and Perrin's two-way algorithm answers in O(n + m).  It is not the
first thing tried -- it pays a factorization up front -- so the scan counts
the candidates it rejects and switches once that work would exceed the
haystack's own length.

The sizes here are small enough to run in a test suite and large enough that a
quadratic implementation would not finish: at N = 40,000 the naive scan is
about 3 x 10^9 byte comparisons.
"""

import time

N = 40_000
BUDGET = 10.0           # generous; the quadratic version needs minutes


def timed(fn):
    start = time.time()
    value = fn()
    return value, time.time() - start


def check(label, fn, expected):
    value, elapsed = timed(fn)
    assert value == expected, '%s: %r != %r' % (label, value, expected)
    assert elapsed < BUDGET, '%s took %.1fs' % (label, elapsed)


def test_adaptive_find_str():
    a, b = 'a' * N, 'b' * N
    haystack = a + a + b + a + a
    needle = a + b + b + a
    check('find miss', lambda: haystack.find(needle), -1)
    check('count miss', lambda: haystack.count(needle), 0)
    check('in miss', lambda: needle in haystack, False)
    check('find hit', lambda: (haystack + needle).find(needle), len(haystack))
    check('count hit', lambda: (haystack + needle).count(needle), 1)
    check('index hit', lambda: (haystack + needle).index(needle), len(haystack))


def test_adaptive_find_bytes():
    a, b = b'a' * N, b'b' * N
    haystack = a + a + b + a + a
    needle = a + b + b + a
    check('bytes find', lambda: haystack.find(needle), -1)
    check('bytes count', lambda: haystack.count(needle), 0)
    check('bytes hit', lambda: (haystack + needle).find(needle), len(haystack))
    ba = bytearray(haystack)
    check('bytearray find', lambda: ba.find(needle), -1)
    check('bytearray count', lambda: ba.count(needle), 0)


def test_find_with_memory():
    """The periodic arm: a needle that is its own repetition.

    This is the case that needs the `memory` -- without it the two-way search
    re-compares the overlap at every window and is quadratic again.
    """
    for n in (1000, 5000, 20_000):
        needle = 'ab' * n
        haystack = ('ab' * (n - 1) + 'b') * 2
        check('memory miss %d' % n, lambda: haystack.find(needle), -1)
        check('memory count %d' % n, lambda: haystack.count(needle), 0)
        check('memory hit %d' % n,
              lambda: (haystack + needle).find(needle), len(haystack))


def test_the_answers_are_still_right():
    """Correctness over every short binary string, which is where a
    factorization bug shows: the first version held the first cut in r8
    across a call that clobbers it, and `"aba".find("aa")` answered 1."""
    import itertools

    def brute(h, n):
        if not n:
            return 0
        for i in range(len(h) - len(n) + 1):
            if h[i:i + len(n)] == n:
                return i
        return -1

    for hlen in range(0, 9):
        for chars in itertools.product('ab', repeat=hlen):
            hay = ''.join(chars)
            for nlen in range(0, 5):
                for nchars in itertools.product('ab', repeat=nlen):
                    nee = ''.join(nchars)
                    assert hay.find(nee) == brute(hay, nee), (hay, nee)
                    assert hay.encode().find(nee.encode()) == brute(hay, nee)


for fn in (test_adaptive_find_str,
           test_adaptive_find_bytes,
           test_find_with_memory,
           test_the_answers_are_still_right):
    fn()
    print(fn.__name__, 'ok')
print('OK')
