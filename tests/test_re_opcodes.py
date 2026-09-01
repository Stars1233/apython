# The opcodes the engine used to get wrong, driven through _sre directly.
#
# This is the one that runs in the default `make check`.  tests/re_probe.sh
# is the real differential test, but it needs CPython's Lib/re on the path
# and so cannot be part of the ordinary gate -- and the two tests that were
# here before, test_sre.py and test_sre2.py, feed HAND-WRITTEN integer arrays
# that between them contain no REPEAT, REPEAT_ONE, IN or ASSERT.  Those are
# exactly the opcodes that were broken, which is why nothing caught:
#
#   .*x   matched "abc"          -- MAXREPEAT is 0xFFFFFFFF, which as an
#   .*    matched nothing           int32 is -1, and the greedy loop's
#   .*x\Z segfaulted                comparison was signed
#   (ab)+ matched zero times     -- the repeat context started its count at
#   (?=a)a failed                   0 instead of -1, and ASSERT was decoded
#                                   with three operand words where CPython
#                                   emits two
#
# The arrays below are CPython 3.12's own output, generated with
# re._compiler._code(re._parser.parse(p), 0) -- not written by hand, so they
# cannot drift from what the real compiler emits.

import _sre

CASES = [
    ('.*a', 'abc', [14, 4, 0, 1, 4294967295, 24, 5, 0, 4294967295, 2, 1, 16, 97, 1], 0, {}, (None,)),
    ('.*', 'abc', [14, 4, 0, 0, 4294967295, 24, 5, 0, 4294967295, 2, 1, 1], 0, {}, (None,)),
    ('a*', 'aaa', [14, 4, 0, 0, 4294967295, 24, 6, 0, 4294967295, 16, 97, 1, 1], 0, {}, (None,)),
    ('a+', 'aaa', [14, 4, 0, 1, 4294967295, 24, 6, 1, 4294967295, 16, 97, 1, 1], 0, {}, (None,)),
    ('a{2,3}', 'aaaa', [14, 4, 0, 2, 3, 24, 6, 2, 3, 16, 97, 1, 1], 0, {}, (None,)),
    ('[abc]+', 'abc', [14, 4, 0, 1, 4294967295, 24, 10, 1, 4294967295, 13, 5, 22, 97, 99, 0, 1, 1], 0, {}, (None,)),
    ('a*?b', 'aab', [14, 4, 0, 1, 4294967295, 26, 6, 0, 4294967295, 16, 97, 1, 16, 98, 1], 0, {}, (None,)),
    ('(ab)+', 'abab', [14, 4, 0, 2, 4294967295, 23, 11, 1, 4294967295, 17, 0, 16, 97, 16, 98, 17, 1, 18, 1], 1, {}, (None, None)),
    ('(a|b)*c', 'abc', [14, 4, 0, 1, 4294967295, 23, 13, 0, 4294967295, 17, 0, 13, 5, 22, 97, 98, 0, 17, 1, 18, 16, 99, 1], 1, {}, (None, None)),
    ('x(?:yz)?', 'xyz', [14, 8, 1, 1, 3, 1, 1, 120, 0, 16, 120, 23, 7, 0, 1, 16, 121, 16, 122, 18, 1], 0, {}, (None,)),
    ('(?=abc)a', 'abc', [14, 4, 0, 1, 1, 4, 9, 0, 16, 97, 16, 98, 16, 99, 1, 16, 97, 1], 0, {}, (None,)),
    ('(?!b)a', 'abc', [14, 4, 0, 1, 1, 5, 5, 0, 16, 98, 1, 16, 97, 1], 0, {}, (None,)),
    ('(?<=a)b', 'ab', [14, 4, 0, 1, 1, 4, 5, 1, 16, 97, 1, 16, 98, 1], 0, {}, (None,)),
    ('(?<!a)b', 'xb', [14, 4, 0, 1, 1, 5, 5, 1, 16, 97, 1, 16, 98, 1], 0, {}, (None,)),
    ('\\d+', '123', [14, 4, 0, 1, 4294967295, 24, 9, 1, 4294967295, 13, 4, 8, 10, 0, 1, 1], 0, {}, (None,)),
    ('(?>a*)b', 'aab', [14, 4, 0, 1, 4294967295, 27, 9, 24, 6, 0, 4294967295, 16, 97, 1, 1, 16, 98, 1], 0, {}, (None,)),
    ('a*+b', 'aab', [14, 4, 0, 1, 4294967295, 29, 6, 0, 4294967295, 16, 97, 1, 16, 98, 1], 0, {}, (None,)),
    ('.*x', 'abc', [14, 4, 0, 1, 4294967295, 24, 5, 0, 4294967295, 2, 1, 16, 120, 1], 0, {}, (None,)),
    ('a??b', 'ab', [14, 4, 0, 1, 2, 26, 6, 0, 1, 16, 97, 1, 16, 98, 1], 0, {}, (None,)),
    ('[^a]+', 'bcd', [14, 4, 0, 1, 4294967295, 24, 6, 1, 4294967295, 20, 97, 1, 1], 0, {}, (None,)),

    # Bounded repeats.  The general REPEAT counter starts at -1, as
    # sre_lib.h has it, and the two UNTIL handlers compare against max
    # STRICTLY because of that.  Nothing here exercised a bound before, so an
    # off-by-one that made every {m,n} match n+1 times passed the whole suite.
    ('(ab){2}', 'ababab', [14, 4, 0, 4, 4, 23, 11, 2, 2, 17, 0, 16, 97, 16, 98, 17, 1, 18, 1], 1, {}, (None, None)),
    ('(ab){2}', 'abab', [14, 4, 0, 4, 4, 23, 11, 2, 2, 17, 0, 16, 97, 16, 98, 17, 1, 18, 1], 1, {}, (None, None)),
    ('(ab){1,2}', 'ababab', [14, 4, 0, 2, 4, 23, 11, 1, 2, 17, 0, 16, 97, 16, 98, 17, 1, 18, 1], 1, {}, (None, None)),
    ('(ab){2,3}', 'ababababab', [14, 4, 0, 4, 6, 23, 11, 2, 3, 17, 0, 16, 97, 16, 98, 17, 1, 18, 1], 1, {}, (None, None)),
    ('(ab){0,1}', 'abab', [14, 4, 0, 0, 2, 23, 11, 0, 1, 17, 0, 16, 97, 16, 98, 17, 1, 18, 1], 1, {}, (None, None)),
    ('(a)?', 'aa', [14, 4, 0, 0, 1, 23, 9, 0, 1, 17, 0, 16, 97, 17, 1, 18, 1], 1, {}, (None, None)),
    ('(a|b){2}', 'abab', [14, 4, 0, 2, 2, 23, 13, 2, 2, 17, 0, 13, 5, 22, 97, 98, 0, 17, 1, 18, 1], 1, {}, (None, None)),
    ('(?:ab){2,}', 'ababab', [14, 4, 0, 4, 4294967295, 23, 7, 2, 4294967295, 16, 97, 16, 98, 18, 1], 0, {}, (None,)),
    ('(ab)+?', 'ababab', [14, 4, 0, 2, 4294967295, 23, 11, 1, 4294967295, 17, 0, 16, 97, 16, 98, 17, 1, 19, 1], 1, {}, (None, None)),
    ('(ab){2,3}?', 'ababababab', [14, 4, 0, 4, 6, 23, 11, 2, 3, 17, 0, 16, 97, 16, 98, 17, 1, 19, 1], 1, {}, (None, None)),
    ('(a){3}', 'aaaa', [14, 4, 0, 3, 3, 23, 9, 3, 3, 17, 0, 16, 97, 17, 1, 18, 1], 1, {}, (None, None)),
    ('(a)*', 'aaa', [14, 4, 0, 0, 4294967295, 23, 9, 0, 4294967295, 17, 0, 16, 97, 17, 1, 18, 1], 1, {}, (None, None)),
]

print("--- match ---")
for pattern, subject, code, groups, groupindex, indexgroup in CASES:
    rx = _sre.compile(pattern, 0, code, groups, groupindex, indexgroup)
    m = rx.match(subject)
    print("%-10s %-6s %s" % (pattern, subject, m.span() if m else None))

print()
print("--- search, which drives the same opcodes from every start ---")
for pattern, subject, code, groups, groupindex, indexgroup in CASES:
    rx = _sre.compile(pattern, 0, code, groups, groupindex, indexgroup)
    m = rx.search(subject)
    print("%-10s %-6s %s" % (pattern, subject, m.span() if m else None))

print()
print("--- the groups the repeats capture ---")
for pattern, subject, code, groups, groupindex, indexgroup in CASES:
    if not groups:
        continue
    rx = _sre.compile(pattern, 0, code, groups, groupindex, indexgroup)
    m = rx.match(subject)
    print("%-10s %-6s %r" % (pattern, subject, m.groups() if m else None))

print()
print("--- a greedy repeat backing off one position at a time ---")
# .* over a long subject with a tail that only matches near the front: the
# back-off loop runs the whole length, which is where the position used to
# walk off the front of the string and segfault.
long_code = [24, 5, 0, 4294967295, 2, 1, 16, 97, 1]   # REPEAT_ONE ANY, then 'a'
rx = _sre.compile(".*a", 0, long_code, 0, {}, (None,))
for n in (0, 1, 2, 10, 100, 1000):
    subject = "a" + "b" * n
    m = rx.match(subject)
    print("len %-5d %s" % (len(subject), m.span() if m else None))
for n in (1, 10, 100):
    subject = "b" * n
    m = rx.match(subject)
    print("no 'a', len %-4d %s" % (n, m.span() if m else None))
