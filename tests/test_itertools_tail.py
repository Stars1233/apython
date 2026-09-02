# The itertools functions that were missing: takewhile, dropwhile,
# filterfalse, zip_longest, pairwise, permutations, combinations,
# combinations_with_replacement, groupby and tee.
#
# lib/itertools.py is ordinary Python, so these are too.  Two differ from
# CPython's in a way the answers do not show: groupby materialises each group
# rather than sharing the source iterator, and tee materialises the source.

import itertools as it
print(list(it.takewhile(lambda x: x < 3, [1, 2, 3, 1])))
print(list(it.dropwhile(lambda x: x < 3, [1, 2, 3, 1])))
print(list(it.filterfalse(lambda x: x % 2, range(6))), list(it.filterfalse(None, [0, 1, 2, 0])))
print(list(it.zip_longest("ab", [1, 2, 3])), list(it.zip_longest("ab", [1], fillvalue="-")))
print(list(it.pairwise("abcd")), list(it.pairwise("a")), list(it.pairwise("")))
print(list(it.permutations("abc")), list(it.permutations("abc", 2)))
print(list(it.combinations("abcd", 2)), list(it.combinations("ab", 3)))
print(list(it.combinations_with_replacement("ab", 2)))
print([(k, list(g)) for k, g in it.groupby("aabbbc")])
print([(k, list(g)) for k, g in it.groupby([1, 1, 2, 3, 3], key=lambda x: x % 2)])
a, b = it.tee([1, 2, 3])
print(list(a), list(b))
print(len(list(it.tee("ab", 3))))
