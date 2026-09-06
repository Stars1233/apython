# `is` is one compare on the two raw Values.
#
# A Value is a canonical bit pattern: a pointer is its own Value, an integer
# immediate is unique per value, and a float per bit pattern.  So bit equality
# IS identity, and op_is_op needed neither of the V_UNPACKs it opened with nor
# the separate payload and tag comparison they fed -- which sat under a comment
# claiming the tag was needed "for SmallInt correctness".  It is the encoding
# that provides that.
#
# What this pins down, because it is the argument the handler now rests on:
#
#   - an object is identical to itself whatever its Value encodes -- an
#     immediate, a float, a pointer -- and to nothing else in the sweep
#   - 0.0 and -0.0 have different bit patterns and so are not identical, which
#     is what CPython says too
#   - None, True and False are singletons and compare by pointer
#   - a container is identical only to itself
#
# It also covers the deopt-free mixing the handler has to survive: the same
# site seeing an immediate, then a pointer, then a float.

import sys
V = [None, True, False, 0, 1, -1, 7, 2**49, 2**50, 2**60, -(2**60), 10**30,
     0.0, -0.0, 1.5, float('inf'), "", "a", "abc", [], [1], (), (1,), {}, object]
S = [x for x in V]
def isop(a, b): return (a is b, a is not b)
for i, a in enumerate(V):
    for j, b in enumerate(S):
        for _ in range(3):
            r = isop(a, b)
        print(i, j, r)
# identity against itself
for a in V:
    print(repr(a), a is a, a is not a)
# Deliberately NOT here: whether two separately written equal integers are
# one object.  That is a question about the representation, not about `is`,
# and `make INT_STRESS=1` changes the answer on purpose by boxing every small
# int.  What matters to this handler is that identity is decided by the Value
# alone, which the pairwise sweep above covers: V and S hold the same objects,
# so every True in it is a genuine identity.
a = [1]; b = a; c = [1]
print(a is b, a is c, a is not c)
n = None
def f(v):
    if v is None: return "none"
    if v is True: return "true"
    if v is False: return "false"
    return "other"
for v in (None, True, False, 0, 1, "", [], 0.0):
    for _ in range(4): r = f(v)
    print(repr(v), r)
