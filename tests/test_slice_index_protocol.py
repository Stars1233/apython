# A slice bound may be any object with __index__.
#
# `[1,2,3][X():]` was a TypeError for every X that was not an int, because
# pyobj_to_i64 -- the converter every slice bound goes through -- tested for an
# int and refused anything else.  obj_as_index, which is the funnel for the
# same protocol everywhere else in the tree (subscripts, repetition counts,
# hex()), was never reached from here.
#
# The refusal was also worded from before the protocol existed: "slice indices
# must be integers or None", where CPython says "... or None or have an
# __index__ method".
#
# It came out of CPython's test_mmap, whose gh-103987 case builds an X whose
# __index__ closes the mapping -- so the conversion has to happen at the same
# point CPython's does, which is before anything else and with a re-check
# after.
import sys


class Five:
    def __index__(self):
        return 5


class Neg:
    def __index__(self):
        return -2


class Huge:
    def __index__(self):
        return 2 ** 70


class Bad:
    def __index__(self):
        return "five"


class Raises:
    def __index__(self):
        raise ZeroDivisionError("from __index__")


class Nothing:
    pass


SEQ = [0, 1, 2, 3, 4, 5, 6, 7, 8, 9]
B = bytes(SEQ)
S = "abcdefghij"
T = tuple(SEQ)

print(slice(Five(), 20).indices(30))
print(slice(None, Five()).indices(30))
print(slice(Five(), None, Five()).indices(30))
print(slice(Neg(), None).indices(30))
print(slice(Huge(), None).indices(30))
print(slice(None, Huge()).indices(30))

print()
for seq in (SEQ, T, B, S, range(10), bytearray(B)):
    name = type(seq).__name__
    print("%-10s %r %r %r"
          % (name, list(seq[Five():]), list(seq[:Five()]),
             list(seq[::Five()])))
    print("%-10s %r %r"
          % ("", list(seq[Neg():]), list(seq[Huge():])))

# A bool is an int, and was never the problem; it is here because the
# conversion now runs for it too.
print()
print(SEQ[True:], SEQ[:True], B[True:])

# --- what is still refused, and in CPython's words --------------------------

print()
for bad in ("a", 1.5, None if False else 2.5, [1], Nothing()):
    try:
        SEQ[bad:]
        print("accepted %r" % (bad,))
    except TypeError as e:
        print("%-10s %s" % (type(bad).__name__, e))

try:
    slice("a", 20).indices(30)
except TypeError as e:
    print("indices:", e)

try:
    SEQ[Bad():]
except TypeError as e:
    print("non-int:", e)

# An exception from __index__ passes through untouched.
try:
    SEQ[Raises():]
except ZeroDivisionError as e:
    print("passed through:", e)

# --- a bound with a side effect runs exactly once ---------------------------

class Counting:
    def __init__(self):
        self.n = 0

    def __index__(self):
        self.n += 1
        return 3


c = Counting()
print()
print(SEQ[c:], "calls:", c.n)
c = Counting()
print(SEQ[c:c], "calls:", c.n)

# --- a bound wider than an index clamps rather than raising ------------------
#
# A slice is the one place an out-of-range int is not an error: CPython's
# _PyEval_SliceIndex clamps to the nearest end.

print()
print(SEQ[2 ** 70:], SEQ[:2 ** 70], SEQ[-2 ** 70:], SEQ[:-2 ** 70])
print(SEQ[Huge():], SEQ[:Huge()])
print(B[2 ** 70:], B[:2 ** 70])

print("done")
