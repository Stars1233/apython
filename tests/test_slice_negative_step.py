# A negative step whose stop is at or past the end.
#
# Two defects, one on top of the other, and both silent until a slice happened
# to be written that way.
#
# slice.indices clamped `stop` to `length` whatever the sign of the step.
# PySlice_AdjustIndices clamps the upper bound to `length - 1` when the step is
# negative, because a negative step walks DOWN from start and the largest
# index it can ever name is length - 1.  So `slice(0, 256, -1).indices(256)`
# answered (0, 256, -1) where CPython says (0, 255, -1).
#
# And bytes' extended-slice arm computed its element count as
# (start - stop - 1) / -step without first testing whether the span was
# positive at all.  The positive-step arm beside it has always had that guard.
# A negative count went into `div` as a number near 2^64: b"..."[5:10:-1]
# built a bytes whose ob_size was NEGATIVE, and b"..."[0:256:-1] was
# "Fatal: out of memory".
#
# The start bound has the same asymmetry and had the same defect: a start
# below the sequence clamps to 0 for a positive step and to -1 for a negative
# one, -1 being "one before the first index", which is where a downward walk
# stops.  slice(-300, None, -1).indices(256) answered (0, -1, -1) -- a
# ONE-element slice -- where CPython answers (-1, -1, -1) and it is empty.
#
# That half is invisible to a matrix that compares one sequence against
# another, because every sequence reads the same slice_indices and so every
# one of them is wrong together.  Only the printed indices catch it.
#
# It is a shape that hides well.  Every other sequence -- list, tuple, str,
# bytearray, range, memoryview -- got it right, so a test over "slicing" that
# used a list would have proved nothing, and the usual way to write a reversed
# slice (`s[::-1]`, `s[10:5:-1]`) never reaches it.
import sys

S = bytes(range(256))
L = list(range(256))
T = tuple(L)
U = "".join([chr(65 + (i % 26)) for i in range(256)])
BA = bytearray(S)
R = range(256)

# --- slice.indices, which is where it starts --------------------------------

CASES = [
    (0, 256, -1), (0, 300, -1), (0, 256, -300), (5, 10, -1),
    (None, None, -1), (300, None, -1), (sys.maxsize, None, -1),
    (0, None, -2), (-31, -300, -1), (256, 256, -1), (255, 255, -1),
    (0, 1, -1), (1, 0, -1), (-1, -1, -1), (None, 256, -1),
    (0, 256, 1), (0, 300, 1), (None, None, 1), (0, None, 3),
    # A start below the sequence, which clamps to -1 for a negative step.
    (-300, None, -1), (-300, -300, -1), (-300, 0, -1), (-300, 255, -1),
    (-256, None, -1), (-257, None, -1), (-300, None, 1), (-300, None, 2),
    (None, -300, -1), (-1, -300, -1), (0, -300, -1), (-300, -1, -1),
]
for args in CASES:
    print("%-28s %s" % (args, slice(*args).indices(256)))

print()
for length in (0, 1, 2, 5):
    for args in ((0, length, -1), (None, None, -1), (length, 0, -1),
                 (0, length + 5, -1)):
        print("len %d %-22s %s" % (length, args, slice(*args).indices(length)))

# --- and every sequence has to agree with a list ----------------------------

print()
INDICES = (0, None, 1, 3, 19, 300, sys.maxsize, -1, -2, -31, -300, 256, 255)
STEPS = (1, 2, 3, 19, 300, -1, -2, -3, -31, -300, sys.maxsize, -sys.maxsize)

mismatches = []
for start in INDICES:
    for stop in INDICES:
        for step in STEPS:
            want = L[start:stop:step]
            for name, seq, conv in (
                ("bytes", S, list),
                ("bytearray", BA, list),
                ("tuple", T, list),
                ("range", R, list),
            ):
                got = conv(seq[start:stop:step])
                if got != want:
                    mismatches.append((name, start, stop, step, len(got),
                                       len(want)))
            # str holds different values, so only the length and order are
            # comparable; the mapping i -> U[i] is what makes it so.
            got = U[start:stop:step]
            if got != "".join([U[i] for i in want]):
                mismatches.append(("str", start, stop, step, len(got),
                                   len(want)))
print("combinations:", len(INDICES) * len(INDICES) * len(STEPS))
print("mismatches:", mismatches)

# --- the two cases that were actually broken, written out -------------------

print()
print("b[0:256:-1]:", S[0:256:-1])
print("b[0:300:-1]:", S[0:300:-1])
print("b[5:10:-1]:", S[5:10:-1])
print("b[0:256:-3]:", S[0:256:-3])
print("b[10:5:-1]:", list(S[10:5:-1]))
print("b[::-1] first four:", list(S[::-1])[:4])

# A negative ob_size is the part a length check would not catch on its own:
# the object it produced was malformed, not merely wrong.
for sl in ((0, 256, -1), (5, 10, -1), (0, 300, -2)):
    got = S[slice(*sl)]
    print("%-16s len=%d bytes=%r type=%s"
          % (sl, len(got), got, type(got).__name__))

# --- empty and one-element sequences ----------------------------------------

print()
for seq in (b"", b"a", b"ab", bytearray(b""), bytearray(b"ab")):
    print("%-16r %r %r %r"
          % (seq, seq[0:len(seq):-1], seq[::-1], seq[len(seq):0:-1]))

print("done")
