# The FOR_ITER_LIST specialization has to answer for an iterator that is
# ALREADY exhausted.
#
# list_iter_next clears it_seq when it runs out, precisely so that a __del__
# re-entering the iterator finds it empty rather than pointing at storage being
# freed -- and it tests for that NULL on the way in.  The specialization read
# ob_size off it without testing, so the second `for` over an iterator that the
# first had already drained dereferenced address zero.
#
# It takes two loops over one iterator, and the site has to be WARM: the first
# call through the function is what rewrites FOR_ITER into FOR_ITER_LIST, so a
# single call never reaches the specialized handler at all.  itertools.dropwhile
# is written this way and `list(dropwhile(pred, []))` after any other dropwhile
# was a segfault.

from itertools import dropwhile


def two_loops(seq):
    it = iter(seq)
    seen = []
    for x in it:
        seen.append(("first", x))
        break
    for x in it:
        seen.append(("second", x))
    return seen


print("warm:", two_loops([1, 2, 3]))
print("empty after warm:", two_loops([]))
print("one after warm:", two_loops([9]))
print("empty again:", two_loops([]))


def underten(x):
    return x < 10


print("dropwhile data:", list(dropwhile(underten, [1, 3, 5, 20, 2, 4])))
print("dropwhile empty:", list(dropwhile(underten, [])))
print("dropwhile all-true:", list(dropwhile(underten, [1, 2, 3])))
print("dropwhile empty again:", list(dropwhile(underten, [])))

# The same shape without itertools: an exhausted iterator handed back to a
# for-loop, over and over, so the site is thoroughly warm.
def drain_twice(seq):
    it = iter(seq)
    a = list(it)
    b = list(it)
    return a, b


for trial in ([1, 2], [], [3], [], []):
    print("drain:", drain_twice(trial))
print("done")
