# islice consumes exactly the elements it is asked for.
#
# It pulled the element at the stop index and only then noticed it was past
# the end, so a second islice over the same iterator started one late:
# `it = iter([1,2,3,4]); islice(it,2)` gave [1,2] and left the iterator at 4.
# The index arithmetic was also done after skipping `start`, which is not
# where CPython counts from.

from itertools import islice, count

it = iter([1, 2, 3, 4, 5, 6])
print("first two:", list(islice(it, 2)))
print("rest:", list(islice(it, 10)))
print("after:", list(it))

it = iter(range(10))
print("chunks:", [list(islice(it, 3)) for _ in range(4)])

# start / stop / step, and the element it stops before
print("stop only:", list(islice("ABCDEFG", 2)))
print("start stop:", list(islice("ABCDEFG", 2, 4)))
print("with step:", list(islice("ABCDEFG", 0, None, 2)))
print("start step:", list(islice("ABCDEFG", 1, 6, 2)))
print("stop None:", list(islice("ABC", 0, None)))
print("empty range:", list(islice("ABCDEFG", 2, 2)))
print("past the end:", list(islice("AB", 0, 10)))
print("start past end:", list(islice("AB", 5, 10)))

# How much it took, measured on a counting source
taken = []


def counted(n):
    for i in range(n):
        taken.append(i)
        yield i


taken.clear()
list(islice(counted(100), 3))
print("took for stop=3:", len(taken))
taken.clear()
list(islice(counted(100), 2, 5))
print("took for 2..5:", len(taken))
taken.clear()
list(islice(counted(100), 0, 6, 2))
print("took for 0..6 step 2:", len(taken))

# An infinite source is fine as long as stop is finite.
print("from count():", list(islice(count(), 5)))
print("done")
