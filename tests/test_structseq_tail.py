# A struct sequence takes its named-only tail from the constructor too.
#
# time.struct_time declares nine fields in the sequence and eleven in all --
# tm_zone and tm_gmtoff are reachable by name only.  CPython's constructor
# accepts any length between the two and leaves the rest None; ours demanded
# exactly nine, which is why `time.struct_time(tt[:11])` -- the last line of
# _strptime.py, and so every strptime call in the stdlib -- raised TypeError.
#
# The wording changes with the shape: a struct sequence whose two counts are
# equal says "takes a N-sequence", one where they differ says "at least" or
# "at most".

import os
import time

tt = (2026, 9, 10, 12, 0, 0, 3, 253, 0, "UTC", 7200)

for n in range(7, 13):
    try:
        st = time.struct_time(tt[:n])
    except TypeError as e:
        print(n, "TypeError:", e)
    else:
        print(n, len(st), st.tm_year, st.tm_isdst, st.tm_zone, st.tm_gmtoff)

try:
    time.struct_time(tt + (1,))
except TypeError as e:
    print("TypeError:", e)

# A short one leaves the tail None, and the tuple half is unaffected.
st9 = time.struct_time(tt[:9])
print(tuple(st9), st9.tm_zone, st9.tm_gmtoff)

# The full one round-trips through the tuple protocol, which only ever sees
# the nine.
st11 = time.struct_time(tt)
print(tuple(st11) == tt[:9], len(st11), st11[0], st11[-1])
print(st11.tm_zone, st11.tm_gmtoff)
print(st9 == st11)

# repr shows all eleven, tail included.
print(repr(st11))
print(repr(st9))

# A struct sequence with no tail at all keeps the exact-length wording.
ts = os.terminal_size((80, 24))
print(tuple(ts), ts.columns, ts.lines)
for bad in ((80,), (80, 24, 1)):
    try:
        os.terminal_size(bad)
    except TypeError as e:
        print("TypeError:", e)

# Any iterable, not just a tuple.
print(tuple(time.struct_time(iter(tt))), time.struct_time(list(tt)).tm_zone)


def gen():
    yield from tt[:10]


print(time.struct_time(gen()).tm_zone, time.struct_time(gen()).tm_gmtoff)

# A raising __next__ propagates rather than becoming a length error.
class Boom(Exception):
    pass


def bad_gen():
    yield from tt[:5]
    raise Boom("from __next__")


try:
    time.struct_time(bad_gen())
except Boom as e:
    print("propagated:", e)

# Not a sequence at all.
try:
    time.struct_time(5)
except TypeError as e:
    print("TypeError:", e)

print("done")
