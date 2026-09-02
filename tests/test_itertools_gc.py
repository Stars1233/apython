# The wrapper iterators are visible to the collector.
#
# enumerate, zip, map, filter, reversed and chain all came from ap_malloc
# with tp_flags 0, so a cycle through one leaked outright: none of them had a
# tp_traverse for the collector to walk, and
#
#     a = []; a.append(zip(a, a))
#
# collected nothing.  The plain container iterators were tracked earlier and
# share one traverse/clear pair, because each keeps exactly one owned pointer
# at the same offset.  Only enumerate and reversed fit that shape; zip and
# chain keep an array of iterators, map keeps an array AND a Value for the
# function, and filter's function slot is legitimately NULL for
# filter(None, xs) -- so a clear must not read 0 there as "already cleared".
#
# The counts are not compared against CPython's: this collector reports a
# two-object cycle as one where CPython reports two, which CLAUDE.md records.
# What is compared is whether anything was collected at all.

import gc
import itertools


def leaks(make):
    gc.collect()
    make()
    return gc.collect() > 0


def through(f):
    def make():
        a = []
        a.append(f(a))
    return make


print("=== a cycle through each is collectable ===")
print("zip      ", leaks(through(lambda a: zip(a, a))))
print("zip one  ", leaks(through(lambda a: zip(a))))
print("map      ", leaks(through(lambda a: map(str, a))))
print("map many ", leaks(through(lambda a: map(min, a, a, a))))
print("filter   ", leaks(through(lambda a: filter(None, a))))
print("filter fn", leaks(through(lambda a: filter(bool, a))))
print("enumerate", leaks(through(enumerate)))
print("reversed ", leaks(through(reversed)))
print("chain    ", leaks(through(lambda a: itertools.chain(a, a))))

# The function a map or filter holds can be the cycle instead of the iterable.
def through_the_function(wrap):
    def make():
        held = []
        def f(x):
            return held
        held.append(wrap(f))
    return make


print("map's func   ", leaks(through_the_function(lambda f: map(f, []))))
print("filter's func", leaks(through_the_function(lambda f: filter(f, []))))

print("=== and they all still work ===")
print(list(zip([1, 2], [3, 4])), list(zip()), list(zip([1])))
print(list(map(str, [1, 2])), list(map(min, [1, 5], [4, 2])))
print(list(filter(None, [0, 1, 2])), list(filter(bool, [0, 1])))
print(list(enumerate("ab")), list(enumerate("ab", 5)))
print(list(reversed([1, 2, 3])), list(reversed("abc")))
print(list(itertools.chain([1], [2, 3])), list(itertools.chain()))
print(list(reversed(range(3))), sorted([3, 1, 2]))

# Exhausting one and dropping it is still just refcounting.
z = zip([1], [2])
print(list(z), list(z))
