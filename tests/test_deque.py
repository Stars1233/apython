# collections.deque.
#
# List-backed, where CPython's is a block-linked list: appendleft and popleft
# are O(1) there and O(n) here, and every observable answer is the same.

from collections import deque
d = deque([1, 2, 3])
print(d, len(d), list(d), d[0], d[-1])
d.append(4); d.appendleft(0)
print(d, d.pop(), d.popleft(), d)
d.extend([7, 8]); d.extendleft([9])
print(d, 7 in d, 99 in d)
d.rotate(); print(d)
d.rotate(-2); print(d)
d.reverse(); print(d)
print(d.count(7), d.index(7))
m = deque([1, 2, 3], 3)
m.append(4); print(m, m.maxlen)
m.appendleft(0); print(m)
print(deque(), bool(deque()), bool(deque([1])))
print(deque([1, 2]) == deque([1, 2]), deque([1]) == deque([2]), deque([1]) == [1])
print(deque([1]) + deque([2]))
print(list(reversed(deque([1, 2, 3]))))
c = deque([1, 2]); c += [3]; print(c, c.copy())
try:
    deque().pop()
except IndexError:
    print("empty pop => IndexError")

# The three accelerated names live in _collections, and collections imports
# them back -- CPython's own arrangement, and not a detail: CPython's
# collections/__init__.py does `from _collections import deque` in a
# try/except and exports the name from __all__ either way, so with no
# _collections module at all `from collections import deque` was an
# ImportError under a real stdlib rather than a slower deque.  contextlib,
# shlex and getpass could not be imported for want of it.
import _collections
import collections

# Only what is true of CPython's _collections too: it is the C accelerator
# there, so its contents differ, but these three names are the same objects
# collections re-exports.
print("same deque:", collections.deque is _collections.deque)
print("same defaultdict:", collections.defaultdict is _collections.defaultdict)
print("same OrderedDict:", collections.OrderedDict is _collections.OrderedDict)

from _collections import deque as direct_deque
print("direct:", list(direct_deque([1, 2, 3])))

# The rest of collections is unchanged and still reachable.
for name in ("Counter", "ChainMap", "namedtuple", "deque", "defaultdict",
             "OrderedDict"):
    print(name, hasattr(collections, name))
