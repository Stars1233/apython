# collections.deque: the whole surface, and the cost of its two ends.
#
# The old one was list-backed with no window: appendleft inserted at index 0
# and popleft deleted index 0, so both were O(n).  deque is THE queue in
# Python, so that made every breadth-first search quadratic -- and the
# docstring admitted it rather than fixing it.  It also had no __slots__, so
# every instance carried a __dict__ and a subclass declaring
# `__slots__ = ("x", "y", "__dict__")` -- which CPython's own test_deque does
# -- was refused with "__dict__ slot disallowed: we already got one", taking
# the whole module down at import.
#
# And it was missing __mul__, __rmul__, __imul__, __copy__, __reduce__,
# __class_getitem__, the four ordering comparisons and __hash__ = None.
import copy
import time
from collections import deque

# --- construction and repr ---------------------------------------------
print(deque())
print(deque([1, 2, 3]))
print(deque("abc"))
print(deque([1, 2, 3], 2))
print(deque(maxlen=0))
print(deque([1, 2, 3]).maxlen, deque([1, 2, 3], 5).maxlen)
try:
    deque([], -1)
except ValueError as e:
    print("negative maxlen:", e)

# --- the two ends ------------------------------------------------------
d = deque()
for i in range(5):
    d.append(i)
    d.appendleft(-i)
print("both ends:", d)
print("pop/popleft:", d.pop(), d.popleft(), d)
d.extend([9, 8])
d.extendleft([7, 6])
print("extend both:", d)
e = deque()
try:
    e.pop()
except IndexError as ex:
    print("pop empty:", ex)
try:
    e.popleft()
except IndexError as ex:
    print("popleft empty:", ex)

# --- maxlen, on both sides ---------------------------------------------
m = deque(maxlen=3)
m.extend([1, 2, 3, 4, 5])
print("maxlen extend:", m)
m2 = deque(maxlen=3)
m2.extendleft([1, 2, 3, 4, 5])
print("maxlen extendleft:", m2)
m3 = deque([1, 2, 3], maxlen=3)
m3.append(4)
print("push right:", m3)
m3.appendleft(0)
print("push left:", m3)
z = deque(maxlen=0)
z.append(1)
z.appendleft(2)
print("maxlen 0:", z, len(z))
try:
    deque([1, 2, 3], 3).insert(1, 9)
except IndexError as ex:
    print("insert at max:", ex)

# --- indexing, slicing-by-index, deletion ------------------------------
d = deque("abcdef")
print("index:", d[0], d[-1], d[3])
d[1] = "B"
print("setitem:", d)
del d[2]
print("delitem:", d)
del d[-1]
print("delitem neg:", d)
try:
    d[99]
except IndexError as ex:
    print("out of range:", ex)
print("contains:", "a" in d, "z" in d)
print("count:", deque("abcabc").count("a"))
print("index of:", deque("abcabc").index("c"), deque("abcabc").index("c", 3))
try:
    deque("abc").index("z")
except ValueError as ex:
    print("index missing:", ex)
r = deque("abc")
r.remove("b")
print("remove:", r)
try:
    r.remove("z")
except ValueError as ex:
    print("remove missing:", ex)

# --- insert, reverse, rotate -------------------------------------------
i = deque("abc")
i.insert(1, "X")
print("insert:", i)
i.insert(0, "Y")
print("insert 0:", i)
i.insert(99, "Z")
print("insert past end:", i)
i.insert(-99, "W")
print("insert before start:", i)
v = deque("abcde")
v.reverse()
print("reverse:", v)
for n in (1, -1, 2, -2, 0, 7, -7):
    w = deque("abcde")
    w.rotate(n)
    print("rotate %-3d %s" % (n, w))
w = deque()
w.rotate(3)
print("rotate empty:", w)

# --- iteration ---------------------------------------------------------
print("iter:", list(deque("abc")))
print("reversed:", list(reversed(deque("abc"))))
print("bool:", bool(deque()), bool(deque([0])))
print("len:", len(deque("abcd")))

# --- comparison --------------------------------------------------------
print("eq:", deque([1, 2]) == deque([1, 2]), deque([1, 2]) == deque([1, 3]))
print("ne:", deque([1, 2]) != deque([1, 3]))
print("lt:", deque([1, 2]) < deque([1, 3]), deque([1, 2]) < deque([1, 2]))
print("le:", deque([1, 2]) <= deque([1, 2]))
print("gt:", deque([2]) > deque([1]))
print("ge:", deque([2]) >= deque([2]))
print("vs list:", deque([1, 2]) == [1, 2])
print("hashable:", deque.__hash__ is None)

# --- arithmetic --------------------------------------------------------
print("add:", deque([1]) + deque([2]))
a = deque([1])
a += deque([2])
print("iadd:", a)
a += [3]
print("iadd list:", a)
print("mul:", deque([1, 2]) * 3)
print("rmul:", 3 * deque([1, 2]))
print("mul 0:", deque([1, 2]) * 0)
print("mul neg:", deque([1, 2]) * -1)
b = deque([1, 2])
b *= 3
print("imul:", b)
c = deque([1, 2])
c *= 0
print("imul 0:", c)

# --- copying and pickling protocol -------------------------------------
o = deque([1, [2], 3], 5)
print("copy:", o.copy(), o.copy().maxlen)
print("copy.copy:", copy.copy(o))
dc = copy.deepcopy(o)
dc[1].append(9)
print("deepcopy independent:", o, dc)
# __reduce__ is compared by what it REBUILDS, not by its shape: CPython
# answers a four-tuple whose last element is a _deque_iterator, so pickle
# appends lazily, and this answers the two-tuple (type, (items, maxlen)).
# Both reconstruct the same deque, which is the contract.
for src in (o, deque([1]), deque(maxlen=2), deque("ab", 4)):
    r = src.__reduce__()
    rebuilt = r[0](*r[1])
    if len(r) > 3 and r[3] is not None:      # CPython's lazy-append form
        rebuilt.extend(r[3])
    print("reduce rebuilds:", list(rebuilt) == list(src),
          rebuilt.maxlen == src.maxlen)
print("class_getitem:", deque[int])

# --- clear, and reuse afterwards ---------------------------------------
k = deque("abc")
k.clear()
print("clear:", k, len(k))
k.append(1)
k.appendleft(0)
print("reuse:", k)

# --- a subclass, with slots and with a dict ----------------------------
class WithSlots(deque):
    __slots__ = ("x", "y", "__dict__")


ws = WithSlots([1, 2])
ws.x = 1
ws.anything = 2
print("subclass slots:", list(ws), ws.x, ws.anything)


class Plain(deque):
    pass


print("subclass copy type:", type(Plain([1]).copy()).__name__)
print("subclass repr:", repr(Plain([1])))

# --- self-referential extend must terminate ----------------------------
se = deque([1, 2])
se.extend(se)
print("extend self:", se)
sl = deque([1, 2])
sl.extendleft(sl)
print("extendleft self:", sl)

# --- and the reason for all of it: the ends are not O(n) ---------------
# A million operations at each end.  Quadratic would not finish; the ratio
# against the same count of append/pop is what is printed, rounded hard
# enough to be stable on any machine.
N = 200000


def timed(fn):
    t = time.monotonic()
    fn()
    return time.monotonic() - t


right = timed(lambda: [deque().append(i) for i in range(0)])  # warm
q = deque()
right = timed(lambda: [q.append(i) for i in range(N)])
left = timed(lambda: [q.appendleft(i) for i in range(N)])
popl = timed(lambda: [q.popleft() for i in range(N)])
popr = timed(lambda: [q.pop() for i in range(N)])
print("ends within 20x of each other:",
      left < right * 20 + 0.5, popl < popr * 20 + 0.5)
print("empty after:", len(q))
print("survived")
