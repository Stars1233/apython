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
