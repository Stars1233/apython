# An augmented assignment evaluates its target once.  Ours emitted the target
# expression for the load and again for the store, so `d[next(it)] += 5` drew
# two values from the iterator and `obj().n += 5` called obj() twice.  CPython
# duplicates the already-evaluated pieces with COPY and puts them back in
# order with SWAP.
calls = []


def side(v):
    calls.append(v)
    return v


d = {0: 10, 1: 20}
it = iter([0, 1])
d[next(it)] += 5
print(d, list(it))

lst = [10, 20]
it2 = iter([0, 1])
lst[next(it2)] += 5
print(lst, list(it2))


class O:
    def __init__(self):
        self.n = 1
        self.items = {"k": 1}


o = O()


def get_o():
    calls.append("get_o")
    return o


get_o().n += 5
print(o.n, calls)

calls.clear()
get_o().items[side("k")] += 5
print(o.items, calls)

# Every operator, on each target shape.
calls.clear()
box = {"v": 10}
box[side("v")] -= 3
box[side("v")] *= 2
box[side("v")] //= 3
box[side("v")] **= 2
box[side("v")] %= 7
print(box, len(calls))

o.n = 6
o.n &= 3
o.n |= 8
o.n ^= 1
o.n <<= 2
o.n >>= 1
print(o.n)

# Names are unchanged.
x = 1
x += 2
x *= 3
print(x)


def f():
    y = 1
    y += 2
    return y


print(f())

# In-place list and str semantics still hold.
a = [1]
b = a
a += [2]
print(a, b, a is b)

s = "a"
t = s
s += "b"
print(s, t, s is t)

# Nested subscripts.
grid = [[1, 2], [3, 4]]
i = iter([0, 1])
grid[next(i)][next(i)] += 100
print(grid, list(i))
