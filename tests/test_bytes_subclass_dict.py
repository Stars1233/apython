# A bytes subclass keeps its data inline exactly as a str subclass does, so
# its __dict__ goes at the tail too.  Putting one at the base's basicsize
# landed it *inside* the data, and setting any attribute corrupted the object.
class B(bytes):
    pass


b = B(b"xy")
print(b, len(b), type(b).__name__)
b.attr = 5
print(b.attr, sorted(b.__dict__))
print(b, len(b), b == b"xy", b[0], b[-1])

long = B(b"abcdefghijklmnopqrstuvwxyz")
long.tag = "t"
print(long, len(long), long.tag, long.decode())

empty = B(b"")
empty.z = 1
print(empty, len(empty), empty.z)


class C(bytes):
    def __init__(self, v):
        self.seen = len(self)


c = C(b"abc")
print(c, c.seen)


# Many of them at once, so a corrupted neighbour would show.
items = []
for i in range(50):
    x = B(bytes([i]) * (i + 1))
    x.n = i
    items.append(x)
print(sum(v.n for v in items), len(items[49]), items[49].n)
print(all(len(v) == i + 1 for i, v in enumerate(items)))

# The ordinary bytes type is unchanged and still has no dict.
plain = b"xy"
try:
    plain.attr = 1
except AttributeError:
    print("plain bytes rejects an attribute")
print(plain, len(plain), plain.hex())

# A bytearray subclass is resizable, so it cannot carry a tail dict; it gets
# none at all rather than one that lands inside the buffer.  (CPython gives it
# a real __dict__; we refuse the attribute instead of corrupting the object.)
class BA(bytearray):
    pass


ba = BA(b"pq")
print(bytes(ba), len(ba), type(ba).__name__)
