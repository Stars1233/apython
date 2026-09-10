# `x[i] = v` reports failure with a negative int, and op_store_subscr tests
# the LOW HALF of it.  So a slot that never sets its answer does not fail
# loudly -- it fails exactly when whatever was left in that register happens
# to have bit 31 set.
#
# list_setitem left ob_item there.  While ob_item came from glibc's brk arena
# that was a low address with bit 31 clear and the accident held; a list of
# two hundred thousand elements is served by mmap instead, at an address
# where it does not.  So every container here is built big enough to be
# mmap-backed, which is what makes the test able to see the bug at all --
# the same file over ten-element containers passes against the broken build.
#
# The symptom is "TypeError: item assignment failed without an exception",
# which is op_store_subscr noticing that a slot claimed to fail and left no
# exception behind to say why.
N = 200000
out = []

a = [0] * N
a[0] = 1; a[N-1] = 2; a[N//2] = 3
out.append(("list item", a[0], a[N-1], a[N//2]))
a[1:3] = [7, 8]
out.append(("list slice", a[1], a[2]))
a[::2] = [9] * ((len(a) + 1) // 2)
out.append(("list ext slice", a[0], a[2], a[4]))
del a[::2]
out.append(("list ext del", len(a)))
del a[1]
out.append(("list del", len(a)))

b = bytearray(N)
b[0] = 1; b[N-1] = 2
out.append(("bytearray item", b[0], b[N-1]))
b[1:3] = b"xy"
out.append(("bytearray slice", b[1], b[2]))
del b[1]
out.append(("bytearray del", len(b)))

d = {}
for i in range(N // 4):
    d[i] = i
d[0] = 99
out.append(("dict item", d[0], len(d)))
del d[1]
out.append(("dict del", len(d)))

try:
    import array
    ar = array.array('i', bytes(4 * N))
    ar[0] = 5; ar[len(ar)-1] = 6
    out.append(("array item", ar[0], ar[len(ar)-1]))
except Exception as e:
    out.append(("array", type(e).__name__))

m = memoryview(bytearray(N))
m[0] = 1; m[N-1] = 2
out.append(("memoryview item", m[0], m[N-1]))


class S:
    __slots__ = ("v",)


s = S()
s.v = 1
out.append(("slots setattr", s.v))


class G:
    def __setitem__(self, k, v):
        self.last = (k, v)


g = G()
g[N] = "z"
out.append(("__setitem__", g.last))

for row in out:
    print(row)
