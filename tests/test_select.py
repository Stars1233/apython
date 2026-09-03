# select: the two interfaces selectors knows how to use.
#
# Both are over the same poll() primitive.  Everything here is a socketpair,
# which is a descriptor pair with no port and no listener, so nothing can
# collide and nothing can hang: a test that blocks on the wrong answer is a
# test that fails by timing out, which is the worst kind.

import select
import _socket

print("=== the module ===")
print(select.error is OSError)
print(select.POLLIN, select.POLLPRI, select.POLLOUT, select.POLLERR,
      select.POLLHUP, select.POLLNVAL)
print(hasattr(select, "select"), hasattr(select, "poll"))

a, b = _socket.socketpair()

print("=== select ===")
print("nothing readable", select.select([a, b], [], [], 0))
r, w, x = select.select([], [a, b], [], 0)
print("both writable", r == [], sorted([s.fileno() for s in w]) == sorted([a.fileno(), b.fileno()]), x == [])
a.send(b"one")
r, w, x = select.select([b], [], [], 1.0)
print("readable", r == [b], b.recv(8))
print("empty lists", select.select([], [], [], 0))

# The objects come back, not their descriptors -- select is indexed by what
# the caller passed in, and a raw int is passed through unchanged.
r, w, x = select.select([], [a.fileno()], [], 0)
print("raw fd", r, w == [a.fileno()], x)

try:
    select.select([a], [], [], -1)
    print("negative timeout accepted")
except (ValueError, OSError):
    print("negative timeout rejected")

class Fake:
    def __init__(self, fd):
        self._fd = fd
    def fileno(self):
        return self._fd

f = Fake(a.fileno())
r, w, x = select.select([], [f], [], 0)
print("fileno object", w == [f])

print("=== poll ===")
p = select.poll()
p.register(b, select.POLLIN)
print("nothing", p.poll(0))
a.send(b"two")
got = p.poll(0)
print("ready", got == [(b.fileno(), select.POLLIN)])
print("blocking read", b.recv(8))
print("again", p.poll(0))

p.register(a, select.POLLOUT)
out = sorted(p.poll(0))
print("writable", out == [(a.fileno(), select.POLLOUT)])
p.modify(a, select.POLLIN)
print("modified", p.poll(0))
p.unregister(a)
try:
    p.unregister(a)
    print("second unregister accepted")
except KeyError:
    print("second unregister rejected")
try:
    p.modify(a, select.POLLIN)
    print("modify of an unregistered fd accepted")
except OSError:
    print("modify of an unregistered fd rejected")

print("=== a closed peer ===")
c, d = _socket.socketpair()
c.close()
r, w, x = select.select([d], [], [], 0.5)
print("hangup is readable", r == [d], d.recv(8))
d.close()

a.close()
b.close()
print("done")
