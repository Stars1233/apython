# select.epoll, and the selector that now picks it.
#
# The module wrapped poll() only, and said so: "epoll, kqueue and devpoll are
# absent, and their absence is the supported way to say so".  That is true of
# kqueue and devpoll, which this kernel does not have -- epoll is Linux's own
# and is what every modern Python program on Linux ends up using, through
# selectors.DefaultSelector and through asyncio under it.
#
# Adding it CHANGES that default: Lib/selectors.py gates on
# `hasattr(select, "epoll")`, so DefaultSelector is EpollSelector from here
# on.  That is the reason this test exercises the selector layer as well as
# the syscalls -- a backend nothing runs is a backend nothing checks.
#
# struct epoll_event is PACKED on x86-64: a u32 of events then the 8-byte
# union, twelve bytes and not sixteen.  Writing it as two aligned words puts
# the descriptor four bytes past where the kernel reads it, and every event
# comes back for descriptor zero.
import os
import select
import selectors

print("epoll is present:", hasattr(select, "epoll"))
print("DefaultSelector:", selectors.DefaultSelector.__name__)
print("the constants:", hex(select.EPOLLIN), hex(select.EPOLLOUT),
      hex(select.EPOLLHUP), hex(select.EPOLLET), hex(select.EPOLLONESHOT))

# --- the object's own lifecycle ---------------------------------------
ep = select.epoll()
print("fileno is a descriptor:", ep.fileno() > 2)
print("closed:", ep.closed)
ep.close()
print("closed after close:", ep.closed)
ep.close()
print("close is idempotent")
for call, what in ((lambda: ep.poll(0), "poll"),
                   (lambda: ep.fileno(), "fileno"),
                   (lambda: ep.register(0), "register")):
    try:
        call()
        print("%-9s after close: NOT REFUSED" % what)
    except ValueError as exc:
        print("%-9s after close: %s" % (what, exc))

with select.epoll() as e:
    print("context manager:", e.fileno() > 2, e.closed)
print("closed on exit:", e.closed)

# --- registering, waiting, modifying, unregistering -------------------
r, w = os.pipe()
try:
    ep = select.epoll()
    ep.register(r, select.EPOLLIN)
    print("nothing written yet:", ep.poll(0))
    os.write(w, b"x")
    ready = ep.poll(0)
    print("after a write:", [(fd == r, ev & select.EPOLLIN != 0)
                             for fd, ev in ready], len(ready))
    print("the descriptor is the one registered:", ready[0][0] == r)
    os.read(r, 1)
    print("after reading it back:", ep.poll(0))

    # A second descriptor, to show the pairs are not positional
    r2, w2 = os.pipe()
    ep.register(r2, select.EPOLLIN)
    os.write(w2, b"y")
    ready = dict(ep.poll(0))
    print("two registered, one ready:", sorted(ready) == [r2], len(ready))
    os.write(w, b"z")
    ready = dict(ep.poll(0))
    print("both ready:", sorted(ready) == sorted([r, r2]))

    ep.modify(r, 0)
    print("modified to no events:", sorted(dict(ep.poll(0))) == [r2])
    ep.unregister(r)
    ep.unregister(r2)
    print("after unregistering both:", ep.poll(0))

    # Registering twice is an error; unregistering what is not there is too.
    ep.register(r, select.EPOLLIN)
    try:
        ep.register(r, select.EPOLLIN)
        print("double register: NOT REFUSED")
    except OSError as exc:
        print("double register:", exc.errno == 17)
    ep.unregister(r)
    try:
        ep.unregister(r)
        print("double unregister: NOT REFUSED")
    except OSError as exc:
        print("double unregister:", exc.errno == 2)

    # maxevents caps the batch
    ep.register(r, select.EPOLLIN)
    ep.register(r2, select.EPOLLIN)
    print("maxevents caps it:", len(ep.poll(0, 1)))
    print("and the default does not:", len(ep.poll(0)))
    ep.close()

    # --- the same work through selectors, on BOTH backends ------------
    for cls in (selectors.PollSelector, selectors.EpollSelector):
        sel = cls()
        sel.register(r, selectors.EVENT_READ, data="r")
        sel.register(r2, selectors.EVENT_READ, data="r2")
        events = sel.select(0)
        got = sorted((k.fileobj, k.data) for k, _ in events)
        print("%-14s %s" % (cls.__name__, got))
        print("%-14s get_key: %s, len: %d"
              % (cls.__name__, sel.get_key(r).data, len(sel.get_map())))
        sel.unregister(r)
        print("%-14s after unregister: %d"
              % (cls.__name__, len(sel.select(0))))
        sel.close()

    # --- a timeout that expires ---------------------------------------
    import time

    os.read(r, 1)
    os.read(r2, 1)
    ep = select.epoll()
    ep.register(r, select.EPOLLIN)
    start = time.monotonic()
    out = ep.poll(0.05)
    waited = time.monotonic() - start
    print("an expiring timeout:", out, waited >= 0.03)
    ep.close()

    os.close(r2)
    os.close(w2)
finally:
    os.close(r)
    os.close(w)

# --- what must be refused ---------------------------------------------
ep = select.epoll()
try:
    ep.poll(0, 0)
    print("maxevents 0: accepted (clamped)")
except ValueError as exc:
    print("maxevents 0:", exc)
try:
    ep.register(-1, select.EPOLLIN)
    print("a negative descriptor: NOT REFUSED")
except ValueError as exc:
    print("a negative descriptor:", exc)
try:
    select.poll().register(-1)
    print("and poll agrees: NOT REFUSED")
except ValueError as exc:
    print("and poll agrees:", exc)
ep.close()
print("survived")
