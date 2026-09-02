# A generator dropped mid-flight runs its finally.
#
# gen_dealloc freed the frame outright, so a generator suspended inside a
# try/finally -- or a with block, which is the same thing -- never reached
# the cleanup.  CPython closes a generator when it is collected, and code
# that opens a file or takes a lock in a generator relies on it.
#
# Doing it here means running Python inside a dealloc, which needs three
# things, all of them visible in the cases below: a resurrection bump, so the
# cleanup taking and dropping a reference to the generator does not re-enter
# the dealloc at refcount zero; the pending exception saved and put back,
# because a dealloc runs at arbitrary points including the middle of somebody
# else's raise; and no unwinding, because a dealloc is called from arbitrary
# depth -- including from inside the collector's own release loop.
#
# The cases that RAISE from a cleanup are exercised separately rather than
# here: the behaviour is right (the exception is reported and dropped, and
# the interpreter carries on) but the stderr wording is a one-line note where
# CPython prints a full traceback, the same gap "Exception ignored in
# __del__" already has.

import gc

log = []


def suspended():
    try:
        yield 1
        yield 2
    finally:
        log.append("finally")


print("=== dropped mid-flight ===")
it = suspended()
next(it)
del it
gc.collect()
print(log)

print("=== exhausted, which needs no cleanup on drop ===")
del log[:]
it = suspended()
for _ in it:
    pass
del it
gc.collect()
print(log)

print("=== never started ===")
del log[:]
it = suspended()
del it
gc.collect()
print(log)

print("=== a with block is the same thing ===")
del log[:]


class Guard:
    def __enter__(self):
        log.append("enter")
        return self

    def __exit__(self, *a):
        log.append("exit")
        return False


def guarded():
    with Guard():
        yield 1
        yield 2


it = guarded()
next(it)
del it
gc.collect()
print(log)

print("=== close() still does it explicitly, and only once ===")
del log[:]
it = suspended()
next(it)
it.close()
print(log)
it.close()
del it
gc.collect()
print("after a second close and a drop:", log)

print("=== a cycle through a suspended generator ===")
del log[:]


def in_a_cycle():
    a = []
    it = suspended()
    next(it)
    a.append(it)          # the list holds the generator, whose frame holds it


gc.collect()
in_a_cycle()
gc.collect()
print(log)

print("=== the cleanup does not disturb an exception already in flight ===")
del log[:]
try:
    it = suspended()
    next(it)
    del it
    raise KeyError("outer")
except KeyError as e:
    gc.collect()
    print(type(e).__name__, str(e), log)

print("=== and generators still work ===")
def counter(n):
    total = 0
    for i in range(n):
        total += i
        yield total

print(list(counter(5)))
g = counter(3)
print(next(g), next(g), next(g))
try:
    next(g)
except StopIteration:
    print("StopIteration")


def delegating():
    yield from counter(3)
    yield "after"


print(list(delegating()))
