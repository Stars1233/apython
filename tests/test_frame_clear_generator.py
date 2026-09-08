"""`frame.clear()` on a suspended generator closes it.

frameobj_method_clear refused any frame still attached to a live PyFrame, so a
suspended generator's frame could not be cleared.  CPython closes it: the
finally blocks run, gi_frame becomes None, and a later next() raises
StopIteration.

`gi_frame` is here too.  It did not exist -- the comment refusing it said a
PyFrame "is pooled and recycled and is not an object with a type, so there is
nothing to hand back" -- but frameobj_for has handed out an owned frame object
for a live pooled PyFrame ever since sys._getframe was written on it.  Without
gi_frame the bug was only reachable through sys._getframe() inside the
generator, which is not how anyone writes it.
"""

import gc
import sys


def gen(log):
    try:
        yield 1
        yield 2
    finally:
        log.append("finally")


print("--- gi_frame exists ---")
g = gen([])
print("before start:", g.gi_frame is not None)
next(g)
print("suspended:", g.gi_frame is not None)
print("is a frame:", type(g.gi_frame).__name__)
print("code matches:", g.gi_frame.f_code is g.gi_code)
print("f_lineno is an int:", isinstance(g.gi_frame.f_lineno, int))
list(g)
print("exhausted:", g.gi_frame)

print("--- clearing a suspended generator closes it ---")
log = []
g = gen(log)
print("first:", next(g))
g.gi_frame.clear()
print("finally ran:", log)
print("gi_frame now:", g.gi_frame)
try:
    next(g)
    print("still running - wrong")
except StopIteration:
    print("exhausted after clear")

print("--- clearing twice is fine ---")
log = []
g = gen(log)
next(g)
g.gi_frame.clear()
print("second clear:", g.gi_frame)

print("--- an unstarted generator ---")
log = []
g = gen(log)
f = g.gi_frame
f.clear()
print("unstarted cleared:", log, g.gi_frame)
try:
    next(g)
    print("ran - wrong")
except StopIteration:
    print("unstarted then exhausted")

print("--- an exhausted generator has no frame to clear ---")
g = gen([])
list(g)
print("no frame:", g.gi_frame)

print("--- clear() on an executing frame is still refused ---")


def executing():
    f = sys._getframe()
    try:
        f.clear()
        return "cleared - wrong"
    except RuntimeError as e:
        return str(e)


print("executing:", executing())

print("--- a plain frame from sys._getframe ---")


def outer():
    return sys._getframe()


fr = outer()
print("detached frame:", type(fr).__name__)
fr.clear()
print("cleared a dead frame: ok")

print("--- coroutines expose cr_frame ---")


async def coro():
    return 1


c = coro()
print("cr_frame:", c.cr_frame is not None, type(c.cr_frame).__name__)
c.close()

print("--- generator locals go away ---")


def holder(box):
    big = box
    try:
        yield 1
    finally:
        pass


class Marker:
    def __del__(self):
        released.append(1)


released = []
m = Marker()
g = holder(m)
next(g)
del m
g.gi_frame.clear()
gc.collect()
print("local released:", released == [1])

print("done")
