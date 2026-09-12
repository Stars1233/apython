# sys.get_asyncgen_hooks() / sys.set_asyncgen_hooks()
#
# PEP 525's pair.  An event loop sets them so that it learns about every async
# generator started under it, and closes the ones still suspended when it shuts
# down.  CPython's BaseEventLoop.run_forever reads them on its FIRST line:
#
#     old_agen_hooks = sys.get_asyncgen_hooks()
#     sys.set_asyncgen_hooks(firstiter=..., finalizer=...)
#     ...
#     sys.set_asyncgen_hooks(*old_agen_hooks)
#
# so with them missing, CPython's own asyncio could not start at all -- exactly
# the shape time.get_clock_info had.  The result has to round-trip through that
# splat, so it is a two-item tuple and not a namespace.
#
# The hooks themselves fire from the generator: firstiter once, the first time
# an async generator is iterated, and finalizer when one is collected while
# still suspended.  shutdown_asyncgens() is what reads the set they build.

import sys


def drive(coro):
    try:
        coro.send(None)
    except StopIteration as e:
        return e.value
    raise AssertionError("did not finish")


# --- the round trip -------------------------------------------------------
saved = sys.get_asyncgen_hooks()
print(len(saved), "get_asyncgen_hooks answers a pair")
print(saved.firstiter, saved.finalizer, "and names both halves")
print(tuple(saved) == (saved.firstiter, saved.finalizer), "the names match the pair")


# type(agen).__name__ rather than agen.__name__: an async generator does not
# publish its own __name__ in this tree yet, and that is not what is under test.
def first(agen):
    seen.append(("first", type(agen).__name__))


def final(agen):
    seen.append(("final", type(agen).__name__))


sys.set_asyncgen_hooks(firstiter=first, finalizer=final)
now = sys.get_asyncgen_hooks()
print(now.firstiter is first, now.finalizer is final, "keywords round-trip")

sys.set_asyncgen_hooks(*saved)
print(sys.get_asyncgen_hooks() == saved, "the splat form restores them")

sys.set_asyncgen_hooks(first, final)
now = sys.get_asyncgen_hooks()
print(now.firstiter is first, now.finalizer is final, "positional works too")

# One at a time: an omitted argument leaves that half alone, as CPython's does.
sys.set_asyncgen_hooks(firstiter=None)
print(sys.get_asyncgen_hooks().finalizer is final, "finalizer survives a partial set")
sys.set_asyncgen_hooks(*saved)

# A non-callable is refused.
try:
    sys.set_asyncgen_hooks(firstiter=42)
    print(False, "a non-callable firstiter must be refused")
except TypeError:
    print(True, "a non-callable firstiter is refused")
sys.set_asyncgen_hooks(*saved)


# --- firstiter actually fires --------------------------------------------
seen = []
sys.set_asyncgen_hooks(firstiter=first, finalizer=final)


async def counter():
    yield 1
    yield 2


async def main():
    out = []
    async for value in counter():
        out.append(value)
    return out


print(drive(main()), "the generator ran")
print(seen, "firstiter fired once, with the generator")
print(len(seen) == 1, "and exactly once")

# Creating one without iterating it fires nothing.
seen = []
unused = counter()
print(seen, "merely creating an async generator fires nothing")
drive(unused.aclose())

sys.set_asyncgen_hooks(*saved)
print(sys.get_asyncgen_hooks() == saved, "restored at the end")


# --- the finalizer the generator keeps --------------------------------------
#
# firstiter fires once and the FINALIZER is snapshotted beside it, so a
# generator collected after its loop has moved on is still closed by the loop
# that started it.  That snapshot is a reference the generator owns, and it was
# owned by nothing: never released when the generator was freed, so five
# generators took the finalizer's refcount from 3 to 8, and never handed to the
# collector, so a finalizer that referred back to its own generator -- which is
# exactly what a bound method of an event loop holding the generator does --
# was a cycle nothing could break.
#
# And it was never CALLED.  CPython's gen_finalize hands a suspended async
# generator to the finalizer INSTEAD of closing it, because closing it means
# running `await` outside a loop; the hook schedules an aclose on the loop that
# owns it.
import gc


def noop_finalizer(agen):
    pass


sys.set_asyncgen_hooks(firstiter=lambda a: None, finalizer=noop_finalizer)


async def one():
    yield 1
    yield 2


def start_and_drop():
    # Started, so the hooks fire, and then dropped while still suspended.
    it = one().__aiter__()
    try:
        it.__anext__().send(None)
    except StopIteration:
        pass
    del it


before = sys.getrefcount(noop_finalizer)
for _ in range(5):
    start_and_drop()
gc.collect()
print(sys.getrefcount(noop_finalizer) - before, "extra references after five")

# The generator hands the finalizer over to the collector, so a cycle through
# it is collectable.
finalized = []


class Loop:
    def __init__(self):
        self.held = []

    def finalizer(self, agen):
        # What asyncio does: keep the generator so it can be closed later.
        self.held.append(agen)
        finalized.append(1)


loop = Loop()
sys.set_asyncgen_hooks(firstiter=lambda a: None, finalizer=loop.finalizer)
start_and_drop()
gc.collect()
print(len(finalized), "the finalizer ran for a suspended generator")
print(len(loop.held), "and it could keep the generator")

sys.set_asyncgen_hooks(*saved)
print(sys.get_asyncgen_hooks() == saved, "restored again")


# --- and the awaitables can be thrown into ----------------------------------
#
# The finalizer hook's whole purpose is to schedule an aclose, and asyncio does
# that with create_task -- which asks isinstance(x, collections.abc.Coroutine).
# That ABC is a STRUCTURAL check over send, throw, close and __await__, and the
# aclose awaitable had no `throw`, so create_task refused it with "a coroutine
# was expected" and the aclose the hook asked for never ran.  Throwing is also
# how a task awaiting one of these gets CANCELLED.
import collections.abc


async def two():
    try:
        yield 1
    except ValueError as e:
        yield "caught %s" % e


agen = two()
print(isinstance(agen.__anext__(), collections.abc.Coroutine),
      isinstance(agen.aclose(), collections.abc.Coroutine),
      isinstance(agen.athrow(ValueError), collections.abc.Coroutine),
      "all three awaitables are coroutines")

it = two().__aiter__()
first = it.__anext__()
try:
    first.send(None)
except StopIteration as e:
    print(e.value, "the first item")

# An async yield COMPLETES the await, so it arrives as StopIteration's value.
second = it.__anext__()
try:
    second.throw(ValueError("boom"))
    print("NO StopIteration from a throw the generator handled")
except StopIteration as e:
    print(e.value, "what the generator yielded after handling the throw")

# Thrown into an aclose, an exception the generator does not handle propagates.
async def plain():
    yield 1
    yield 2


def started(agen_factory):
    it = agen_factory().__aiter__()
    try:
        it.__anext__().send(None)
    except StopIteration:
        pass
    return it


closer = started(plain).aclose()
try:
    closer.throw(ValueError("cancel"))
    print("NO ERROR from a throw into aclose")
except ValueError as e:
    print("ValueError:", e, "propagates out of an aclose")
except BaseException as e:
    print("UNEXPECTED", type(e).__name__, e)

# Whether an awaitable whose throw already propagated may be thrown into again
# is not pinned here: 3.12.3 re-raises and 3.12.14 refuses with "cannot reuse
# already awaited aclose()/athrow()", so the answer would be a test of which
# patch level is installed.

# A generator that CATCHES what aclose throws and yields anyway has ignored the
# shutdown, which is its own error rather than an answer.
catcher = started(two).aclose()
try:
    catcher.throw(ValueError("cancel"))
    print("NO ERROR from a generator that kept going")
except RuntimeError as e:
    print("RuntimeError:", e)
except BaseException as e:
    print("UNEXPECTED", type(e).__name__, e)
