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
