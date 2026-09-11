# What __anext__ returns has to be an awaitable, and saying so is what keeps
# `async for` from spinning.
#
# GET_ANEXT pushed whatever __anext__ answered and let the SEND that follows
# drive it.  For a plain int that loop never ended: the process spun with no
# output, no traceback and no exit -- a hang is the one failure that leaves
# no diagnostic at all.  CPython runs _PyCoro_GetAwaitableIter over the value
# first and raises TypeError naming the type.
#
# The same helper answers GET_AWAITABLE, whose refusal CPython words three
# ways depending on which syntax asked: a bare `await`, the value __aenter__
# returned, and __aexit__'s.  Both `async with` forms say "does not implement
# __await__", which is the actual advice.
#
# Driven with .send(None) rather than an event loop: the defect is in the
# opcode, and a loop only adds a scheduler between the two.


def drive(fn, label):
    c = fn()
    try:
        while True:
            c.send(None)
    except StopIteration:
        print(label, "stopped")
    except TypeError as e:
        print(label, "TypeError:", e)
    except Exception as e:
        print(label, type(e).__name__ + ":", e)


class BadAnext:
    def __aiter__(self):
        return self

    def __anext__(self):
        return 123


class RaisingAwait:
    def __aiter__(self):
        return self

    def __anext__(self):
        return self

    def __await__(self):
        raise ValueError("from __await__")


class NonIterAwait:
    def __aiter__(self):
        return self

    def __anext__(self):
        return self

    def __await__(self):
        return 5


class BadAiter:
    def __aiter__(self):
        return 42


class NoAiter:
    pass


class BadAenter:
    def __aenter__(self):
        return 7

    def __aexit__(self, *a):
        return 7


class BadAexit:
    async def __aenter__(self):
        return 1

    def __aexit__(self, *a):
        return 7


async def anext_int():
    async for i in BadAnext():
        print(i)


async def anext_raises():
    async for i in RaisingAwait():
        print(i)


async def anext_non_iter():
    async for i in NonIterAwait():
        print(i)


async def aiter_int():
    async for i in BadAiter():
        print(i)


async def no_aiter():
    async for i in NoAiter():
        print(i)


async def await_int():
    await 5


async def aenter_int():
    async with BadAenter():
        pass


async def aexit_int():
    async with BadAexit():
        pass


for fn, label in (
    (anext_int, "anext-int"),
    (anext_raises, "anext-raises"),
    (anext_non_iter, "anext-noniter"),
    (aiter_int, "aiter-int"),
    (no_aiter, "no-aiter"),
    (await_int, "await-int"),
    (aenter_int, "aenter-int"),
    (aexit_int, "aexit-int"),
):
    drive(fn, label)


# The shapes that must keep working.  An object whose __await__ returns a
# generator is the ordinary way a third-party awaitable is written, and it is
# what the conversion above must not break.
class Awaitable:
    def __init__(self, value):
        self.value = value

    def __await__(self):
        yield "suspended"
        return self.value


class Iter:
    def __init__(self, n):
        self.n = n

    def __aiter__(self):
        return self

    def __anext__(self):
        if self.n == 0:
            raise StopAsyncIteration
        self.n -= 1
        return Awaitable(self.n)


async def good():
    seen = []
    async for v in Iter(3):
        seen.append(v)
    seen.append(await Awaitable("bare"))
    return seen


c = good()
sent = 0
try:
    while True:
        print("yielded:", c.send(None))
        sent += 1
        if sent > 20:
            print("runaway")
            break
except StopIteration as e:
    print("returned:", e.value)


# An `async def __anext__` -- the common case -- still works, and so does an
# async context manager.
class AsyncIter:
    def __init__(self, n):
        self.n = n

    def __aiter__(self):
        return self

    async def __anext__(self):
        if self.n == 0:
            raise StopAsyncIteration
        self.n -= 1
        return self.n


class CM:
    async def __aenter__(self):
        return "entered"

    async def __aexit__(self, *a):
        print("exited")
        return False


async def good2():
    out = []
    async for v in AsyncIter(3):
        out.append(v)
    async with CM() as e:
        out.append(e)
    return out


c = good2()
try:
    while True:
        c.send(None)
except StopIteration as e:
    print("returned:", e.value)

print("done")


# GET_AWAITABLE and GET_ANEXT fall back to tp_iter, because this tree has no
# am_await slot and the awaitables the interpreter builds for itself -- an
# async generator's asend, a sleep, a gather, a task -- answer through it.
#
# That fallback accepted every ORDINARY iterable too.  An __anext__ returning
# an empty tuple handed `async for` a perfectly good tuple iterator to drive,
# so the loop ran for ever where CPython raises TypeError -- and it is
# CPython's own test_coroutines that does it, three tests in a row.
class TupleAnext:
    def __aiter__(self):
        return self

    def __anext__(self):
        return ()


class ListAnext:
    def __aiter__(self):
        return self

    def __anext__(self):
        return [1, 2, 3]


class StrAnext:
    def __aiter__(self):
        return self

    def __anext__(self):
        return "ab"


class DictAnext:
    def __aiter__(self):
        return self

    def __anext__(self):
        return {1: 2}


class SetAnext:
    def __aiter__(self):
        return self

    def __anext__(self):
        return {1}


class GenAnext:
    # A plain generator is not awaitable either: only one carrying
    # CO_ITERABLE_COROUTINE is, which is what @types.coroutine sets.
    def __aiter__(self):
        return self

    def __anext__(self):
        return (x for x in (1, 2))


async def over(o):
    n = 0
    async for i in o:
        n += 1
        if n > 3:
            return "RUNAWAY"
    return n


for cls in (TupleAnext, ListAnext, StrAnext, DictAnext, SetAnext, GenAnext):
    drive(lambda cls=cls: over(cls()), cls.__name__)

# The same for a bare await.
async def await_iterable(o):
    return await o


for o in ((), [1], "a", {1: 2}, {1}, (x for x in (1,))):
    drive(lambda o=o: await_iterable(o), type(o).__name__)


# ...and the things that ARE awaitable still are.
import types


@types.coroutine
def iterable_coro():
    yield "from a generator-based coroutine"
    return "done-gen"


async def uses_iterable_coro():
    return await iterable_coro()


c = uses_iterable_coro()
try:
    while True:
        print("yielded:", c.send(None))
except StopIteration as e:
    print("returned:", e.value)


async def inner():
    return "inner"


async def uses_coro():
    return await inner()


c = uses_coro()
try:
    while True:
        c.send(None)
except StopIteration as e:
    print("returned:", e.value)


async def agen():
    yield 1
    yield 2


async def uses_agen():
    out = []
    async for v in agen():
        out.append(v)
    g = agen()
    out.append(await g.asend(None))
    return out


c = uses_agen()
try:
    while True:
        c.send(None)
except StopIteration as e:
    print("returned:", e.value)

print("done 2")
