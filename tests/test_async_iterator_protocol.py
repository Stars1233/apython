# A class that implements the asynchronous iterator protocol itself.
#
# `async for` read tp_iter and tp_iternext and nothing else, and slot_table
# has no row for __aiter__ or __anext__ -- so a class defining exactly the
# protocol had both slots at zero and was refused with "'async for' requires
# an object with __aiter__ method", naming the very method it defined.  That
# is the shape asyncio's streams have, and every hand-written async iterator.
#
# Both names are looked up by name first now, with the slot left as the
# fallback: the builtin async generator has tp_iter and keeps no __aiter__ in
# its tp_dict, so it still takes the fast road.
#
# The lookup has to tell a missing dunder from one that ran and raised.  A
# NULL is both, and treating a raise as "not defined" would report a
# TypeError about the protocol instead of the exception the method threw.

import asyncio


class Counter:
    """__aiter__ returns self; __anext__ is a coroutine."""
    def __init__(self, n):
        self.i = 0
        self.n = n

    def __aiter__(self):
        return self

    async def __anext__(self):
        if self.i >= self.n:
            raise StopAsyncIteration
        self.i += 1
        return self.i


class Awaiting(Counter):
    """The same, but __anext__ actually suspends."""
    async def __anext__(self):
        if self.i >= self.n:
            raise StopAsyncIteration
        self.i += 1
        await asyncio.sleep(0)
        return self.i


class SeparateIterator:
    """__aiter__ returns something else, as CPython's streams do."""
    def __aiter__(self):
        return Counter(2)


class AnextRaises:
    def __aiter__(self):
        return self

    async def __anext__(self):
        raise ValueError("from anext")


class AiterRaises:
    def __aiter__(self):
        raise RuntimeError("from aiter")


class NoAiter:
    async def __anext__(self):
        raise StopAsyncIteration


class NoAnext:
    def __aiter__(self):
        return self


class Inherited(Counter):
    """The lookup walks the MRO, so a base's methods count."""


async def collect(obj):
    out = []
    try:
        async for x in obj:
            out.append(x)
    except Exception as e:
        return (out, type(e).__name__, str(e))
    return (out, None, None)


async def main():
    print("counter    :", await collect(Counter(3)))
    print("empty      :", await collect(Counter(0)))
    print("awaiting   :", await collect(Awaiting(2)))
    print("separate   :", await collect(SeparateIterator()))
    print("inherited  :", await collect(Inherited(2)))
    print("anext raise:", await collect(AnextRaises()))
    print("aiter raise:", await collect(AiterRaises()))
    print("no aiter   :", await collect(NoAiter()))
    print("no anext   :", await collect(NoAnext()))
    print("not one    :", await collect(42))
    print("nor a list :", await collect([1, 2]))

    # The builtin async generator is unchanged.
    async def ag():
        yield "a"
        yield "b"
    print("asyncgen   :", await collect(ag()))

    # Two of them at once, and a comprehension over one.
    both = []
    async for a in Counter(2):
        async for b in Counter(2):
            both.append((a, b))
    print("nested     :", both)
    print("comprehend :", [x async for x in Counter(3)])

asyncio.run(main())
