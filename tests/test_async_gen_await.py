# An async generator whose body awaits.
#
# Its frame yields for two different reasons: `yield v` produces an item for
# whoever is iterating it, and an `await` inside the body yields outward to the
# event loop.  Only the first ends one round of __anext__.  CPython tells them
# apart by wrapping the item -- CALL_INTRINSIC_1 INTRINSIC_ASYNC_GEN_WRAP right
# before the YIELD_VALUE -- and the driver unwraps it.  Treating every yield as
# an item makes an awaited value show up as one, so consuming `wrapgen` below
# yields the inner generator's values interleaved with its own.
import asyncio


async def gen(n):
    for i in range(n):
        yield i


async def wrapgen(n):
    async for x in gen(n):
        yield x + 1


async def doubled(n):
    async for x in wrapgen(n):
        yield x * 2


async def awaiting(n):
    async def one(v):
        return v + 100
    for i in range(n):
        yield await one(i)


async def collect(ait):
    out = []
    async for v in ait:
        out.append(v)
    return out


async def main():
    print(await collect(gen(4)))
    print(await collect(wrapgen(4)))
    print(await collect(doubled(3)))
    print(await collect(awaiting(3)))
    # A generator expression over an async generator is an async generator too.
    print(await collect(x * 10 async for x in gen(3)))
    print([v async for v in wrapgen(3)])


asyncio.run(main())
