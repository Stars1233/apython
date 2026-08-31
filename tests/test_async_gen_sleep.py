# An `await` inside an async generator body yields outward to the event loop.
#
# ags_iternext packs the yielded value into a Value and then the passthrough
# path unpacked it again, while op_send unpacks what tp_iternext returns.  A
# pointer survives being decoded twice; the SLEEP and IO_WAIT sentinels do not
# -- they lose the tag in their top 16 bits, and the event loop then reads the
# delay as an object pointer.
#
# So `await asyncio.sleep(0)` inside an async generator produced a stray None
# in the output, and a non-zero delay dereferenced the nanosecond count.
import asyncio


async def gen(n):
    for i in range(n):
        await asyncio.sleep(0)
        yield i


async def main_zero():
    return [x async for x in gen(3)]


print(asyncio.run(main_zero()))


async def gen_delay(n):
    for i in range(n):
        await asyncio.sleep(0.001)
        yield i * 2


async def main_delay():
    out = []
    async for x in gen_delay(3):
        out.append(x)
    return out


print(asyncio.run(main_delay()))


# A sleep between yields, and one after the last.
async def gen_mixed():
    yield "a"
    await asyncio.sleep(0)
    yield "b"
    await asyncio.sleep(0.001)


async def main_mixed():
    return [x async for x in gen_mixed()]


print(asyncio.run(main_mixed()))


# Awaiting a coroutine rather than a sleep: a plain pointer, which survived the
# double decode and must keep working.
async def helper(v):
    return v + 1


async def gen_coro():
    for i in range(3):
        yield await helper(i)


async def main_coro():
    return [x async for x in gen_coro()]


print(asyncio.run(main_coro()))


# NOTE: `await g.asend(None)` is not supported here -- the asend wrapper is not
# awaitable -- so the asend/athrow surface stays uncovered.  It is reachable
# only through `async for`, which is what the cases above drive.
