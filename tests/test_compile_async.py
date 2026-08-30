# The source compiler, phase 8: async def, await, async for, async with.
#
# Every construct here is built out of one shape, the send loop: SEND round to
# YIELD_VALUE and back until the awaited object is exhausted.  `await x` is that
# loop over x.__await__(), `async for` is that loop over each __anext__(), and
# `async with` is that loop over __aenter__() and again over each __aexit__().
# The loop is emitted by one function; the constructs differ only in what they
# push before it and in how they leave.
#
# An `async for` leaves its loop by raising StopAsyncIteration, so the exit edge
# is an exception edge and the loop head has to sit inside a protected region --
# which is why the exception table matters here and not in a plain `for`.
import asyncio

SRC = """
async def inner(n):
    return n * 2

async def outer():
    a = await inner(3)
    b = await inner(a)
    return a + b

async def gen(n):
    i = 0
    while i < n:
        yield i * i
        i += 1

async def consume(n):
    out = []
    async for v in gen(n):
        out.append(v)
    else:
        out.append(-1)
    return out

async def brk(n):
    out = []
    async for v in gen(n):
        if v > 4:
            break
        out.append(v)
    return out
class CM:
    def __init__(self, name, log, suppress=False):
        self.name = name; self.log = log; self.suppress = suppress
    async def __aenter__(self):
        self.log.append("enter " + self.name)
        return self.name
    async def __aexit__(self, t, v, tb):
        self.log.append("exit " + self.name + " " + repr(t and t.__name__))
        return self.suppress

async def plain(log):
    async with CM("a", log) as x:
        log.append("body " + x)
    return log

async def nested(log):
    async with CM("a", log), CM("b", log) as y:
        log.append("body " + y)
    return log

async def raising(log):
    try:
        async with CM("a", log):
            raise ValueError("boom")
    except ValueError as e:
        log.append("caught " + str(e))
    return log

async def suppressed(log):
    async with CM("a", log, True):
        raise ValueError("boom")
    log.append("after")
    return log

async def early(log):
    async with CM("a", log):
        return "returned"

async def loopy(log):
    for i in range(3):
        async with CM(str(i), log):
            if i == 1:
                break
    return log

async def lc(n):
    return [x async for x in gen(n)]

async def lc2(n):
    return [x * 2 async for x in gen(n) if x % 2]

async def sc(n):
    return sorted({x async for x in gen(n)})

async def dc(n):
    return {x: x * x async for x in gen(n)}

async def ge(n):
    return [v async for v in (x + 1 async for x in gen(n))]

async def aw(n):
    async def double(v):
        return v * 2
    return [await double(x) for x in range(n)]
"""

ns = {}
exec(compile(SRC, "<async>", "exec"), ns)

# await, async for with else, and break out of an async for
print(asyncio.run(ns['outer']()))
print(asyncio.run(ns['consume'](5)))
print(asyncio.run(ns['brk'](5)))

# The three co_flags kinds are mutually exclusive.
CO_GENERATOR, CO_COROUTINE, CO_ASYNC_GENERATOR = 0x20, 0x80, 0x200
for name in ['inner', 'outer', 'gen', 'consume']:
    f = ns[name].__code__.co_flags
    print(name, bool(f & CO_GENERATOR), bool(f & CO_COROUTINE),
          bool(f & CO_ASYNC_GENERATOR))

# async with: plain, nested, raising, suppressing, and left early
for name in ['plain', 'nested', 'raising', 'suppressed', 'loopy']:
    print(name, asyncio.run(ns[name]([])))
log = []
print('early', asyncio.run(ns['early'](log)), log)

# async comprehensions of every kind, and an async generator expression
for name in ['lc', 'lc2', 'sc', 'dc', 'ge', 'aw']:
    print(name, asyncio.run(ns[name](4)))
