# An exception raised inside an awaited coroutine has to reach the awaiting
# frame.
#
# SEND treated a NULL from the receiver as exhaustion without asking whether
# it had RAISED, so it pushed the return value the coroutine never set, jumped
# past the await, and left the exception sitting in the pending-exception
# global until something else tripped over it.  Every error inside awaited
# code was lost that way: `await boom()` inside a try/except ran neither the
# handler nor the else.

import asyncio


async def boom(exc):
    raise exc


async def quiet(v):
    return v


async def main():
    print("=== every exception propagates out of await ===")
    for E in (ValueError, KeyError, RuntimeError, ZeroDivisionError,
              StopAsyncIteration, IndexError, AttributeError):
        try:
            await boom(E("x"))
            print(E.__name__, "NOT RAISED")
        except BaseException as e:
            print(E.__name__, "caught", type(e).__name__, e.args)

    print("=== and a coroutine that does not raise still returns ===")
    print(await quiet(1), await quiet("s"), await quiet(None))

    print("=== through a chain of awaits ===")
    async def outer():
        return await middle()

    async def middle():
        return await boom(IndexError("deep"))

    try:
        await outer()
    except IndexError as e:
        print("caught", e.args)

    print("=== try/finally around an await ===")
    order = []

    async def guarded():
        try:
            await boom(ValueError("v"))
        finally:
            order.append("finally")

    try:
        await guarded()
    except ValueError:
        order.append("except")
    print(order)

    print("=== an except that catches and continues ===")
    async def recover():
        try:
            await boom(KeyError("k"))
        except KeyError:
            return "recovered"
        return "not reached"

    print(await recover())

    print("=== the exception's own attributes survive ===")
    try:
        await boom(ValueError("message here"))
    except ValueError as e:
        print(str(e), e.args, type(e).__name__)

    print("=== an await inside a loop ===")
    seen = []
    for i in range(4):
        try:
            await boom(ValueError(i))
        except ValueError as e:
            seen.append(e.args[0])
    print(seen)

    print("=== raising from an async generator's consumer ===")
    async def agen():
        yield 1
        yield 2

    total = 0
    async for v in agen():
        total += v
    print(total)


asyncio.run(main())
print("done")
