# Test task cancellation

import asyncio

# Test 1: CancelledError catchable with except BaseException
async def test_cancel_catch():
    async def sleeper():
        await asyncio.sleep(10)

    t = asyncio.create_task(sleeper())
    t.cancel()
    try:
        await t
    except BaseException:
        return "caught"
    return "not caught"

r = asyncio.run(test_cancel_catch())
print("cancel_catch:", r)

# Test 2: cancel returns True
async def test_cancel_returns():
    async def sleeper():
        await asyncio.sleep(10)

    t = asyncio.create_task(sleeper())
    result = t.cancel()
    try:
        await t
    except BaseException:
        pass
    return result

r = asyncio.run(test_cancel_returns())
print("cancel_returns:", r)

print("all async cancel tests passed")
