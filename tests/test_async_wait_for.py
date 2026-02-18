# Test asyncio.wait_for

import asyncio

# Test 1: fast coro completes before timeout
async def fast_coro():
    return 42

async def test_fast():
    result = await asyncio.wait_for(fast_coro(), timeout=10)
    return result

r = asyncio.run(test_fast())
print("fast:", r)

# Test 2: timeout=0 on slow coro raises TimeoutError
async def slow_coro():
    await asyncio.sleep(100)
    return "never"

async def test_timeout():
    try:
        await asyncio.wait_for(slow_coro(), timeout=0)
        return "no error"
    except TimeoutError:
        return "timed out"

r = asyncio.run(test_timeout())
print("timeout:", r)

print("all async wait_for tests passed")
