# Test async with (async context managers)

import asyncio

# Test 1: Simple async context manager
class AsyncCM:
    def __init__(self, name):
        self.name = name
        self.entered = False
        self.exited = False

    async def __aenter__(self):
        self.entered = True
        return self

    async def __aexit__(self, exc_type, exc_val, exc_tb):
        self.exited = True
        return False

async def test_async_with():
    cm = AsyncCM("test")
    async with cm:
        pass
    return cm.entered and cm.exited

r = asyncio.run(test_async_with())
print("basic:", r)

# Test 2: Async context manager with value
class AsyncCounter:
    def __init__(self):
        self.count = 0

    async def __aenter__(self):
        self.count = 1
        return self.count

    async def __aexit__(self, exc_type, exc_val, exc_tb):
        self.count = 0
        return False

async def test_with_value():
    counter = AsyncCounter()
    async with counter as val:
        result = val
    return result

r = asyncio.run(test_with_value())
print("value:", r)

# Test 3: Nested async with
async def test_nested():
    cm1 = AsyncCM("outer")
    cm2 = AsyncCM("inner")
    async with cm1:
        async with cm2:
            pass
    return cm1.exited and cm2.exited

r = asyncio.run(test_nested())
print("nested:", r)

print("all async with tests passed")
