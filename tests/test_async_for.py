# Test async for loops with async generators

import asyncio

# Test 1: Basic async generator
async def async_range(n):
    i = 0
    while i < n:
        yield i
        i += 1

async def test_async_for():
    result = []
    async for x in async_range(5):
        result.append(x)
    return result

r = asyncio.run(test_async_for())
print("async_for:", r)

# Test 2: Async generator with computation
async def async_squares(n):
    i = 0
    while i < n:
        yield i * i
        i += 1

async def test_async_squares():
    result = []
    async for x in async_squares(4):
        result.append(x)
    return result

r = asyncio.run(test_async_squares())
print("squares:", r)

# Test 3: Async generator with filtering
async def async_evens(n):
    i = 0
    while i < n:
        if i % 2 == 0:
            yield i
        i += 1

async def test_evens():
    result = []
    async for x in async_evens(10):
        result.append(x)
    return result

r = asyncio.run(test_evens())
print("evens:", r)

# Test 4: Multiple async for loops
async def test_multi():
    a = []
    async for x in async_range(3):
        a.append(x)
    b = []
    async for x in async_range(2):
        b.append(x)
    return a + b

r = asyncio.run(test_multi())
print("multi:", r)

print("all async for tests passed")
