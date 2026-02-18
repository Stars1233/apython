# Test async generators (patterns not in test_async_for.py)

import asyncio

# Test 1: countdown (reverse iteration)
async def countdown(n):
    while n > 0:
        yield n
        n -= 1

async def test_countdown():
    result = []
    async for x in countdown(5):
        result.append(x)
    return result

r = asyncio.run(test_countdown())
print("countdown:", r)

# Test 2: running sum
async def running_sum(items):
    total = 0
    for x in items:
        total += x
        yield total

async def test_running_sum():
    result = []
    async for x in running_sum([1, 2, 3, 4]):
        result.append(x)
    return result

r = asyncio.run(test_running_sum())
print("running_sum:", r)

# Test 3: multiple generators sequentially
async def gen_a():
    yield "a1"
    yield "a2"

async def gen_b():
    yield "b1"
    yield "b2"

async def test_sequential():
    result = []
    async for x in gen_a():
        result.append(x)
    async for x in gen_b():
        result.append(x)
    return result

r = asyncio.run(test_sequential())
print("sequential:", r)

# Test 4: powers of two
async def powers_of_two(n):
    val = 1
    i = 0
    while i < n:
        yield val
        val *= 2
        i += 1

async def test_powers():
    result = []
    async for x in powers_of_two(6):
        result.append(x)
    return result

r = asyncio.run(test_powers())
print("powers:", r)

print("all async gen tests passed")
