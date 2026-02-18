# Test asyncio.run with basic coroutines

import asyncio

# Test 1: Simple asyncio.run
async def simple():
    return 42

result = asyncio.run(simple())
print("simple:", result)

# Test 2: asyncio.run with computation
async def compute(x, y):
    return x + y

result = asyncio.run(compute(10, 20))
print("compute:", result)

# Test 3: asyncio.run with string result
async def greet(name):
    return "hello " + name

result = asyncio.run(greet("world"))
print("greet:", result)

# Test 4: asyncio.run with multiple operations
async def multi_op():
    a = 1 + 2
    b = a * 3
    c = b - 1
    return c

result = asyncio.run(multi_op())
print("multi_op:", result)

# Test 5: asyncio.run with None return
async def returns_none():
    x = 1 + 1  # do something

result = asyncio.run(returns_none())
print("none:", result)

print("all async run tests passed")
