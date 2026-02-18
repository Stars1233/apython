# Test await chains and value propagation

import asyncio

# Test 1: simple await chain (inner -> outer)
async def inner():
    return 42

async def outer():
    val = await inner()
    return val

result = asyncio.run(outer())
print("chain:", result)

# Test 2: deep await (3 levels)
async def level3():
    return "deep"

async def level2():
    return await level3()

async def level1():
    return await level2()

result = asyncio.run(level1())
print("deep:", result)

# Test 3: computation between awaits
async def get_x():
    return 10

async def get_y():
    return 20

async def compute():
    x = await get_x()
    y = await get_y()
    return x + y

result = asyncio.run(compute())
print("compute:", result)

# Test 4: await with mutation
async def get_list():
    return [1, 2, 3]

async def mutate():
    lst = await get_list()
    lst.append(4)
    return lst

result = asyncio.run(mutate())
print("mutate:", result)

# Test 5: await with string concatenation
async def get_greeting():
    return "hello"

async def get_name():
    return "world"

async def greet():
    g = await get_greeting()
    n = await get_name()
    return g + " " + n

result = asyncio.run(greet())
print("greet:", result)

print("all async await tests passed")
