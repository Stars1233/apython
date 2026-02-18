# Test asyncio.create_task and task management

import asyncio

# Test 1: create_task + await result
async def worker():
    return 42

async def test_create_task():
    t = asyncio.create_task(worker())
    result = await t
    return result

r = asyncio.run(test_create_task())
print("create_task:", r)

# Test 2: task.done() method
async def test_done():
    async def slow():
        return "finished"
    t = asyncio.create_task(slow())
    result = await t
    done = t.done()
    return (result, done)

r = asyncio.run(test_done())
print("done:", r)

# Test 3: multiple create_task + await each
async def make_val(x):
    return x * 10

async def test_multi_tasks():
    t1 = asyncio.create_task(make_val(1))
    t2 = asyncio.create_task(make_val(2))
    t3 = asyncio.create_task(make_val(3))
    r1 = await t1
    r2 = await t2
    r3 = await t3
    return [r1, r2, r3]

r = asyncio.run(test_multi_tasks())
print("multi_tasks:", r)

# Test 4: concurrent interleave via create_task
async def append_to(lst, val):
    lst.append(val)

async def test_interleave():
    result = []
    t1 = asyncio.create_task(append_to(result, "a"))
    t2 = asyncio.create_task(append_to(result, "b"))
    await t1
    await t2
    return result

r = asyncio.run(test_interleave())
print("interleave:", r)

print("all async tasks tests passed")
