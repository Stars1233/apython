# A Task holds its coroutine, the coroutine holds its frame, and the frame's
# locals can hold the Task -- an ordinary reference cycle that only the
# collector can break.  Tasks used to be invisible to it: ap_malloc'd, no
# TYPE_FLAG_HAVE_GC, no traverse and no clear, so every such cycle leaked.
#
# Making them visible is only safe once nothing holds a live Task through a
# raw pointer, which is what the commit before this one is about: the ready
# queue, the loop's root task, the waiters array, the poll backend's fd array
# and timer heap, and both kinds of io_uring submission.
#
# This runs under collection pressure on purpose.  gc.set_threshold(1) means
# a collection between very nearly every pair of allocations, so a task that
# is reachable only through one of those raw pointers is collected while the
# loop is still using it.

import asyncio
import gc


async def leaf(n):
    await asyncio.sleep(0)
    return n


async def spread(n):
    tasks = [asyncio.create_task(leaf(i)) for i in range(n)]
    total = 0
    for t in tasks:
        total += await t
    return total


async def selfref():
    # The frame holds the list, the list holds itself, and the coroutine's
    # frame is reachable from the task: a cycle through the task's own
    # coroutine.
    box = []
    box.append(box)
    await asyncio.sleep(0)
    return len(box)


async def nested(depth):
    if depth == 0:
        return 0
    return 1 + await asyncio.create_task(nested(depth - 1))


gc.set_threshold(1)
try:
    for _ in range(5):
        print(asyncio.run(spread(20)))
    for _ in range(5):
        print(asyncio.run(selfref()))
    print(asyncio.run(nested(10)))
finally:
    gc.set_threshold(700, 10, 10)

print(gc.collect() >= 0)


# A task that is never awaited, and a loop that finishes without it: the
# queue and the backend both have to let go of what they were holding.
async def orphan():
    await asyncio.sleep(0)
    return "orphan"


async def abandon():
    asyncio.create_task(orphan())
    return "done"


print(asyncio.run(abandon()))
print(gc.collect() >= 0)


# Cancellation takes a different exit out of every holder.
async def slow():
    await asyncio.sleep(10)
    return "never"


async def canceller():
    t = asyncio.create_task(slow())
    t.cancel()
    # asyncio.CancelledError is not exposed here, and BaseException is what
    # tests/test_async_cancel.py catches it with.
    try:
        await t
    except BaseException as e:
        return "cancelled " + type(e).__name__
    return "not cancelled"


print(asyncio.run(canceller()))
print(gc.collect() >= 0)
print("ok")
