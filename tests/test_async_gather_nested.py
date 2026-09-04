# A gather can hold another gather, and the awaitables are collectable.
#
# gather() hands back a GatherAwaitable, which is not a generator and cannot
# be sent to, so task_new refused it: `gather(gather(...))` was a TypeError
# here and [[3]] in CPython.  Wrapping one meant wrapping an arbitrary
# awaitable in a coroutine, which is what CPython's ensure_future does and
# what there is no way to do from assembly.
#
# It needs no coroutine.  A task drives a generator with gen_send and anything
# else through tp_iternext, which is how op_send already steps one of these
# when a coroutine awaits it -- and GatherAwaitable keeps its return value at
# the offset PyGenObject keeps gi_return_value at, which is the whole reason
# that field is where it is.
#
# The three awaitables are GC-tracked with it.  A gather holds its tasks in a
# raw AsyncTask*[] rather than a list, which was safe only while the object
# was invisible to the collector; now that a task can hold a gather, the
# cycle that makes is one the collector has to be able to walk.

import asyncio
import gc


async def value(v):
    return v


async def slow():
    await asyncio.sleep(5)
    return "never"


async def boom():
    raise ValueError("inner")


async def main():
    print("nested        ", await asyncio.gather(asyncio.gather(value(3))))
    print("three deep    ",
          await asyncio.gather(asyncio.gather(asyncio.gather(value(1)))))
    print("mixed         ",
          await asyncio.gather(value(1), asyncio.gather(value(2), value(3)),
                               asyncio.create_task(value(4))))
    print("empty inner   ", await asyncio.gather(asyncio.gather()))
    print("wide          ",
          await asyncio.gather(*[asyncio.gather(value(i)) for i in range(5)]))

    # An exception inside a nested gather reaches the outer await.
    try:
        await asyncio.gather(asyncio.gather(boom()))
    except ValueError as exc:
        print("nested raise  ", exc)

    # ...and return_exceptions keeps it as a value.
    got = await asyncio.gather(asyncio.gather(boom(), return_exceptions=True),
                               return_exceptions=True)
    print("return_exc    ", type(got[0][0]).__name__)

    # wait_for takes one; create_task does not, which is CPython's rule.
    print("wait_for      ", await asyncio.wait_for(asyncio.gather(value(8)), 5))
    try:
        asyncio.create_task(asyncio.gather(value(7)))
    except TypeError as exc:
        # The message names the object, and the two interpreters have
        # different internal types to name.
        print("create_task   ", str(exc).startswith("a coroutine was expected"))

    # ...and a wait_for over a gather can time out, which cancels the task
    # holding it.  A plain awaitable cannot be thrown into, so the
    # CancelledError is recorded rather than delivered.
    try:
        await asyncio.wait_for(asyncio.gather(slow()), 0.02)
    except asyncio.TimeoutError:
        print("timed out     ", True)

    # The exceptions asyncio raises are reachable by name from the module.
    print("exports       ", asyncio.CancelledError is not None,
          asyncio.TimeoutError is not None,
          isinstance(asyncio.create_task(value(0)), asyncio.Task))


asyncio.run(main())


# --- the collector -----------------------------------------------------------

class Box:
    pass


async def cycle_through_gather():
    box = Box()
    box.g = asyncio.gather(value(1))
    box.self = box
    return await box.g


print("cycle result  ", asyncio.run(cycle_through_gather()))
gc.collect()
print("collected     ", True)


async def cycle_through_sleep():
    box = Box()
    box.s = asyncio.sleep(0)
    box.self = box
    await box.s
    return "slept"


print("sleep cycle   ", asyncio.run(cycle_through_sleep()))
gc.collect()
print("collected     ", True)


async def cycle_through_wait_for():
    box = Box()
    box.w = asyncio.wait_for(value(9), 5)
    box.self = box
    return await box.w


print("wait_for cycle", asyncio.run(cycle_through_wait_for()))
gc.collect()
print("collected     ", True)
print("done")
