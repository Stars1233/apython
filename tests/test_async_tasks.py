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

# A task that RAISED used to look like a task that returned.
#
# task_step tested gen_send's tag for "the coroutine finished" and assumed
# that meant "returned".  Nothing outside the cancellation path ever wrote
# AsyncTask.exception, so `await t` on a raising task saw a done task with no
# exception, took task_iternext's .ti_done arm, and evaluated to None --
# try/except around the await caught nothing, and the exception surfaced at
# interpreter exit instead.  t.result() and asyncio.wait_for read the same
# never-set field.
#
# The exception was available all along: a raise inside a coroutine body does
# not abandon the C stack past the generator frame -- the unwinder's
# no-handler arm returns normally through eval_return -- and gen_send leaves
# it pending on purpose.  The whole re-raise path downstream already existed
# and was simply unreachable.
#
# asyncio.run(boom()) appeared to work even so, by accident: eventloop_run
# hands back the root task's result, and op_call reads a NULL return with a
# set current_exception as "the callee raised".  It is the leaked global
# doing the work, not the task, which is why the bug is invisible until a
# create_task boundary sits between the raise and the awaiter.


async def boom(msg="kaboom"):
    raise ValueError(msg)


async def returns(v=7):
    return v


async def test_raising_tasks():
    t = asyncio.create_task(boom())
    try:
        await t
    except ValueError as e:
        print("caught:", e)
    else:
        print("NOT CAUGHT")

    # A finished task re-raises on every await, not only the first.
    try:
        await t
    except ValueError as e:
        print("caught again:", e)

    # result() reads the same field.
    t2 = asyncio.create_task(boom("second"))
    try:
        await t2
    except ValueError:
        pass
    try:
        t2.result()
    except ValueError as e:
        print("result raised:", e)
    print("done flag:", t2.done())

    # A task that returns normally is untouched.
    t3 = asyncio.create_task(returns())
    print("value:", await t3, t3.result())

    # Two waiters on one failing task, one after the other.
    t4 = asyncio.create_task(boom("shared"))

    async def waiter(n):
        try:
            await t4
        except ValueError as e:
            return "w%d:%s" % (n, e)
        return "w%d:none" % n

    print(await waiter(1), await waiter(2))

    # The type and the message survive.
    class MyErr(Exception):
        pass

    async def custom():
        raise MyErr("custom message")

    try:
        await asyncio.create_task(custom())
    except MyErr as e:
        print("custom:", type(e).__name__, e)

    # asyncio.wait_for reads AsyncTask.exception too.
    try:
        await asyncio.wait_for(boom("waited"), 1)
    except ValueError as e:
        print("wait_for:", e)

    # And a task awaited from inside another task.
    async def outer():
        await asyncio.create_task(boom("nested"))

    try:
        await asyncio.create_task(outer())
    except ValueError as e:
        print("nested:", e)

    # An exception raised while one is being handled still chains inside the
    # frame that raised it.
    try:
        raise KeyError("first")
    except KeyError:
        try:
            raise ValueError("second")
        except ValueError as e:
            print("chain:", type(e.__context__).__name__)


asyncio.run(test_raising_tasks())
print("raising tasks: ok")
