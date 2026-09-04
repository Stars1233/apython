# asyncio.gather, and an exception that reaches the root task.
#
# gather built one task per coroutine and handed back the LIST of them, with a
# TODO saying the awaiting was still to do -- so `await asyncio.gather(...)`
# awaited a list, which is not awaitable, and answered None.  And an exception
# that reached the root task was dropped by eventloop_run, which read only
# .result: `asyncio.run(main())` where main raises answered None and the
# exception was never seen again.
import asyncio


def t(label, fn):
    try:
        print(label, "=>", repr(fn()))
    except BaseException as e:
        print(label, "!!", type(e).__name__, e)


async def one():
    return 1


async def two():
    return 2


async def slow():
    await asyncio.sleep(0)
    return "slow"


async def boom():
    raise ValueError("b")


async def keyboom():
    raise KeyError("k")


# --- the ordinary shapes
async def basic():
    return await asyncio.gather(one(), two())


t("two", lambda: asyncio.run(basic()))


async def empty():
    return await asyncio.gather()


t("empty", lambda: asyncio.run(empty()))


async def single():
    return await asyncio.gather(one())


t("one", lambda: asyncio.run(single()))


async def many():
    return await asyncio.gather(one(), two(), one(), two(), one())


t("five", lambda: asyncio.run(many()))


# Results come back in ARGUMENT order, not completion order.
async def ordered():
    return await asyncio.gather(slow(), one())


t("order", lambda: asyncio.run(ordered()))


async def nested():
    a = await asyncio.gather(one(), two())
    b = await asyncio.gather(one())
    return a + b


t("twice", lambda: asyncio.run(nested()))


# --- exceptions
async def raises():
    return await asyncio.gather(one(), boom())


t("raises", lambda: asyncio.run(raises()))


async def caught():
    try:
        await asyncio.gather(one(), boom())
    except ValueError as e:
        return "caught " + str(e)
    return "not caught"


t("caught", lambda: asyncio.run(caught()))


async def first_of_two():
    try:
        await asyncio.gather(boom(), keyboom())
    except BaseException as e:
        return type(e).__name__
    return "none"


t("first exception", lambda: asyncio.run(first_of_two()))


async def collected():
    r = await asyncio.gather(one(), boom(), two(), return_exceptions=True)
    return [type(x).__name__ if isinstance(x, BaseException) else x for x in r]


t("return_exceptions", lambda: asyncio.run(collected()))


async def all_raise():
    r = await asyncio.gather(boom(), keyboom(), return_exceptions=True)
    return [type(x).__name__ for x in r]


t("all raise", lambda: asyncio.run(all_raise()))


async def exc_false():
    try:
        await asyncio.gather(boom(), return_exceptions=False)
    except ValueError:
        return "raised"
    return "no"


t("return_exceptions=False", lambda: asyncio.run(exc_false()))


# --- a task passed straight in
async def with_task():
    t1 = asyncio.create_task(one())
    return await asyncio.gather(t1, two())


t("task argument", lambda: asyncio.run(with_task()))


# --- an exception reaching the root task
async def root_raises():
    raise ValueError("root")


t("root exception", lambda: asyncio.run(root_raises()))


async def root_awaits_failing_task():
    tk = asyncio.create_task(boom())
    await tk
    return "no raise"


t("root awaits failure", lambda: asyncio.run(root_awaits_failing_task()))


async def root_catches():
    try:
        await asyncio.create_task(boom())
    except ValueError as e:
        return "handled " + str(e)
    return "no"


t("root catches", lambda: asyncio.run(root_catches()))

# --- argument checking
async def bad_arg():
    return await asyncio.gather(5)


t("gather int", lambda: asyncio.run(bad_arg()))


async def bad_kw():
    # The coroutine is deliberately built before the refusal, so nothing here
    # depends on whether it is ever awaited.
    return await asyncio.gather(nosuch=True)


t("gather bad kw", lambda: asyncio.run(bad_kw()))

# --- the loop still works afterwards
t("after all", lambda: asyncio.run(basic()))

print("done")
