# A coroutine that suspends, or finishes, inside an except block.
#
# current_exception is one global and it is also the exception BEING HANDLED,
# so "is one pending?" cannot mean "did that raise?".  Two places read it that
# way.  task_step took a normally-returning coroutine's ambient exception for
# its own -- `except E: return await f()` came back out of asyncio.run as the
# exception the coroutine had already caught -- and gen_send's resume path
# could not tell a raise from a body suspended at an await inside a handler,
# so it left that exception pending in the event loop and the interpreter
# reported it at exit, after the program had finished.

import asyncio


async def inner():
    return 7


print("=== returning from inside a handler ===")


async def returns_in_handler():
    try:
        raise ValueError("handled")
    except ValueError:
        await asyncio.sleep(0)
        return 42


print(asyncio.run(returns_in_handler()))


async def awaits_in_handler():
    try:
        raise KeyError("k")
    except KeyError:
        return await inner()


print(asyncio.run(awaits_in_handler()))


async def gathers_in_handler():
    try:
        raise RuntimeError("r")
    except RuntimeError:
        return await asyncio.gather(inner(), inner())


print(asyncio.run(gathers_in_handler()))

print("=== nested handlers, and one that really does raise ===")


async def raises_in_handler():
    try:
        raise ValueError("first")
    except ValueError:
        await asyncio.sleep(0)
        raise KeyError("second")


try:
    asyncio.run(raises_in_handler())
except KeyError as e:
    print("KeyError", e.args[0])


async def two_deep():
    try:
        raise ValueError("outer")
    except ValueError:
        try:
            raise KeyError("inner")
        except KeyError:
            await asyncio.sleep(0)
            return "both handled"


print(asyncio.run(two_deep()))

print("=== a task that raises is still a task that raises ===")


async def child_raises():
    await asyncio.sleep(0)
    raise ZeroDivisionError("child")


async def parent():
    try:
        await child_raises()
    except ZeroDivisionError as e:
        return "caught " + e.args[0]


print(asyncio.run(parent()))

print("=== and nothing is left pending afterwards ===")
import sys
print(sys.exc_info())
print("done")
