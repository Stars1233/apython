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


# An exception from the generator body must leave the loop as an exception.
#
# It did not.  ags_iternext decides "exhausted or yielded" from the frame's
# instr_ptr, and a body that raised leaves the frame finished -- so a raise
# read as exhaustion, and the StopAsyncIteration manufactured for it DECREF'd
# the real exception away.  `async for x in ag()` over a generator that
# raises ended the loop cleanly and lost the exception outright: not caught
# by an enclosing except, not reported at exit, the program carried on past a
# `raise` as though it had not been written.
#
# END_ASYNC_FOR's re-raise arm was the second half: it popped the exception
# without republishing the stack depth the unwinder restores from, so once
# real exceptions started arriving there the slot came back to life while
# current_exception still pointed at it.

async def raiser(n, exc):
    for i in range(n):
        yield i
    raise exc


async def exc_caught_outside():
    seen = []
    try:
        async for x in raiser(2, ValueError("boom")):
            seen.append(x)
    except ValueError as e:
        return ("caught", seen, str(e))
    return ("fell through", seen, None)


async def exc_before_first_yield():
    async def nothing():
        raise KeyError("k")
        yield 1
    try:
        async for x in nothing():
            pass
    except KeyError as e:
        return "caught " + str(e)
    return "fell through"


async def exc_through_a_finally():
    order = []
    try:
        try:
            async for x in raiser(1, RuntimeError("r")):
                order.append(x)
        finally:
            order.append("finally")
    except RuntimeError:
        order.append("except")
    return order


async def exc_across_an_await():
    async def inner():
        async for x in raiser(1, IndexError("i")):
            pass
    try:
        await inner()
    except IndexError as e:
        return "outer caught " + str(e)
    return "outer fell through"


async def still_ends_normally():
    async def ok():
        yield 1
        yield 2
    out = []
    async for x in ok():
        out.append(x)
    # And an empty one still ends, rather than propagating anything.
    async def empty():
        return
        yield
    async for x in empty():
        out.append("never")
    return out


async def test_raising_generators():
    print("caught outside :", await exc_caught_outside())
    print("before yield   :", await exc_before_first_yield())
    print("through finally:", await exc_through_a_finally())
    print("across await   :", await exc_across_an_await())
    print("normal endings :", await still_ends_normally())

asyncio.run(test_raising_generators())

# An async generator that yields from inside an `except` block leaves
# current_exception set on purpose -- it never reaches its POP_EXCEPT -- so
# reading that global as "the body raised" made `async for` re-raise what the
# generator had already caught.  A raise is a NULL result, not a set global.
async def caught():
    try:
        raise ValueError("handled")
    except ValueError:
        yield 1
        yield 2
    yield 3


async def still_raises():
    yield 1
    raise KeyError("real")


async def main_caught():
    out = []
    async for v in caught():
        out.append(v)
    print("caught:", out)

    seen = []
    try:
        async for v in still_raises():
            seen.append(v)
    except KeyError as e:
        print("raised after", seen, "->", e)


asyncio.run(main_caught())
