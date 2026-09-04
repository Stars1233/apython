# The exception being HANDLED belongs to the frame, not to the interpreter.
#
# There used to be one global for two questions -- "is an exception in
# flight?" and "which exception is this code handling?" -- and it could only
# answer one of them at a time.  Two things went wrong wherever a frame
# suspended:
#
#   * A coroutine that awaited inside an except block came back with
#     sys.exc_info() empty, so a bare `raise` there was a RuntimeError and
#     anything that logs exc_info() saw nothing.
#   * An exception raised by an awaited TASK got no __context__, because the
#     awaiting coroutine's handler state was not current when the task's
#     exception was re-raised.  The other two shapes -- a generator resume and
#     an `await` on a plain coroutine -- were right, which is what made it
#     look like a task-machinery bug rather than a frame-state one.
#
# handled_exception is now its own word, and eval_frame swaps it with
# PyFrame.exc_state around every frame: a frame handling nothing shares its
# caller's, which is what gives `except E: f()` a __context__ of E inside f,
# and a generator carries its own across a suspension.

import asyncio
import sys


# --- sys.exc_info() survives a suspension ----------------------------------

def gen_exc_info():
    try:
        raise ValueError("gen")
    except ValueError:
        print("gen before yield:", sys.exc_info()[0].__name__)
        yield 1
        print("gen after yield :", sys.exc_info()[0].__name__)
        try:
            raise
        except ValueError as exc:
            print("gen bare raise  :", type(exc).__name__, exc.args)


it = gen_exc_info()
next(it)
try:
    next(it)
except StopIteration:
    pass

print("outside a handler:", sys.exc_info())


async def nap():
    await asyncio.sleep(0)


async def coro_exc_info():
    try:
        raise KeyError("coro")
    except KeyError:
        print("coro before await:", sys.exc_info()[0].__name__)
        await nap()
        print("coro after await :", sys.exc_info()[0].__name__)
        try:
            raise
        except KeyError as exc:
            print("coro bare raise  :", type(exc).__name__, exc.args)


asyncio.run(coro_exc_info())


# --- __context__ across all three ways of resuming -------------------------

async def boom(exc):
    raise exc


def gen_boom():
    yield 1
    raise ValueError("from a generator")


async def contexts():
    try:
        raise KeyError("handler A")
    except KeyError:
        task = asyncio.create_task(boom(ValueError("from a task")))
        try:
            await task
        except ValueError as exc:
            print("await task:", repr(exc.__context__))

    try:
        raise KeyError("handler B")
    except KeyError:
        try:
            await boom(ValueError("from a coroutine"))
        except ValueError as exc:
            print("await coro:", repr(exc.__context__))

    try:
        raise KeyError("handler C")
    except KeyError:
        g = gen_boom()
        next(g)
        try:
            next(g)
        except ValueError as exc:
            print("gen resume:", repr(exc.__context__))


asyncio.run(contexts())


# --- a frame that handles nothing shares its caller's ----------------------

def raises():
    raise ValueError("inner")


try:
    raise KeyError("outer")
except KeyError:
    try:
        raises()
    except ValueError as exc:
        print("called frame:", repr(exc.__context__))

# ...and one that is not inside a handler chains to nothing.
try:
    raises()
except ValueError as exc:
    print("no handler  :", repr(exc.__context__))


# --- nesting: an inner handler does not outlive itself ---------------------

async def nested():
    try:
        raise ValueError("outer")
    except ValueError:
        try:
            raise KeyError("inner")
        except KeyError:
            await nap()
            print("inner alive :", sys.exc_info()[0].__name__)
        await nap()
        print("outer back  :", sys.exc_info()[0].__name__)
    await nap()
    print("both gone   :", sys.exc_info()[0])


asyncio.run(nested())


# --- a generator abandoned inside a handler releases what it held ----------

def abandoned():
    try:
        raise ValueError("never popped")
    except ValueError:
        yield 1
        yield 2


a = abandoned()
next(a)
del a
print("abandoned generator dropped")

# The awaiting coroutine's handler is what a gather's exception chains to.
async def gathered():
    try:
        raise KeyError("handler D")
    except KeyError:
        try:
            await asyncio.gather(boom(ValueError("from a gather")))
        except ValueError as exc:
            print("await gather:", repr(exc.__context__))


asyncio.run(gathered())
print("done")
