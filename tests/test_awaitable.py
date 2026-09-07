# The awaitable protocol: __await__, __aiter__ and __aenter__.
#
# GET_AWAITABLE accepted a coroutine and then looked for tp_iter, which is
# __iter__ and not __await__ -- so every object that implements the protocol
# the way asyncio.Future, every asyncio lock and essentially every third-party
# awaitable implement it raised "object can't be used in 'await' expression".
#
# Nothing in this tree exercised it, because apython's asyncio is native.
# CPython's cannot run without it.
import types


def drive(coro):
    """Run a coroutine to completion, collecting what it yields out."""
    sent = []
    try:
        while True:
            sent.append(coro.send(None))
    except StopIteration as e:
        return sent, e.value


# --- __await__ returning a plain iterator --------------------------------
class Iterish:
    def __await__(self):
        return iter([1, 2])


async def a1():
    return await Iterish()


print("iterator   :", drive(a1()))


# --- __await__ written as a generator function ---------------------------
class Genish:
    def __await__(self):
        yield "step"
        return 7


async def a2():
    return await Genish()


print("generator  :", drive(a2()))


# --- nothing yielded at all ----------------------------------------------
class Empty:
    def __await__(self):
        return iter(())


async def a3():
    return await Empty()


print("empty      :", drive(a3()))


# --- @types.coroutine, which is how the stdlib wraps a generator ---------
@types.coroutine
def wrapped():
    yield "from generator"
    return 11


async def a4():
    return await wrapped()


print("types.coro :", drive(a4()))


# --- awaiting a coroutine still works ------------------------------------
async def inner():
    return 3


async def a5():
    return await inner() + 1


print("coroutine  :", drive(a5()))


# --- the refusals --------------------------------------------------------
def show(label, fn):
    try:
        print(label, "->", fn())
    except BaseException as e:
        print(label, "->", type(e).__name__ + ":", e)


class NotIter:
    def __await__(self):
        return 5            # not an iterator


class NoAwait:
    pass


async def bad(x):
    return await x


show("not an iterator", lambda: drive(bad(NotIter())))
show("no __await__   ", lambda: drive(bad(NoAwait())))
show("an int         ", lambda: drive(bad(1)))
show("a plain gen    ", lambda: drive(bad((i for i in range(1)))))


# --- __await__ that raises propagates ------------------------------------
class Angry:
    def __await__(self):
        raise ValueError("no")


show("raises         ", lambda: drive(bad(Angry())))


# --- async for over a class with __aiter__ / __anext__ -------------------
class Counter:
    def __init__(self, n):
        self.n = n
        self.i = 0

    def __aiter__(self):
        return self

    async def __anext__(self):
        if self.i >= self.n:
            raise StopAsyncIteration
        self.i += 1
        return self.i


async def a6():
    out = []
    async for v in Counter(3):
        out.append(v)
    return out


print("async for  :", drive(a6()))


# --- async with -----------------------------------------------------------
class Ctx:
    async def __aenter__(self):
        return "entered"

    async def __aexit__(self, *a):
        return False


async def a7():
    async with Ctx() as v:
        return v


print("async with :", drive(a7()))


# --- an async generator ---------------------------------------------------
async def agen(n):
    for i in range(n):
        yield i


async def a8():
    out = []
    async for v in agen(2):
        out.append(v)
    return out


print("async gen  :", drive(a8()))
