# __context__ across a generator resume, and across await.
#
# gen_send CLEARED current_exception before resuming the frame, so an
# exception raised inside the generator had nothing to chain to: `next(it)`
# from inside an except block produced one with __context__ of None.  `await`
# goes through gen_send too, so every awaited exception lost its context the
# same way.  The caller's exception is left in place now; the generator's own
# handler state lives on its value stack and is restored by its POP_EXCEPT.


def show(label, fn):
    try:
        print(label, "=>", repr(fn()))
    except BaseException as e:
        print(label, "!!", type(e).__name__, e)


def ctx_of(e):
    return type(e.__context__).__name__ if e.__context__ is not None else None


# --- the plain cases, which already worked
def sync():
    try:
        raise KeyError("outer")
    except KeyError:
        try:
            raise ValueError("inner")
        except ValueError as e:
            return ctx_of(e)


show("direct", sync)


def via_call():
    def boom():
        raise ValueError("inner")

    try:
        raise KeyError("outer")
    except KeyError:
        try:
            boom()
        except ValueError as e:
            return ctx_of(e)


show("through a call", via_call)


# --- across a generator resume
def via_generator():
    def g():
        yield 1
        raise ValueError("inner")

    it = g()
    next(it)
    try:
        raise KeyError("outer")
    except KeyError:
        try:
            next(it)
        except ValueError as e:
            return ctx_of(e)


show("generator resume", via_generator)


def generator_no_context():
    def g():
        yield 1
        raise ValueError("inner")

    it = g()
    next(it)
    try:
        next(it)
    except ValueError as e:
        return ctx_of(e)


show("no outer", generator_no_context)


# A generator that handles its own exception must not leak it to the caller.
def generator_handles_own():
    def g():
        try:
            raise KeyError("mine")
        except KeyError:
            yield 1
        yield 2

    it = g()
    next(it)
    next(it)
    try:
        raise ValueError("after")
    except ValueError as e:
        return ctx_of(e)


show("generator's own", generator_handles_own)


# One yielded from inside an except block, then dropped.
def generator_suspended_in_except():
    def g():
        try:
            raise KeyError("mine")
        except KeyError:
            yield 1

    it = g()
    next(it)
    del it
    try:
        raise ValueError("after")
    except ValueError as e:
        return ctx_of(e)


show("suspended in except", generator_suspended_in_except)


# --- across await
import asyncio


async def boom():
    raise ValueError("inner")


async def await_coro():
    try:
        raise KeyError("outer")
    except KeyError:
        try:
            await boom()
        except ValueError as e:
            return ctx_of(e)


show("await coroutine", lambda: asyncio.run(await_coro()))


async def await_no_context():
    try:
        await boom()
    except ValueError as e:
        return ctx_of(e)


show("await no outer", lambda: asyncio.run(await_no_context()))


async def deep():
    async def middle():
        await boom()

    try:
        raise KeyError("outer")
    except KeyError:
        try:
            await middle()
        except ValueError as e:
            return ctx_of(e)


show("await two deep", lambda: asyncio.run(deep()))


# --- the chain has more than one link
def two_links():
    try:
        raise KeyError("a")
    except KeyError:
        try:
            raise IndexError("b")
        except IndexError:
            try:
                raise ValueError("c")
            except ValueError as e:
                return (ctx_of(e), ctx_of(e.__context__))


show("two links", two_links)

# --- explicit `raise ... from` still wins
def explicit():
    try:
        raise KeyError("outer")
    except KeyError:
        try:
            raise ValueError("inner") from None
        except ValueError as e:
            return (e.__cause__, ctx_of(e), e.__suppress_context__)


show("raise from None", explicit)

print("done")
