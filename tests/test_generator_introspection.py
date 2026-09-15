# What a generator, a coroutine and an async generator say about themselves.
#
# Three gaps, and the first is the one with reach.  gen_new set gi_name to
# NULL with the comment "not critical" -- but gen_get_name is what answers
# __name__ and __qualname__, so EVERY generator, coroutine and async
# generator reported None: to inspect, to a traceback, and to asyncio, which
# names a Task after the coroutine it wraps.
#
# gi_yieldfrom was answerable all along: gen_yf is what throw() and close()
# use to reach a delegated-to child first, under PEP 380, and it was never
# published.  cr_await and ag_await are the same reader under other names.
#
# gi_suspended needed one bit that did not exist.  instr_ptr cannot tell a
# fresh generator from a suspended one -- it is non-zero for both, and zero
# only once the generator has finished -- so the distinction
# inspect.getgeneratorstate rests on had nothing to read.
#
# And an async generator had no ag_frame, ag_code or ag_running at all, where
# the coroutine beside it has had cr_* since it was written.
import asyncio
import inspect


def plain():
    yield 1
    yield 2


def delegating():
    yield from plain()


async def coroutine():
    return 1


async def agen():
    yield 1


# --- names --------------------------------------------------------------
g = plain()
print("generator:", g.__name__, g.__qualname__)
c = coroutine()
print("coroutine:", c.__name__, c.__qualname__)
c.close()
a = agen()
print("async generator:", a.__name__, a.__qualname__)


def outer():
    def inner():
        yield 1

    return inner()


print("a nested one keeps its qualname:", outer().__qualname__)

# The name is the CODE's, so it follows a rename of the function object and
# not of the binding.
renamed = plain
print("the binding does not change it:", renamed().__name__)

# --- gi_yieldfrom / cr_await / ag_await ---------------------------------
print("not delegating:", g.gi_yieldfrom)
d = delegating()
print("before it starts:", d.gi_yieldfrom)
next(d)
print("while delegating:", type(d.gi_yieldfrom).__name__,
      d.gi_yieldfrom.__name__)
d.close()
_c3 = coroutine()
print("coroutine cr_await:", _c3.cr_await)
_c3.close()
print("async generator ag_await:", a.ag_await)

# --- gi_suspended / cr_suspended / ag_suspended -------------------------
g2 = plain()
print("fresh:", g2.gi_suspended, "running:", g2.gi_running)
next(g2)
print("suspended:", g2.gi_suspended, "running:", g2.gi_running)
next(g2)
try:
    next(g2)
except StopIteration:
    pass
print("finished:", g2.gi_suspended, "frame:", g2.gi_frame)

c2 = coroutine()
print("coroutine fresh:", c2.cr_suspended, c2.cr_running)
c2.close()
print("async generator fresh:", a.ag_suspended, a.ag_running)

# A generator that asks about itself while running sees gi_running True.
def introspects():
    yield introspects_self.gi_running, introspects_self.gi_suspended


introspects_self = introspects()
print("from inside:", next(introspects_self))

# --- inspect's four states ----------------------------------------------
g3 = plain()
print("created:", inspect.getgeneratorstate(g3))
next(g3)
print("suspended:", inspect.getgeneratorstate(g3))
g3.close()
print("closed:", inspect.getgeneratorstate(g3))

# --- ag_frame and ag_code ------------------------------------------------
print("ag_frame:", a.ag_frame is not None, "ag_code:", a.ag_code.co_name)
print("ag_running:", a.ag_running)
print("they match the generator's:",
      type(a.ag_frame).__name__, type(a.ag_code).__name__)

# --- and everything still runs -------------------------------------------
print("generator:", list(plain()))
print("delegating:", list(delegating()))


async def drive():
    return [v async for v in agen()], await coroutine()


print("async:", asyncio.run(drive()))
print("survived")
