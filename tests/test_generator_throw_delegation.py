# throw() and close() delegate through `yield from`.
#
# A generator suspended at a `yield from` is not suspended at its own yield:
# the value came from a SUB-generator, and PEP 380 says throw() and close()
# reach that one first.  Neither did.
#
# close() looked as though it did, and that is the part worth writing down.
# gen_close is "throw GeneratorExit and interpret the result"; the unwinder
# then pops the outer frame's value stack, which drops the last reference to
# the sub-generator, and gen_dealloc runs its finally.  The cleanup happened,
# but by REFCOUNT rather than by delegation -- so holding a second reference to
# the inner generator left it suspended forever, and its finally never ran at
# all.  That is the shape the first case here is for.
import sys

# --- close() actually reaches the inner generator ----------------------------

log = []


def inner_logging():
    try:
        yield 1
        yield 2
    finally:
        log.append("inner finally")


def outer_logging(sub):
    yield from sub


sub = inner_logging()
g = outer_logging(sub)
print("first:", next(g))
g.close()
print("after close, log:", log)
# The second reference is the point: without delegation the inner generator is
# still suspended here and only its eventual dealloc would run the finally.
print("inner is closed:", sub.gi_frame is None)
try:
    next(sub)
except StopIteration:
    print("inner exhausted")

# --- throw() reaches the inner generator, which catches ----------------------

def inner_catches():
    try:
        yield 1
    except ValueError:
        yield "inner caught"


def outer(sub):
    yield from sub


g = outer(inner_catches())
print("start:", next(g))
print("thrown:", g.throw(ValueError("v")))

# --- the inner catches and RETURNS: the outer sees the value ----------------

def inner_returns():
    try:
        yield 1
    except ValueError:
        return "inner returned"


def outer_uses(sub):
    value = yield from sub
    yield ("outer got", value)


g = outer_uses(inner_returns())
print("start:", next(g))
print("after return:", g.throw(ValueError()))

# --- the inner does NOT catch: it propagates out of the outer ---------------

def inner_bare():
    yield 1


g = outer(inner_bare())
next(g)
try:
    g.throw(KeyError("k"))
except KeyError as e:
    print("propagated:", e)
print("outer closed after propagation:", g.gi_frame is None)

# --- three deep ------------------------------------------------------------

def level0():
    try:
        yield "deep"
    except ValueError:
        yield "caught at the bottom"


def level1(sub):
    yield from sub


def level2(sub):
    yield from sub


g = level2(level1(level0()))
print("deep start:", next(g))
print("deep thrown:", g.throw(ValueError()))

# --- a sub-iterator that is not a generator ---------------------------------
#
# PEP 380 says throw() looks for a `throw` method and falls back to raising
# locally when there is none.  A plain list iterator has none.

def over_a_list():
    try:
        yield from [1, 2, 3]
    except ValueError:
        yield "outer caught"


g = over_a_list()
print("list start:", next(g))
print("list thrown:", g.throw(ValueError()))

# ...and one that DOES define throw and close.
class Manual:
    def __init__(self):
        self.events = []

    def __iter__(self):
        return self

    def __next__(self):
        return "manual"

    def throw(self, *exc):
        self.events.append("throw")
        return "manual threw"

    def close(self):
        self.events.append("close")


m = Manual()


def over_manual():
    yield from m


g = over_manual()
print("manual start:", next(g))
print("manual thrown:", g.throw(ValueError()))
g2 = over_manual()
next(g2)
g2.close()
print("manual events:", m.events)

# --- GeneratorExit is not forwarded; the child is CLOSED --------------------
#
# close() closes the sub-iterator and then raises GeneratorExit in the outer
# generator itself, rather than forwarding it -- so an inner `except
# GeneratorExit` sees it as its own close, not as a throw from above.

order = []


def inner_notes():
    try:
        yield 1
    except GeneratorExit:
        order.append("inner saw exit")
        raise


def outer_notes(sub):
    try:
        yield from sub
    except GeneratorExit:
        order.append("outer saw exit")
        raise


s = inner_notes()
g = outer_notes(s)
next(g)
g.close()
print("exit order:", order)

# --- throwing into a generator that has not started -------------------------

def not_started():
    yield from inner_catches()


g = not_started()
try:
    g.throw(ValueError("early"))
except ValueError as e:
    print("not started:", e)

# --- an exhausted generator --------------------------------------------------

def done_quickly():
    yield from []


g = done_quickly()
try:
    next(g)
except StopIteration:
    pass
try:
    g.throw(ValueError("late"))
except ValueError as e:
    print("exhausted:", e)
g.close()
print("close after exhaustion is fine")

# --- the same over a coroutine ----------------------------------------------

def drive(coro, throw=None):
    try:
        coro.send(None)
        if throw is not None:
            return coro.throw(throw)
        return "no throw"
    except StopIteration as e:
        return ("stopped", e.value)


class Awaitable:
    def __await__(self):
        try:
            yield "awaiting"
        except ValueError:
            return "awaitable caught"


async def uses_awaitable():
    return await Awaitable()


print("coroutine:", drive(uses_awaitable(), ValueError()))

# close() on a coroutine suspended in an awaitable.
closed = []


class ClosingAwaitable:
    def __await__(self):
        try:
            yield "awaiting"
        finally:
            closed.append("awaitable finally")


async def uses_closing():
    await ClosingAwaitable()


c = uses_closing()
c.send(None)
c.close()
print("coroutine close:", closed)

print("done")
