# The awaitable async_gen.__anext__() returns has methods, by name.
#
# It had a live tp_iternext and no tp_dict at all, so `.send(None)` -- the
# only way to drive an async generator without an event loop, and what
# CPython's own test_asyncgen does throughout to compare a sync generator
# with an async one -- raised AttributeError.
#
# tp_iternext answers NULL for the generator's own item and leaves it in
# gi_return_value, because that is the shape SEND's exhaustion path reads.
# By name the protocol is the other one: StopIteration carrying the value.


def to_list_sync(gen):
    res = []
    while True:
        try:
            res.append(gen.__next__())
        except StopIteration:
            break
        except Exception as ex:
            res.append(type(ex).__name__)
            break
    return res


def to_list_async(agen):
    res = []
    while len(res) < 12:
        an = agen.__anext__()
        try:
            while True:
                try:
                    an.send(None)
                except StopIteration as ex:
                    res.append(ex.args[0] if ex.args else "EMPTY")
                    break
                except StopAsyncIteration:
                    raise
                except Exception as ex:
                    res.append(type(ex).__name__)
                    raise
        except StopAsyncIteration:
            break
        except Exception:
            break
    return res


def sync_gen():
    try:
        yield 1
        1 / 0
    finally:
        yield 2
        yield 3
    yield 100


async def async_gen():
    try:
        yield 1
        1 / 0
    finally:
        yield 2
        yield 3
    yield 100


print(to_list_sync(sync_gen()))
print(to_list_async(async_gen()))


# The plain shape, and the names themselves.
async def plain():
    yield "a"
    yield "b"


g = plain()
an = g.__anext__()
print(type(an).__name__)
for name in ("send", "close", "__next__", "__iter__", "__await__"):
    print(name, hasattr(an, name))

# __iter__ and __await__ both answer self.
print(an.__iter__() is an, an.__await__() is an)

# __next__ is send(None).
try:
    an.__next__()
except StopIteration as e:
    print("next ->", e.args)

an = g.__anext__()
try:
    an.send(None)
except StopIteration as e:
    print("send ->", e.args)

an = g.__anext__()
try:
    an.send(None)
except StopAsyncIteration:
    print("exhausted")

# close() marks the awaitable closed and leaves the generator alone.
async def three():
    yield 1
    yield 2


g = three()
an = g.__anext__()
print(an.close())
try:
    an.send(None)
except RuntimeError as e:
    print("closed asend refused:", e)
# ...and the generator itself still has its items.
an = g.__anext__()
try:
    an.send(None)
except StopIteration as e:
    print("generator still live:", e.args)

# An asend is one step of one iteration: driving the same one twice is the
# caller's bug, and CPython says so rather than reporting exhaustion.
an = g.__anext__()
try:
    an.send(None)
except StopIteration as e:
    print("first send:", e.args)
try:
    an.send(None)
except RuntimeError as e:
    print("reuse refused:", e)

# Arity and receiver.
an = three().__anext__()
for bad in ((), (1, 2)):
    try:
        an.send(*bad)
    except TypeError:
        print("send arity refused", len(bad))
try:
    type(an).send(5, None)
except TypeError:
    print("send receiver refused")
try:
    an.close(1)
except TypeError:
    print("close arity refused")

# A value sent in reaches the generator.
async def echo():
    got = yield "first"
    yield got


g = echo()
an = g.__anext__()
try:
    an.send(None)
except StopIteration as e:
    print("first:", e.args)
an = g.asend("hello") if hasattr(g, "asend") else g.__anext__()
try:
    an.send(None)
except StopIteration as e:
    print("echoed:", e.args)

print("done")


# ---------------------------------------------------------------------------
# asend(), aclose() and athrow() each answer an AWAITABLE.  All three were
# aliases for the SYNCHRONOUS send/close/throw, so `await agen.aclose()`
# awaited None and raised TypeError -- which is where every `async with` over
# an @asynccontextmanager generator failed -- and `await agen.asend(v)`
# awaited the wrapped yield the sync path had already produced, so v never
# reached the generator.
def run(coro):
    while True:
        try:
            coro.send(None)
        except StopIteration as e:
            return e.value


async def echo2():
    got = yield "first"
    yield got


async def use_asend():
    g = echo2()
    out = [await g.asend(None)]
    out.append(await g.asend("sent"))
    return out


print(run(use_asend()))


async def closer():
    try:
        yield 1
        yield 2
    finally:
        print("finally ran")


async def use_aclose():
    g = closer()
    first = await g.asend(None)
    print("aclose ->", await g.aclose())
    return first


print(run(use_aclose()))


async def catcher():
    try:
        yield 1
    except ValueError as e:
        print("caught", e)
        yield 99


async def use_athrow():
    g = catcher()
    out = [await g.asend(None)]
    out.append(await g.athrow(ValueError("boom")))
    return out


print(run(use_athrow()))


# An unhandled athrow comes back out.
async def deaf():
    yield 1


async def use_athrow_unhandled():
    g = deaf()
    await g.asend(None)
    try:
        await g.athrow(KeyError("k"))
    except KeyError as e:
        print("unhandled athrow:", e.args)


run(use_athrow_unhandled())

# The awaitables carry the same five names.
async def two():
    yield 1
    yield 2


g2 = two()
for obj in (g2.asend(None), g2.aclose(), g2.athrow(ValueError)):
    print(type(obj).__name__, [hasattr(obj, n) for n in
                               ("send", "close", "__next__", "__iter__", "__await__")])

# aclose() on a generator that never started, and twice.
async def never():
    yield 1


async def use_double_aclose():
    g = never()
    a = g.aclose()
    print("first:", await a)
    try:
        a.send(None)
    except RuntimeError as e:
        print("reuse refused:", e)
    print("second:", await g.aclose())


run(use_double_aclose())

# Arity.
g3 = two()
# (athrow's own arity is not checked here: CPython's takes 1 to 3 arguments
# and builds the exception from the first two, while gen.throw and athrow
# both take exactly one here.)
for call, label in ((lambda: g3.aclose(1), "aclose"),
                    (lambda: g3.asend(), "asend")):
    try:
        call()
    except TypeError:
        print(label, "arity refused")

print("done 2")


# ---------------------------------------------------------------------------
# ags_sendval means two different things: asend's resume value and athrow's
# EXCEPTION.  send() is one method over both awaitables, and it stored the
# sent value into that field whichever it was -- so `agen.aclose().send(3)`
# handed gen_throw a plain int to raise, which is a segfault from ordinary
# Python, and the athrow form dropped the exception's reference on the way.
import sys


async def one():
    yield 1


a = one()
an = a.__anext__()
try:
    an.send(None)
except StopIteration:
    pass
for make, label in ((lambda g: g.aclose(), "aclose"),
                    (lambda g: g.athrow(ValueError("x")), "athrow")):
    try:
        make(one()).send(3)
    except RuntimeError as e:
        print(label, "refuses a sent value:", e)

# The sent value replaces what asend() carried, and neither leaks.
class T:
    pass


async def echoing():
    while True:
        got = yield
        del got


g = echoing()
try:
    g.asend(None).send(None)
except StopIteration:
    pass
t = T()
base = sys.getrefcount(t)
for _ in range(4):
    try:
        g.asend(t).send(None)
    except StopIteration:
        pass
    try:
        g.asend(T()).send(t)
    except StopIteration:
        pass
print("asend argument leak:", sys.getrefcount(t) - base)

# ...and neither does the item a drive hands back through StopIteration.
async def yielding(v):
    while True:
        yield v


item = T()
gy = yielding(item)
base = sys.getrefcount(item)
for _ in range(5):
    try:
        gy.__anext__().send(None)
    except StopIteration:
        pass
print("yielded item leak:", sys.getrefcount(item) - base)

# athrow takes CPython's deprecated (type, value, traceback) spelling too.
async def catching():
    try:
        yield 1
    except ValueError as e:
        print("caught", e.args)
        yield 2


def drive(coro):
    try:
        while True:
            coro.send(None)
    except StopIteration as e:
        return e.value


async def use(args):
    g = catching()
    await g.asend(None)
    return await g.athrow(*args)


import warnings

# The deprecated spellings warn, and the filter is what keeps the message --
# whose text names this FILE, spelled differently by a script run directly and
# by its .pyc -- out of the comparison below.
warnings.simplefilter("ignore", DeprecationWarning)

print(drive(use((ValueError,))))
print(drive(use((ValueError, "msg"))))
print(drive(use((ValueError, "msg", None))))
print(drive(use((ValueError("inst"),))))

# ...and a filter that makes it an error stops the athrow before it throws.
with warnings.catch_warnings():
    warnings.simplefilter("error")
    try:
        drive(use((ValueError, "msg")))
    except DeprecationWarning as e:
        print("deprecated:", e)
    print(drive(use((ValueError,))))
for bad in ((), (ValueError, 1, 2, 3)):
    try:
        drive(use(bad))
    except TypeError as e:
        print("athrow arity:", e)

# athrow's first argument has to BE an exception class or instance.  It was
# handed straight to type_call, which jumps through whatever sits at tp_call
# in an object of another shape -- `athrow(5, 6)` was a segfault from ordinary
# Python.  And a value that is ALREADY an instance of the class is the
# exception, not an argument to build one from.
# (GeneratorExit is NOT caught below, so a generator left suspended closes
# cleanly at the end and none of this prints "Exception ignored in".)
async def catching2():
    try:
        yield 1
    except (ValueError, KeyError) as e:
        yield (type(e).__name__, e.args)


async def use2(args):
    g = catching2()
    await g.asend(None)
    try:
        return await g.athrow(*args)
    except TypeError as e:
        return ("TypeError", str(e))
    finally:
        await g.aclose()


for args in ((ValueError("x"), "y", None), (5, 6), ("s", "t"), ([1], 2),
             (None, 1), (ValueError, ValueError("z")), (ValueError, "w"),
             (ValueError, None), (KeyError,)):
    print(args[0].__class__.__name__, "->", drive(use2(args)))

print("done 3")
