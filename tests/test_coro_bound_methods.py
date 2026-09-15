# A coroutine's send/throw/close are BOUND methods.
#
# coro_getattr answered them with the raw cached builtin and an incref, so the
# coroutine itself was never part of the callable.  The attribute then worked
# only by accident: LOAD_ATTR's method fast path supplies the receiver
# separately, so `o.close()` written out in full happened to pass one.  Take
# the attribute out of that shape -- store it in a variable, or call it with
# `*args` -- and the receiver is gone:
#
#     async def sc(): pass
#     f = sc().close
#     f(*())              # SIGSEGV
#
# _gen_close_impl then read args[0] off a NULL array.  Worse than the crash is
# the quiet case: `f()` reads whatever the caller's stack left there and
# treats it as a generator.
#
# gen_getattr had the identical defect and was fixed, with a comment that
# names this exact failure -- "returning the raw builtin left self to
# LOAD_ATTR's method fast path".  async_gen_getattr was written correctly.
# coro_getattr is the third copy, and it was never brought along.
#
# Found in CPython's test_asyncio.test_base_events.test_call_coroutine, which
# does `self.addCleanup(coro_obj.close)` -- unittest stores the bound method
# and calls it later with no arguments at all.
import sys


async def sc():
    return 5


def gen():
    yield 1


async def agen():
    yield 1


# The three, as attributes of an instance.  What is compared is __self__ and
# behaviour, not the type of the callable: CPython hands back a
# builtin_function_or_method bound to the coroutine, while a builtin bound to
# an instance is a `method` here -- a divergence DIVERGENCES.md records, and
# not the one under test.
o = sc()
print("close self is the coroutine:", o.close.__self__ is o)
print("send self is the coroutine:", o.send.__self__ is o)
print("throw self is the coroutine:", o.throw.__self__ is o)

# Stored and called later, with no arguments and with an empty spread --
# which is what unittest's addCleanup does.
f = o.close
f()
print("stored close ran")

o2 = sc()
f2 = o2.close
f2(*())
print("spread close ran")

# send() on a fresh coroutine runs it to completion; the StopIteration
# carries the return value.
o3 = sc()
try:
    o3.send(None)
except StopIteration as e:
    print("send through a stored bound method:", e.value)

# throw() likewise reaches the coroutine rather than nothing.
o4 = sc()
t = o4.throw
try:
    t(ValueError("thrown in"))
except ValueError as e:
    print("throw reached the coroutine:", e)

# The generator and async-generator spellings, which were already right, so
# all three now agree.
g = gen()
print("gen close bound:", g.close.__self__ is g)
a = agen()
print("agen aclose bound:", a.aclose.__self__ is a)
ac = a.aclose()
try:
    ac.send(None)
except StopIteration:
    pass

# Unbound through the type still needs its receiver passed, and still refuses
# to run without one.
o5 = sc()
print("unbound with receiver:", type(o5).close(o5))
o6 = sc()
try:
    type(o6).close(*())
except TypeError as e:
    print("unbound with none:", e)
o6.close()  # or it is a coroutine never awaited, and CPython warns

print("survived")
