# `async with` finds __aenter__ and __aexit__ on the MRO, not just on the
# object's own type dict.
#
# op_before_async_with read ob_type->tp_dict with dict_get and then called
# method_new on whatever came back: no MRO walk, and no __get__.  So an
# INHERITED __aexit__ was invisible -- `'async with' requires __aexit__
# method` for a class whose base defines it -- while hasattr() said True and
# the synchronous `with` worked.
#
# The sync BEFORE_WITH had the identical defect and was converted to
# dunder_lookup_special, which walks the MRO and hands back an already-bound
# callable.  The async one was left behind.
#
# Every mixin-based async context manager has this shape.  asyncio.Lock,
# Condition, Semaphore and BoundedSemaphore all inherit __aenter__/__aexit__
# from _ContextManagerMixin, so `async with lock:` could not work at all.

import sys

order = []


def drive(coro):
    """Run a coroutine that never awaits anything that blocks."""
    try:
        coro.send(None)
    except StopIteration as e:
        return e.value
    raise AssertionError("coroutine did not finish")


class Mixin:
    async def __aenter__(self):
        order.append("enter")
        return self

    async def __aexit__(self, *exc):
        order.append("exit")
        return False


class Inherited(Mixin):
    pass


class DeepMixin(Inherited):
    pass


async def use(cls):
    async with cls() as obj:
        order.append("body")
    return type(obj).__name__


del order[:]
print(drive(use(Mixin)), order, "defined on the class itself")

del order[:]
print(drive(use(Inherited)), order, "inherited from a base")

del order[:]
print(drive(use(DeepMixin)), order, "inherited two levels up")


# A classmethod is a descriptor, and must be bound as one rather than wrapped
# in a plain bound method over the underlying function.
class ClassMethodCM:
    @classmethod
    async def __aenter__(cls):
        order.append("cm-enter")
        return cls

    @classmethod
    async def __aexit__(cls, *exc):
        order.append("cm-exit")
        return False


async def use_cm():
    async with ClassMethodCM():
        order.append("body")


del order[:]
drive(use_cm())
print(order, "a classmethod __aenter__/__aexit__")


# A missing __aexit__ still says so, and names the type as CPython does.
class NoExit:
    async def __aenter__(self):
        return self


async def broken():
    async with NoExit():
        pass


try:
    drive(broken())
    print(False, "a missing __aexit__ must raise")
except TypeError as e:
    print(True, "a missing __aexit__ raises TypeError")


# And a __aexit__ that is not callable is a TypeError too, not a crash.
class BadExit:
    async def __aenter__(self):
        return self

    __aexit__ = 42


async def bad():
    async with BadExit():
        pass


try:
    drive(bad())
    print(False, "a non-callable __aexit__ must raise")
except TypeError:
    print(True, "a non-callable __aexit__ raises TypeError")
