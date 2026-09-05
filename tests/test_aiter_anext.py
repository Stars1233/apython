# aiter() and anext(), and what they do with something that is not one.
#
# anext read its argument's ob_type without asking whether it was a pointer,
# so `anext(0)` read a small integer's Value as one.  aiter called tp_iter --
# the ORDINARY iterator protocol -- so `aiter([])` answered a list_iterator
# for a list, which is not asynchronously iterable at all; it asks for
# __aiter__ by name now, as CPython does.
#
# Which meant the async generator needed those names.  It had a getattr of its
# own and no tp_dict, so `hasattr(g, "__aiter__")` was False and aiter()
# refused a genuine async generator.

import asyncio


def show(label, fn):
    try:
        print("%-26s %r" % (label, fn()))
    except BaseException as e:
        print("%-26s %s: %s" % (label, type(e).__name__, e))


for label, fn in (("aiter(0)", lambda: aiter(0)),
                  ("aiter(None)", lambda: aiter(None)),
                  ("aiter([])", lambda: aiter([])),
                  ("aiter('x')", lambda: aiter("x")),
                  ("aiter({})", lambda: aiter({})),
                  ("anext(0)", lambda: anext(0)),
                  ("anext(None)", lambda: anext(None)),
                  ("anext([])", lambda: anext([])),
                  ("anext(0, 1)", lambda: anext(0, 1)),
                  ("aiter()", lambda: aiter()),
                  ("anext()", lambda: anext())):
    show(label, fn)


async def gen():
    yield 1
    yield 2


async def main():
    g = gen()
    print("has the names", hasattr(g, "__aiter__"), hasattr(g, "__anext__"))
    it = aiter(g)
    print("aiter is itself", it is g)
    print(await anext(it), await anext(it))
    try:
        await anext(it)
        print("not exhausted?")
    except StopAsyncIteration:
        print("exhausted")
    async for v in gen():
        print("loop", v)
    print("by name", await gen().__anext__())


asyncio.run(main())
print("done")
