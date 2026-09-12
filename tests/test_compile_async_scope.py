# `await`, `async for` and `async with` need an async def around them.
#
# The symbol table checked the scope's KIND and nothing else, so
# `def f(): await x` compiled, and so did an `async for` in a plain def and a
# comprehension with an `async for` in it -- thirty-four assertions in
# CPython's test_coroutines.  SCF_COROUTINE could not answer the question:
# a bare `await` sets it just as `async def` does.  SCF_ASYNC_DEF is the half
# that means DECLARED async.
#
# The refusals are in tests/syntax_corpus.txt, where all five SyntaxError
# fields are compared against CPython's.  This file is the other direction:
# everything that must keep compiling, and keep working.
#
# A GENERATOR EXPRESSION is exempt, and completely -- PEP 530 lets
# `(i async for i in a)` and `(await i for i in a)` appear anywhere, because
# what they build is an async generator the caller drives.

import asyncio


async def arange(n):
    for i in range(n):
        yield i


async def plain():
    return 1


async def uses_await():
    return await plain() + 1


async def uses_async_for(n):
    out = []
    async for i in arange(n):
        out.append(i)
    return out


class Ctx:
    async def __aenter__(self):
        return "in"

    async def __aexit__(self, *a):
        return False


async def uses_async_with():
    async with Ctx() as v:
        return v


async def async_listcomp(n):
    return [i * 2 async for i in arange(n)]


async def async_setcomp(n):
    return sorted({i async for i in arange(n)})


async def async_dictcomp(n):
    return dict([(k, v) for k, v in (await async_dict(n)).items()])


async def async_dict(n):
    return {i: i * i async for i in arange(n)}


async def awaits_in_comp(n):
    return [await plain() for _ in range(n)]


async def plain_comp_in_async():
    return [x for x in range(3)]


async def nested_async():
    async def inner():
        return await plain()

    return await inner()


async def has_lambda():
    f = lambda: 7
    return f()


async def async_generator(n):
    async for i in arange(n):
        yield i * 3


# A generator expression is legal in a PLAIN def, async clause and all.
def genexp_from_plain_def(a):
    return (i async for i in a)


def await_in_genexp_from_plain_def(a):
    return (await i for i in a)


def await_in_genexp_condition(a):
    return (i for i in a if await i)


class HasGenexp:
    # And in a class body, which is not a function at all.
    maker = staticmethod(lambda a: (i async for i in a))


# `yield` makes an async def an async generator, which is fine; only
# `yield from` in one is refused.
async def async_gen_with_yield():
    yield 1
    yield 2


def plain_yield_from():
    yield from (1, 2)


async def main():
    print(await uses_await())
    print(await uses_async_for(4))
    print(await uses_async_with())
    print(await async_listcomp(3))
    print(await async_setcomp(3))
    print(await async_dict(3))
    print(await awaits_in_comp(2))
    print(await plain_comp_in_async())
    print(await nested_async())
    print(await has_lambda())
    print([i async for i in async_generator(3)])
    print([i async for i in async_gen_with_yield()])
    print([i async for i in genexp_from_plain_def(arange(3))])
    print(list(plain_yield_from()))

    g = genexp_from_plain_def(arange(2))
    print(type(g).__name__, [i async for i in g])

    h = HasGenexp.maker(arange(2))
    print([i async for i in h])

    print(await_in_genexp_from_plain_def.__name__,
          await_in_genexp_condition.__name__)


asyncio.run(main())

# `await (await x)` is legal; `await await x` is not.
print(compile("async def f():\n    await (await x)\n", "<t>", "exec").co_name)
