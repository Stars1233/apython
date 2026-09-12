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


# PEP 530 again, and the half the first cut of the scope check got wrong in
# the OTHER direction: an enclosing GENERATOR EXPRESSION is as good as an
# async def, because a genexp with an async comprehension inside it is itself
# an async generator the caller drives.  The climb out of the comprehension
# scopes has to STOP at one.
def outer_genexp(y):
    return ([i async for i in x] for x in y)


def outer_genexp_set(y):
    return ({i async for i in x} for x in y)


def outer_genexp_dict(y):
    return ({i: i async for i in x} for x in y)


def outer_genexp_await(y):
    return ([await i for i in x] for x in y)


class HoldsGenexp:
    maker = staticmethod(lambda y: ([i async for i in x] for x in y))


def nested_genexps(w):
    return (([i async for i in x] for x in y) for y in w)


# What they ACCEPT is what this is about, and each of them builds SOMETHING
# iterable.  What KIND is not yet right: a genexp containing an async
# comprehension is an async generator in CPython and a plain generator when
# our own compiler builds it, because the nested comprehension marks its own
# scope a coroutine and not the genexp around it.  bugs.md records that, and
# it is why the type names are not printed here -- they would differ between a
# CPython .pyc and our compiler over the same source.
for maker in (outer_genexp, outer_genexp_set, outer_genexp_dict,
              outer_genexp_await, nested_genexps, HoldsGenexp.maker):
    g = maker([])
    print(maker.__name__ if hasattr(maker, "__name__") else "lambda",
          hasattr(g, "__next__") or hasattr(g, "__anext__"))

# A LIST comprehension around one is still refused; that pair is in
# tests/syntax_corpus.txt with all five fields compared.
for src in ("[[i async for i in x] for x in y]",
            "{[i async for i in x] for x in y}",
            "def f():\n    return [[i async for i in x] for x in y]\n"):
    try:
        compile(src, "<t>", "exec")
        print("ACCEPTED", repr(src))
    except SyntaxError as e:
        print("rejected", e.lineno, e.offset, e.end_lineno, e.end_offset)
