# A decorator over an `async def`.
#
# ps_decorated accepted only TOK_DEF and TOK_CLASS after a decorator, so
# `@wraps(f)` over a coroutine was a hard SyntaxError -- which is how
# contextlib.py, _collections_abc.py and a hundred other Lib/ sites are
# written.  ps_async already returns exactly the node AST_DECORATED wants, so
# accepting the token is the whole fix; the only care needed is that `async
# for` and `async with` after a decorator stay errors, as they are in CPython.
import asyncio


def tag(f):
    f.tagged = True
    return f


@tag
async def go():
    return 7


print(asyncio.run(go()), go.tagged)


# Stacked decorators, and an attribute-form decorator.
class Reg:
    def __init__(self):
        self.seen = []

    def add(self, f):
        self.seen.append(f.__name__)
        return f


reg = Reg()


def twice(f):
    f.twice = True
    return f


@twice
@reg.add
async def stacked():
    return "s"


print(asyncio.run(stacked()), stacked.twice, reg.seen)


# A decorator with arguments, over an async def in a class body.
def note(label):
    def deco(f):
        f.label = label
        return f

    return deco


class C:
    @note("m")
    async def m(self):
        return "from m"


print(asyncio.run(C().m()), C.m.label)


# The coroutine flag survives decoration.
print(bool(go.__code__.co_flags & 0x80))


# `async for` and `async with` after a decorator are still errors.
for src in ("@d\nasync for x in y:\n    pass\n",
            "@d\nasync with c:\n    pass\n"):
    try:
        compile(src, "<t>", "exec")
        print("accepted, should not have")
    except SyntaxError:
        print("rejected")


# An undecorated async def is unaffected.
async def plain():
    return "plain"


print(asyncio.run(plain()))
