# Whose exception is a generator handling?
#
# A frame that is handling nothing of its own SHARES its caller's -- that is
# what gives a raise inside `except E: f()` a __context__ of E.  A generator
# is the one thing that can have its own, by suspending inside an except
# block, and it has to carry that one across the suspension and give it up
# again when the block ends.
#
# The two got confused: every frame kept a copy of whatever was being handled
# when it returned, so a generator first advanced from inside an `except`
# adopted its caller's exception and re-installed it on every later resume --
# long after the block that raised it had ended, and in front of whatever the
# resumer was really handling.

import sys


def where():
    e = sys.exc_info()[1]
    return None if e is None else "%s(%s)" % (type(e).__name__, e)


def plain():
    yield 1
    print("  plain, second   ", where())
    yield 2
    print("  plain, third    ", where())
    yield 3


# First advanced inside an except block, then outside it.
print("a generator does not adopt its caller's")
g = plain()
next(g)
try:
    raise ValueError("outer")
except ValueError:
    next(g)
next(g)


# ...and it does not adopt one from a DIFFERENT caller either.
print("nor a second caller's")
g = plain()
next(g)
try:
    raise KeyError("first")
except KeyError:
    next(g)
try:
    raise TypeError("second")
except TypeError:
    next(g)


def owns():
    """Suspended inside its own except block: this one it does keep."""
    try:
        raise IndexError("mine")
    except IndexError:
        yield 1
        print("  owns, resumed   ", where())
        yield 2
    print("  owns, after      ", where())
    yield 3


print("a generator suspended in its own except block keeps it")
g = owns()
next(g)
print("  caller sees      ", where())
try:
    raise ValueError("caller's")
except ValueError:
    next(g)
    print("  caller still     ", where())
next(g)
print("  caller after     ", where())


def nested():
    """Two deep, and only one of them survives the suspension."""
    try:
        raise ValueError("outer")
    except ValueError:
        try:
            raise KeyError("inner")
        except KeyError:
            yield "in the inner"
            print("  nested, inner   ", where())
        yield "in the outer"
        print("  nested, outer   ", where())
    yield "in neither"
    print("  nested, neither ", where())


print("nesting")
for v in nested():
    print(" ", v)


def reraises():
    """The exception escapes the generator, and the caller's is untouched."""
    try:
        raise RuntimeError("thrown")
    except RuntimeError:
        yield 1
        raise


print("an exception leaving a generator")
g = reraises()
next(g)
try:
    raise ValueError("held")
except ValueError:
    try:
        next(g)
    except RuntimeError as e:
        print("  caught           ", type(e).__name__, e)
        print("  its context      ", type(e.__context__).__name__)
    print("  caller still     ", where())
print("  caller after     ", where())


# A generator abandoned mid-except: closing it throws GeneratorExit in, which
# unwinds through the except block.  Nothing of it may be left behind.
print("an abandoned generator")
g = owns()
next(g)
g.close()
print("  after close      ", where())
try:
    raise ValueError("still ours")
except ValueError:
    print("  inside except    ", where())
print("  outside          ", where())
print("done")
