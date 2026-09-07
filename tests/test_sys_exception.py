"""sys.exception(), and that it agrees with sys.exc_info().

3.11 added sys.exception() as the one-value form of exc_info(), and CPython's
own traceback.py is written against it: format_exc() is literally
`"".join(format_exception(sys.exception(), ...))`.  Without it every
traceback.format_exc() and print_exc() raised AttributeError, which is a long
way from where the caller was standing.

Both read the exception being HANDLED, not the one merely in flight, so the
interesting cases are the ones where those differ: outside any handler, inside
a nested handler, after the handler has finished, across a generator's
suspension, and inside a finally.
"""

import sys

print("outside any handler:", sys.exception(), sys.exc_info())


def basic():
    try:
        raise ValueError("v")
    except ValueError:
        return repr(sys.exception()), sys.exc_info()[0].__name__


print("inside:", basic())


def agrees():
    try:
        raise KeyError("k")
    except KeyError:
        return sys.exception() is sys.exc_info()[1]


print("agrees with exc_info:", agrees())


def after():
    try:
        raise ValueError("v")
    except ValueError:
        pass
    return sys.exception()


print("after the handler:", after())


def nested():
    out = []
    try:
        raise ValueError("outer")
    except ValueError:
        out.append(repr(sys.exception()))
        try:
            raise TypeError("inner")
        except TypeError:
            out.append(repr(sys.exception()))
        out.append(repr(sys.exception()))
    out.append(repr(sys.exception()))
    return out


print("nested:", nested())


def in_finally():
    try:
        try:
            raise ValueError("f")
        finally:
            inside = sys.exception()
    except ValueError:
        return repr(inside), repr(sys.exception())


print("finally:", in_finally())


def bare_reraise():
    try:
        try:
            raise ValueError("b")
        except ValueError:
            raise
    except ValueError:
        return repr(sys.exception())


print("bare raise:", bare_reraise())


def gen():
    try:
        raise ValueError("g")
    except ValueError:
        yield repr(sys.exception())
        yield repr(sys.exception())


g = gen()
print("generator:", next(g))
print("outside while suspended:", sys.exception())
print("generator again:", next(g))


def from_a_call():
    def look():
        return repr(sys.exception())
    try:
        raise ValueError("c")
    except ValueError:
        return look()


print("seen by a callee:", from_a_call())


def in_except_star():
    seen = []
    try:
        raise ExceptionGroup("eg", [ValueError("a")])
    except* ValueError:
        seen.append(type(sys.exception()).__name__)
    return seen


print("except*:", in_except_star())


class CM:
    def __enter__(self):
        return self

    def __exit__(self, et, ev, tb):
        print("  __exit__ sees:", sys.exception())
        return True


def in_with():
    with CM():
        raise ValueError("w")
    return sys.exception()


print("with:", in_with())

print("done")
