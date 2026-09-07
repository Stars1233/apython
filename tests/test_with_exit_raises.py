"""A context-manager slot that raises.

`with` is the one place where a slot's exception has to be delivered while
another exception is already in flight, and every arm of that is here.

Until now two of them were wrong, and neither was a wrong answer -- one was a
SIGSEGV and the other lost the exception entirely:

  * `__exit__` raising while an exception was in flight pushed the NULL its
    call returned onto the value stack.  POP_JUMP_IF_TRUE popped the 0 and
    obj_is_true dereferenced address 0.

  * `__enter__` raising pushed the same NULL, and the exception then escaped
    the enclosing `try` rather than being caught by it.

Both are CPython's `ERROR_IF(res == NULL, error)`.  The async forms and the
clean-exit form were already right and are here so they stay that way.
"""

import sys


# --- __exit__ raising while an exception is in flight -----------------------

class ExitRaises:
    def __enter__(self):
        return self

    def __exit__(self, et, ev, tb):
        raise ValueError("from exit")


try:
    with ExitRaises():
        raise TypeError("inner")
except ValueError as e:
    print("exit-in-flight:", type(e).__name__, e)
    print("  context:", type(e.__context__).__name__, e.__context__)

# The same, with the with-block inside a function, so the unwinder has a
# frame to leave rather than module scope.
def exit_in_flight_in_func():
    try:
        with ExitRaises():
            raise TypeError("inner-func")
    except ValueError as e:
        return type(e).__name__, str(e), type(e.__context__).__name__

print("in function:", exit_in_flight_in_func())


# An AttributeError raised inside __exit__ is the shape unittest's
# assertRaises actually produces, via traceback.clear_frames.
class ExitAttributeError:
    def __enter__(self):
        return self

    def __exit__(self, et, ev, tb):
        ev.no_such_attribute


try:
    with ExitAttributeError():
        raise TypeError("inner2")
except AttributeError as e:
    print("exit attr:", type(e).__name__)
    print("  context:", type(e.__context__).__name__)


# --- __exit__ raising on a clean exit ---------------------------------------

class ExitRaisesClean:
    def __enter__(self):
        return self

    def __exit__(self, et, ev, tb):
        raise ValueError("exit-clean")


try:
    with ExitRaisesClean():
        pass
except ValueError as e:
    print("exit-clean:", e, "context:", type(e.__context__).__name__)


# --- __enter__ raising ------------------------------------------------------

class EnterRaises:
    def __enter__(self):
        raise ValueError("from enter")

    def __exit__(self, *a):
        print("  (__exit__ must NOT run)")
        return False


try:
    with EnterRaises():
        print("  (body must NOT run)")
except ValueError as e:
    print("enter:", type(e).__name__, e)


def enter_in_func():
    try:
        with EnterRaises():
            return "body ran - wrong"
    except ValueError as e:
        return "caught " + str(e)

print("in function:", enter_in_func())


# __enter__ raising while another exception is in flight.
def enter_while_handling():
    try:
        raise TypeError("outer")
    except TypeError:
        try:
            with EnterRaises():
                pass
        except ValueError as e:
            return type(e).__name__, type(e.__context__).__name__

print("enter while handling:", enter_while_handling())


# --- the exit is still called when only the body raises ---------------------

class ExitSwallows:
    def __enter__(self):
        return self

    def __exit__(self, et, ev, tb):
        print("  exit saw:", et.__name__ if et else None)
        return True


with ExitSwallows():
    raise RuntimeError("swallowed")
print("swallowed ok")


class ExitDeclines:
    def __enter__(self):
        return self

    def __exit__(self, et, ev, tb):
        return False


try:
    with ExitDeclines():
        raise RuntimeError("declined")
except RuntimeError as e:
    print("declined:", e)


# --- nesting ----------------------------------------------------------------

def nested():
    try:
        with ExitDeclines():
            with ExitRaises():
                raise TypeError("innermost")
    except ValueError as e:
        return type(e).__name__, type(e.__context__).__name__

print("nested:", nested())


# The value stack has to come back to the right depth: a `with` whose exit
# raises, inside a loop, must not drift.
def in_a_loop(n):
    seen = []
    for i in range(n):
        try:
            with ExitRaises():
                raise TypeError(i)
        except ValueError as e:
            seen.append(str(e.__context__))
    return seen

print("loop:", in_a_loop(4))


# A `return` out of a with-block whose exit then raises.
def return_then_exit_raises():
    try:
        with ExitRaisesClean():
            return "returned"
    except ValueError as e:
        return "exit raised: " + str(e)

print("return:", return_then_exit_raises())


# --- generators -------------------------------------------------------------

def gen_with_exit_raises():
    try:
        with ExitRaises():
            yield 1
            raise TypeError("in gen")
    except ValueError as e:
        yield "caught " + str(e)

print("generator:", list(gen_with_exit_raises()))


# --- async ------------------------------------------------------------------

import asyncio


class AEnterRaises:
    async def __aenter__(self):
        raise ValueError("from aenter")

    async def __aexit__(self, *a):
        return False


class AExitRaises:
    async def __aenter__(self):
        return self

    async def __aexit__(self, *a):
        raise ValueError("from aexit")


async def a_enter():
    try:
        async with AEnterRaises():
            return "body ran - wrong"
    except ValueError as e:
        return "caught " + str(e)


async def a_exit_in_flight():
    try:
        async with AExitRaises():
            raise TypeError("async inner")
    except ValueError as e:
        return type(e).__name__, type(e.__context__).__name__


async def amain():
    print("async enter:", await a_enter())
    print("async exit:", await a_exit_in_flight())


asyncio.run(amain())


# --- a non-manager still reports the same TypeError -------------------------

for bad in (5, 1.5, None, "abc"):
    try:
        with bad:
            pass
    except TypeError as e:
        print("not a manager:", type(bad).__name__, "->", type(e).__name__)

print("done")
