# sys.unraisablehook -- what the interpreter calls for an exception that
# cannot be propagated.
#
# This file compares against a recorded transcript, for ONE reason that has
# nothing to do with the hook: the report names a file, and CPython
# absolutizes the path of a script it runs directly where a run from a .pyc
# does not.  Every other line matches CPython 3.12 byte for byte -- the second
# reason, that CPython block-buffers stdout when it is not a terminal and so
# ordered the two streams differently, is gone: stdout is buffered here now
# too.
#
# There are two of those: a __del__ that raises, and a cleanup that raises in
# a generator being finalised.  Neither has a caller left to hand the
# exception to.  Both used to print a single line naming the kind of failure
# and nothing else -- not the object, not the line, not even the exception --
# and neither went through the hook, so a program had no way to see them at
# all.

import sys

seen = []


def hook(unraisable):
    seen.append((unraisable.exc_type.__name__,
                 str(unraisable.exc_value),
                 unraisable.exc_traceback is not None,
                 unraisable.err_msg,
                 type(unraisable.object).__name__))


old = sys.unraisablehook
sys.unraisablehook = hook


class Boom:
    def __del__(self):
        raise ValueError("delfail")


b = Boom()
del b


def gen():
    try:
        yield 1
    finally:
        raise RuntimeError("genfail")


g = gen()
next(g)
del g

print(seen)

# The argument is a five-field structseq, in CPython's order.
seen.clear()
c = Boom()
del c
args_len = None


def shape(unraisable):
    global args_len
    args_len = len(unraisable)
    print(unraisable.exc_type is ValueError,
          unraisable.exc_value.args,
          unraisable.err_msg,
          unraisable.object.__name__)


sys.unraisablehook = shape
d = Boom()
del d
print(args_len)

# A hook that raises does not propagate: there is nobody to give it to.  It is
# reported against the HOOK -- and the exception the hook was given is
# dropped, because reporting both would be reporting one failure twice.
#
# The hook is an object with a repr of its own so that the report has no
# address in it; that is also why this file compares against a recorded
# transcript rather than against python3, since the traceback still names the
# file, and CPython absolutizes the path of a script it runs directly.
class BadHook:
    def __repr__(self):
        return "<the bad hook>"

    def __call__(self, unraisable):
        raise KeyError("hook failed")


sys.unraisablehook = BadHook()
e = Boom()
del e
print("survived a raising hook")

# Neither report walks a chain.  CPython reaches for PyTraceBack_Print rather
# than the display routine an uncaught exception goes through, so a __cause__
# or a __context__ the exception really carries is not printed -- one
# exception, not three paragraphs.  The chain is still ON the exception: a
# hook of one's own can read __context__ and see it.
class ChainedHook:
    def __repr__(self):
        return "<the chaining hook>"

    def __call__(self, unraisable):
        try:
            raise KeyError("first")
        except KeyError as exc:
            raise ValueError("second") from exc


sys.unraisablehook = ChainedHook()
e = Boom()
del e
print("survived a chaining hook")


def context_hook(unraisable):
    print("context seen by a hook:", repr(unraisable.exc_value.__context__))


sys.unraisablehook = context_hook


def chained_gen():
    try:
        yield 1
    finally:
        raise RuntimeError("cleanup")


g = chained_gen()
next(g)
del g

# And putting the default back restores the printed report.
sys.unraisablehook = old
print(old is sys.__unraisablehook__)
print("done")
