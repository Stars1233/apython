# sys.excepthook, including what it is handed by mistake.
#
# The hook is what runs when there is nothing left to catch anything, so a
# wrong argument is REPORTED rather than raised: CPython prints the TypeError
# on stderr and returns None.  This used to check only that the value was a
# pointer, which None is -- so `sys.excepthook(None, None, None)`, and
# `sys.excepthook(*sys.exc_info())` outside an except block, read an
# exception's fields off the None singleton and segfaulted.
#
# Everything is flushed as it is printed: CPython block-buffers stdout when it
# is a pipe, and the hook writes to stderr, so without this the two streams
# come out in different orders under the two interpreters.

import sys


def say(*a):
    print(*a)
    sys.stdout.flush()


# No traceback: the report is then one line and carries no file path, which
# is what keeps this comparable against python3 -- CPython absolutizes the
# path of a script it runs directly and this runs a .pyc.
say("=== a real exception ===")
sys.excepthook(ValueError, ValueError("reported"), None)
sys.stderr.flush()
say("=== one with a cause ===")
inner = KeyError("inner")
outer = RuntimeError("outer")
outer.__cause__ = inner
sys.excepthook(RuntimeError, outer, None)
sys.stderr.flush()

say("=== what it must not crash on ===")
for value in (None, 5, "text", [1, 2], (), object(), 1.5, b"bytes"):
    sys.excepthook(type(value), value, None)
    sys.stderr.flush()
say("still here")

say("=== exc_info() outside an except block ===")
say(sys.exc_info())
sys.excepthook(*sys.exc_info())
sys.stderr.flush()
say("still here")

say("=== too few arguments ===")
try:
    sys.excepthook(None)
    say("no arity check")
except TypeError:
    say("arity checked")

say("done")

# __cause__, __context__, __traceback__ and __suppress_context__ are fields of
# the object, and exc_getattr reads them from there; assigning to one used to
# put it in the instance dict instead, where nothing ever looked.  The report
# above is what noticed: a hand-built cause chain printed as though there were
# no cause.
say("=== the chain attributes are writable ===")
a = KeyError("in")
b = RuntimeError("out")
say(b.__cause__, b.__context__, b.__suppress_context__, b.__traceback__)
b.__cause__ = a
say(b.__cause__, b.__suppress_context__)
b.__context__ = a
say(b.__context__)
b.__cause__ = None
say(b.__cause__, b.__context__)
b.__suppress_context__ = False
say(b.__suppress_context__)
try:
    b.__suppress_context__ = 1
    say("non-bool accepted:", b.__suppress_context__)
except TypeError as e:
    say("non-bool rejected:", e)
try:
    raise ValueError("with a traceback")
except ValueError as e:
    tb = e.__traceback__
say(tb is not None)
b.__traceback__ = tb
say(b.__traceback__ is tb)
b.__traceback__ = None
say(b.__traceback__)
say("done for real")
