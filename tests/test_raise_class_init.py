# `raise SomeExceptionClass` constructs the class, rather than assembling an
# instance behind its back.
#
# The bare-class form of a raise reached exc_new, which allocates the object
# and builds its args tuple directly: it never consults tp_new and never runs
# __init__.  So a class with a constructor of its own got none of it, and
# `raise OSError` produced an object with no errno attribute AT ALL where
# CPython gives None -- because OSError's whole constructor, the one that
# rewrites the class from the errno and fills the four named fields, lives in
# a tp_new that was being stepped around.
#
# The instance form was always right: `raise ValueError("x")` builds the
# instance with CALL before RAISE_VARARGS ever sees it, so only the bare-class
# spelling was affected.  The `from` clause had the same split.


class Custom(Exception):
    def __init__(self):
        super().__init__("built by __init__")


class Counting(Exception):
    made = 0

    def __init__(self):
        Counting.made += 1
        super().__init__("n=%d" % Counting.made)


class Blows(Exception):
    def __init__(self):
        raise RuntimeError("init blew up")


try:
    raise Custom
except Custom as e:
    print("custom:", e.args, str(e))

try:
    raise Counting
except Counting as e:
    print("counting:", e.args, Counting.made)
try:
    raise Counting
except Counting as e:
    print("counting:", e.args, Counting.made)

try:
    raise Blows
except RuntimeError as e:
    print("blows:", e.args)

# A Python-level __new__ on an exception subclass is covered by
# tests/test_exception_new.py: it was not run for `E()` either, so it is a
# separate defect from this one and not a property of the bare-class raise.

# The instance form keeps working, and is not double-constructed.
Counting.made = 0
try:
    raise Counting()
except Counting as e:
    print("instance form:", e.args, Counting.made)

# OSError's constructor is a tp_new, and the bare class must reach it.
try:
    raise OSError
except OSError as e:
    print("oserror:", type(e).__name__, e.args, e.errno, e.strerror, e.filename)

# ...and the errno remap still happens for the instance form.
try:
    raise FileNotFoundError(2, "no such file")
except OSError as e:
    print("remap:", type(e).__name__, e.errno, e.strerror)

# Builtins with nothing of their own are unchanged.
for cls in (ValueError, KeyError, StopIteration, ZeroDivisionError,
            RecursionError, UnicodeError):
    try:
        raise cls
    except BaseException as e:
        print("%-18s %r %r" % (cls.__name__, e.args, str(e)))

# The `from` clause takes a class too, and constructs it the same way.
try:
    raise ValueError("v") from Custom
except ValueError as e:
    print("from:", type(e.__cause__).__name__, e.__cause__.args)

try:
    raise ValueError("v") from OSError
except ValueError as e:
    print("from oserror:", type(e.__cause__).__name__, e.__cause__.errno)

# A bare re-raise of a class inside an except block still chains.
try:
    try:
        raise KeyError("first")
    except KeyError:
        raise Custom
except Custom as e:
    print("chained:", e.args, type(e.__context__).__name__)
