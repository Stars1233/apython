# int() and the two deprecations CPython 3.12 attaches to it.
#
# Falling back to __trunc__ is deprecated, and so is a __int__, __index__ or
# __trunc__ that answers a strict SUBCLASS of int rather than an int.  Neither
# warned here -- nothing in this interpreter raised a Python warning at all --
# so a suite that turns the deprecation into an error had nothing to turn, and
# the subclass case answered an object whose type was not int.

import warnings


def probe(label, fn):
    with warnings.catch_warnings(record=True) as log:
        warnings.simplefilter("always")
        try:
            answer = repr(fn())
        except Exception as e:
            answer = "%s: %s" % (type(e).__name__, e)
    print("%-26s %-22s %s" % (label, answer,
                              [(w.category.__name__, str(w.message)[:40])
                               for w in log]))


class JustTrunc:
    def __trunc__(self):
        return 42


class TruncSubclass:
    def __trunc__(self):
        return True


class IndexSubclass:
    def __index__(self):
        return True


class IntSubclass:
    def __int__(self):
        return True


class Sub(int):
    pass


class IntReturnsSub:
    def __int__(self):
        return Sub(5)


class IntSubWithInt(int):
    def __int__(self):
        return True


class Plain:
    def __int__(self):
        return 7


probe("__trunc__ only", lambda: int(JustTrunc()))
probe("__trunc__ -> bool", lambda: int(TruncSubclass()))
probe("__index__ -> bool", lambda: int(IndexSubclass()))
probe("__int__ -> bool", lambda: int(IntSubclass()))
probe("__int__ -> int subclass", lambda: int(IntReturnsSub()))
probe("int subclass' __int__", lambda: int(IntSubWithInt()))
probe("__int__ -> int", lambda: int(Plain()))
probe("no protocol", lambda: int(object()))

print("=== and the answer is an exact int ===")
for label, fn in (("bool via __index__", lambda: int(IndexSubclass())),
                  ("subclass via __int__", lambda: int(IntReturnsSub())),
                  ("bool via __trunc__", lambda: int(TruncSubclass()))):
    with warnings.catch_warnings():
        warnings.simplefilter("ignore")
        v = fn()
    print("%-22s %r %s" % (label, v, type(v).__name__))

print("=== a filter set to error makes it raise ===")
with warnings.catch_warnings():
    warnings.simplefilter("error")
    for label, fn in (("__trunc__", lambda: int(JustTrunc())),
                      ("__index__ -> bool", lambda: int(IndexSubclass()))):
        try:
            print(label, int(fn()))
        except Exception as e:
            print("%-18s %s: %s" % (label, type(e).__name__, str(e)[:40]))

print("=== and __trunc__ does not run when the warning raised ===")
ran = []


class Watched:
    def __trunc__(self):
        ran.append(1)
        return 1


with warnings.catch_warnings():
    warnings.simplefilter("error")
    try:
        int(Watched())
    except DeprecationWarning:
        pass
print("ran", ran)
print("ordinary int()", int(3.7), int("5"), int(True), int(7))
print("done")
