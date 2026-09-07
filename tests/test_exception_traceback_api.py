"""BaseException.with_traceback(), and the __traceback__ it writes.

`raise e.with_traceback(tb)` is the reason the method returns self rather than
None, and unittest's assertRaises uses the other half -- it stores
`exc_value.with_traceback(None)` so the exception it hands back does not keep
the frames alive.  Without the method that line raised AttributeError from
inside a __exit__ that was already handling an exception, which is where nearly
every CPython test stopped.

The setter behind it already existed; what is checked here is that the method
agrees with it, in both directions and on every argument shape.
"""


def make_tb(msg="v"):
    try:
        raise ValueError(msg)
    except ValueError as e:
        return e, e.__traceback__


e, tb = make_tb()
print("has traceback:", tb is not None, type(tb).__name__)

r = e.with_traceback(None)
print("returns self:", r is e)
print("cleared:", e.__traceback__)

r = e.with_traceback(tb)
print("restored:", r is e, e.__traceback__ is tb)

# The attribute and the method must agree.
e.__traceback__ = None
print("via attribute:", e.__traceback__)
e.__traceback__ = tb
print("via attribute again:", e.__traceback__ is tb)

# Setting it to a traceback from somewhere else.
e2, tb2 = make_tb("second")
e.with_traceback(tb2)
print("foreign traceback:", e.__traceback__ is tb2, e.__traceback__ is not tb)


print("--- rejected arguments ---")
for bad in (5, 1.5, "abc", [], (), object(), e, True):
    try:
        e.with_traceback(bad)
        print("accepted", type(bad).__name__, "- wrong")
    except TypeError as te:
        print(type(bad).__name__, "->", te)

for call in (lambda: e.with_traceback(),
             lambda: e.with_traceback(tb, tb)):
    try:
        call()
        print("no error - wrong")
    except TypeError as te:
        print("arity:", te)


print("--- it is inherited by every exception ---")
for cls in (ValueError, KeyError, StopIteration, OSError, RecursionError,
            BaseException, Exception, KeyboardInterrupt, SystemExit,
            ZeroDivisionError, UnicodeDecodeError):
    inst = cls.__new__(cls)
    got = inst.with_traceback(None)
    print(cls.__name__, "->", got is inst, inst.__traceback__)


class MyError(ValueError):
    def __init__(self, code):
        super().__init__("code %d" % code)
        self.code = code


m = MyError(7)
print("subclass:", m.with_traceback(None) is m, m.code, str(m))


print("--- raise e.with_traceback(tb) ---")
e3, tb3 = make_tb("third")
try:
    raise e3.with_traceback(tb3)
except ValueError as caught:
    print("caught:", caught is e3, str(caught))


print("--- an exception group ---")
eg = ExceptionGroup("eg", [ValueError("a"), TypeError("b")])
print("group:", eg.with_traceback(None) is eg, eg.__traceback__)


print("--- the method object itself ---")
print("name:", e.with_traceback.__name__)
print("on the class:", hasattr(BaseException, "with_traceback"))
print("callable:", callable(BaseException.with_traceback))

try:
    BaseException.with_traceback([], None)
    print("wrong receiver accepted - wrong")
except TypeError as te:
    print("wrong receiver:", type(te).__name__)

print("done")
