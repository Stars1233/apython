# An exception group takes an attribute assignment like any other exception.
#
# The two static group types set tp_getattr and left tp_setattr at zero, so
# every write refused: "AttributeError: cannot set attribute".  Their four
# chaining fields sit at the same offsets as any other exception's, so
# exc_setattr is the setter they wanted.
#
# unittest's _clean_tracebacks does `value.__traceback__ = tb` to every error
# it reports, so CPython's test_exception_group could not print even its first
# failure -- it died reporting one and ran nothing.

import gc


def make():
    try:
        raise ExceptionGroup("m", [ValueError(1)])
    except BaseException as e:
        return e


g = make()
tb = g.__traceback__
print(tb is not None, type(g).__name__, g.message)

g.__traceback__ = tb
print("set", g.__traceback__ is tb)
g.__traceback__ = None
print("cleared", g.__traceback__)

cause = ValueError("the cause")
g.__cause__ = cause
print("cause", g.__cause__ is cause, g.__suppress_context__)
g.__cause__ = None
print("cause cleared", g.__cause__)

ctx = TypeError("the context")
g.__context__ = ctx
print("context", g.__context__ is ctx)

g.__suppress_context__ = False
print("suppress", g.__suppress_context__)

# Ordinary attributes go in the instance dict.
g.tag = "tagged"
g.count = 3
print(g.tag, g.count, sorted(g.__dict__.items()))
del g.tag
print(sorted(g.__dict__.items()))

# BaseExceptionGroup too, and a subclass of each.
b = BaseExceptionGroup("b", [KeyboardInterrupt()])
b.__cause__ = ValueError("c")
b.note = 1
print(type(b).__name__, type(b.__cause__).__name__, b.note)


class MyGroup(ExceptionGroup):
    pass


m = MyGroup("mine", [ValueError(2)])
m.__cause__ = ValueError("mc")
m.extra = "e"
print(m.message, len(m.exceptions), m.extra, type(m.__cause__).__name__)

# The group's own fields are still read-only where CPython says so.
for name in ("message", "exceptions"):
    try:
        setattr(g, name, "nope")
    except AttributeError:
        print(name, "AttributeError")
    else:
        print(name, "assigned", getattr(g, name))

# And an assignment does not disturb the group.
gc.collect()
print(g.message, [type(x).__name__ for x in g.exceptions], len(g.args))
print("done")
