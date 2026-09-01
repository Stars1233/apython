"""An __init__ that raises must not hand back the half-built instance.

type_call calls __new__, then __init__, and ignored what __init__ returned.
A raise leaves NULL there and the exception pending, so the construction
"succeeded", the caller got an object whose __init__ never finished, and the
exception surfaced at whatever ran next -- as a "During handling of the above
exception" chain attached to code that had nothing to do with it.

io.StringIO(5) is where this turned up: it raises TypeError from __init__ and
the interpreter returned a StringIO.
"""


def check(label, fn):
    try:
        got = fn()
    except Exception as exc:
        got = type(exc).__name__ + ": " + str(exc)
    print(label.ljust(34), repr(got))


class Plain:
    def __init__(self):
        raise TypeError("plain")


class Base:
    def __init__(self, x=1):
        self.x = x


class Derived(Base):
    def __init__(self):
        super().__init__(2)
        raise ValueError("after super")


class WithArgs:
    def __init__(self, a, b=None):
        if not isinstance(a, str):
            raise TypeError("a must be str, not %s" % type(a).__name__)
        self.a = a


class Conditional:
    def __init__(self, ok):
        if not ok:
            raise RuntimeError("refused")
        self.ok = ok


check("no base", lambda: Plain())
check("after super", lambda: Derived())
check("with arguments", lambda: WithArgs(5))
check("arguments accepted", lambda: WithArgs("s").a)
check("conditional, refused", lambda: Conditional(False))
check("conditional, accepted", lambda: Conditional(True).ok)

# The exception must be gone once it has been caught, not left pending for
# the next thing to run.
check("nothing left pending", lambda: WithArgs("after").a)


# It has to propagate through an intervening frame too, not only out of the
# call that made it.
def outer():
    return Plain()


def middle():
    return outer()


check("through two frames", lambda: middle())


# A __new__ that raises was already handled; check it still is.
class NewRaises:
    def __new__(cls, *args):
        raise KeyError("from new")


check("__new__ raises", lambda: NewRaises())


# And an __init__ that raises inside a try of its own, catching it, must
# still construct.
class Caught:
    def __init__(self):
        try:
            raise ValueError("handled")
        except ValueError:
            self.handled = True


check("caught inside __init__", lambda: Caught().handled)


# A subclass whose base's __init__ raises.
class BadBase:
    def __init__(self):
        raise OSError("bad base")


class Inherits(BadBase):
    pass


check("inherited __init__ raises", lambda: Inherits())


# The instance must not be reachable afterwards: nothing keeps a reference to
# it, so this is really a check that the decref happened without crashing.
for _ in range(500):
    try:
        Plain()
    except TypeError:
        pass
print("survived the loop".ljust(34), repr(True))


# The same through a metaclass, where __init__ is the metaclass's.
class Meta(type):
    def __init__(cls, name, bases, ns):
        super().__init__(name, bases, ns)
        raise ValueError("metaclass init")


def make_class():
    class C(metaclass=Meta):
        pass

    return C


check("metaclass __init__ raises", make_class)
