# An exception subclass that defines __str__ or __repr__ gets to use it.
#
# type_from_parts overwrote tp_repr and tp_str with exc_repr / exc_str for
# every exception subclass, so the class's own dunder was never called -- the
# same mistake the int-subclass arm three lines below already had fixed for
# int, and with the same fix: leave the slots at instance_repr / instance_str,
# which look for the class's own dunder first and fall back to the base's slot
# when there is none.
#
# argparse is where this showed: `ArgumentError.__str__` builds the message,
# so every argparse error came out as the args tuple --
# "(None, 'the following arguments are required: {foo,bar}')".

class E(Exception):
    def __init__(self, argument, message):
        self.argument = argument
        self.message = message

    def __str__(self):
        return "custom: %s" % (self.message,)


e = E(None, "msg")
print(str(e))
print("%s" % (e,))
print("{}".format(e))
print(f"{e}")
print(repr(e))

try:
    raise E(None, "raised")
except E as x:
    print("caught", str(x))

# __repr__ alone, with str() falling through to it as it does for object.
class F(Exception):
    def __repr__(self):
        return "<F %r>" % (self.args,)


print(repr(F(1, 2)), str(F(1, 2)))

# A subclass of a builtin exception other than Exception.
class G(ValueError):
    def __str__(self):
        return "G!"


print(str(G(1)), "%s" % (G(1),), repr(G(1)))

# Neither: the builtin behaviour is unchanged.
class H(KeyError):
    pass


print(str(H("k")), repr(H("k")))
print(str(Exception("a", "b")), repr(Exception("a", "b")))
print(str(ValueError()), repr(ValueError()))

# Inherited from an intermediate class, and overridden again below it.
class Base(Exception):
    def __str__(self):
        return "base<%s>" % (self.args,)


class Mid(Base):
    pass


class Leaf(Base):
    def __str__(self):
        return "leaf"


print(str(Base(1)), str(Mid(2)), str(Leaf(3)))

# A subclass of KeyError keeps KeyError's own rendering: CPython gives
# KeyError a tp_str of its own and subclasses inherit it, where exc_str
# compared the type pointer for equality.
class K(KeyError):
    pass


print(str(K("k")), repr(K("k")), str(K("a", "b")))

# __str__ that raises propagates rather than being swallowed.
class Bad(Exception):
    def __str__(self):
        raise RuntimeError("no str")


try:
    str(Bad())
except RuntimeError as r:
    print("propagated", r)

# A group subclass keeps the group's own rendering unless it says otherwise.
class GE(ExceptionGroup):
    pass


g = GE("m", [ValueError(1)])
print(str(g), repr(g))


class GS(ExceptionGroup):
    def __str__(self):
        return "group!"


print(str(GS("m", [ValueError(1)])))

# The attributes exc_getattr answers are still there.
print(e.args, e.argument, e.message, G(1).args, H("k").args)
print("done")
