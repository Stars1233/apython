"""The metaclass of a class with several bases is the most derived of theirs.

CPython computes the "winner" among the explicit metaclass and every base's
type: whichever is a subclass of all the others, and TypeError when no such
one exists.  Taking the first base's instead is right most of the time and
wrong exactly where it matters -- `class RawIOBase(_io._RawIOBase, IOBase)`
in Lib/io.py, where the first base is a plain type and the second carries
ABCMeta.  The class came out a plain type, and RawIOBase.register() -- which
is how io tells isinstance() that FileIO is a raw stream -- did not exist.
"""

import abc


class Meta(type):
    def marker(cls):
        return "meta"


class Meta2(Meta):
    def marker2(cls):
        return "meta2"


class Plain:
    pass


class WithMeta(metaclass=Meta):
    pass


class WithMeta2(metaclass=Meta2):
    pass


class Abstract(metaclass=abc.ABCMeta):
    pass


def check(label, fn):
    try:
        got = fn()
    except Exception as exc:
        got = type(exc).__name__ + ": " + str(exc)
    print(label.ljust(34), repr(got))


def name_of(cls):
    return type(cls).__name__


# --- the base that carries the metaclass is second ---
class MetaSecond(Plain, WithMeta):
    pass


class MetaFirst(WithMeta, Plain):
    pass


check("metaclass first", lambda: name_of(MetaFirst))
check("metaclass second", lambda: name_of(MetaSecond))
check("its method, first", lambda: MetaFirst.marker())
check("its method, second", lambda: MetaSecond.marker())


# --- the shape io.py uses ---
class AbstractSecond(Plain, Abstract):
    pass


check("ABCMeta second", lambda: name_of(AbstractSecond))
check("register exists", lambda: hasattr(AbstractSecond, "register"))


class Duck:
    pass


AbstractSecond.register(Duck)
check("register works", lambda: (issubclass(Duck, AbstractSecond),
                                 isinstance(Duck(), AbstractSecond)))


# --- the most derived of two metaclasses wins, in either order ---
class DerivedSecond(WithMeta, WithMeta2):
    pass


class DerivedFirst(WithMeta2, WithMeta):
    pass


check("derived metaclass second", lambda: name_of(DerivedSecond))
check("derived metaclass first", lambda: name_of(DerivedFirst))
check("its methods", lambda: (DerivedSecond.marker(), DerivedSecond.marker2()))


# --- an explicit metaclass must still be at least as derived ---
class ExplicitOk(WithMeta, metaclass=Meta2):
    pass


check("explicit more derived", lambda: name_of(ExplicitOk))


def unrelated():
    class Other(type):
        pass

    class WithOther(metaclass=Other):
        pass

    class Bad(WithOther, WithMeta):
        pass

    return name_of(Bad)


check("unrelated metaclasses", unrelated)


def explicit_too_shallow():
    class Bad(WithMeta2, metaclass=Meta):
        pass

    return name_of(Bad)


check("explicit less derived", explicit_too_shallow)


# --- three bases, the metaclass on the last ---
class ThreeBases(Plain, Duck, WithMeta2):
    pass


check("three bases", lambda: name_of(ThreeBases))

# --- and the ordinary case is unchanged ---
check("no metaclass anywhere", lambda: name_of(Plain))
check("one plain base", lambda: name_of(type("X", (Plain,), {})))
check("type() three-arg", lambda: name_of(type("Y", (WithMeta,), {})))
