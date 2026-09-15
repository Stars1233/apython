# `__match_args__` that is not a tuple of strings.
#
#     class C:
#         __match_args__ = None
#     match C():
#         case C(y): ...
#
# was a SIGSEGV.  op_match_class took whatever the MRO answered with and read
# ob_size and ob_item off it -- None has neither at those offsets, so the
# positional loop walked a pointer made of the singleton's header.
#
# CPython checks three things and has a different message for each, and two
# of them are TypeErrors the pattern cannot swallow: a match that refuses
# because the subject does not fit is not the same as a class whose
# __match_args__ is malformed, and the second must be reported.
#
# This is test_patma.TestTypeErrors, which crashed the whole module here.


def attempt(cls, npos=1):
    """-> the error a one-positional class pattern against `cls` raises."""
    obj = cls()
    try:
        if npos == 1:
            match obj:
                case cls(_a):
                    return "matched"
        else:
            match obj:
                case cls(_a, _b):
                    return "matched"
        return "no match"
    except TypeError as exc:
        return "TypeError: %s" % exc


def with_match_args(value):
    class C:
        pass

    C.__match_args__ = value
    return C


for value, label in ((None, "None"), (1, "an int"), ("ab", "a str"),
                     ([1], "a list"), ({1: 2}, "a dict"), (object(), "an object"),
                     ({"a"}, "a set")):
    print("%-12s %s" % (label, attempt(with_match_args(value))))

# --- a tuple whose elements are not strings ---------------------------
for value, label in (((1,), "an int element"), ((None,), "a None element"),
                     ((b"a",), "a bytes element")):
    print("%-16s %s" % (label, attempt(with_match_args(value))))

# --- too few entries for the sub-patterns given -----------------------
class NoArgs:
    pass


print("no __match_args__ at all:", attempt(NoArgs))
print("an empty tuple:", attempt(with_match_args(())))
print("one entry, two sub-patterns:",
      attempt(with_match_args(("a",)), npos=2))


# --- and what must still work -----------------------------------------
class Point:
    __match_args__ = ("x", "y")

    def __init__(self, x, y):
        self.x = x
        self.y = y


def classify(p):
    match p:
        case Point(0, 0):
            return "origin"
        case Point(x, 0):
            return ("on the x axis", x)
        case Point(x, y):
            return ("a point", x, y)
        case _:
            return "not a point"


print("working:", classify(Point(0, 0)), classify(Point(3, 0)),
      classify(Point(1, 2)), classify(object()))

# A tuple SUBCLASS is refused: CPython uses PyTuple_CheckExact here, not
# PyTuple_Check, which is the opposite of the rule __defaults__ and
# __kwdefaults__ follow.
class TupleSub(tuple):
    pass


print("a tuple subclass:", attempt(with_match_args(TupleSub(("x",)))))

# Inherited __match_args__ comes off the MRO, not off the class itself.
class Base:
    __match_args__ = ("v",)


class Derived(Base):
    def __init__(self):
        self.v = 11


match Derived():
    case Derived(got):
        print("inherited:", got)

# Keyword sub-patterns do not consult __match_args__ at all, so a broken one
# does not stop them.
class BrokenButKeyworded:
    __match_args__ = None

    def __init__(self):
        self.k = 5


match BrokenButKeyworded():
    case BrokenButKeyworded(k=v):
        print("keyword sub-patterns are unaffected:", v)

# Nothing was bound by a pattern that raised.
y = z = None
try:
    match with_match_args(None)():
        case object(y):
            z = 0
except TypeError:
    pass
print("nothing bound:", y, z)
print("survived")
