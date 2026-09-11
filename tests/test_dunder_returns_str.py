# __str__, __repr__ and __format__ have to RETURN a str, and nothing checked.
#
# `def __str__(self): return 5` handed an int to every caller of str(), and an
# f-string, a %-format, str.join and the container reprs all read
# PyStrObject.data out of it.  That is a segfault from four lines of ordinary
# Python -- and a silently wrong answer wherever it happened not to be.
#
# The classification has to be on the TAG: instance_str's result has already
# been unpacked, so rax is a payload, and an int payload of 5 passes a pointer
# test and dereferences address 5.


class BadStr:
    def __str__(self):
        return 5


class BadRepr:
    def __repr__(self):
        return 5


class BadFormat:
    def __format__(self, spec):
        return 5


def show(label, fn):
    try:
        print(label, "->", fn())
    except TypeError as e:
        print(label, "->", e)


b = BadStr()
show("str", lambda: str(b))
show("fstring", lambda: f"{b}")
show("percent", lambda: "%s" % b)
show("join", lambda: "".join([str(b)]))
show("print", lambda: print(b))

r = BadRepr()
show("repr", lambda: repr(r))
show("list", lambda: repr([r]))
show("dict", lambda: repr({1: r}))
show("percent r", lambda: "%r" % r)
show("fstring r", lambda: f"{r!r}")
show("str falls back", lambda: str(r))

f = BadFormat()
show("format", lambda: format(f))
show("format spec", lambda: format(f, "x"))
show("fstring f", lambda: f"{f}")
show("fstring fspec", lambda: f"{f:>5}")

# None and a float are refused the same way, and so is a bytes.
for bad in (None, 1.5, b"x", (), object()):
    C = type("C", (), {"__str__": lambda self, bad=bad: bad})
    show("str " + type(bad).__name__, lambda C=C: str(C()))

# A str SUBCLASS is a str, and is handed back as itself.
class S(str):
    pass


class GoodSub:
    def __str__(self):
        return S("sub")

    def __repr__(self):
        return S("subr")

    def __format__(self, spec):
        return S("subf")


g = GoodSub()
print(str(g), repr(g), format(g), type(str(g)).__name__)

# ...and an ordinary one is untouched.
class Good:
    def __str__(self):
        return "s"

    def __repr__(self):
        return "r"

    def __format__(self, spec):
        return "f" + spec


o = Good()
print(str(o), repr(o), format(o), format(o, "z"), f"{o}", f"{o:y}", f"{o!r}")
print("%s %r" % (o, o))
print([o], {1: o})

print("done")


# A __repr__ that calls str() on something else NESTS.  The choice of which
# name to report has to be per-invocation: a global flag is cleared by the
# inner call on the way out, and left set by an inner raise.
class Inner:
    pass


class Outer:
    def __repr__(self):
        str(Inner())
        return 5


show("str(Outer)", lambda: str(Outer()))
show("repr(Outer)", lambda: repr(Outer()))
show("str again", lambda: str(Outer()))


class Deep:
    def __repr__(self):
        try:
            str(Outer())
        except TypeError:
            pass
        return 7


show("str(Deep)", lambda: str(Deep()))
show("repr(Deep)", lambda: repr(Deep()))

print("done 2")
