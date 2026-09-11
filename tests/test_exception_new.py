# A Python __new__ on an exception subclass runs.
#
# type_call's exception arm went straight to exc_type_call -- which finds a
# BUILTIN tp_new along tp_base -- and then ran __init__.  It never looked for a
# __new__ defined in Python, so `class E(Exception): def __new__(...)` had its
# constructor silently skipped, for `E()` as much as for `raise E`.  Anything
# that builds an exception through __new__ -- a singleton, a cache, a class
# that rewrites itself into a subclass the way OSError's C constructor does --
# simply did not happen.
#
# CPython's rule is type_call's: call __new__, and run __init__ only if what
# came back is an instance of the class asked for.
order = []


class Plain(Exception):
    def __new__(cls, *args):
        order.append("new")
        return super().__new__(cls, *args)

    def __init__(self, *args):
        order.append("init")
        super().__init__(*args)


e = Plain("a", "b")
print(order, e.args, type(e).__name__)

order.clear()
try:
    raise Plain("x")
except Plain as caught:
    print(order, caught.args)

# __new__ alone, with no __init__ of its own.
class NewOnly(Exception):
    def __new__(cls, *args):
        print("  newonly")
        return super().__new__(cls, *args)


n = NewOnly("z")
print(n.args, type(n).__name__)

# __new__ that returns something else entirely: __init__ must NOT run.
class Redirect(Exception):
    def __new__(cls, *args):
        return ValueError("redirected")

    def __init__(self, *args):
        print("  SHOULD NOT RUN")


r = Redirect("ignored")
print(type(r).__name__, r.args)

# __new__ returning an instance of a DIFFERENT exception subclass, which is
# what a self-rewriting constructor does.
class Base(Exception):
    def __new__(cls, code):
        if code == 1 and cls is Base:
            return super().__new__(One, code)
        return super().__new__(cls, code)


class One(Base):
    pass


b = Base(1)
print(type(b).__name__, b.args)
b2 = Base(2)
print(type(b2).__name__, b2.args)

# __new__ that raises.
class NewBlows(Exception):
    def __new__(cls, *args):
        raise RuntimeError("new blew up")


try:
    NewBlows()
except RuntimeError as ex:
    print("newblows:", ex.args)
try:
    raise NewBlows
except RuntimeError as ex:
    print("newblows raised:", ex.args)

# Inherited: a subclass with no __new__ of its own runs its base's.
class Derived(Plain):
    pass


order.clear()
d = Derived("q")
print(order, d.args, type(d).__name__)

# A class with neither is unchanged, and so is every builtin.
class Bare(Exception):
    pass


print(Bare("k").args, ValueError("v").args, OSError(2, "x").errno)
