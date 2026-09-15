# Writing `func.__defaults__`.
#
# func_setattr has an arm for __kwdefaults__, __name__, __qualname__,
# __doc__, __annotations__ and __code__ -- each because func_getattr answers
# that attribute from a FIELD, so an assignment landing in func_dict reads
# back as whatever the `def` was written with.  The comments in that function
# say so three times over.  __defaults__ was the one nobody added: the
# assignment went into __dict__, the read came from func_defaults, and
#
#     f.__defaults__ = (9,)
#     f.__defaults__          # None
#     f(1)                    # TypeError: missing required argument
#
# was the whole of it.  Nothing raised; the write was simply lost, and a list
# was accepted where CPython insists on a tuple.
#
# collections.namedtuple is what found it.  CPython builds the generated
# __new__ with exec() and then does
#
#     if defaults is not None:
#         __new__.__defaults__ = tuple(defaults)
#
# so every namedtuple with a `defaults=` argument had no defaults at all.
def f(a, b):
    return (a, b)


print("before:", f.__defaults__)
f.__defaults__ = (9,)
print("after:", f.__defaults__)
print("and the call takes it:", f(1), f(1, 2))
print("__dict__ stays empty:", f.__dict__)

f.__defaults__ = (8, 9)
print("both defaulted:", f(), f(1), f(1, 2))

f.__defaults__ = ()
print("an empty tuple:", f.__defaults__)
try:
    f()
    print("and the call needs both: NOT REFUSED")
except TypeError:
    print("and the call needs both: TypeError")

f.__defaults__ = None
print("None clears it:", f.__defaults__)
try:
    f(1)
    print("cleared, one argument: NOT REFUSED")
except TypeError:
    print("cleared, one argument: TypeError")


# --- replacing defaults a def already had ------------------------------
def g(a, b=1):
    return (a, b)


print("as written:", g.__defaults__, g(0))
g.__defaults__ = (5,)
print("replaced:", g.__defaults__, g(0))
g.__defaults__ = (2, 3)
print("widened:", g(), g(9))

# --- del -------------------------------------------------------------
del g.__defaults__
print("after del:", g.__defaults__, g.__dict__)
try:
    g(0)
    print("del left a default: NOT REFUSED")
except TypeError:
    print("del left no default: TypeError")


def h(a, b):
    return 1


del h.__defaults__
print("del on a function that had none:", h.__defaults__)

# --- what must be refused --------------------------------------------
for value, what in (([1], "a list"), ("ab", "a str"), (1, "an int"),
                    ({1: 2}, "a dict"), (set(), "a set")):
    try:
        f.__defaults__ = value
        print("%-10s ACCEPTED %r" % (what, f.__defaults__))
    except TypeError as exc:
        print("%-10s %s" % (what, exc))

# --- __kwdefaults__, whose setter had two SIGSEGVs in it -------------
# func_call reads that field with dict_get, so whatever is there has to be a
# dict or nothing.  The test was only "is it a heap pointer", which None and
# a list both pass -- so `f.__kwdefaults__ = None` and
# `f.__kwdefaults__ = [1]` each stored an object dict_get then read as a dict
# header.  A plain assignment, and a core dump.
def k(a, *, x=1, y=2):
    return (a, x, y)


print("kwdefaults:", k.__kwdefaults__, k(0))
k.__kwdefaults__ = {"x": 7, "y": 8}
print("kwdefaults set:", k(0))
k.__kwdefaults__ = None
print("set to None reads back as None:", k.__kwdefaults__)
try:
    k(0)
    print("kwdefaults cleared: NOT REFUSED")
except TypeError:
    print("kwdefaults cleared: TypeError")
k.__kwdefaults__ = {"x": 1, "y": 2}
del k.__kwdefaults__
print("after del:", k.__kwdefaults__, k.__dict__)
for value, what in (([1], "a list"), ("ab", "a str"), (1, "an int"),
                    ((), "a tuple"), (set(), "a set")):
    try:
        k.__kwdefaults__ = value
        print("%-10s ACCEPTED %r" % (what, k.__kwdefaults__))
    except TypeError as exc:
        print("%-10s %s" % (what, exc))
# A dict subclass is accepted, as CPython's PyDict_Check is; so is a tuple
# subclass for __defaults__.
class D(dict):
    pass


class T(tuple):
    pass


k.__kwdefaults__ = D(x=4, y=5)
print("a dict subclass:", k(0))
f.__defaults__ = T((1, 2))
print("a tuple subclass:", f())

# --- and the shape it was found in -----------------------------------
# Built by hand rather than through collections, so this test stands on its
# own: it is namedtuple's exec-then-assign, which is the only way to give a
# generated function defaults.
namespace = {}
exec("def made(a, b, c): return (a, b, c)", namespace)
made = namespace["made"]
made.__defaults__ = (2, 3)
print("exec then assign:", made(1), made(1, 9), made(1, 9, 9))

print("survived")
