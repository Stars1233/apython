# A dunder found on the type can be a DESCRIPTOR, and then its __get__ is what
# produces the callable.
#
# dunder_call_1/2/3 called whatever the lookup found, with self prepended.
# That is right for a plain function -- CPython's METHOD_DESCRIPTOR fast path
# -- and wrong for everything else: a descriptor's __get__ never ran, and the
# descriptor OBJECT was called instead.  For one with no __call__ of its own
# that is a jump to a NULL tp_call, which is a segfault from ordinary Python.
#
# unittest.mock installs exactly this shape -- one MagicProxy per magic method,
# each a plain object whose only job is __get__ -- so every MagicMock test
# crashed the interpreter.


class Proxy:
    def __init__(self, n):
        self.n = n

    def __get__(self, obj, objtype=None):
        return lambda *a: ("called", self.n, a)


def named(name):
    C = type("C", (), {})
    setattr(C, name, Proxy(name))
    return C


def show(label, fn):
    try:
        print(label, "->", repr(fn())[:60])
    except Exception as e:
        print(label, "->", type(e).__name__, str(e)[:60])


# One dunder at a time, by name and through its operator.
o = named("__len__")()
show("len", lambda: len(o))
o = named("__iter__")()
show("iter", lambda: iter(o))
o = named("__getitem__")()
show("getitem", lambda: o[3])
o = named("__contains__")()
show("contains", lambda: 7 in o)
o = named("__add__")()
show("add", lambda: o + 1)
o = named("__eq__")()
show("eq", lambda: o == 1)
o = named("__lt__")()
show("lt", lambda: o < 1)
o = named("__neg__")()
show("neg", lambda: -o)
o = named("__setitem__")()
show("setitem", lambda: o.__setitem__(1, 2))
o = named("__round__")()
show("round", lambda: round(o))

# A descriptor that RAISES from __get__ propagates.
class Boom:
    def __get__(self, obj, objtype=None):
        raise ValueError("from __get__")


C = type("C", (), {})
C.__len__ = Boom()
show("raising __get__", lambda: len(C()))

# A descriptor whose __get__ answers something that is not callable.
class NotCallable:
    def __get__(self, obj, objtype=None):
        return 42


C = type("C", (), {})
C.__len__ = NotCallable()
show("non-callable bind", lambda: len(C()))

# An object with neither __get__ nor __call__ is not callable, and saying so
# is better than the RuntimeError a NULL used to become.
class Inert:
    pass


C = type("C", (), {})
C.__len__ = Inert()
show("inert", lambda: len(C()))

# staticmethod and classmethod are descriptors too, and both bind.
class S:
    __len__ = staticmethod(lambda: 3)


print(len(S()))


class K:
    __len__ = classmethod(lambda cls: 4)


print(len(K()))

# A plain function is still called with self, which is the whole point of the
# fast path this sits beside.
class F:
    def __len__(self):
        return 5

    def __getitem__(self, i):
        return ("item", i)

    def __eq__(self, o):
        return ("eq", o)


f = F()
print(len(f), f[1], f == 2)

# __get__ is consulted on every use, not once.
calls = []


class Counting:
    def __get__(self, obj, objtype=None):
        calls.append(1)
        return lambda: 0


C = type("C", (), {})
C.__len__ = Counting()
c = C()
len(c)
len(c)
len(c)
print("binds:", len(calls))

print("done")


# ---------------------------------------------------------------------------
# BEFORE_WITH had its own lookup: the type's OWN tp_dict, no MRO walk and no
# descriptor protocol, so a context manager whose __enter__ or __exit__ is a
# descriptor bound the DESCRIPTOR as the method.
class CM:
    def __get__(self, obj, objtype=None):
        return lambda *a: ("cm", self.which, a)

    def __init__(self, which):
        self.which = which


C = type("C", (), {})
C.__enter__ = CM("enter")
C.__exit__ = CM("exit")
with C() as v:
    print("with ->", v)

# ...and inherited from a base, which the dict-only lookup could not see
# either.
class BaseCM:
    def __enter__(self):
        return "base-enter"

    def __exit__(self, *a):
        print("base-exit")
        return False


class Derived(BaseCM):
    pass


with Derived() as v:
    print("inherited ->", v)

# A __get__ that raises comes out of the `with` rather than past it.
class BoomCM:
    def __get__(self, obj, objtype=None):
        raise ValueError("no enter")


C = type("C", (), {})
C.__enter__ = BoomCM()
C.__exit__ = CM("exit")
try:
    with C():
        print("entered")
except ValueError as e:
    print("with __get__ raised:", e)

# ...and one whose __exit__ is missing entirely is still a protocol error.
C = type("C", (), {})
C.__enter__ = CM("enter")
try:
    with C():
        pass
except TypeError as e:
    print("no __exit__:", e)

# An ordinary context manager, and a file, must not have changed.
class Plain:
    def __enter__(self):
        return "plain"

    def __exit__(self, *a):
        return False


with Plain() as v:
    print(v)

import os
path = "test_dunder_descriptor_tmp.txt"
with open(path, "w") as f:
    f.write("x")
with open(path) as f:
    print("file:", f.read())
os.unlink(path)

print("done 2")


# ---------------------------------------------------------------------------
# __call__ had the same private lookup: slot_tp_call found it on the MRO and
# called it with self prepended, whatever it was.
class CallProxy:
    def __get__(self, obj, objtype=None):
        return lambda *a, **k: ("call", a, k)


C = type("C", (), {})
C.__call__ = CallProxy()
c = C()
print(c())
print(c(1, 2))
print(c(1, x=2))
print(callable(c))

# ...and it is reached through every road into a call.
print(list(map(c, [1])))
f = c
print(f(*[1, 2], **{"k": 3}))

# An ordinary __call__ must not have changed.
class Ordinary:
    def __call__(self, *a, **k):
        return ("ordinary", a, k)


o = Ordinary()
print(o(), o(1), o(1, k=2), callable(o))

# A __call__ that is not callable at all still says so.
C = type("C", (), {})
C.__call__ = CallProxy()
c = C()


class NotCallableGet:
    def __get__(self, obj, objtype=None):
        return 42


C2 = type("C2", (), {})
C2.__call__ = NotCallableGet()
try:
    C2()()
except TypeError as e:
    print("bound to an int:", e)

print("done 3")


# ---------------------------------------------------------------------------
# A __call__ whose __get__ answers something uncallable: the bound object is
# this frame's to release, on the failing road as well as the working one.
class BadGet:
    def __get__(self, obj, objtype=None):
        return "not callable"


import sys

C = type("C", (), {})
C.__call__ = BadGet()
c = C()
s = "not callable"
base = sys.getrefcount(s)
for _ in range(5):
    try:
        c()
    except TypeError as e:
        msg = str(e)
print(msg)
print("leak:", sys.getrefcount(s) - base)

# ...and one whose __get__ answers an immediate, which has no type to read.
class IntGet:
    def __get__(self, obj, objtype=None):
        return 42


C2 = type("C2", (), {})
C2.__call__ = IntGet()
try:
    C2()()
except TypeError as e:
    print(e)

print("done 4")
