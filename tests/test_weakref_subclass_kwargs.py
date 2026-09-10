# ref.__new__ counts only the POSITIONAL arguments.
#
# The keyword values arrive in the same array as the positionals, with their
# names parked in kw_names_pending, and ref's constructor never looked at that
# -- so `MyRef(o, value=24)` handed 24 to weakref_make as the callback and
# INCREF'd an int immediate as if it were a pointer.  CPython's weakref___new__
# unpacks the positional tuple and ignores the keywords entirely, leaving
# ref.__init__ to refuse them; a subclass that overrides __init__ therefore
# gets to have keyword arguments of its own, which is what
# CPython's test_weakref.test_subclass_refs does.

import gc
import _weakref


class Object:
    def __init__(self, arg):
        self.arg = arg


calls = []


def cb(r):
    calls.append(1)


# --- a subclass with its own keyword ---------------------------------------
class MyRef(_weakref.ref):
    def __init__(self, ob, callback=None, value=42):
        self.value = value
        super().__init__(ob, callback)

    def __call__(self):
        self.called = True
        return super().__call__()


o = Object("foo")
mr = MyRef(o, value=24)
print("deref:", mr() is o, mr.called, mr.value)
del o
gc.collect()
print("after del:", mr(), mr.called)

# The default is still the default, and a positional callback still works.
o = Object("bar")
m2 = MyRef(o)
print("default value:", m2.value)
m3 = MyRef(o, cb)
print("positional callback:", m3.value, m3() is o)
try:
    MyRef(o, cb, 7)
    print("three positionals -> ok")
except TypeError as e:
    print("three positionals ->", e)
del o
gc.collect()
print("callback fired:", len(calls), m3())

# --- the arities, and the refusal of keywords by ref itself -----------------
o = Object("baz")
for expr in ("_weakref.ref()",
             "_weakref.ref(o, None, 3)",
             "_weakref.ref(o, callback=None)",
             "_weakref.ref.__new__(_weakref.ref)"):
    try:
        eval(expr)
        print(expr, "-> ok")
    except TypeError as e:
        print(expr, "->", e)

# A subclass that does NOT override __init__ inherits ref's refusal.
class Plain(_weakref.ref):
    pass


try:
    Plain(o, extra=1)
    print("Plain extra kw -> ok")
except TypeError as e:
    print("Plain extra kw ->", e)

# --- the shape weakref.KeyedRef is written in ------------------------------
class Keyed(_weakref.ref):
    __slots__ = "key",

    def __new__(type, ob, callback, key):
        self = super().__new__(type, ob, callback)
        return self

    def __init__(self, ob, callback, key):
        super().__init__(ob, callback)
        self.key = key


o = Object("keyed")
k = Keyed(o, cb, "K")
print("keyed:", k.key, k() is o)
del o, k
gc.collect()
print("done")
