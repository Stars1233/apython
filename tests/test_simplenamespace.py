# types.SimpleNamespace had no constructor at all.
#
# Calling the type fell through to the ordinary class-construction path: the
# object came out of instance_new with no ns_dict -- "namespace has no
# attribute storage" on the first assignment -- and was then freed at the
# object pointer by namespace_dealloc, sixteen bytes above the block gc_alloc
# had handed out.  An invalid free on `types.SimpleNamespace()`, which
# test/test_importlib/util.py does at import time and half of CPython's suite
# imports.
#
# Four more of the type's own gaps came out with it: a delete stored a NULL
# instead of removing the entry, __dict__ was not answered, equality fell
# through to identity, and a namespace holding itself reprd for ever.

import types
n = types.SimpleNamespace(a=1, b=2)
print("kwargs:", n, n.a, n.b)
n.c = 3
print("setattr:", n.c, n)
del n.a
print("delattr:", n)
e = types.SimpleNamespace()
print("empty:", e, repr(e))
print("eq:", types.SimpleNamespace(x=1) == types.SimpleNamespace(x=1))
print("ne:", types.SimpleNamespace(x=1) == types.SimpleNamespace(x=2))
print("dict:", sorted(vars(n).items()))
try:
    types.SimpleNamespace(1)
except TypeError as ex:
    print("positional:", ex)
try:
    e.missing
except AttributeError as ex:
    print("missing:", ex)
class Sub(types.SimpleNamespace):
    pass
s = Sub(q=9)
print("subclass:", s, s.q, type(s).__name__)
print("done")

SN = types.SimpleNamespace
n = SN(a=1, b=2)
print("dict:", n.__dict__, sorted(vars(n).items()))
del n.a
print("after del:", n, n.__dict__)
try:
    del n.missing
except AttributeError as e:
    print("del missing:", e)
print("eq same:", SN(x=1) == SN(x=1), "ne:", SN(x=1) == SN(x=2))
print("eq empty:", SN() == SN())
print("eq other:", SN(x=1) == 5, SN(x=1) != SN(x=1))
class Sub(SN): pass
s = Sub(q=9)
print("sub:", s, repr(s), s.q, s == Sub(q=9), s == SN(q=9))
print("dict writable:", n.__dict__.__class__.__name__)
n.__dict__["z"] = 3
print("through dict:", n.z)
print("recursive:", end=" ")
r = SN()
r.me = r
print(repr(r))
print("done")
