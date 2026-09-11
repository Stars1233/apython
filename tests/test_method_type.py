# types.MethodType is a constructor, not just a name for the bound-method type.
#
# `method_type` had tp_new = 0, so type_call fell through to the ordinary
# class-construction path and refused every argument: "method() takes no
# arguments".  contextlib.py's `return MethodType(cm_exit, cm)` is the line 24
# CPython test modules die on, and functools, unittest.mock and inspect all
# build one the same way.

import types

MethodType = types.MethodType


def f(self, x):
    return ("f", self, x)


class C:
    def __repr__(self):
        return "<C>"

    def m(self, x):
        "docstring"
        return ("m", self, x)


c = C()

# A plain function bound to an arbitrary object.
b = MethodType(f, 7)
print(type(b) is MethodType, b.__func__ is f, b.__self__, b(3))

# The type of an ordinary bound method IS this type, and rebinding round-trips.
bm = c.m
print(type(bm) is MethodType, bm.__self__ is c, bm.__func__ is C.m)
print(MethodType(bm.__func__, bm.__self__)(5))

# A bound method as the function half: CPython allows it, and the new self is
# passed as the first *argument* of the one already bound.
print(MethodType(bm, "outer")())

# A builtin, a lambda, and a callable instance.
print(MethodType(len, [1, 2, 3])())
print(MethodType(lambda self: self * 2, 21)())


class Callable:
    def __call__(self, self2, x):
        return ("call", self2, x)


print(MethodType(Callable(), "s")(1))

# self may be almost anything, an immediate int included -- but not None,
# which CPython refuses by name.
print(MethodType(f, 5)(2))
try:
    MethodType(f, None)
except TypeError as e:
    print("None refused:", e)

# Equality is (func, self) pairwise, and hash agrees.
b2 = MethodType(f, 7)
print(b == b2, b == MethodType(f, 8), hash(b) == hash(b2))

# The function's attributes show through.
print(b.__name__, b.__func__.__name__, bm.__doc__)
print(repr(bm))

# Arity: exactly two, and the first must be callable.
for args in ((), (f,), (f, 1, 2)):
    try:
        MethodType(*args)
    except TypeError:
        print("TypeError", len(args))
    else:
        print("accepted", len(args))

try:
    MethodType(3, 4)
except TypeError:
    print("not callable refused")
else:
    print("not callable ACCEPTED")

# Keyword arguments are refused.
try:
    MethodType(func=f, obj=7)
except TypeError:
    print("kwargs refused")

# It is not subclassable, exactly as in CPython.
try:
    class Sub(MethodType):
        pass
except TypeError:
    print("not subclassable")
else:
    print("subclassable")

# The classic use: contextlib's _create_cb_wrapper shape.
class CM:
    def __repr__(self):
        return "<CM>"

    def __exit__(self, *exc):
        return ("exit", self, exc)


cm = CM()
wrapper = MethodType(type(cm).__exit__, cm)
print(wrapper(None, None, None))

# Rebinding one class's function onto another instance.
c2 = C()
print(MethodType(C.m, c2)(1)[1] is c2)

# It is a GC-tracked object with a cycle through itself.
class Node:
    def __repr__(self):
        return "<Node>"

    def go(self):
        return 1


n = Node()
n.bound = MethodType(Node.go, n)
print(n.bound())
del n
print("done")
