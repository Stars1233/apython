# Calling a function-valued attribute OF A BOUND METHOD must not pass the
# method as a first argument.
#
# `x.y(...)` compiles to a LOAD_ATTR with the method bit set, and the handler
# decides whether the attribute it found should be called with `x` as self.
# For an attribute that came from the TYPE's dict that is right; for one that
# a type's own tp_getattr produced out of a namespace it is wrong, and
# src/opcodes/load.asm already carried an exclusion list of five types where
# it is wrong -- module, function, classmethod, staticmethod and super, each
# added after a bug.
#
# `method` was missing from it.  So `bm.__func__(a, 1)` passed the BOUND
# METHOD as well as the two written arguments:
#
#     TypeError: A.im() takes 2 positional arguments but 3 were given
#
# and the same for anything else reached through a bound method, which
# matters most for the attributes a decorator hangs off the wrapper --
# `obj.method.cache_clear()` on an lru_cache'd method is exactly this shape.
#
# The value was never wrong: `m = bm.__func__` then `m(a, 1)` worked, and so
# did `getattr(bm, '__func__')(a, 1)`.  Only the fused call site was, which is
# why nothing smaller than a call through the attribute shows it.

import functools


class A:
    def im(self, x):
        return ("im", x)

    @classmethod
    def cm(cls, x):
        return ("cm", cls.__name__, x)

    @staticmethod
    def sm(x):
        return ("sm", x)


a = A()

print("== __func__ called through the bound method ==")
print(a.im.__func__(a, 1))
print(A.cm.__func__(A, 2))
print(a.cm.__func__(A, 3))

print()
print("== and the forms that always worked, as controls ==")
m = a.im.__func__
print(m(a, 4))
print(getattr(a.im, "__func__")(a, 5))
bm = a.im
print(bm.__func__(a, 6))

print()
print("== __self__ is still the method's own ==")
print(a.im.__self__ is a, A.cm.__self__ is A)

print()
print("== an attribute hung off the UNDERLYING function is reached through ==")
print("== the method, and must not be bound either                        ==")


def tag(fn):
    fn.marker = lambda *args: ("marker", args)
    return fn


class B:
    @tag
    def meth(self, x):
        return ("meth", x)


b = B()
print(b.meth.marker())
print(b.meth.marker(1, 2))
print(b.meth(7))


print()
print("== the shape this really costs: a decorated method's own helpers ==")


class C:
    def __init__(self):
        self.calls = 0

    @functools.lru_cache(maxsize=None)
    def slow(self, n):
        return n * 2


c = C()
print(c.slow(3), c.slow(3))
print(c.slow.cache_info().hits >= 1)
c.slow.cache_clear()
print(c.slow.cache_info().hits)

print()
print("== a plain function's attributes, the case already fixed ==")


@tag
def free(x):
    return ("free", x)


print(free.marker(9))
print(free(10))

print()
print("== and a method on a builtin instance ==")
# A builtin method has __self__ but no __func__ -- there is no Python
# function under it -- so only the first is asked for here.
s = "abc"
print(s.upper.__self__)
print(s.upper())
print([].append.__self__)
