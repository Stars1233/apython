"""A method-style load does not build a method to take apart.

`c.m()` used to allocate a PyMethodObject -- a gc_alloc, two increfs and a
gc_track -- which op_load_attr then unwrapped into [func, self] two
instructions later and released, for a malloc, a free and two GC list
operations per method load.  instance_getattr_where takes a flag now, and
answers with the function itself when the caller says it will take one.

The flag has to be asked for rather than implied, because obj_getattr_opt
shares that entry: `getattr(c, 'm')`, `hasattr` and `vars()`'s slow path all
go through it, and `getattr(c, 'm')` must still be a bound method with a
__self__.  This file is mostly about that boundary -- everything that is NOT a
plain function reached through the type, and every way of asking.
"""


class C:
    def __init__(self):
        self.x = 1

    def m(self, v):
        return self.x + v

    def noargs(self):
        return "noargs"

    @staticmethod
    def s(v):
        return v * 2

    @classmethod
    def k(cls, v):
        return cls.__name__ + str(v)

    @property
    def p(self):
        return "prop"

    def __getattr__(self, n):
        return "fallback:" + n


c = C()

print("--- calling ---")
print("call:", c.m(1))
print("noargs:", c.noargs())
print("twice:", c.m(1), c.m(2))
print("in a loop:", [c.m(i) for i in range(3)])

print("--- the bound method itself ---")
b = c.m
print("type:", type(b).__name__)
print("self:", b.__self__ is c)
print("func:", b.__func__ is C.m)
print("name:", b.__name__)
print("call it:", b(5))
print("repr:", repr(b).startswith("<bound method C.m of "))
print("two loads are not the same object:", c.m is not c.m)

print("--- every way of asking ---")
print("getattr:", type(getattr(c, "m")).__name__, getattr(c, "m")(1))
print("hasattr:", hasattr(c, "m"), hasattr(c, "nothing_at_all"))
print("vars:", sorted(vars(c)))
print("dir:", "m" in dir(c))
print("through the class:", C.m(c, 1), type(C.m).__name__)
print("__dict__ of the class:", type(C.__dict__["m"]).__name__)

print("--- what must not take the unbound path ---")
print("static:", c.s(3), C.s(3), type(c.s).__name__)
print("classmethod:", c.k(4), C.k(4), type(c.k).__name__)
print("property:", c.p)
print("__getattr__:", c.zzz)


class Slotted:
    __slots__ = ("v",)

    def m(self):
        return self.v


sl = Slotted()
sl.v = 7
print("slots:", sl.m(), type(sl.m).__name__)


class WithData:
    def __init__(self):
        self.m = lambda v: "instance " + str(v)

    def m2(self, v):
        return "class " + str(v)


w = WithData()
print("instance shadows:", w.m(1), type(w.m).__name__)
print("class one still:", w.m2(2))

print("--- shadowing after the site is warm ---")
d = C()
for _ in range(50):
    d.m(1)
d.m = lambda v: "shadow" + str(v)
print("shadowed:", d.m(9))
del d.m
print("restored:", d.m(9))

print("--- the class changing under a warm site ---")
e = C()
for _ in range(50):
    e.m(1)
C.m = lambda self, v: "replaced" + str(v)
print("replaced:", e.m(1))


class C2:
    def __init__(self):
        self.x = 1

    def m(self, v):
        return self.x + v


print("--- inheritance ---")


class Base:
    def m(self, v):
        return "base" + str(v)


class Mid(Base):
    pass


class Leaf(Mid):
    pass


lf = Leaf()
print("through two levels:", lf.m(1), type(lf.m).__name__)
Mid.m = lambda self, v: "mid" + str(v)
print("after the middle overrides:", lf.m(1))

print("--- a builtin method is unaffected ---")
s = "abc"
# type(s.upper) is `method` here rather than
# builtin_function_or_method -- DIVERGENCES.md, one builtin callable
# type where CPython has four -- so this asks what is portable.
print("builtin:", s.upper(), callable(s.upper), s.upper.__self__ == "abc")
lst = [1]
print("list:", callable(lst.append), lst.append.__name__)
lst.append(2)
print("appended:", lst)

print("--- a method stored and called later ---")
saved = [C2().m for _ in range(3)]
print("saved:", [f(1) for f in saved])
print("still bound:", all(type(f).__name__ == "method" for f in saved))

print("done")
