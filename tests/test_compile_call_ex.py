# `o.m(*a)` and `o.m(**k)` cannot use LOAD_ATTR's method form: CALL_FUNCTION_EX
# reads [NULL, callable, args], and the method form fills that NULL slot with
# the instance instead.  The choice was being made after the callable had
# already been emitted, so the object was called rather than the method --
# "TypeError: object is not callable" on a perfectly ordinary call.
class P:
    def m(self, *a, **k):
        return ("m", a, sorted(k.items()))

    def __init__(self, tag="t"):
        self.tag = tag


p = P()
print(p.m(*(1, 2)))
print(p.m(**{"x": 1}))
print(p.m(*(1,), **{"y": 2}))
print(p.m(3, *(4,), z=5, **{"w": 6}))

# The plain form must still take the method shortcut.
print(p.m(1, 2))
print(p.m(1, k=2))

# A dunder reached the same way is what found it, in enum's __set_name__.
p.__init__(*("u",))
print(p.tag)
p.__init__(**{"tag": "v"})
print(p.tag)

# Builtin methods, bound methods and nested attributes.
l = [3, 1, 2]
l.sort(*())
print(l)
print(l.count(*(1,)))
bound = p.m
print(bound(*(9,)))


class Holder:
    def __init__(self):
        self.p = P()


h = Holder()
print(h.p.m(*(7,)))

# super() has the same shape: LOAD_SUPER_ATTR's method form fills the same
# slot, and its NULL has to come from a PUSH_NULL of our own.
class B:
    def f(self, *a, **k):
        return ("B.f", a, sorted(k.items()))


class C(B):
    def g(self, a):
        return super().f(*a)

    def h(self, k):
        return super().f(**k)

    def i(self, a, k):
        return super().f(0, *a, **k)

    def j(self, a):
        return super().f(a)


c = C()
print(c.g((1, 2)))
print(c.h({"q": 1}))
print(c.i((3,), {"r": 2}))
print(c.j(8))

# And through our own compiler at run time.
print(eval("p.m(*(1, 2))", {"p": p}))
exec("out = p.m(*(5,), **{'e': 6})", {"p": p}, d := {})
print(d["out"])
