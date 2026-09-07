# CALL rewrites itself into an inline frame push when the callable is a plain
# Python function whose parameters are exactly filled.  Both call shapes go
# through it: `f(a, b)`, where the slot below the callable is NULL, and the
# [method, self] pair LOAD_ATTR_METHOD leaves, where self is already contiguous
# with the arguments and counts as one of them.
#
# Everything here is a shape that must NOT take that path, or a shape that
# takes it and must still be right about argument order, refcounts and
# exceptions.


def exact_arities():
    def zero():
        return "z"

    def one(a):
        return a

    def two(a, b):
        return a - b

    def five(a, b, c, d, e):
        return (a, b, c, d, e)

    out = []
    for i in range(50):
        out.append(zero())
        out.append(one(i))
        out.append(two(i, 1))
        out.append(five(i, 1, 2, 3, 4))
    return out[:4], out[-4:], len(out)


def shapes_that_must_deopt():
    def defaults(a, b=5):
        return a + b

    def star(*a):
        return sum(a)

    def kwargs(a, **k):
        return a + len(k)

    def kwonly(a, *, b):
        return a + b

    def gen(n):
        for i in range(n):
            yield i

    async def coro():
        return 1

    out = []
    for i in range(20):
        out.append(defaults(i))
        out.append(defaults(i, 2))
        out.append(star(1, 2, 3))
        out.append(kwargs(i, z=1))
        out.append(kwonly(i, b=2))
        out.append(list(gen(3)))
    c = coro()
    c.close()
    return out[:6], len(out)


def one_site_many_callables():
    def a(x):
        return x + 1

    def b(x):
        return x + 2

    class C:
        def __call__(self, x):
            return x + 3

    callables = [a, b, C(), len, str]
    out = []
    for _ in range(20):
        for f in callables:
            if f is len or f is str:
                out.append(f([1, 2]) if f is len else f(7))
            else:
                out.append(f(10))
    return out[:5], len(out)


def method_shape():
    class P:
        def __init__(self, v):
            self.v = v

        def m0(self):
            return self.v

        def m1(self, x):
            return self.v + x

        def m2(self, x, y):
            return self.v + x * y

    p = P(10)
    out = []
    for i in range(50):
        out.append(p.m0())
        out.append(p.m1(i))
        out.append(p.m2(i, 2))
    return out[:3], out[-3:], len(out)


def static_and_class_methods():
    class C:
        @staticmethod
        def s(x):
            return x * 3

        @classmethod
        def c(cls, x):
            return x * 4

    out = []
    for i in range(20):
        out.append(C.s(i))
        out.append(C.c(i))
        out.append(C().s(i))
    return out[:3], len(out)


def recursion_and_exceptions():
    def fib(n):
        if n < 2:
            return n
        return fib(n - 1) + fib(n - 2)

    def boom(x):
        if x == 3:
            raise ValueError("boom %d" % x)
        return x

    caught = []
    for i in range(6):
        try:
            caught.append(boom(i))
        except ValueError as e:
            caught.append(str(e))
    return fib(18), caught


def the_callable_is_the_last_reference():
    """The function object's only reference is the stack slot the call
    consumes, so it has to outlive the frame that is running its code."""
    def make(n):
        def inner(x):
            return x + n
        return inner

    out = []
    for i in range(200):
        out.append(make(i)(1))
    return len(out), out[0], out[-1]


def argument_order():
    def f(a, b, c):
        return "%s|%s|%s" % (a, b, c)

    out = []
    for i in range(10):
        out.append(f(i, i + 1, i + 2))
    return out[:3]


def nested_calls():
    def inner(x):
        return x * 2

    def outer(x):
        return inner(x) + inner(x + 1)

    return [outer(i) for i in range(5)]


print(exact_arities())
print(shapes_that_must_deopt())
print(one_site_many_callables())
print(method_shape())
print(static_and_class_methods())
print(recursion_and_exceptions())
print(the_callable_is_the_last_reference())
print(argument_order())
print(nested_calls())

# Deep recursion must still raise RecursionError rather than crash.
def deep(n):
    return deep(n + 1)


try:
    deep(0)
except RecursionError:
    print("RecursionError")

# A function called with the wrong count still reports it.
def two_args(a, b):
    return a + b


for bad in ((1,), (1, 2, 3)):
    try:
        two_args(*bad)
    except TypeError:
        print("TypeError")
