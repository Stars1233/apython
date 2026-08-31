# An identifier written __spam inside `class C` is _C__spam.  Nothing mangled
# at all, so a base's private attribute and a subclass's named the same slot
# and silently collided.
class A:
    def __init__(self):
        self.__x = 1

    def get_a(self):
        return self.__x


class B(A):
    def __init__(self):
        super().__init__()
        self.__x = 2

    def get_b(self):
        return self.__x


b = B()
print(b.get_a(), b.get_b(), sorted(b.__dict__))
print(b._A__x, b._B__x)


# Methods and class attributes mangle too.
class C:
    __attr = 10

    def __priv(self):
        return "priv"

    def call(self):
        return self.__priv() + str(C.__attr)


print(C().call())
print(sorted(k for k in C.__dict__ if "priv" in k or "attr" in k))


# ...but a dunder does not, and neither does a single underscore.
class D:
    _one = 1
    __two__ = 2

    def __init__(self):
        self._a = 1
        self.__b__ = 2


print(sorted(k for k in D.__dict__ if not k.startswith("__")), sorted(D().__dict__))


# A nested class mangles against the innermost one, and a function nested in a
# class body still mangles against the class.
class Outer:
    class Inner:
        def __init__(self):
            self.__v = "inner"

    def make(self):
        def helper(o):
            return o.__v

        return helper


i = Outer.Inner()
print(sorted(i.__dict__))
try:
    print(Outer().make()(i))
except AttributeError as e:
    print("helper mangles against Outer")


# Outside any class, nothing mangles.
class E:
    pass


e = E()
e.__z = 5
print(sorted(e.__dict__))


# `global` and parameter names mangle inside a class body.
class F:
    def g(self, __arg):
        return __arg


print(F().g(7))

# A class whose name is all underscores mangles nothing.
class ___:
    def __init__(self):
        self.__q = 1


print(sorted(___().__dict__))


# A `global` or `nonlocal` declaration is mangled like every other identifier.
# `global __v` inside a method of C is a declaration about _C__v; interning it
# raw bound a different name from the one every use of it resolved to.
ns = {}
exec("class C:\n    def m(self):\n        global __v\n        __v = 7\n"
     "        return __v\nout = C().m()\n", ns)
print(ns["out"], "__v" in ns, "_C__v" in ns)

# nonlocal is mangled the same way, and the two halves have to agree: the
# declaration and the assignment both become _E__w inside class E's method.
ns = {}
exec("def outer():\n    __w = 1\n    class E:\n        def m(self):\n"
     "            return 'ok'\n    def inner():\n        nonlocal __w\n"
     "        __w = 2\n    inner()\n    return __w\nout = outer()\n", ns)
print(ns["out"])
