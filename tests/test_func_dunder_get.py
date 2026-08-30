# A function is a descriptor, and so are staticmethod and classmethod.  The
# binding LOAD_ATTR does natively was not reachable as `__get__`, so
# `hasattr(f, '__get__')` was False -- and enum decides whether a name in a
# class body is a member or a method by exactly that test, which made every
# ordinary helper in an Enum body an enum member.
def f(a, b=2):
    return ("f", a, b)


print(hasattr(f, '__get__'), hasattr(f, '__set__'), hasattr(f, '__delete__'))


class K:
    @staticmethod
    def s(a):
        return ("s", a)

    @classmethod
    def c(cls, a):
        return ("c", cls.__name__, a)

    def m(self, a):
        return ("m", a)


for n in ('s', 'c', 'm'):
    v = K.__dict__[n]
    print(n, hasattr(v, '__get__'), hasattr(v, '__set__'), hasattr(v, '__delete__'))


class Obj:
    pass


o = Obj()

# Binding a plain function to an instance is what a method is.
bound = f.__get__(o)
print(bound(1)[0], bound(1)[2])
print(f.__get__(o, Obj)(7)[2])

# Through the class, with no instance, the function is itself.
print(f.__get__(None, Obj) is f)

# staticmethod hands back the plain function either way.
sm = K.__dict__['s']
print(sm.__get__(None, K)("x"), sm.__get__(K(), K)("y"))

# classmethod binds the class, from the instance or from the class.
cm = K.__dict__['c']
print(cm.__get__(None, K)("x"))
print(cm.__get__(K(), K)("y"))
print(cm.__get__(K())("z"))

# The ordinary attribute paths must be unchanged.
k = K()
print(k.m(1), k.s(2), k.c(3), K.s(4), K.c(5))

# _is_descriptor, as enum spells it.
def is_descriptor(obj):
    return (hasattr(obj, '__get__') or hasattr(obj, '__set__')
            or hasattr(obj, '__delete__'))

print([is_descriptor(x) for x in (f, sm, cm, K.__dict__['m'], 1, "s", property(f))])
