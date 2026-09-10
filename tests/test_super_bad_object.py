# super(type, obj) checks what obj IS before it walks anything.
#
# The second operand is a Value and need not be a pointer at all, and both of
# the questions the handler asked were put to type_is_subtype -- which walks a
# tp_mro.  So an ordinary instance had a field of its own read as an MRO
# tuple, and `super(C, 5)` dereferenced the number.  It is what CPython's
# test_descr.test_proxy_super has been dying on, and test_super with it.
#
# The refusal is CPython's supercheck, in its order: a type that is a subtype
# binds to itself, then the object's own type, then what the object SAYS its
# class is -- which is what makes super() work through a proxy that forwards
# attribute access -- and only then a TypeError.  None is not an object at
# all: CPython makes an unbound super out of it, and nothing answers.


class B:
    def f(self):
        return "B.f"

    @classmethod
    def g(cls):
        return "B.g:" + cls.__name__


class C(B):
    def f(self):
        return super(C, self).f() + "->C.f"

    @classmethod
    def g(cls):
        return super(C, cls).g() + "->C.g"


class D(C):
    pass


print("instance:", C().f())
print("classmethod:", C.g())
print("subclass:", D().f(), D.g())
print("explicit type:", super(C, D).g())

for bad in (5, "x", 1.5, True, (1, 2), [1], {"a": 1}, None, object()):
    try:
        r = super(C, bad).f
        print("super(C, %r) -> %r" % (bad, r))
    except TypeError as e:
        print("super(C, %s) -> TypeError: %s" % (type(bad).__name__, e))
    except AttributeError as e:
        print("super(C, %s) -> AttributeError: %s" % (type(bad).__name__, e))

print("done")
