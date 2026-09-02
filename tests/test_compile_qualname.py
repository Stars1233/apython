# __qualname__, built by OUR compiler.
#
# CompUnit.qualname was set to the bare name, so a method compiled from source
# reported "m" where the same file's .pyc reported "C.m", and a nested
# function "i" rather than "o.<locals>.i".  It is invisible from a .pyc --
# CPython's compiler put the right string in co_qualname -- so nothing caught
# it until a test read __qualname__ and check-source ran the same file through
# both compilers.
#
# A class's own __qualname__ is a second store: it comes from the
# `__qualname__ = ...` the class body opens with, not from its code object,
# and that carried the bare name too.
#
# The chain comes from the SCOPE tree.  Units are not linked to each other and
# Comp.cur_unit is not maintained across the nesting, so the enclosing names
# are read from Scope.name -- a field that existed and was never written.


class A:
    def m(self):
        pass

    class B:
        def n(self):
            pass

        def deep(self):
            def inner():
                pass
            return inner


def f():
    def g():
        def h():
            pass
        return h
    return g


def outer():
    class Local:
        def meth(self):
            pass
    return Local


lam = lambda: 1


class C:
    lam2 = lambda self: 1


print("method        :", A.m.__qualname__)
print("nested class  :", A.B.n.__qualname__)
print("both          :", A.B().deep().__qualname__)
print("nested funcs  :", f()().__qualname__)
print("lambdas       :", lam.__qualname__, C.lam2.__qualname__)
print("class in func :", outer().__qualname__, outer().meth.__qualname__)
print("the classes   :", [x.__qualname__ for x in (A, A.B, f, outer)])
print("co_qualname   :", A.m.__code__.co_qualname, f()().__code__.co_qualname)
print("__name__ kept :", A.m.__name__, f()().__name__, A.B.__name__)

# An assignment still wins, and still does not touch the dict.
A.m.__qualname__ = "renamed"
print("assigned      :", A.m.__qualname__, A.m.__dict__)

print("=== and the code still runs ===")
print(A().m(), A.B().n(), f()()(), outer()().meth(), lam(), C().lam2())
