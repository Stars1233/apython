# Implicit chaining must not hang on a context cycle that is already there.
#
# `ex.__context__ = ex` is a legal assignment, so the chain a raise walks can
# be circular before the raise ever happens.  exc_set_context scans that chain
# for the exception being raised -- so a re-raise cannot make it point at
# itself -- and walked a pre-existing cycle for ever.
#
# CPython uses Floyd's tortoise and hare in _PyErr_SetObject for exactly this,
# and its test_exceptions has three tests named for not hanging on it
# (issue 25782).  All three are here.


# --- cycle in the middle of the chain -------------------------------------
def cycle():
    try:
        raise ValueError(1)
    except ValueError as ex:
        ex.__context__ = ex
        raise TypeError(2)


try:
    cycle()
except Exception as e:
    exc = e

print(type(exc).__name__, type(exc.__context__).__name__)
print(exc.__context__.__context__ is exc.__context__)


# --- cycle at the head: C -> B -> A -> C -----------------------------------
class A(Exception):
    pass


class B(Exception):
    pass


class C(Exception):
    pass


a = b = c = None
caught = None
try:
    try:
        raise A()
    except A as _a:
        a = _a
        try:
            raise B()
        except B as _b:
            b = _b
            try:
                raise C()
            except C as _c:
                c = _c
                a.__context__ = c
                raise c
except C as got:
    caught = got

print(caught is c, c.__context__ is b, b.__context__ is a, a.__context__ is c)


# --- a longer chain with a cycle in it: E -> D -> C -> B -> A -> C ---------
class D(Exception):
    pass


class E(Exception):
    pass


a2 = b2 = c2 = d2 = e2 = None
caught2 = None
try:
    try:
        raise A()
    except A as _a:
        a2 = _a
        try:
            raise B()
        except B as _b:
            b2 = _b
            try:
                raise C()
            except C as _c:
                c2 = _c
                a2.__context__ = c2
                try:
                    raise D()
                except D as _d:
                    d2 = _d
                    e2 = E()
                    raise e2
except E as got:
    caught2 = got

print(caught2 is e2, e2.__context__ is d2, d2.__context__ is c2)
print(c2.__context__ is b2, b2.__context__ is a2, a2.__context__ is c2)


# --- a two-element cycle, raised into repeatedly --------------------------
x = ValueError("x")
y = ValueError("y")
x.__context__ = y
y.__context__ = x
last = None
for i in range(50):
    try:
        try:
            raise x
        except ValueError:
            raise TypeError(i)
    except TypeError as t:
        last = (t.args, type(t.__context__).__name__)
print("two-element cycle survived", last)

# The ordinary chain is untouched.
try:
    try:
        raise KeyError("inner")
    except KeyError:
        raise IndexError("outer")
except IndexError as o:
    print(type(o).__name__, type(o.__context__).__name__, o.__context__.args)
    print(o.__context__.__context__)

# A re-raise of the exception being handled still gets no self-reference.
try:
    try:
        raise LookupError("one")
    except LookupError as le:
        raise le
except LookupError as out:
    print(type(out).__name__, out.args, out.__context__)
print("done")
