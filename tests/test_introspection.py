# The introspection surface the stdlib is built on.  All of this raised
# AttributeError before: types.py alone needs __class__, type.__dict__ (as a
# mappingproxy), func.__code__ / __globals__ / __defaults__ / __closure__,
# the co_* fields, sys.implementation, list[int] and int | str.


def err(fn, *a):
    try:
        return fn(*a)
    except BaseException as e:
        return type(e).__name__


class C:
    a = 1

    def m(self):
        pass


c = C()

# __class__ works for every kind of value, immediates included
print(c.__class__, (5).__class__, (1.5).__class__, "s".__class__)
print([].__class__, {}.__class__, None.__class__, True.__class__)
print(getattr(5, "__class__"), hasattr(1.5, "__class__"))

# instance __dict__, created on demand
print(c.__dict__)
c.x = 1
print(c.__dict__, C().__dict__)

# a class dict is a read-only proxy
print(type(C.__dict__).__name__, type(type.__dict__).__name__)
print("a" in C.__dict__, C.__dict__["a"], sorted(k for k in C.__dict__ if k == "m"))
print(err(lambda: C.__dict__.__setitem__("b", 2)))

# object is introspectable
print(callable(object.__init__), callable(object.__str__), callable(object.__repr__))


# function attributes
def f(a=1, b=2):
    "the doc"
    return a


print(f.__defaults__, f.__doc__, f.__module__, f.__name__, f.__qualname__)
print(f.__globals__ is globals(), f.__closure__)


def outer():
    v = 1

    def inner():
        return v
    return inner


print(len(outer().__closure__), outer()())

# code objects
print(f.__code__.co_name, f.__code__.co_argcount, f.__code__.co_flags > 0)
print(f.__code__.co_filename.endswith("test_introspection.py"))
print(type(f.__code__.co_consts).__name__, f.__code__.co_firstlineno > 0)
print(type(type(f).__code__).__name__, type(type(f).__globals__).__name__)

# the default repr of a plain instance is not the base type's
class L(list):
    pass


class S(str):
    pass


print(L([1, 2]), str(S("ab")), repr(S("ab")))


class R:
    def __repr__(self):
        return "R!"


print(repr(R()), str(R()))

# sys
import sys

# .name is legitimately "apython" here, so only the parts a loader
# depends on are compared against CPython.
print(isinstance(sys.implementation.name, str), sys.implementation.cache_tag)
print(type(sys.implementation).__name__, sys.warnoptions)

# PEP 585 and PEP 604
print(list[int], dict[str, int], tuple[int, ...], set[str])
print(type(list[int]).__name__, list[int].__origin__)
print(int | str, type(int | str).__name__, int | None)
print(list[int]() == [], dict[str, int]() == {})
