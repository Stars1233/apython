# What a wrong call says.
#
# "function missing required argument" was the whole of it: one string, for
# every shape of missing argument, naming neither the function nor the
# argument nor how many were wanted.  CPython names all three, and this is
# the most-seen error message in Python -- it is what a typo in a call
# produces, and a test suite that asserts on a TypeError almost always
# asserts on this one.
#
# And "takes 1 positional arguments" was plural where CPython is singular.
# CPython pluralises on the count it is reporting, not unconditionally.
#
# The qualname is what is reported, not the name: a nested function says
# "outer.<locals>.inner()" and a method says "C.m()".


def f1(a, b):
    pass


def f2(a, b, c):
    pass


def f3(a, b, c, d):
    pass


def f4(a, *, k):
    pass


def f5(*, k, j):
    pass


def f6(a, b=1, *, k):
    pass


def f7(a, b, c, d, e, g):
    pass


def show(fn, *args, **kwargs):
    try:
        fn(*args, **kwargs)
        print("NOT REFUSED")
    except TypeError as exc:
        print(exc)


# --- one, two, three and more missing positionals ----------------------
# The name list is CPython's: one name bare, two joined with "and", three or
# more comma-separated with an Oxford comma before the last.
show(f1, 1)
show(f1)
show(f2, 1, 2)
show(f2, 1)
show(f2)
show(f3)
show(f3, 1)
show(f3, 1, 2)
show(f7)
show(f7, 1)
show(f7, 1, 2)

# --- keyword-only, which is a different word and a separate tally ------
# A missing positional wins: CPython reports positionals if any are missing
# and keyword-only only when the positionals are all filled.
show(f4)
show(f4, 1)
show(f5)
show(f6)
show(f6, 1)
show(f6, 1, 2)

# --- filling some by keyword ------------------------------------------
show(f2, 1, c=3)
show(f2, c=3)
show(f3, b=2)
show(f4, k=1)

# --- too many, where the plural was wrong ----------------------------
def one(a):
    pass


def none():
    pass


def two_with_default(a, b=1):
    pass


show(one, 1, 2)
show(none, 1)
show(none, 1, 2)
show(two_with_default, 1, 2, 3)
show(f2, 1, 2, 3, 4)

# --- the qualname, not the name --------------------------------------
def outer():
    def inner(a, b):
        pass

    show(inner, 1)
    show(inner, 1, 2, 3)


outer()


class C:
    def m(self, a):
        pass

    @staticmethod
    def s(a, b):
        pass

    @classmethod
    def c(cls, a):
        pass


show(C().m)
show(C.s, 1)
show(C().c)

# --- a lambda and a comprehension-shaped call -----------------------
show(lambda a, b: None, 1)

# --- defaults that were assigned rather than written -----------------
# The count is read off func_defaults, so a function whose defaults were
# written after the fact has to report the new arity.
def assigned(a, b, c):
    pass


assigned.__defaults__ = (3,)
show(assigned)
show(assigned, 1)
show(assigned, 1, 2, 3, 4)

# --- and what must still be accepted ---------------------------------
print("correct calls:", f1(1, 2), f4(1, k=2), f5(k=1, j=2), f6(1, k=2),
      two_with_default(1), one(1), none())
print("survived")
