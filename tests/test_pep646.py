# PEP 646's starred annotation: `def g(*rest: *Ts)`.
#
# The parser took `*Ts` in a type-parameter list, which is where the shape was
# added, and not in an annotation, where it also belongs -- so the whole
# construct was "can't use starred expression here".  CPython's grammar has a
# rule of its own for it, star_annotation, and only the *args parameter may
# use it.
#
# What it compiles to is the expression followed by UNPACK_SEQUENCE 1: a
# TypeVarTuple's __iter__ yields Unpack[Ts], and a GenericAlias's yields
# itself with __unpacked__ set.  Those are the values that belong in
# __annotations__, which is why the unpack is there at all.

import warnings


def g[T, *Ts](a: T, *rest: *Ts) -> T:
    return a


print(g(1, 2, 3), g.__type_params__)
print(g.__annotations__)


def h(*args: *tuple[int, str]):
    return args


print(h.__annotations__, h(1, "a"))
print(h.__annotations__["args"].__unpacked__,
      tuple[int, str].__unpacked__)


class C[*Ts]:
    def m(self, *a: *Ts):
        return a


print(C.__type_params__, C().m(1, 2), C.m.__annotations__)


def mixed[*Ts](a, /, b, *c: *Ts, d=1, **e: str) -> int:
    return a


print(sorted(mixed.__annotations__), mixed.__annotations__["c"])

print("=== iteration is what the unpack reads ===")
print(list(iter(tuple[int, str])), repr(list(iter(tuple[int, str]))[0]))
print(type(list(iter(tuple[int, str]))[0]).__name__)

print("=== and only *args may carry one ===")
for src in ("def f(*a: *Ts): pass", "def f(*a: *tuple[int]): pass",
            "def f(*a: int): pass", "def f(a: *int): pass",
            "def f(**k: *int): pass", "def f(*, x: *int): pass",
            "def f(*a: *): pass", "def f(*a: *Ts, b: int): pass",
            "def f(a, /, b, *c: *Ts, d, **e): pass", "lambda *a: a",
            "class C[*Ts]:\n    def m(self, *a: *Ts): pass"):
    with warnings.catch_warnings():
        warnings.simplefilter("ignore")
        try:
            compile(src, "<t>", "exec")
            print("accepted", src)
        except SyntaxError as e:
            print("rejected", src, "|", e.msg)
print("done")
