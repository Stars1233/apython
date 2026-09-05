"""PEP 3107 annotations: emitted by the compiler, kept on the function.

MAKE_FUNCTION popped the annotations tuple and threw it away, and the
compiler never built one, so every function in the process answered no
`__annotations__` at all -- which is what functools.singledispatch reads to
find the type a `@register`ed implementation dispatches on.

Two things are worth pinning beyond "it works".  The ORDER is observable,
because __annotations__ is a dict: CPython emits regular positional
parameters first, THEN the positional-only ones, then *args, then
keyword-only, then **kwargs, then "return" -- not the order they are written
in.  And annotations are evaluated in the DEFINING scope at def time, exactly
as defaults are, which is why a name used only in one still has to be
classified there.
"""


def plain(a: int, b: "str" = "x") -> bool:
    return True


print("--- the dict, and its order ---")
print(plain.__annotations__)
print(list(plain.__annotations__))


def full(p: int, /, a: str, *args: float, k: bool = 1, **kw: bytes) -> None:
    pass


print(full.__annotations__)
print(list(full.__annotations__))


def none_at_all(a, b=1):
    pass


print(none_at_all.__annotations__)


def only_return() -> int:
    return 1


print(only_return.__annotations__)


def only_star(*args: int, **kw: str):
    pass


print(only_star.__annotations__, list(only_star.__annotations__))

print("--- the dict is real, and the same one every time ---")
print(none_at_all.__annotations__ is none_at_all.__annotations__)
none_at_all.__annotations__["z"] = float
print(none_at_all.__annotations__)
print(plain.__annotations__ is plain.__annotations__)

print("--- evaluated in the defining scope, at def time ---")
order = []


def note(tag):
    order.append(tag)
    return tag


def watched(a: note("A"), b: note("B") = 1) -> note("R"):
    pass


print(order)
print(watched.__annotations__)

print("--- and the defining scope is the enclosing one ---")


def outer():
    T = str

    def inner(x: T) -> T:
        return x

    return inner


print(outer().__annotations__)


def two_deep():
    T = bytes

    def middle():
        def inner(x: T):
            return x

        return inner

    return middle


print(two_deep()().__annotations__)


class WithLocal:
    Local = complex

    def m(self, v: Local) -> Local:
        pass


print(WithLocal.m.__annotations__)

print("--- a lambda has none, and keeps its body ---")
lam = lambda x, y=2: x + y
print(lam.__annotations__, lam(1), lam(1, 5))

print("--- defaults and annotations together ---")


def both(a=1, *, b: int = 2, **kw: str):
    return a, b


print(both.__annotations__, both.__defaults__, both.__kwdefaults__)
print(both(), both(9, b=8))

print("--- a name that is only in an annotation is still looked up ---")
try:
    def bad(x: undefined_name):
        pass
except NameError as e:
    print("NameError", e)

print("--- assigning the attribute outright ---")
plain.__annotations__ = {"a": "replaced"}
print(plain.__annotations__)

print("--- a method, a nested class, and a closure over the annotation ---")


def make(kind):
    class Holder:
        def take(self, v: kind) -> kind:
            return v

    return Holder


print(make(int)().take(3), make(int).take.__annotations__)
print(make(str).take.__annotations__)
print("done")
