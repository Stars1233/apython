# repr(type) printed only the bare name, so every class showed as <class 'C'>
# where CPython shows <class '__main__.C'>.  A builtin stays unqualified.


class C:
    pass


class Sub(C):
    pass


class Err(Exception):
    pass


print(C, repr(C), str(C))
print(Sub, Err, type(C()), type(Sub()))
print(int, str, float, dict, list, tuple, set, bytes, type, object)
print(ValueError, KeyError, BaseException)
print(C.__name__, C.__module__, C.__qualname__)
print(type(len), type(None), type(...), type(NotImplemented))

T = type("T", (), {})
print(T, T.__name__)


def outer():
    class Inner:
        pass
    return Inner


print(outer().__name__)
