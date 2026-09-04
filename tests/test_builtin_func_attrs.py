# A builtin function's __name__, __qualname__ and __module__.
#
# builtin_func_type had no tp_getattr at all, so every lookup fell through to
# a type-dict search that answered nothing.  The stdlib asks for the first two
# constantly -- anything that wraps a callable reads f.__name__, and
# functools.wraps copies both -- and `statistics` stopped on it.
print("-- a plain builtin")
print(len.__name__, len.__qualname__, len.__module__)
print(abs.__name__, abs.__qualname__, abs.__module__)

print()
print("-- a method, which is qualified by the type that owns it")
print("x".upper.__name__, "x".upper.__qualname__, "x".upper.__module__)
print([].append.__name__, [].append.__qualname__)
print({}.get.__name__, {}.get.__qualname__)
print((7).bit_length.__name__, (7).bit_length.__qualname__)

print()
print("-- unbound, off the type")
print(str.upper.__name__, str.upper.__qualname__)
print(list.append.__name__, list.append.__qualname__)

print()
print("-- something that is not an attribute of one")
try:
    len.nosuchthing
except AttributeError as e:
    print("AttributeError:", type(e).__name__)

print()
print("-- and it survives being wrapped")


def wrap(f):
    def inner(*a):
        return f(*a)
    inner.__name__ = f.__name__
    inner.__qualname__ = f.__qualname__
    return inner


w = wrap(len)
print(w.__name__, w.__qualname__, w([1, 2, 3]))
