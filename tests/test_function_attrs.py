# __qualname__ and __doc__ live ON the function, not in its __dict__.
#
# Both used to be stored in func_dict like any other assigned attribute, so
# `f.__dict__` was non-empty after anything that set them -- and setting them
# is exactly what functools.wraps does, on every decorated function.  CPython
# keeps both on the function object and leaves the dict empty until something
# else is assigned.  __name__ already had a field; these two did not.
#
# Reading still falls back the way it did: the field, then the dict (for code
# that wrote there directly), then the code object -- co_qualname for the
# name and co_consts[0] for the docstring.

def with_doc():
    "from the source"
    return 1


def without_doc():
    return 2


print("=== what a fresh function carries ===")
print("doc      :", repr(with_doc.__doc__), repr(without_doc.__doc__))
print("qualname :", with_doc.__qualname__, without_doc.__qualname__)
print("name     :", with_doc.__name__)
print("dict     :", with_doc.__dict__, without_doc.__dict__)

print("=== assigning does not touch the dict ===")
with_doc.__doc__ = "assigned"
with_doc.__qualname__ = "Assigned.name"
print("doc      :", repr(with_doc.__doc__))
print("qualname :", with_doc.__qualname__)
print("dict     :", with_doc.__dict__)

with_doc.__doc__ = None
print("doc=None :", repr(with_doc.__doc__), with_doc.__dict__)

with_doc.other = 1
print("other    :", with_doc.__dict__)
print("and the two are still there:", repr(with_doc.__doc__), with_doc.__qualname__)

print("=== __qualname__ has to be a string ===")
for bad in (5, None, [1]):
    try:
        without_doc.__qualname__ = bad
        print(type(bad).__name__, "-> set")
    except TypeError as e:
        print(type(bad).__name__, "->", e)

print("=== what functools.wraps does, by hand ===")
def target():
    "target doc"
    return 3

def replacement():
    return 4

replacement.__name__ = target.__name__
replacement.__qualname__ = target.__qualname__
replacement.__doc__ = target.__doc__
print("copied   :", replacement.__name__, replacement.__qualname__,
      repr(replacement.__doc__))
print("dict     :", replacement.__dict__)

print("=== a method, and a nested function ===")
class Holder:
    def method(self):
        "method doc"
        return 5

def outer():
    def inner():
        "inner doc"
        return 6
    return inner

print("method   :", Holder.method.__qualname__, repr(Holder.method.__doc__),
      Holder.method.__dict__)
print("nested   :", outer().__qualname__, repr(outer().__doc__))
print("lambda   :", (lambda: 1).__qualname__, repr((lambda: 1).__doc__))

print("=== and the functions still run ===")
print(with_doc(), without_doc(), target(), replacement(), Holder().method(),
      outer()())
