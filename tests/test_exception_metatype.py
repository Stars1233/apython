# An exception class is a class.  Builtin exception types carry a metatype of
# their own -- exc_metatype, which exists so they can have a tp_call that
# ordinary classes do not -- and that metatype was declared with tp_base 0.
# type_is_subtype walks tp_base for a static type, so the walk from
# exc_metatype stopped at once and `isinstance(ValueError, type)` was False.
#
# It is hard to overstate how ordinary the question is.  CPython's own
# warnings.py opens with
#
#     assert isinstance(category, type), "category must be a class"
#
# so `import warnings` failed outright, and with it every stdlib module that
# imports warnings -- pathlib, tempfile, shutil, random, argparse, tarfile,
# configparser, hmac and the sre_* trio among them.

print("=== an exception type is an instance of type ===")
for t in (ValueError, Exception, BaseException, KeyError, OSError,
          StopIteration, ZeroDivisionError):
    print(t.__name__, isinstance(t, type))

print("=== so are the other kinds of class, as before ===")
class Plain: pass
class Derived(ValueError): pass
class Meta(type): pass
class WithMeta(metaclass=Meta): pass
for t in (int, str, type, object, Plain, Derived, WithMeta, Meta):
    print(t.__name__, isinstance(t, type))

print("=== the metatype is a subclass of type ===")
# Not its name: an exception type's metatype is `type` in CPython and a
# private one here, and that difference is deliberate -- it is what carries
# the tp_call.  What has to agree is where it sits, which is under `type`.
em = type(ValueError)
print("issubclass:", issubclass(em, type))
print("type in mro:", type in em.__mro__)
print("mro ends at object:", em.__mro__[-1] is object)

print("=== and the answers that hang off it ===")
print("isinstance(ValueError, (int, type)):", isinstance(ValueError, (int, type)))
print("callable:", callable(ValueError))
print("still an object:", isinstance(ValueError, object))
print("an instance is not a type:", isinstance(ValueError("x"), type))
print("issubclass(ValueError, Exception):", issubclass(ValueError, Exception))
print("raising still works:", end=" ")
try:
    raise ValueError("boom")
except ValueError as e:
    print(type(e).__name__, e)

print("=== the assert that CPython's warnings.py opens with ===")
def filterwarnings(category):
    assert isinstance(category, type), "category must be a class"
    return category.__name__
for c in (DeprecationWarning, UserWarning, Warning, ResourceWarning):
    print(filterwarnings(c))
