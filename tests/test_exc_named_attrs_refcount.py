# ImportError and AttributeError are the only builtin exceptions that take
# keyword arguments, and the two values they keep were each stored with ONE
# REFERENCE TOO MANY.
#
# exc_store_named INCREF'd the value and then handed it to exc_setattr, whose
# generic path is dict_set -- which INCREFs for itself.  So every
# `ImportError(msg, name=n, path=p)` leaked both n and p, and every
# `AttributeError(msg, name=n, obj=o)` leaked both.
#
# It was invisible for two reasons, and both are worth keeping in mind:
#
#   * the values in almost every call are string CONSTANTS out of co_consts,
#     which live as long as the code object, so an extra reference on one
#     changes nothing observable and frees nothing late;
#   * `obj=None` and `path=None` are singletons, so the default call leaks a
#     reference on an object that never dies anyway.
#
# A freshly built value shows it immediately, which is what this measures:
# the refcount of a string handed to the constructor must come back to where
# it started once the exception is gone.
#
# The delta is what is compared, not the absolute count, so this is portable
# between interpreters.

import gc
import sys


def balance(label, build, *values):
    before = [sys.getrefcount(v) for v in values]
    e = build()
    during = [sys.getrefcount(v) for v in values]
    del e
    gc.collect()
    after = [sys.getrefcount(v) for v in values]
    print("%-42s held=%s  leaked=%s" % (
        label,
        [d - b for d, b in zip(during, before)],
        [a - b for a, b in zip(after, before)]))


# Build the values at run time so they are not constants.
name = "".join(["nm", "_uniq_", "1"])
path = "".join(["pt", "_uniq_", "1"])
obj = ["a list, so it is not a singleton"]

balance("ImportError(name=, path=)",
        lambda: ImportError("m", name=name, path=path), name, path)
balance("ImportError(name=) only",
        lambda: ImportError("m", name=name), name)
balance("ImportError(path=) only",
        lambda: ImportError("m", path=path), path)
balance("ModuleNotFoundError(name=, path=)",
        lambda: ModuleNotFoundError("m", name=name, path=path), name, path)
balance("AttributeError(name=, obj=)",
        lambda: AttributeError("m", name=name, obj=obj), name, obj)
balance("AttributeError(obj=) only",
        lambda: AttributeError("m", obj=obj), obj)

# A subclass takes the same keywords and must balance the same way.
class MyImportError(ImportError):
    pass


class MyAttributeError(AttributeError):
    pass


balance("subclass of ImportError",
        lambda: MyImportError("m", name=name, path=path), name, path)
balance("subclass of AttributeError",
        lambda: MyAttributeError("m", name=name, obj=obj), name, obj)

# Raising and catching must balance too -- that is the path the import system
# itself takes.
def raised():
    try:
        raise ImportError("m", name=name, path=path)
    except ImportError as e:
        return None


balance("raised and caught", raised, name, path)

# Reassigning the attribute afterwards must not leak the old value either.
def reassigned():
    e = ImportError("m", name=name, path=path)
    e.name = "something else"
    e.path = "elsewhere"
    return e


balance("reassigned after construction", reassigned, name, path)

# And the values must still READ BACK correctly, which is the whole point of
# storing them.
e = ImportError("m", name=name, path=path)
print("name reads back:", e.name == name, "path reads back:", e.path == path)
a = AttributeError("m", name=name, obj=obj)
print("attr name:", a.name == name, "attr obj:", a.obj is obj)
print("defaults:", ImportError("m").name, ImportError("m").path,
      AttributeError("m").name, AttributeError("m").obj)
