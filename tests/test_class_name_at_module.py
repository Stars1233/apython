# `__class__` is only special inside a class body.

# sym_enclosing_binds walks outward from a nested block looking for a
# function-like block that binds the name, and skips class blocks -- except
# for `__class__` and `__classdict__`, the two names a class body does hand
# down.  It applied that exception to every block that is not function-like,
# and the MODULE is one of those.  So a module-level `__class__` read from any
# nested scope resolved as a free variable of the module, and .bound_here then
# made the module bind it in a CELL.  A module frame has no cells: the
# LOAD_DEREF read `[NULL + ob_ref]` and the process died on address 0x10.
#
# CPython compiles all of these to LOAD_GLOBAL / LOAD_NAME.  Nothing about the
# spelling of the name matters outside a class.

__class__ = 2
__classdict__ = 3

print((lambda: __class__)())
print((lambda: __classdict__)())
print([__class__ for _ in [1]])
print([__classdict__ for _ in [1]])
print({k: __class__ for k in "a"})
print(list(__class__ for _ in [1, 2]))


def read():
    return __class__


def read_nested():
    def inner():
        return __class__
    return inner()


print(read(), read_nested())


def writes_global():
    global __class__
    __class__ = 7
    return (lambda: __class__)()


print(writes_global(), __class__)
__class__ = 2

# A function-level `__class__` is an ordinary local, and an ordinary cell when
# a nested block reads it.
def local_class():
    __class__ = "local"
    return (lambda: __class__)()


print(local_class())

# Inside a class body it keeps its meaning: the implicit cell zero-argument
# super() reads, which shadows the module-level name.
class C:
    def who(self):
        return __class__

    def sup(self):
        return super().__init__ is object.__init__


class D(C):
    pass


print(C().who(), D().who(), C.__mro__[0] is C)
print(D().sup())


# ...and a method of a class nested in a function still closes over the
# function's variables, with the class body carrying them through.
def outer():
    marker = "carried"

    class M:
        def get(self):
            return marker, __class__

    return M().get()


got = outer()
print(got[0], got[1].__name__)


# A class body may also read the module-level name explicitly before binding
# anything: at class scope __class__ is not yet in the namespace, so the
# lookup falls through to the module.
class E:
    seen = __class__


print(E.seen)
print("done")
