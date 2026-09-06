# Two things about the moment a class is created or written to.
#
# 1. A static type is immutable.  `str.foo = 1` used to succeed, putting a key
#    in str's own dict for every str in the process.
# 2. A class is registered against its bases BEFORE __init_subclass__ runs, so
#    that hook sees Base.__subclasses__() the way CPython's does.  It is also
#    the order any later cache keyed on a type needs: a class that specialized
#    a call site before it was registered would sit outside the walk that
#    invalidates such a cache.

# --- static types refuse writes --------------------------------------------
for expr in ("str.foo = 1", "del str.join", "int.x = 1", "list.y = 2",
             "type.z = 3", "object.w = 4", "str.__name__ = 'q'"):
    try:
        exec(expr)
        print(expr, "-> no error")
    except TypeError as e:
        print(expr, "->", e)

# --- heap types stay writable ----------------------------------------------
class H:
    pass

H.a = 1
print(H.a)
del H.a
print(hasattr(H, 'a'))
H.__name__ = 'Renamed'
print(H.__name__)

# A dunder assigned after the class exists still takes effect.
class Op:
    pass

Op.__len__ = lambda self: 7
print(len(Op()))

# ...including through an existing subclass.
class OpBase:
    pass

class OpDerived(OpBase):
    pass

OpBase.__iter__ = lambda self: iter([1, 2, 3])
print(list(OpDerived()))

OpBase.__getattribute__ = lambda self, n: "hooked"
od = OpDerived()
print(od.anything)

# --- __subclasses__ during __init_subclass__ -------------------------------
seen = []


class Reg:
    def __init_subclass__(cls, **kw):
        seen.append(sorted(c.__name__ for c in Reg.__subclasses__()))


class RegA(Reg):
    pass


class RegB(Reg):
    pass


print(seen)
print(sorted(c.__name__ for c in Reg.__subclasses__()))

# The class the hook is told about is the one being built.
told = []


class Reg2:
    def __init_subclass__(cls, **kw):
        told.append(cls.__name__)


class Reg2A(Reg2):
    pass


print(told)
