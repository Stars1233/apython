# A property's __doc__, and what reading a property off the CLASS gives.
#
# Two things had to be true before dis.py could import.  It opens with
#
#     _Instruction.opname.__doc__ = "Human readable name for operation"
#
# so `_Instruction.opname` has to be the PROPERTY -- not the result of calling
# its getter -- and the property's __doc__ has to be writable.  Neither was:
# a property had no doc field at all, and reading one off a class ran the
# getter with the class as self.
#
# The second is the subtler half.  CPython does not run the getter for a
# property found in the class's own MRO, and does run one found on the
# METATYPE -- which is what makes Enum.__members__ work.  Deciding that from
# "is the object a class" gets the metatype case wrong, so type_getattr
# reports which of its two walks answered and the descriptor protocol asks.

print("=== __doc__ is a real field ===")
p = property(lambda s: 1)
print("initial:", repr(p.__doc__))
p.__doc__ = "written afterwards"
print("written:", repr(p.__doc__))
p.__doc__ = None
print("cleared:", repr(p.__doc__))

q = property(lambda s: 1, None, None, "from the fourth argument")
print("positional:", repr(q.__doc__))


def documented(self):
    "the getter's own docstring"
    return 1


print("from fget:", repr(property(documented).__doc__))
print("no docstring:", repr(property(lambda s: 1).__doc__))

print("=== a property read off the class is the property ===")
class C:
    @property
    def x(self):
        "x's docstring"
        return 42

    @property
    def plain(self):
        return "no doc"

print("type:", type(C.x).__name__)
print("doc :", repr(C.x.__doc__))
print("same as the dict's:", C.x is C.__dict__["x"])
print("plain doc:", repr(C.plain.__doc__))
C.x.__doc__ = "rewritten"
print("rewritten:", repr(C.x.__doc__))

print("=== and off an instance it still runs ===")
c = C()
print("instance:", c.x, c.plain)
print("getattr :", getattr(c, "x"), getattr(C, "x") is C.__dict__["x"])
print("hasattr :", hasattr(c, "x"), hasattr(C, "x"))

print("=== a property on the METATYPE still runs ===")
class Meta(type):
    @property
    def members(cls):
        return "from the metatype"

class WithMeta(metaclass=Meta):
    pass

print("metatype property:", WithMeta.members)

print("=== setters and deleters are unchanged ===")
class D:
    def __init__(self): self._v = 0
    @property
    def v(self): return self._v
    @v.setter
    def v(self, n): self._v = n * 2
    @v.deleter
    def v(self): self._v = "deleted"

d = D()
d.v = 5
print("setter:", d.v)
del d.v
print("deleter:", d._v)
print("fget/fset/fdel:", D.v.fget is not None, D.v.fset is not None,
      D.v.fdel is not None)

print("=== only __doc__ is writable ===")
for name in ("fget", "fset", "fdel"):
    try:
        setattr(D.v, name, None)
        print(name, "-> set")
    except AttributeError:
        print(name, "-> AttributeError")
