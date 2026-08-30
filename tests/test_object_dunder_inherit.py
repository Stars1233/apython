# Attribute lookup on a builtin instance walks the MRO.
#
# It stopped at the exact type's tp_dict, so anything object supplies -- and
# only object supplies it -- was invisible from an instance: `None.__new__`
# raised AttributeError while `object.__new__` was fine, because the second is
# a lookup on a TYPE, and only that path walked.  enum's _find_new_ reaches for
# `None.__new__` by name.
print(hasattr(None, "__new__"), hasattr(None, "__init__"), hasattr(None, "__eq__"))
print(hasattr(1, "__new__"), hasattr("s", "__new__"), hasattr([], "__new__"))
print(hasattr((), "__new__"), hasattr({}, "__new__"), hasattr(1.5, "__new__"))


class C:
    pass


c = C()
print(hasattr(c, "__new__"), hasattr(c, "__init__"), hasattr(c, "__repr__"))
print(type(C.__new__(C)).__name__, type(object.__new__(C)).__name__)


class L(list):
    pass


print(hasattr(L(), "__new__"), type(L.__new__(L)).__name__)

# The exact type still wins over the base.
class Over:
    def __repr__(self):
        return "over"


print(repr(Over()), Over().__repr__())

# A builtin's own dunder still wins, and object's still reaches through.
print(().__eq__(()), (1).__eq__(1), None.__eq__(None))
print(type(None).__new__ is type(None).__new__)

# getattr agrees with attribute syntax here too.
print(getattr(None, "__new__") is not None, getattr(c, "__init__") is not None)
