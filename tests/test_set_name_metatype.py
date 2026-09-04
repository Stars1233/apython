"""What a descriptor's __set_name__ is handed: a class that is already its
metaclass's instance.

`type.__new__(mcls, name, bases, ns)` builds the class and then runs every
descriptor's `__set_name__`, and the metatype has to be on the class before
that happens -- a __set_name__ is entitled to read whatever the metaclass
supplies.  enum.py is written exactly that way: EnumType.__members__ is a
property on the metaclass, `_proto_member.__set_name__` calls
`enum_class._new_member_`, and an Enum whose __new__ reads `cls.__members__`
(inspect's _ParameterKind is one) got an AttributeError naming 'type'.
"""

seen = []


class Desc:
    def __set_name__(self, owner, name):
        seen.append((name, type(owner).__name__, owner.__name__,
                     getattr(owner, "tag", None), hasattr(owner, "extra")))


class Meta(type):
    @property
    def tag(cls):
        return "from-" + cls.__name__

    extra = 1


class C(metaclass=Meta):
    a = Desc()
    b = Desc()


print(seen)
print(type(C).__name__, C.tag)


print("--- through a metaclass that overrides __new__ ---")
seen.clear()


class Meta2(type):
    @property
    def tag(cls):
        return "two"

    def __new__(mcls, name, bases, ns, **kw):
        return super().__new__(mcls, name, bases, ns, **kw)


class D(metaclass=Meta2):
    d = Desc()


print(seen)
print(type(D).__name__, D.tag)


print("--- and through type.__new__ called directly ---")
seen.clear()
E = type.__new__(Meta2, "E", (), {"e": Desc()})
print(seen)
print(type(E).__name__, E.tag)


print("--- a subclass inherits the metatype, and sees it too ---")
seen.clear()


class F(D):
    f = Desc()


print(seen)
print(type(F).__name__, F.tag)


print("--- the enum shape itself ---")
import enum


class Kind(enum.IntEnum):
    FIRST = "first"
    SECOND = "second"
    THIRD = "third"

    def __new__(cls, description):
        value = len(cls.__members__)
        obj = int.__new__(cls, value)
        obj._value_ = value
        obj.description = description
        return obj


print([(k.name, int(k), k.description) for k in Kind])
print(sorted(Kind.__members__))
print(Kind.FIRST < Kind.THIRD, Kind(0).name)


print("--- no metaclass: the default is still what a __set_name__ sees ---")
seen.clear()


class G:
    g = Desc()


print(seen)
print(type(G).__name__)


print("--- a __set_name__ that raises still aborts the class ---")
class Boom:
    def __set_name__(self, owner, name):
        raise RuntimeError("no: " + type(owner).__name__)


try:
    class H(metaclass=Meta):
        h = Boom()
except RuntimeError as e:
    print("RuntimeError", e)


print("--- a class built by a metaclass is released as a class ---")
#
# The dealloc that runs is the object's TYPE's, and that type is the
# metaclass, so a metatype has to carry a type's lifecycle slots or the block
# goes back to the allocator with the GC's list and each base's subclass list
# still pointing into it.
import gc


class Held(metaclass=Meta):
    pass


# A class is in a cycle with its own MRO tuple, so it is the collector that
# has to free it -- and for a metaclass-made class the collector cannot yet,
# which bugs.md records.  What is tested here is that whatever IS freed is
# freed correctly: the run survives, every surviving subclass is intact, and
# valgrind is quiet.  How many are left is the leak, not the bug this covers.
made_names = []
for i in range(20):
    made = Meta("Gone%d" % i, (Held,), {"x": Desc()})
    made_names.append(made.__name__)
    del made
gc.collect()
print(made_names[0], made_names[-1])
print(all(c.__name__.startswith("Gone") for c in Held.__subclasses__()))
print(all(type(c) is Meta and c.tag.startswith("from-")
          for c in Held.__subclasses__()))

seen.clear()
for i in range(5):
    try:
        Meta("Bad%d" % i, (Held,), {"b": Boom()})
    except RuntimeError:
        pass
gc.collect()
print(seen == [], all(not c.__name__.startswith("Bad")
                      for c in Held.__subclasses__()))
print("survived")
