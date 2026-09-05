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


# A class is in a cycle with its own MRO tuple -- the tuple's first element
# is the class -- so the collector is the only thing that ever frees one, and
# a metatype has to carry a type's traverse and clear for it to be able to.
before = len(object.__subclasses__())
for i in range(20):
    made = Meta("Gone%d" % i, (Held,), {"x": Desc()})
    del made
gc.collect()
print(len(Held.__subclasses__()))
print(len(object.__subclasses__()) == before)

seen.clear()
for i in range(5):
    try:
        Meta("Bad%d" % i, (Held,), {"b": Boom()})
    except RuntimeError:
        pass
gc.collect()
print(len(Held.__subclasses__()), seen == [])
print("survived")


# The metatype is a REGISTRATION -- a global that type_from_parts leaves for
# the class it is building -- and it has to be put down before anything that
# might build a class of its own picks it up.  It was stamped AFTER
# __init_subclass__ ran, so a class defined inside one came out with the outer
# class's metaclass instead of `type`.
print("=== a class built while another is being built ===")


class Meta(type):
    pass


made = []


class Base(metaclass=Meta):
    def __init_subclass__(cls, **kw):
        class Inner:
            pass

        made.append(("init_subclass", type(Inner).__name__))
        super().__init_subclass__(**kw)


class Child(Base):
    pass


print(type(Child).__name__, type(Base).__name__, made)


class Named:
    def __set_name__(self, owner, name):
        class Inner2:
            pass

        made.append(("set_name", type(Inner2).__name__, type(owner).__name__))


class WithDescr(metaclass=Meta):
    d = Named()


print(type(WithDescr).__name__, made[-1])


class Meta2(type):
    def __init__(cls, *a, **kw):
        class Inner3:
            pass

        made.append(("meta init", type(Inner3).__name__))
        super().__init__(*a, **kw)


class UsesMeta2(metaclass=Meta2):
    pass


print(type(UsesMeta2).__name__, made[-1])


# ...and nesting one metaclass inside another still gives each its own.
class MetaA(type):
    def __init__(cls, *a, **kw):
        class Deep(metaclass=Meta):
            pass

        made.append(("deep", type(Deep).__name__))
        super().__init__(*a, **kw)


class UsesMetaA(metaclass=MetaA):
    pass


print(type(UsesMetaA).__name__, made[-1])
print("metatypes done")
