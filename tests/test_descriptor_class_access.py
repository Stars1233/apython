# Reading a descriptor off the CLASS passes None as the instance.
#
# CPython calls type.__getattribute__ -> descr.__get__(None, cls) when the
# attribute is looked up on a class, and descr.__get__(obj, type(obj)) only
# when it is looked up on an instance.  This passed the class itself as the
# instance and the METAclass as the owner, so every descriptor that
# distinguishes the two answered the instance case for a class access.
#
# enum is the load-bearing example: enum.property.__get__ returns the member
# when instance is None and looks the name up in the instance's value
# otherwise, so `SomeEnum.MEMBER` reached a branch that wants a member map
# the class does not have -- which is what stopped `import ast` and `import
# uuid` from working.


class Descr:
    def __get__(self, instance, owner=None):
        return ("get", instance, owner)


class Plain:
    d = Descr()


class Meta(type):
    pass


class WithMeta(metaclass=Meta):
    d = Descr()


class Sub(Plain):
    pass


def shape(x):
    kind, instance, owner = x
    return (kind,
            "None" if instance is None else type(instance).__name__,
            owner.__name__)


print(shape(Plain.d))
print(shape(Plain().d))
print(shape(WithMeta.d))
print(shape(Sub.d))
print(shape(Sub().d))


# A descriptor on the METAclass is an ordinary instance access when read off
# the class: the class is the instance there.
class OnMeta(type):
    d = Descr()


class Instance(metaclass=OnMeta):
    pass


print(shape(Instance.d))


# The same rule for a data descriptor with __set__.
class Data:
    def __get__(self, instance, owner=None):
        return ("data", instance, owner)

    def __set__(self, instance, value):
        pass


class HasData:
    d = Data()


print(shape(HasData.d))
print(shape(HasData().d))


# property, classmethod and staticmethod already followed the rule; they are
# here so a change to the general path cannot break them.
class Mixed:
    @property
    def p(self):
        return "prop"

    @classmethod
    def c(cls):
        return "cls:" + cls.__name__

    @staticmethod
    def s():
        return "static"


print(type(Mixed.p).__name__, Mixed().p)
print(Mixed.c(), Mixed().c())
print(Mixed.s(), Mixed().s())


# And enum, which is what found this.
import enum


class Colour(enum.IntEnum):
    RED = 1
    GREEN = 2


print(Colour.RED, Colour.RED.value, list(Colour))


@enum._simple_enum(enum.IntEnum)
class Simple:
    A = enum.auto()
    B = enum.auto()


print(list(Simple), Simple.A.value, Simple.B.name)
