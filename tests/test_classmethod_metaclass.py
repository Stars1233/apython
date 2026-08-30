# A classmethod reached through a class binds that class.  "Is this object a
# class?" was asked as `ob_type is user_type_metatype`, which is false for a
# class built by a metaclass of its own -- so `A.who()` inside a metaclass
# hierarchy bound the *metaclass*.  enum's Flag._missing_ is written that way,
# and came out with cls = EnumType.
class Meta(type):
    pass


class A(metaclass=Meta):
    @classmethod
    def who(cls):
        return cls.__name__

    @classmethod
    def make(cls, v):
        o = cls()
        o.v = v
        return o


class B(A):
    pass


print(A.who(), B.who(), A().who(), B().who())
print(type(A.make(1)).__name__, type(B.make(2)).__name__, B.make(3).v)

# Without a metaclass, and on a builtin.
class P:
    @classmethod
    def who(cls):
        return cls.__name__


class Q(P):
    pass


print(P.who(), Q.who(), Q().who())
print(int.from_bytes(b'\x01\x02', 'big'))


# A deeper metaclass chain, and a metaclass that itself has classmethods.
class Meta2(Meta):
    @classmethod
    def meta_who(mcls):
        return mcls.__name__


class C(metaclass=Meta2):
    @classmethod
    def who(cls):
        return cls.__name__


print(C.who(), Meta2.meta_who(), type(C).__name__)


# staticmethod through the same path stays unbound.
class S(metaclass=Meta):
    @staticmethod
    def plain(a):
        return ("plain", a)


print(S.plain(1), S().plain(2))


# super() from a classmethod resolves against the bound class.
class D(A):
    @classmethod
    def who(cls):
        return "D:" + super().who()


print(D.who())
