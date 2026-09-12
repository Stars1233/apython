# super()'s second argument may be an immediate.
#
# An int inside +-2^50 and every float are NaN-boxed Values, not pointers, so
# super_construct's "is this a pointer" guard refused them outright:
# `super(int, 1)` and `super(float, 1.5)` were
# "obj must be an instance or subtype of type", while `super(int, 10**30)`
# worked because a big int is boxed onto the heap.
#
# The object is a Value from here on, which is the rest of the fix: it is
# INCREF'd, DECREF'd, traversed and handed back as `__self__` as a Value
# rather than as a pointer.


def show(cls, obj):
    try:
        s = super(cls, obj)
    except TypeError as e:
        print("TypeError", cls.__name__, type(obj).__name__, e)
        return
    print(repr(s), "|", type(s.__self__).__name__, s.__self_class__.__name__,
          s.__thisclass__.__name__)


# The two that were refused, and their boxed and heap counterparts beside them.
show(int, 1)
show(int, -7)
show(int, 10 ** 30)
show(int, -(10 ** 30))
show(float, 1.5)
show(float, -0.0)
show(float, float("inf"))
show(bool, True)
show(str, "a")
show(list, [1])
show(dict, {})
show(tuple, ())
show(bytes, b"a")


# The refusals stay refusals, and keep their wording.
show(int, "a")
show(str, 1)
show(list, 1)
show(float, 1)
show(int, 1.5)


# super(B, None) is the unbound form, so __self__ is None and there is no
# __self_class__ to report.
u = super(int, None)
print(repr(u), u.__self__, u.__self_class__)
v = super(int)
print(repr(v), v.__self__, v.__self_class__)


# An attribute reached through a bound super over an immediate: the MRO walk
# starts past __thisclass__, and the descriptor binds to the immediate.
class MyInt(int):
    def bit_length(self):
        return "mine"

    def parent_bit_length(self):
        return super().bit_length()


m = MyInt(255)
print(m.bit_length(), m.parent_bit_length())
print(super(MyInt, m).bit_length())
print(super(MyInt, MyInt).bit_length is int.bit_length)


class MyFloat(float):
    def as_integer_ratio(self):
        return "mine"

    def parent_ratio(self):
        return super().as_integer_ratio()


f = MyFloat(0.5)
print(f.as_integer_ratio(), f.parent_ratio())


# A classmethod reached through super over an immediate subclass instance.
class Base(int):
    @classmethod
    def make(cls):
        return "Base.make " + cls.__name__


class Derived(Base):
    @classmethod
    def make(cls):
        return "Derived.make -> " + super().make()


print(Derived(3).make())
print(super(Derived, Derived).make())


# The super object survives a collection with an immediate inside it: the
# collector has to see the field as a Value, not visit it as a pointer.
import gc

keep = [super(int, i) for i in range(5)] + [super(float, i * 0.5) for i in range(5)]
gc.collect()
print(len(keep), [type(s.__self__).__name__ for s in keep[:2] + keep[-2:]])
del keep
gc.collect()
print("collected")

# And a reference cycle through one, which is what makes the traverse matter.
class Holder(int):
    pass


h = Holder(9)
h_super = super(Holder, h)
cyc = [h_super]
cyc.append(cyc)
del h_super, h, cyc
print(gc.collect() >= 0)


# `super(C, o).attr` written out in full is LOAD_SUPER_ATTR, which has the
# three operands and no super object.  It searched the MRO for the four names
# super answers for itself and came back empty.
print(super(list, [1]).__self__)
print(super(list, [1]).__self_class__)
print(super(list, [1]).__thisclass__)
print(super(list, [1]).__class__)
print(super(int, 1).__self__, super(int, 1).__thisclass__)
print(super(float, 0.5).__self__, super(float, 0.5).__self_class__)
print(super(MyInt, m).__self__, super(MyInt, m).__self_class__)
print(super(int, None).__self__, super(int, None).__self_class__)

# And when nothing after __thisclass__ defines the name, the MRO walk running
# out used to fall through to "argument 1 must be a type".
for get in (lambda: super(list, [1]).__len__,
            lambda: super(list, [1]).__len__(),
            lambda: super(list, [1]).nosuchname,
            lambda: super(list, [1]).nosuchname()):
    try:
        print("got", get())
    except AttributeError as e:
        print("AttributeError", e)
    except TypeError as e:
        print("TypeError", e)
