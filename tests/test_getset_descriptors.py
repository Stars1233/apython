# Getset descriptors, and the numeric attributes that are one.
#
# getset_descr_type was a stub: gs_get and gs_set were written by
# getset_descr_new and read by nothing, and its single instance was built with
# both accessors NULL so types.py could name GetSetDescriptorType.  real, imag,
# numerator and denominator were four separate tp_getattr strcmp chains
# instead -- which answer an instance and leave `int.real` an AttributeError,
# because a strcmp chain is not a thing a type's dict can hold, and dir() has
# nothing to find.

print(type(int.real).__name__, type(int.imag).__name__)
print(type(int.numerator).__name__, type(int.denominator).__name__)
print(type(float.real).__name__, type(float.imag).__name__)
# complex is a getset here and a member descriptor in CPython -- both are
# descriptors of the type, which is what this file is about; the kind is
# CPython's own storage choice, so only the fact of one is compared.
print(type(complex.imag).__name__.endswith("descriptor"), type(bool.real).__name__)

# The instance reads, in every shape an int comes in.
print((5).real, (5).imag, (5).numerator, (5).denominator)
print((-7).real, (0).real, (2 ** 70).real, (2 ** 70).imag)
print((2 ** 70).real == 2 ** 70, (2 ** 70).real is (2 ** 70))
print((1.5).real, (1.5).imag, (-0.0).real)
print((1 + 2j).real, (1 + 2j).imag, complex(0, -1).imag)
print(True.real, True.imag, True.numerator, True.denominator, False.real)

# A subclass answers with the plain base type, as CPython does.
class I(int):
    pass
class F(float):
    pass
i, f = I(5), F(1.5)
print(i.real, type(i.real).__name__, i.imag, i.numerator, i.denominator)
print(f.real, type(f.real).__name__, f.imag)

# getattr() takes the same route.
print(getattr(5, "real"), getattr(1.5, "imag"), getattr(i, "numerator"))
print(getattr(int, "real") is int.real)

# They are data descriptors: assignment raises rather than shadowing.
for target in (5, i, 1.5, f):
    try:
        target.real = 9
        print(type(target).__name__, "assigned")
    except AttributeError:
        print(type(target).__name__, "AttributeError")

# And they are in dir(), on the type and on the instance.
for o in (int, float, complex, bool, 5, 1.5, 1 + 2j, True, i, f):
    names = dir(o)
    print(names == sorted(names), "real" in names, "imag" in names)

# __class__ is in dir() too, though it lives in no tp_dict here.
class C:
    pass
for o in (5, 1.5, "a", [], C, C(), int):
    print("__class__" in dir(o), end=" ")
print()

# Nothing else moved: the ordinary methods still resolve and still bind.
print((5).bit_length(), (255).to_bytes(1, "big"), (1.5).is_integer())
print((1 + 2j).conjugate(), (5).conjugate(), i.bit_length())
print(int.bit_length(7), float.is_integer(2.0))

# A descriptor by NAME, not only by slot.  getset_descr_type had no tp_repr
# and no tp_dict, so repr(int.real) was the empty string -- obj_repr fell to
# its no-repr arm -- and hasattr(int.real, '__get__') was False.  That second
# answer is the one that matters: inspect.isdatadescriptor and the enum and
# dataclasses classifiers walk a __dict__ and ask exactly it.
print(repr(int.real), repr(int.imag))
print(repr(int.numerator), repr(int.denominator))
print(repr(float.real), repr(float.imag))
print(str(int.real) == repr(int.real))

# bool inherits int's, and says so.
print(repr(bool.real))

# A getset is a DATA descriptor: all three names, whether or not it has a
# setter.  A method descriptor is a non-data one: __get__ and nothing else.
for label, v in (("int.real", int.real), ("float.imag", float.imag),
                 ("int.numerator", int.numerator),
                 ("int.bit_length", int.bit_length), ("str.upper", str.upper),
                 ("object.__init__", object.__init__)):
    print(label, hasattr(v, "__get__"), hasattr(v, "__set__"),
          hasattr(v, "__delete__"))

# __get__ reached by name answers what the attribute read answers, and
# __get__(None, cls) hands back the descriptor itself.
print(int.real.__get__(5), float.real.__get__(2.5), int.imag.__get__(5))
print(int.numerator.__get__(7), int.denominator.__get__(7))
print(int.real.__get__(None, int) is int.real)

# A read-only getset refuses both writes.
for f in (lambda: int.real.__set__(5, 1), lambda: int.real.__delete__(5)):
    try:
        f()
    except AttributeError as e:
        print("AttributeError", e)
