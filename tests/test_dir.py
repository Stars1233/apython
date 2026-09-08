# dir(): what the object says it holds, sorted.
#
# dir() used to walk the MRO's tp_dicts and ask the object nothing.  A module
# keeps its names in its own dict and has no tp_dict, so dir(a_module) came
# back with object's dunders; an instance's own attributes were invisible; a
# class defining __dir__ was ignored; and the answer arrived in MRO order,
# which no CPython output matches.  object.__dir__ made the circle complete by
# calling dir() straight back.

import errno

# A module: its own contents, and nothing borrowed from object.
names = dir(errno)
print(names == sorted(names))
print("ENOENT" in names, "EEXIST" in names, "errorcode" in names)
print("__init__" in names, "__str__" in names, "__format__" in names)

# An instance: class attributes, inherited ones, and its own __dict__.
class Base:
    b = 1
    def bm(self): pass

class C(Base):
    c = 2
    def cm(self): pass

o = C()
o.own = 3
d = dir(o)
print(d == sorted(d))
print("b" in d, "bm" in d, "c" in d, "cm" in d, "own" in d)
print("__init__" in d, "__class__" in d)
print(d.count("b"), d.count("__init__"))     # each name once, not once per base
print(d.count("__class__"))                  # once, however it got there

# A class lists its MRO but not any instance's attributes.
dc = dir(C)
print(dc == sorted(dc), "b" in dc, "c" in dc, "own" in dc)

# __dir__ is consulted, and whatever it answers is sorted.
class WithDir:
    def __dir__(self):
        return ["zeta", "alpha", "mu"]
print(dir(WithDir()))

# Any iterable will do, not just a list.
class WithSet:
    def __dir__(self):
        return {"b", "a", "c"}
print(dir(WithSet()))

class WithTuple:
    def __dir__(self):
        return ("y", "x")
print(dir(WithTuple()))

# A __dir__ that raises propagates, rather than being discarded along with its
# answer.
class Angry:
    def __dir__(self):
        raise ValueError("no dir for you")
try:
    dir(Angry())
    print("no error")
except ValueError as e:
    print("ValueError:", e)

# Builtin types and their instances.
print("upper" in dir("ab"), "append" in dir([]), "items" in dir({}))
print("bit_length" in dir(5), "bit_length" in dir(int))
print(dir([]) == sorted(dir([])))

# dir() of an object whose type defines __slots__ still lists the slots.
class Slotted:
    __slots__ = ("s1", "s2")
ds = dir(Slotted())
print("s1" in ds, "s2" in ds, ds == sorted(ds))


# A builtin attribute answered only by tp_getattr is invisible to dir(), and to
# everything that reads a type's dict rather than calling getattr: inspect,
# help(), and any `'name' in vars(T)` test.  These all read correctly through an
# instance and were absent from the class.
def missing_from(T, names):
    d = dir(T)
    return [n for n in names if n not in d]


print(missing_from(memoryview, ["nbytes", "format", "itemsize", "shape",
                                "strides", "ndim", "obj", "readonly",
                                "suboffsets", "c_contiguous", "f_contiguous",
                                "contiguous"]))
print(missing_from(property, ["fget", "fset", "fdel",
                              "getter", "setter", "deleter"]))
print(missing_from(classmethod, ["__func__", "__wrapped__"]))
print(missing_from(staticmethod, ["__func__", "__wrapped__"]))

# The descriptors work through the class, not only through an instance, and
# repr as CPython's do.
mv = memoryview(b"abcd")
print(memoryview.nbytes.__get__(mv), mv.nbytes)
print(memoryview.readonly.__get__(mv), memoryview.itemsize.__get__(mv))
print(memoryview.shape.__get__(mv), memoryview.ndim.__get__(mv))
print(repr(memoryview.nbytes))

p = property(lambda s: 1)
print(property.fget.__get__(p) is p.fget, property.fset.__get__(p))
print(callable(property.getter.__get__(p)))

c = classmethod(lambda cls: 1)
print(classmethod.__func__.__get__(c) is c.__func__)
s = staticmethod(lambda: 1)
print(staticmethod.__func__.__get__(s) is s.__func__)

# They are descriptors, which is the question the stdlib actually asks:
# inspect.isdatadescriptor and the enum and dataclasses classifiers all walk a
# __dict__ and test hasattr(v, '__get__').
print(hasattr(vars(memoryview)["nbytes"], "__get__"))
print(hasattr(vars(property)["fget"], "__get__"))

# A released view answers AttributeError through the descriptor rather than
# handing back the NULL its tp_getattr uses to mean "not mine".
released = memoryview(bytearray(b"xy"))
released.release()
try:
    memoryview.nbytes.__get__(released)
    print("released view answered")
except (AttributeError, ValueError) as e:
    print("released view:", type(e).__name__)
