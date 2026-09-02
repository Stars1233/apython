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
print("__init__" in d)
print(d.count("b"), d.count("__init__"))     # each name once, not once per base
# CPython also lists __class__ here, from a getset descriptor in object's
# dict.  Ours answers __class__ from a tp_getattr chain with nothing in any
# tp_dict to find, so dir() cannot see it -- the getset_descr gap bugs.md
# records, seen from this end.

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
