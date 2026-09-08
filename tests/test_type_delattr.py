"""Deleting a class attribute.

`del C.x` and `delattr(C, "x")` reach type_setattr with a NULL value, and it
called dict_set either way -- storing the NULL over the key instead of removing
the entry.  Lookup answered correctly, because a NULL value reads as absent, so
`"x" in C.__dict__` was False and `C.__dict__["x"]` raised KeyError.  But the
entry was still occupied, so `keys()`, `values()`, `items()`, `len()` and
iteration all went on reporting it -- and `items()` handed out the NULL Value
itself.

That is not a cosmetic disagreement.  enum.py deletes five names from Enum;
doctest walks `Enum.__dict__.items()`; inspect then called `type()` and
`isinstance()` on the hole.  A NULL reaching an ordinary builtin is a wild
pointer, and one of them decremented a refcount inside a live code object's
bytecode -- which turned a RETURN_VALUE into an opcode CPython 3.12 does not
assign.

`delattr` of a name that was never there also has to raise, and the message
names the class rather than its metatype.
"""


class C:
    a = 1
    b = 2
    c = 3

    def m(self):
        return "m"

    @staticmethod
    def s():
        return "s"

    @classmethod
    def k(cls):
        return "k"

    p = property(lambda self: "p")


def visible(cls):
    return sorted(k for k in cls.__dict__ if not k.startswith("__"))


print("before:", visible(C))
del C.b
print("after del C.b:", visible(C))
print("  'b' in dict:", "b" in C.__dict__)
print("  keys/values/items agree:",
      len(list(C.__dict__.keys())) == len(list(C.__dict__.values()))
      == len(list(C.__dict__.items())) == len(C.__dict__))

# Every view has to agree that it is gone, and items() must not hand out a
# hole: repr of the view is what shows a NULL value if one is there.
print("  values reprs:", sorted(repr(v)[:9] for k, v in C.__dict__.items()
                                if not k.startswith("__")))
print("  iter:", sorted(k for k in iter(C.__dict__) if not k.startswith("__")))

# and reading the attribute through the class is an AttributeError
try:
    C.b
except AttributeError as e:
    print("  read after delete:", e)


print("--- deleting each kind of member ---")
for name in ("m", "s", "k", "p"):
    delattr(C, name)
    print(name, "gone:", name not in C.__dict__, visible(C))


print("--- deleting what was never there ---")
for name in ("nosuch", "a"):
    try:
        delattr(C, name)
        print("delattr", name, "-> ok")
    except AttributeError as e:
        print("delattr", name, "->", e)

try:
    del C.nosuch_either
except AttributeError as e:
    print("del stmt ->", e)

# Deleting the same name twice: the second must raise.
class D:
    x = 1

del D.x
try:
    del D.x
    print("second delete succeeded - wrong")
except AttributeError as e:
    print("second delete:", e)


print("--- the message names the class, not the metatype ---")
class Named:
    pass

for f in (lambda: Named.zzz,
          lambda: delattr(Named, "zzz"),
          lambda: int.zzz,
          lambda: type.zzz):
    try:
        f()
    except AttributeError as e:
        print(" ", e)

# An instance and a module keep their own wordings.
class Inst:
    pass

try:
    Inst().zzz
except AttributeError as e:
    print(" ", e)

import sys
try:
    sys.zzz
except AttributeError as e:
    print(" ", e)


print("--- inheritance is not disturbed ---")
class Base:
    v = "base"

class Sub(Base):
    v = "sub"

print("Sub.v:", Sub.v)
del Sub.v
print("after del Sub.v:", Sub.v, "  in Sub.__dict__:", "v" in Sub.__dict__)
try:
    del Sub.v
except AttributeError as e:
    print("deleting the inherited one:", e)
print("Base still has it:", Base.v)


print("--- a deleted dunder stops answering ---")
class Eq:
    def __eq__(self, other):
        return True

e1, e2 = Eq(), Eq()
print("with __eq__:", e1 == e2)
print("__eq__ in dict:", "__eq__" in Eq.__dict__)


print("--- redefining after a delete ---")
class R:
    z = 1

del R.z
R.z = 2
print("z:", R.z, " in dict:", "z" in R.__dict__, " visible:", visible(R))
del R.z
print("gone again:", "z" in R.__dict__, visible(R))


print("--- a metaclass-built class behaves the same ---")
class Meta(type):
    pass

class M(metaclass=Meta):
    q = 1

del M.q
print("M:", "q" in M.__dict__, visible(M))
try:
    del M.q
except AttributeError as e:
    print("M again:", e)


print("--- a static type still refuses ---")
try:
    del int.__add__
    print("deleted from int - wrong")
except TypeError as e:
    print("static type:", e)

print("done")
