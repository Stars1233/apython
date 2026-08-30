# Dict and attribute lookups must distinguish "found the value 0" from
# "not found".  A hit is signalled by the tag, not by the payload, and the
# payload of the integer 0 is 0 — testing the payload loses the difference.

d = {"k": 0, "f": 0.0, "e": "", "n": None, "b": False}

# dict.get
print(d.get("k"), d.get("f"), repr(d.get("e")), d.get("n"), d.get("b"))
print(d.get("missing"), d.get("missing", "dflt"))

# dict.setdefault must return the existing 0, not overwrite it
print(d.setdefault("k", 99), d["k"])
print(d.setdefault("new", 0), d["new"])

# Subscript and membership agree with get()
print(d["k"], "k" in d, "missing" in d)

# pop with a zero value
e = {"z": 0}
print(e.pop("z"), len(e))

# Class attributes equal to zero, direct and inherited
class Base:
    zero = 0
    empty = ""
    false = False
    none = None

class Derived(Base):
    pass

b = Base()
print(b.zero, repr(b.empty), b.false, b.none)

dv = Derived()
print(dv.zero, repr(dv.empty), dv.false, dv.none)
print(Derived.zero, getattr(dv, "zero"), getattr(dv, "nope", "dflt"))

# Two levels of inheritance
class Deeper(Derived):
    pass

print(Deeper().zero)

# An instance attribute of 0 shadowing a non-zero class attribute
class Shadow:
    v = 7

s = Shadow()
s.v = 0
print(s.v, Shadow.v)

# hasattr must agree
print(hasattr(dv, "zero"), hasattr(dv, "nope"))
