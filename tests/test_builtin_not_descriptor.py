# A plain builtin function stored in a class body is not bound.
#
# `class C: f = len` gives C().f a bound method here and the bare function in
# CPython, so C().f([1,2,3]) was "len() takes exactly one argument (2 given)".
# 219 failures in CPython's test_tempfile alone come from exactly this shape --
# a test class parking os.unlink or tempfile.mkstemp in its body and calling it
# later.
#
# CPython has three types where this tree has one.  builtin_function_or_method
# has no tp_descr_get and does not bind; method_descriptor and
# wrapper_descriptor do.  There is no tp_descr_get here at all -- "this is a
# descriptor" is a comparison against builtin_func_type, open-coded at each
# attribute-lookup site -- so the question has to be asked of the OBJECT
# instead, and func_kind is what answers it: type_stamp_methods marks
# everything in a type's dict, and nothing marks a module's.
#
# What this file pins is that all the answers agree with each other, which is
# the part that was easy to get half-right: two of the seven binding sites used
# to gate on "is it callable" rather than on the type, so C().f and
# getattr(C(), 'f') could disagree.
import io
import os
import sys

MODULE_LEVEL = [
    ("len", len),
    ("abs", abs),
    ("os.unlink", os.unlink),
    ("os.getpid", os.getpid),
]

DESCRIPTORS = [
    ("list.append", list.append),
    ("str.upper", str.upper),
    ("dict.get", dict.get),
    ("int.__add__", int.__add__),
    ("io.BytesIO.read", io.BytesIO.read),
]

# --- __get__ answers per object ---------------------------------------------

for name, f in MODULE_LEVEL:
    print("%-18s __get__: %s" % (name, hasattr(f, "__get__")))
for name, f in DESCRIPTORS:
    print("%-18s __get__: %s" % (name, hasattr(f, "__get__")))

# A BOUND method is a descriptor that answers itself, which is how a class
# body full of already-bound methods keeps working.
class WithMethod:
    def m(self):
        return "m"


bound = WithMethod().m
print("bound method __get__:", hasattr(bound, "__get__"))
print("bound __get__ is itself:", bound.__get__(WithMethod())() == "m")

# `[].append` is deliberately absent here: CPython's is a
# builtin_function_or_method with no __get__, and this tree's is an ordinary
# bound method, which has one.  That is the recorded divergence about having
# one builtin callable type where CPython has four, not this one.

# --- a class body ------------------------------------------------------------

for name, f in MODULE_LEVEL:
    C = type("C", (), {"f": f})
    got = C().f
    print("%-18s in a class body stays itself: %s" % (name, got is f))

# The four access forms must agree with each other.
class Holder:
    f = len
    g = os.unlink


h = Holder()
print("C().f is len:", h.f is len)
print("getattr is len:", getattr(h, "f") is len)
print("C.f is len:", Holder.f is len)
print("vars is len:", vars(Holder)["f"] is len)
print("C().g is unlink:", h.g is os.unlink)
print("getattr g:", getattr(h, "g") is os.unlink)

# ...and it is CALLABLE with its own signature, which is the whole point.
print("called:", h.f([1, 2, 3]), h.f("abcd"))

# Through a subclass, and through super().
class Sub(Holder):
    def call(self):
        return super().f([1, 2])


print("subclass:", Sub().f([1, 2, 3]), Sub.f("ab"))
print("through super:", Sub().call())

# A builtin reached from a class the metaclass built.
Meta = type("Meta", (type,), {})
M = Meta("M", (), {"f": len})
print("metaclass-built:", M().f([1, 2]))

# --- a descriptor still binds ------------------------------------------------

print("list.append binds:", [].append.__self__ == [])
s = "abc"
print("str.upper binds:", s.upper() == "ABC")
b = io.BytesIO(b"hi")
print("BytesIO.read binds:", b.read() == b"hi")

# An unbound descriptor called with an explicit receiver still works.
lst = []
list.append(lst, 7)
print("unbound descriptor call:", lst)
print("unbound str.upper:", str.upper("x"))

# --- a descriptor in the WRONG class body ------------------------------------
#
# CPython refuses at the attribute access: a method descriptor's __get__
# checks the receiver.  So does this now, and with CPython's wording.

class Wrong:
    f = list.append


try:
    Wrong().f
except TypeError as e:
    print("wrong receiver:", e)

class WrongStr:
    f = str.upper


try:
    WrongStr().f
except TypeError as e:
    print("wrong receiver 2:", e)

# ...but reading it off the CLASS is fine, because there is no receiver.
print("off the class:", Wrong.f is list.append)

# And a descriptor in a body whose class IS a subclass of the owner binds.
class MyList(list):
    grab = list.append


m = MyList()
m.grab(3)
print("right receiver:", m)

# --- classmethods and staticmethods are unaffected ---------------------------

print("dict.fromkeys:", dict.fromkeys("ab", 0))
class WithCM:
    f = dict.fromkeys


print("classmethod in a body:", WithCM.f("ab", 1))

class WithSM:
    f = staticmethod(len)


print("staticmethod:", WithSM().f([1, 2, 3]))

# --- the types still read as CPython's, where this tree draws the same line ---

print("len type:", type(len).__name__)
print("bound type is not the unbound one:", type([].append) is not type(list.append))

print("done")
