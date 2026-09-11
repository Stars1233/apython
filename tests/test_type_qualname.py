# __qualname__ is a getset on `type`, not an entry in the class's own dict.
#
# The compiler stores it into the class body's namespace, as CPython's does,
# and CPython's type_new then takes it OUT and keeps it in ht_qualname.  This
# tree left it there, so `'__qualname__' in C.__dict__` was True where
# CPython says False, vars(C) had an entry CPython's has none of, and -- the
# part that is visible from ordinary code -- every INSTANCE of every class
# answered its class's __qualname__, because an instance's lookup walks the
# same dict.


class C:
    def __call__(self):
        return 1


class Outer:
    class Inner:
        pass


def f():
    class Local:
        pass

    return Local


print(C.__qualname__, Outer.Inner.__qualname__, f().__qualname__)
print("__qualname__" in C.__dict__, "__qualname__" in vars(Outer.Inner))

o = C()
try:
    print(o.__qualname__)
except AttributeError as e:
    print("instance:", e)

# A class body that sets it itself is answered with what it set, and it is
# still not in the dict.
class Q:
    __qualname__ = "Renamed"


print(Q.__qualname__, "__qualname__" in Q.__dict__)
try:
    print(Q().__qualname__)
except AttributeError as e:
    print("instance of Q:", e)

# A non-str refuses the class outright.
try:
    class Bad:
        __qualname__ = 42
except TypeError as e:
    print("build:", e)

# Assignment goes to the same place; a non-str and a delete are refused.
Q.__qualname__ = "Again"
print(Q.__qualname__, "__qualname__" in Q.__dict__)
try:
    Q.__qualname__ = 5
except TypeError as e:
    print("assign:", e)
try:
    del Q.__qualname__
except TypeError as e:
    print("delete:", e)
print(Q.__qualname__)

# A subclass gets its own, and a builtin type falls back to its name.
class D(Q):
    pass


print(D.__qualname__, ValueError.__qualname__, int.__qualname__)

# type() with three arguments, with and without the key.
E = type("E", (), {})
print(E.__qualname__, "__qualname__" in E.__dict__)
F = type("F", (), {"__qualname__": "FF"})
print(F.__qualname__, "__qualname__" in F.__dict__)
try:
    type("G", (), {"__qualname__": 1})
except TypeError as e:
    print("three-arg:", e)

# __name__ and __qualname__ move independently.
class H:
    pass


H.__name__ = "renamed_name"
print(H.__name__, H.__qualname__)
H.__qualname__ = "renamed_qual"
print(H.__name__, H.__qualname__)

# A class the collector takes still gives the string back; run it enough
# times that a leak would show.
import gc

for _ in range(200):
    class Temp:
        pass

    Temp.__qualname__ = "t" * 40
gc.collect()
print("collected")

# The repr paths that read it.
class R:
    pass


print(repr(R), repr(R()).split(" object")[0] + " object>")
print("done")


# The interned key the builder uses is this frame's, on the refusing road too.
import sys

name = "x" * 40
base = sys.getrefcount(name)
for _ in range(20):
    try:
        class Bad:
            __qualname__ = 42
    except TypeError:
        pass
print("refused cleanly")


# A nested class body, and one built by type(), keep their own.
def outer():
    class A:
        class B:
            pass

    return A


print(outer().__qualname__, outer().B.__qualname__)
print(type("Z", (), {"__qualname__": "Q.Z"}).__qualname__)
print("done 2")
