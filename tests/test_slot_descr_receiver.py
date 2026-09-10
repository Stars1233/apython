# A __slots__ descriptor is an OFFSET into an instance of the class it was made
# for, and it has to check that it got one.
#
# Nothing stops a program from putting one in another class's body --
# `class Sneaky: borrowed = Class.slot`, which CPython's own test_opcache does
# on purpose -- and `o.borrowed = 42` then wrote at Class's offset into a
# Sneaky, which has a different layout.  A wild store, not an error: the test
# does it 1025 times and the interpreter died afterwards, somewhere else.
#
# member_descriptor does not publish __get__/__set__/__delete__ here, so the
# direct forms are left out; the attribute paths are what the crash came
# through.


class Class:
    __slots__ = ("slot",)


class Wider:
    __slots__ = ("a", "b", "c")


class Sneaky:
    borrowed = Class.slot
    wide = Wider.c


class Derived(Class):
    pass


o = Sneaky()
for op, fn in (("set", lambda: setattr(o, "borrowed", 42)),
               ("get", lambda: o.borrowed),
               ("del", lambda: delattr(o, "borrowed")),
               ("set wide", lambda: setattr(o, "wide", 1)),
               ("get wide", lambda: o.wide)):
    try:
        print(op, "-> ok", repr(fn()))
    except TypeError as e:
        print(op, "->", e)

# Repeatedly, which is what makes the cache warm and the corruption stick.
n = 0
for _ in range(1200):
    try:
        o.borrowed = 42
    except TypeError:
        n += 1
print("refused:", n)

# The descriptor still works on what it was made for, and on a subclass.
c = Class()
c.slot = 7
print("proper:", c.slot)
d = Derived()
d.slot = 9
print("subclass:", d.slot)
del c.slot
try:
    c.slot
except AttributeError as e:
    print("deleted:", e)

# An unrelated instance of an unrelated class, and a non-object.
class Bare:
    pass


b = Bare()
try:
    Class.slot.__class__          # just to touch it
    setattr(b, "x", 1)            # ordinary attribute, unaffected
    print("unrelated ok:", b.x)
except TypeError as e:
    print("unrelated ->", e)
print("done")
