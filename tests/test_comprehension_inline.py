# PEP 709: a list, set or dict comprehension runs in the enclosing frame.
#
# Each used to get a code object and a frame of its own, and three things
# followed: `sys._getframe().f_code.co_name` inside one answered `<listcomp>`,
# a traceback through one had an extra entry, and a name the enclosing scope
# could see but had not made a cell -- `__class__`, which is what `super()`
# reads -- was a NameError.
#
# What replaces the frame is a save and a restore: the targets are the
# enclosing function's locals now, so one that shadows a real local has to put
# it back, on the way out AND on the way out through an exception.
import sys


def frames_of(exc):
    tb = exc.__traceback__
    out = []
    while tb is not None:
        out.append(tb.tb_frame.f_code.co_name)
        tb = tb.tb_next
    return out


print("-- the frame is the enclosing one")


def which_frame():
    return [sys._getframe().f_code.co_name for _ in range(1)]


print(which_frame())
# A comprehension at MODULE or CLASS scope still gets a frame here, because
# inlining one needs its target to be a fast local and CPython gives the same
# name two storages at once -- a global for the outer binding and a fast slot
# for the comprehension.  DIVERGENCES.md records it; what a program sees is
# the co_name, so that is what is not compared.


def genexp_frame():
    return list(sys._getframe().f_code.co_name for _ in range(1))


print("a genexp still has its own:", genexp_frame())

print()
print("-- a traceback has no extra entry")


def boom():
    return [1 / 0 for _ in range(1)]


try:
    boom()
except ZeroDivisionError as e:
    print(frames_of(e))

print()
print("-- super() works, because __class__ is the method's own free variable")


class Base:
    def m(self):
        return "Base.m"


class Derived(Base):
    def m(self):
        return [super().m() for _ in range(2)]

    def m_set(self):
        return {super().m() for _ in range(2)}

    def m_dict(self):
        return {i: super().m() for i in range(2)}


print(Derived().m(), sorted(Derived().m_set()), Derived().m_dict())

print()
print("-- a shadowed local comes back")


def shadow():
    x = "kept"
    r = [x for x in range(3)]
    return r, x


print(shadow())


def shadow_through_raise():
    x = "kept"
    try:
        [1 / 0 for x in range(3)]
    except ZeroDivisionError:
        pass
    return x


print(shadow_through_raise())


def shadow_two():
    a, b = "keep-a", "keep-b"
    r = [a + b for a, b in [("1", "2")]]
    return r, a, b


print(shadow_two())

print()
print("-- and one that was never bound stays unbound")


def never_bound():
    r = [y for y in range(2)]
    try:
        y
        return r, "leaked"
    except NameError:
        return r, "unbound"


print(never_bound())

print()
print("-- the shapes that have to keep working")
print([[y for y in range(x)] for x in range(3)])
print([a + b for a, b in [(1, 2), (3, 4)]])
print([x for x in range(6) if x % 2 if x > 2])
print([x + y for x in range(3) for y in range(x)])
print({x: x * x for x in range(4)})
print(sorted({x % 3 for x in range(9)}))
print([(a, b) for a, (b, c) in [(1, (2, 3))]])
print([x for *x, in [([1, 2],)]])
n = 7
print([n for _ in range(2)], n)
