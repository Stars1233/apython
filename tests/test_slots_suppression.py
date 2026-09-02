# __slots__ has to suppress the instance __dict__.
#
# Two failures.  __slots__ = () skipped the whole block -- the empty tuple hit
# the "no slots" branch -- so the one form whose entire purpose is to refuse
# arbitrary attributes accepted them.  And object.__getattribute__'s __dict__
# arm CREATED the dict on first read, without asking whether the class had
# slots: so reading o.__dict__ handed a slotted instance the dict it was
# supposed not to have, and every attribute after that landed in it.

class Empty:
    __slots__ = ()

class One:
    __slots__ = ("a",)

class Two:
    __slots__ = ("a", "b")

class ListSlots:
    __slots__ = ["x", "y"]

class Plain:
    pass

print("=== no __dict__, and no arbitrary attributes ===")
for C in (Empty, One, Two, ListSlots, Plain):
    o = C()
    print(C.__name__, "has __dict__:", hasattr(o, "__dict__"))
    try:
        o.zzz = 1
        print("  set zzz ->", o.zzz)
    except AttributeError as e:
        print("  AttributeError:", e)
    try:
        print("  __dict__ ->", o.__dict__)
    except AttributeError as e:
        print("  AttributeError:", e)

print("=== the slots themselves work ===")
o = Two()
o.a = 1
o.b = "x"
print(o.a, o.b, Two.__slots__, type(Two.a).__name__)
o.a = 99
print(o.a)
del_ok = True
try:
    print(One().a)
except AttributeError as e:
    print("unset slot:", e)

print("=== reading __dict__ does not create one ===")
p = One()
try:
    p.__dict__
except AttributeError:
    pass
try:
    p.later = 1
    print("LEAKED")
except AttributeError:
    print("still refused")

print("=== a subclass without __slots__ gets a dict again ===")
class Sub(One):
    pass

s = Sub()
s.a = 5
s.zzz = 6
print(s.a, s.zzz, hasattr(s, "__dict__"), s.__dict__)

print("=== a subclass with its own empty __slots__ does not ===")
class Tight(One):
    __slots__ = ()

t = Tight()
t.a = 7
print(t.a, hasattr(t, "__dict__"))
try:
    t.zzz = 1
    print("LEAKED")
except AttributeError:
    print("refused")

print("=== inherited slots still work ===")
class More(One):
    __slots__ = ("c",)

m = More()
m.a = 1
m.c = 2
print(m.a, m.c, hasattr(m, "__dict__"))

print("=== and vars() agrees ===")
try:
    print(vars(Two()))
except TypeError as e:
    print("TypeError:", e)
print(sorted(vars(Plain()).keys()))
