# instance_getattr used to ask "does this type override __getattribute__?" with
# a full MRO walk on every attribute access.  The answer now lives in a
# tp_flags bit, computed at class creation and refreshed by type_setattr down
# through every subclass.  Everything below is a way for that bit to go stale.


class Plain:
    def __init__(self):
        self.x = 1


p = Plain()
print(p.x)


# --- a class that defines one at creation ----------------------------------
class Hooked:
    def __getattribute__(self, name):
        return "hooked:" + name


print(Hooked().anything)


# --- inherited from a base -------------------------------------------------
class HookedChild(Hooked):
    pass


print(HookedChild().inherited)


# --- gained AFTER a subclass already exists --------------------------------
class Late:
    pass


class LateChild(Late):
    pass


lc = LateChild()
lc.real = "instance"
print(lc.real)
Late.__getattribute__ = lambda self, n: "late:" + n
print(lc.real)
print(LateChild().other)
print(Late().direct)


# --- and lost again --------------------------------------------------------
del Late.__getattribute__
print(lc.real)


# --- three deep, assigned at the root --------------------------------------
class A:
    pass


class B(A):
    pass


class C(B):
    pass


c = C()
c.v = "plain"
print(c.v)
A.__getattribute__ = lambda self, n: "deep"
print(c.v)
del A.__getattribute__
print(c.v)


# --- a subclass created while the base already has one ---------------------
class Root:
    def __getattribute__(self, name):
        return "root"


class MadeLater(Root):
    pass


print(MadeLater().z)


# --- setattr on an unrelated name must not disturb the bit -----------------
class Q:
    def __getattribute__(self, name):
        return "q"


Q.unrelated = 5
print(Q().anything)


# --- object.__getattribute__ delegation still terminates -------------------
class Deleg:
    def __getattribute__(self, name):
        if name == "special":
            return "intercepted"
        return object.__getattribute__(self, name)


d = Deleg()
d.ordinary = 42
print(d.special)
print(d.ordinary)


# --- __getattr__ is a different hook and must keep working -----------------
class Fallback:
    def __getattr__(self, name):
        return "fallback:" + name


f = Fallback()
f.present = 1
print(f.present)
print(f.missing)


# --- type(name, bases, ns) carrying one ------------------------------------
T = type('T', (), {'__getattribute__': lambda self, n: "from-ns"})
print(T().anything)
