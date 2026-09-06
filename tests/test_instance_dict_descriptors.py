# The descriptor protocol applies to what a TYPE supplies, not to what an
# instance happens to be holding.  op_load_attr ran it over both:
#
#     c.p = property(lambda s: "PROP")
#     c.p                    -> "PROP"    (CPython: the property object)
#
#     class D:
#         def __get__(self, o, t): return "GET"
#     c.d = D()
#     c.d                    -> "GET"     (CPython: the D instance)
#
# tp_getattr hands back a fully resolved answer for a type-dict hit -- and
# deliberately hands staticmethod, classmethod and property back RAW, because
# LOAD_ATTR is what knows the push convention for those.  So LOAD_ATTR could
# not tell "raw descriptor from the type, unwrap it" from "an ordinary value
# the instance was holding", and unwrapped both.
#
# instance_getattr_where says which, through an out-parameter in the caller's
# frame -- the shape type_getattr / type_getattr_meta already uses, and
# re-entrant where a global would be clobbered by any lookup a descriptor
# makes on its way through.
#
# The type-dict side must not move: that is most of this file.


class Plain:
    pass


def free_function(*a):
    return ("free", a)


class Getter:
    def __get__(self, obj, objtype=None):
        return "Getter.__get__"

    def __repr__(self):
        return "<Getter>"


class DataGetter:
    def __get__(self, obj, objtype=None):
        return "DataGetter.__get__"

    def __set__(self, obj, value):
        pass

    def __repr__(self):
        return "<DataGetter>"


# --- values living in the INSTANCE dict are themselves ---------------------
c = Plain()
c.prop = property(lambda s: "never")
c.getter = Getter()
c.datagetter = DataGetter()
c.func = free_function
c.plain = 42
c.none = None

print(type(c.prop).__name__)
print(c.getter, type(c.getter).__name__)
print(c.datagetter, type(c.datagetter).__name__)
print(c.func.__name__, c.func(1))
print(c.plain, c.none)

for _ in range(4):                      # repeated, for the specializer
    print(type(c.prop).__name__, repr(c.getter), c.plain)

# getattr() and __getattribute__ must agree with the operator
print(type(getattr(c, "prop")).__name__, getattr(c, "getter"))
print(type(object.__getattribute__(c, "prop")).__name__)
print(c.__dict__["prop"] is c.prop, c.__dict__["getter"] is c.getter)


# --- the same names on the TYPE keep the protocol ---------------------------
class OnType:
    prop = property(lambda s: "OnType.prop")
    getter = Getter()
    datagetter = DataGetter()
    func = free_function
    plain = 42
    sm = staticmethod(lambda *a: ("sm", a))
    cm = classmethod(lambda cls, *a: ("cm", a))

    def meth(self, *a):
        return ("meth", a)


t = OnType()
for _ in range(4):
    print(t.prop, t.getter, t.datagetter, t.plain)
    print(type(t.func(1)[1][0]).__name__, t.func(1)[1][1], t.sm(1), t.cm(1), t.meth(1))
print(OnType.prop is OnType.__dict__["prop"], type(OnType.prop).__name__)
print(OnType.getter, OnType.datagetter)


# --- and the interaction: same name on both --------------------------------
class Both:
    shadow = property(lambda s: "TYPE.shadow")
    ng = Getter()


b = Both()
print("data descriptor wins over the instance dict:", b.shadow)
b.__dict__["shadow"] = "INSTANCE.shadow"
print("still the property:", b.shadow)          # property is a DATA descriptor

print("non-data before shadowing:", b.ng)
b.__dict__["ng"] = "INSTANCE.ng"
print("instance dict wins over a non-data descriptor:", b.ng)
for _ in range(4):
    print(b.shadow, b.ng)


# --- __slots__, which reads through a member descriptor --------------------
class Slotted:
    __slots__ = ("s",)


sl = Slotted()
sl.s = property(lambda x: "never")
print(type(sl.s).__name__)
sl.s = Getter()
print(sl.s, type(sl.s).__name__)


# --- a subclass of a builtin, whose instance dict lives at the tail --------
class StrSub(str):
    pass


ss = StrSub("abc")
ss.g = Getter()
print(ss, ss.g, type(ss.g).__name__, ss.upper())


# --- __getattr__ and __getattribute__ hooks still run ----------------------
class Hooked:
    def __getattr__(self, name):
        return "__getattr__(%s)" % name


h = Hooked()
h.there = Getter()
print(h.there, h.missing)


class FullHook:
    def __getattribute__(self, name):
        return "__getattribute__(%s)" % name


fh = FullHook()
print(fh.anything, fh.__class__ if False else "ok")

# --- errors ----------------------------------------------------------------
try:
    c.nope
except AttributeError:
    print("missing attribute -> AttributeError")
