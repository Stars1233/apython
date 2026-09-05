# A DATA descriptor -- one whose type defines __set__ or __delete__ -- outranks
# an entry of the same name in the instance dict.  A non-data descriptor does
# not.  instance_getattr_default used to read the instance dict first,
# unconditionally, so a property was shadowed by whatever was stored under its
# name.


class WithProperty:
    @property
    def x(self):
        return "property"


p = WithProperty()
p.__dict__['x'] = "instance"
print(p.x)
print(getattr(p, 'x'))
print(p.__dict__['x'])


# --- a hand-written data descriptor ----------------------------------------
class DataDescr:
    def __get__(self, obj, cls):
        return "data-get"

    def __set__(self, obj, value):
        obj.__dict__['stored'] = value


class UsesData:
    d = DataDescr()


u = UsesData()
u.__dict__['d'] = "instance"
print(u.d)
print(getattr(u, 'd'))


# --- __delete__ alone is enough to make it a data descriptor ---------------
class DeleteOnly:
    def __get__(self, obj, cls):
        return "delete-only-get"

    def __delete__(self, obj):
        pass


class UsesDeleteOnly:
    d = DeleteOnly()


ud = UsesDeleteOnly()
ud.__dict__['d'] = "instance"
print(ud.d)


# --- a NON-data descriptor loses to the instance dict ----------------------
class NonData:
    def __get__(self, obj, cls):
        return "nondata-get"


class UsesNonData:
    n = NonData()


un = UsesNonData()
print(un.n)
un.__dict__['n'] = "instance"
print(un.n)
print(getattr(un, 'n'))


# --- a method is a non-data descriptor too ---------------------------------
class HasMethod:
    def m(self):
        return "method"


h = HasMethod()
print(h.m())
h.__dict__['m'] = lambda: "shadowed"
print(h.m())


# --- staticmethod and classmethod also lose --------------------------------
class Statics:
    @staticmethod
    def s():
        return "static"

    @classmethod
    def c(cls):
        return "classmethod"


st = Statics()
print(st.s(), st.c())
st.__dict__['s'] = lambda: "shadow-s"
st.__dict__['c'] = lambda: "shadow-c"
print(st.s(), st.c())


# --- inherited data descriptors still win ----------------------------------
class PropBase:
    @property
    def v(self):
        return "base-property"


class PropChild(PropBase):
    pass


pc = PropChild()
pc.__dict__['v'] = "instance"
print(pc.v)


# --- a property gained after instances exist -------------------------------
class Late:
    pass


lt = Late()
lt.__dict__['w'] = "instance"
print(lt.w)
Late.w = property(lambda self: "late-property")
print(lt.w)
del Late.w
print(lt.w)


# --- the ordinary case is untouched ----------------------------------------
class Ordinary:
    cls_attr = "class"

    def __init__(self):
        self.inst_attr = "instance"


o = Ordinary()
print(o.inst_attr, o.cls_attr)
o.cls_attr = "shadowed"
print(o.cls_attr)
print(Ordinary.cls_attr)

# __slots__ members are data descriptors, and there is no dict to lose to.
class Slotted:
    __slots__ = ('s',)


sl = Slotted()
sl.s = 1
print(sl.s)

# missing attributes still raise
try:
    o.nope
except AttributeError as e:
    print("AttributeError")
