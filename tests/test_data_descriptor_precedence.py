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

# A descriptor whose OWN type gains __set__ after it is already installed on a
# class.  That write touches neither the holding class nor any of its bases, so
# a cached "does this MRO hold a data descriptor" bit computed at install time
# is stale from then on -- and the store then went into the instance dict with
# the setter never running.  The bit is an over-approximation for exactly this
# reason; the walk it gates makes the live check.


class Lazy:
    def __get__(self, obj, objtype=None):
        return "descr-get"


class LateHolder:
    pass


LateHolder.attr = Lazy()
h = LateHolder()
# Before __set__ exists it is a NON-data descriptor: the instance dict wins.
h.attr = "instance"
print(h.attr, sorted(h.__dict__))
del h.attr
print(h.attr)

# Now it becomes a data descriptor, and the same store must reach __set__.
_seen = []
Lazy.__set__ = lambda self, obj, value: _seen.append(value)
h2 = LateHolder()
h2.attr = 7
print(h2.attr, sorted(h2.__dict__), _seen)

# ...and the read side has to flip with it.
h3 = LateHolder()
h3.__dict__["attr"] = "shadow"      # the descriptor now outranks it
print(h3.attr, sorted(h3.__dict__))

# __delete__ alone is enough to make it a data descriptor too.
class DelOnly:
    def __get__(self, obj, objtype=None):
        return "del-get"


class DelHolder:
    pass


DelHolder.d = DelOnly()
d1 = DelHolder()
d1.d = "in-dict"
print(d1.d, sorted(d1.__dict__))
DelOnly.__delete__ = lambda self, obj: None
d2 = DelHolder()
d2.__dict__["d"] = "shadow2"        # __delete__ alone still outranks it
print(d2.d, sorted(d2.__dict__))

# A subclass created BEFORE the promotion must see it as well.
class LateSub(LateHolder):
    pass


s2 = LateSub()
s2.attr = 9
print(s2.attr, sorted(s2.__dict__), _seen)
