# STORE_ATTR's inline cache used to refuse any class whose MRO held a data
# descriptor -- TYPE_FLAG_MRO_HAS_DATA_DESCR, which is per-CLASS.  One
# @property therefore made every OTHER attribute of that class, and of every
# subclass, take the whole generic store path: op_store_attr walked the MRO,
# called tp_setattr, and instance_setattr walked the same MRO again.  The
# question is now asked per NAME at install time, the way the load side
# already asks it, and kept true by the type's version.
#
# So the cases that matter are: a class that has both a property and plain
# attributes, __slots__ (whose member descriptor must keep the write out of
# the instance dict), and every way a name can stop being safe to cache after
# a site has already installed for it.


def property_beside_plain_stores():
    class Shape:
        def __init__(self, x, y):
            self.x = x
            self.y = y
            self._s = 1

        @property
        def s(self):
            return self._s * 10

        @s.setter
        def s(self, v):
            self._s = v

    sh = Shape(3, 4)
    for i in range(300):
        sh.x = i          # cacheable
        sh.y = i + 1      # cacheable
        sh.s = i + 2      # the property setter: must not be
    return sh.x, sh.y, sh._s, sh.s


def the_property_comes_down_the_mro():
    class Base:
        @property
        def computed(self):
            return "base"

    class Derived(Base):
        def __init__(self):
            self.plain = 0

    d = Derived()
    for i in range(300):
        d.plain = i
    return d.plain, d.computed


def property_arrives_for_that_very_name():
    class C:
        def __init__(self):
            self.v = 1

    c = C()
    seen = []
    out = []
    for i in range(6):
        c.v = i
        out.append(c.v)
        if i == 2:
            C.v = property(lambda self: "computed",
                           lambda self, x: seen.append(x))
    return out, seen


def property_arrives_on_a_base():
    class Base:
        pass

    class Derived(Base):
        def __init__(self):
            self.w = 0

    d = Derived()
    seen = []
    out = []
    for i in range(6):
        d.w = i
        out.append(d.w)
        if i == 2:
            Base.w = property(lambda self: "from base",
                              lambda self, x: seen.append(x))
    return out, seen


def a_slot_is_not_the_instance_dict():
    """A __slots__ member descriptor is a data descriptor, and the specialized
    handler writes into the instance dict -- so a name that lives in a slot
    must never be cached, or the write would land in the dict and the read
    would still come from the slot."""
    class S:
        __slots__ = ("a",)

    class SD(S):                  # a subclass, so there is a __dict__ too
        pass

    o = SD()
    for i in range(300):
        o.a = i                   # the slot
        o.b = i + 1               # the instance dict
    return o.a, o.b, sorted(o.__dict__)


def a_plain_class_attribute_is_not_a_descriptor():
    """A method or a plain value in the class dict loses to the instance dict,
    so a name that resolves to one is still cacheable for a store."""
    class C:
        shared = "class value"

        def meth(self):
            return "method"

        def __init__(self):
            self.shared = "instance value"

    c = C()
    for i in range(200):
        c.shared = i
    return c.shared, C.shared, c.meth()


def descriptor_type_gains_set_afterwards():
    """The over-approximation exists for this: a descriptor's own type can
    gain __set__ long after the class holding it was refreshed, and that write
    touches neither the class nor its bases -- so no version is stamped and a
    cache installed for the name would never learn.

    ONE store site, run before and after: a fresh site is cold and takes the
    generic path, which is exactly what hides an unsound guard."""
    class Lazy:
        def __get__(self, o, t=None):
            return "lazy"

    class B:
        def __init__(self):
            self.plain = 0

    B.z = Lazy()
    b = B()
    hits = []

    def store(o, n):
        for i in range(n):
            o.z = i               # the one site that must not specialize
        return o

    store(b, 200)
    out = [b.z, b.plain]
    Lazy.__set__ = lambda self, o, v: hits.append(v)
    store(b, 3)
    out.append(b.z)
    out.append(hits)
    return out


def a_slot_shadowed_in_the_instance_dict():
    """The one shape where the per-name refusal is the ONLY thing standing in
    the way: `a` lives in a slot AND has an entry in the instance dict, so the
    install site's dense-index lookup succeeds and the specialized handler --
    which writes the dict entry in place -- would silently stop writing the
    slot."""
    class S:
        __slots__ = ("a",)

    class SD(S):
        pass

    o = SD()
    o.__dict__["a"] = "in the dict"

    def store(n):
        for i in range(n):
            o.a = i               # must reach the slot every time
        return o.a

    last = store(200)
    return last, o.a, o.__dict__["a"]


def setattr_of_the_classs_own():
    class SA:
        def __init__(self):
            object.__setattr__(self, "k", 0)

        def __setattr__(self, n, v):
            object.__setattr__(self, n, v * 2)

    sa = SA()
    for i in range(200):
        sa.k = i
    return sa.k


def setattr_arrives_after_the_site_is_warm():
    class C:
        def __init__(self):
            self.n = 0

    c = C()
    out = []
    for i in range(6):
        c.n = i
        out.append(c.n)
        if i == 2:
            C.__setattr__ = lambda self, k, v: object.__setattr__(self, k, -v)
    return out


def a_getset_descriptor_of_a_builtin_base():
    class E(Exception):
        def __init__(self):
            self.tag = 0

    e = E()
    for i in range(200):
        e.tag = i
    try:
        for _ in range(3):
            e.args = ()           # a getset on the base: not the dict
    except Exception as exc:
        return e.tag, type(exc).__name__
    return e.tag, e.args


def two_classes_at_one_site():
    class A:
        def __init__(self):
            self.k = "a"

    class B:
        @property
        def other(self):
            return 1

        def __init__(self):
            self.k = "b"

    objs = [A(), B(), A(), B()]
    for i in range(50):
        for o in objs:
            o.k = i
    return [o.k for o in objs]


def deleting_then_storing_again():
    class C:
        def __init__(self):
            self.v = "first"

    c = C()
    for i in range(200):
        c.v = i
    del c.v
    out = [getattr(c, "v", "gone")]
    for i in range(200):
        c.v = i
    out.append(c.v)
    return out


print(property_beside_plain_stores())
print(the_property_comes_down_the_mro())
print(property_arrives_for_that_very_name())
print(property_arrives_on_a_base())
print(a_slot_is_not_the_instance_dict())
print(a_plain_class_attribute_is_not_a_descriptor())
print(descriptor_type_gains_set_afterwards())
print(a_slot_shadowed_in_the_instance_dict())
print(setattr_of_the_classs_own())
print(setattr_arrives_after_the_site_is_warm())
print(a_getset_descriptor_of_a_builtin_base())
print(two_classes_at_one_site())
print(deleting_then_storing_again())
