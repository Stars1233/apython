# LOAD_ATTR's inline cache used to refuse any class whose MRO held a data
# descriptor -- TYPE_FLAG_MRO_HAS_DATA_DESCR, which is per-CLASS.  One
# @property therefore made every OTHER attribute of that class take the full
# instance_getattr path.  The question is now asked per NAME at install time
# and kept true by the type's version.
#
# So the cases that matter are: a class that has both a property and plain
# attributes, and every way a name can stop being safe to cache after a site
# has already installed for it.


def property_beside_plain_attributes():
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
    total = 0
    for i in range(300):
        total += sh.x + sh.y + sh._s      # cacheable
        total += sh.s                      # the property: must not be
        sh.s = i
    return total, sh.x, sh.y, sh._s, sh.s


def property_arrives_for_that_very_name():
    class C:
        def __init__(self):
            self.v = 1

    c = C()
    out = []
    for i in range(6):
        out.append(c.v)
        if i == 2:
            C.v = property(lambda self: "computed")
    return out


def property_arrives_on_a_base():
    class Base:
        pass

    class Derived(Base):
        def __init__(self):
            self.w = 1

    d = Derived()
    out = []
    for i in range(6):
        out.append(d.w)
        if i == 2:
            Base.w = property(lambda self: "from base")
    return out


def a_plain_class_attribute_is_not_a_descriptor():
    """A method or a plain value in the class dict does NOT outrank the
    instance dict, so a name that resolves to one is still cacheable."""
    class C:
        shared = "class value"

        def meth(self):
            return "method"

        def __init__(self):
            self.shared = "instance value"

    c = C()
    out = []
    for _ in range(200):
        out.append(c.shared)
    return out[0], len(set(out)), c.meth()


def slots_member_is_a_descriptor():
    class S:
        __slots__ = ("a",)

    s = S()
    s.a = 5
    total = 0
    for _ in range(200):
        total += s.a
    return total, s.a


def getattribute_override_refuses():
    class G:
        def __init__(self):
            self.n = 1

        def __getattribute__(self, k):
            return object.__getattribute__(self, k)

    g = G()
    total = 0
    for _ in range(200):
        total += g.n
    return total


def descriptor_type_gains_set_afterwards():
    """The over-approximation exists for this: a descriptor's own type can
    gain __set__ long after the class holding it was refreshed, and that write
    touches neither the class nor its bases."""
    class Lazy:
        def __get__(self, o, t=None):
            return "lazy"

    class B:
        def __init__(self):
            self.plain = 1

    B.l = Lazy()
    b = B()
    out = [b.l, b.plain]
    Lazy.__set__ = lambda self, o, v: None
    out.append(b.l)
    out.append(b.plain)
    return out


def deleting_the_instance_attribute():
    class C:
        fallback = "class"

        def __init__(self):
            self.v = "instance"

    c = C()
    out = []
    for i in range(6):
        out.append(c.fallback)
        if i == 2:
            C.fallback = "changed"
    del c.v
    out.append(getattr(c, "v", "gone"))
    return out


def two_classes_at_one_site():
    class A:
        def __init__(self):
            self.k = "a"

    class B:
        k_prop = 1

        def __init__(self):
            self.k = "b"

    objs = [A(), B(), A(), B()]
    out = []
    for _ in range(50):
        for o in objs:
            out.append(o.k)
    return out[:4], len(out), sorted(set(out))


print(property_beside_plain_attributes())
print(property_arrives_for_that_very_name())
print(property_arrives_on_a_base())
print(a_plain_class_attribute_is_not_a_descriptor())
print(slots_member_is_a_descriptor())
print(getattribute_override_refuses())
print(descriptor_type_gains_set_afterwards())
print(deleting_the_instance_attribute())
print(two_classes_at_one_site())
