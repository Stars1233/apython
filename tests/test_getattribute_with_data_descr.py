# A class that overrides __getattribute__ AND has a data descriptor.
#
# `type_refresh_attr_flags` answers two independent questions about a type and
# caches each as a bit: does the MRO override __getattribute__, and does it
# hold a data descriptor.  The second was only computed on the path where the
# first answered NO, so any class that overrode __getattribute__ never had its
# data-descriptor bit set at all.
#
# The two have nothing to do with each other.  __getattribute__ governs reads;
# `op_store_attr` consults the data-descriptor bit ALONE to decide whether a
# STORE needs to look for a property first.  With the bit stale, `c.p = 5` put
# `p` straight into the instance dict and the setter never ran -- so the write
# appeared to succeed, and the failure surfaced later as a missing attribute
# somewhere else.
#
# Every case below therefore checks the SIDE EFFECT of the setter, not just
# that the assignment did not raise.  Both orders matter too: the flag is
# pushed down to subclasses, so a class can acquire an overridden
# __getattribute__ from a base it did not have when it was created, and a base
# can acquire a property after its subclasses exist.


def passthrough(self, name):
    return object.__getattribute__(self, name)


def both_on_one_class():
    class C:
        __getattribute__ = passthrough

        @property
        def p(self):
            return self._v

        @p.setter
        def p(self, val):
            self._v = val * 10

    c = C()
    c.p = 5
    print(c.p, c._v, sorted(c.__dict__))
    c.p = 7
    print(c.p, c._v, sorted(c.__dict__))


def inherited_getattribute():
    # The property is on the subclass, the __getattribute__ on the base.
    class Base:
        __getattribute__ = passthrough

    class Sub(Base):
        @property
        def q(self):
            return self._q

        @q.setter
        def q(self, v):
            self._q = v + 1

    s = Sub()
    s.q = 7
    print(s.q, s._q, sorted(s.__dict__))


def inherited_property():
    # The other way round: the property on the base, __getattribute__ on the
    # subclass.
    class Base:
        @property
        def w(self):
            return self._w

        @w.setter
        def w(self, v):
            self._w = v * 2

    class Sub(Base):
        __getattribute__ = passthrough

    s = Sub()
    s.w = 3
    print(s.w, s._w, sorted(s.__dict__))


def added_afterwards():
    # The flags are pushed down to existing subclasses, so a property added to
    # a base AFTER the subclass exists still has to be seen.
    class Base:
        __getattribute__ = passthrough

    class Sub(Base):
        pass

    s = Sub()

    def getter(self):
        return self._late

    def setter(self, v):
        self._late = v - 1

    Base.late = property(getter, setter)
    s.late = 10
    print(s.late, s._late, sorted(s.__dict__))

    # And __getattribute__ added to a base after the fact, over an existing
    # property.
    class B2:
        @property
        def z(self):
            return self._z

        @z.setter
        def z(self, v):
            self._z = v + 100

    class S2(B2):
        pass

    t = S2()
    B2.__getattribute__ = passthrough
    t.z = 1
    print(t.z, t._z, sorted(t.__dict__))


def read_only_still_refuses():
    # A data descriptor with no setter must still refuse the store rather than
    # falling through into the instance dict.
    class C:
        __getattribute__ = passthrough

        @property
        def r(self):
            return 1

    c = C()
    try:
        c.r = 2
    except AttributeError:
        print("AttributeError", c.r, sorted(c.__dict__))


def slots_and_getset():
    # __slots__ members are data descriptors too.
    class C:
        __slots__ = ("a", "b")
        __getattribute__ = passthrough

    c = C()
    c.a = 1
    c.b = 2
    print(c.a, c.b)
    try:
        c.missing = 3
    except AttributeError:
        print("AttributeError on a slot class")


def non_data_descriptor_is_not_one():
    # A plain function is a NON-data descriptor: a store must shadow it in the
    # instance dict, not go looking for a __set__.
    class C:
        __getattribute__ = passthrough

        def m(self):
            return "method"

    c = C()
    print(c.m())
    c.m = lambda: "shadowed"
    print(c.m(), sorted(c.__dict__))


def deletion():
    class C:
        __getattribute__ = passthrough

        @property
        def d(self):
            return self._d

        @d.setter
        def d(self, v):
            self._d = v

        @d.deleter
        def d(self):
            del self._d

    c = C()
    c.d = 4
    print(c.d, sorted(c.__dict__))
    del c.d
    print(sorted(c.__dict__))
    try:
        c.d
    except AttributeError:
        print("AttributeError after delete")


def plain_class_unaffected():
    # The control: no __getattribute__ at all, which always worked.
    class C:
        @property
        def p(self):
            return self._v

        @p.setter
        def p(self, val):
            self._v = val * 10

    c = C()
    c.p = 5
    print(c.p, c._v, sorted(c.__dict__))


both_on_one_class()
inherited_getattribute()
inherited_property()
added_afterwards()
read_only_still_refuses()
slots_and_getset()
non_data_descriptor_is_not_one()
deletion()
plain_class_unaffected()
