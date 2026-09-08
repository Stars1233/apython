# property, staticmethod and classmethod are static types whose tp_new is
# inherited by a subclass, and type_call hands that tp_new the SUBCLASS.  Each
# of the three ignored it and allocated its own size under its own type, so
# every subclass instance was a plain base instance: `type(C.__dict__['v'])`
# on a class built from a property subclass answered `property`, and the
# subclass's own __get__ was never consulted.
#
# The attribute machinery was never at fault -- it falls through to the
# general __get__ lookup for anything that is not EXACTLY one of the four
# builtin descriptor types -- so the cases that matter are the ones that ask
# what a constructed descriptor IS, and the ones that put a subclass in a
# class body and read it.


def the_class_survives_construction():
    class P(property):
        pass

    class S(staticmethod):
        pass

    class K(classmethod):
        pass

    p, s, k = P(lambda self: 1), S(lambda: 2), K(lambda cls: 3)
    return (type(p).__name__, type(s).__name__, type(k).__name__,
            isinstance(p, property), isinstance(s, staticmethod),
            isinstance(k, classmethod))


def a_property_subclass_gets_its_get():
    class Loud(property):
        def __get__(self, obj, objtype=None):
            if obj is None:
                return "class access"
            return ("loud", super().__get__(obj, objtype))

    class C:
        _v = 5
        v = Loud(lambda s: s._v)

    c = C()
    out = [C.v]
    for _ in range(200):          # one site, warm, so the cache sees it too
        out.append(c.v)
    return out[0], out[1], out[-1], len(set(map(str, out[1:])))


def a_property_subclass_keeps_its_class_through_setter():
    """property.getter/setter/deleter build a NEW property, and CPython's
    property_copy builds it as Py_TYPE(old) so a decorator chain on a subclass
    still hands back the subclass."""
    class Cached(property):
        pass

    class C:
        def __init__(self):
            self._v = 0

        v = Cached(lambda s: s._v)

        @v.setter
        def v(self, x):
            self._v = x * 2

    c = C()
    c.v = 21
    return type(C.__dict__["v"]).__name__, c.v


def a_property_subclass_with_state():
    """A subclass with a __dict__ of its own needs the room its tp_basicsize
    asks for; allocating the base's size put the dict slot past the end."""
    class Named(property):
        def __init__(self, fget, label):
            super().__init__(fget)
            self.label = label

    class C:
        _v = 3
        v = Named(lambda s: s._v, "the v")

    c = C()
    return C.__dict__["v"].label, c.v, sorted(C.__dict__["v"].__dict__)


def a_subclass_init_reaches_the_base():
    """Each of the three now has an __init__ in its tp_dict, so a subclass can
    define one and call up.  Without it super().__init__ reached
    object.__init__, which refuses arguments."""
    class NamedP(property):
        def __init__(self, fget, label):
            super().__init__(fget)
            self.label = label

    class NamedS(staticmethod):
        def __init__(self, f, label):
            super().__init__(f)
            self.label = label

    class NamedK(classmethod):
        def __init__(self, f, label):
            super().__init__(f)
            self.label = label

    class C:
        _v = 11
        v = NamedP(lambda s: s._v, "v")
        s = NamedS(lambda: "s!", "s")
        k = NamedK(lambda cls: cls.__name__, "k")

    c = C()
    d = C.__dict__
    return (c.v, c.s(), c.k(),
            d["v"].label, d["s"].label, d["k"].label,
            type(d["v"]).__name__, type(d["s"]).__name__, type(d["k"]).__name__)


def running_init_twice_rebinds():
    """__init__ is re-runnable: the second call replaces the accessors and
    releases the first set rather than leaking them."""
    class P(property):
        pass

    def a(s):
        return "a"

    def b(s):
        return "b"

    p = P(a)
    first = p.fget
    p.__init__(b)
    return first.__name__, p.fget.__name__, p.fset, p.fdel


def a_staticmethod_subclass():
    class Traced(staticmethod):
        def __get__(self, obj, objtype=None):
            inner = super().__get__(obj, objtype)
            return lambda *a: ("traced", inner(*a))

    class C:
        f = Traced(lambda x: x + 1)

    return type(C.__dict__["f"]).__name__, C().f(1), C.f(2)


def a_classmethod_subclass():
    class Loud(classmethod):
        def __get__(self, obj, objtype=None):
            return lambda *a: ("loud", super(Loud, self).__get__(obj, objtype)(*a))

    class C:
        @Loud
        def f(cls):
            return cls.__name__

    return type(C.__dict__["f"]).__name__, C().f(), C.f()


def the_exact_types_are_unchanged():
    class C:
        _v = 1

        @property
        def v(self):
            return self._v

        @staticmethod
        def s():
            return "s"

        @classmethod
        def k(cls):
            return cls.__name__

    c = C()
    return (type(C.__dict__["v"]).__name__, type(C.__dict__["s"]).__name__,
            type(C.__dict__["k"]).__name__, c.v, c.s(), c.k())


def subclass_instances_are_collectable():
    import gc

    class P(property):
        pass

    for _ in range(200):
        p = P(lambda s: 1)
        p.self = p                 # a cycle through the subclass's own dict
    gc.collect()
    return "collected"


print(the_class_survives_construction())
print(a_property_subclass_gets_its_get())
print(a_property_subclass_keeps_its_class_through_setter())
print(a_property_subclass_with_state())
print(a_subclass_init_reaches_the_base())
print(running_init_twice_rebinds())
print(a_staticmethod_subclass())
print(a_classmethod_subclass())
print(the_exact_types_are_unchanged())
print(subclass_instances_are_collectable())
