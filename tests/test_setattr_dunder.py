# __setattr__ and __delattr__ are protocol hooks, not decoration.  They had no
# row in slot_table, so a class defining either kept the instance_setattr that
# type_from_parts installs: the method sat in the class dict, answered
# `'__setattr__' in C.__dict__`, and was never called.  `o.x = v`,
# `setattr(o, "x", v)` and `del o.x` all went straight past it.


def setattr_intercepts():
    class R:
        def __setattr__(self, k, v):
            object.__setattr__(self, k, v * 10)

    r = R()
    r.z = 7
    out = [r.z]
    setattr(r, "w", 3)
    out.append(r.w)
    for i in range(5):
        r.z = i
    out.append(r.z)
    return out


def delattr_intercepts():
    log = []

    class R:
        def __delattr__(self, k):
            log.append(k)
            object.__delattr__(self, k)

    r = R()
    r.a = 1
    r.b = 2
    del r.a
    del r.b
    return log, hasattr(r, "a"), hasattr(r, "b")


def setattr_can_refuse():
    class Frozen:
        def __init__(self, v):
            object.__setattr__(self, "v", v)

        def __setattr__(self, k, val):
            raise AttributeError("%s is read-only" % k)

    f = Frozen(5)
    out = [f.v]
    try:
        f.v = 6
    except AttributeError as e:
        out.append(str(e))
    out.append(f.v)
    return out


def inherited_setattr():
    class Base:
        def __setattr__(self, k, v):
            object.__setattr__(self, k, "base:" + str(v))

    class Derived(Base):
        pass

    d = Derived()
    d.x = 1
    return d.x


def setattr_added_after_instances_exist():
    class C:
        pass

    c = C()
    c.p = 1
    before = c.p
    C.__setattr__ = lambda self, k, v: object.__setattr__(self, k, v * 2)
    c.p = 3
    return before, c.p


def setattr_removed_again():
    class C:
        def __setattr__(self, k, v):
            object.__setattr__(self, k, v * 2)

    c = C()
    c.q = 1
    out = [c.q]
    del C.__setattr__
    c.q = 1
    out.append(c.q)
    return out


def slots_still_work():
    class S:
        __slots__ = ("a",)

        def __setattr__(self, k, v):
            object.__setattr__(self, k, v + 100)

    s = S()
    s.a = 1
    return s.a


def a_class_without_them_is_untouched():
    class Plain:
        pass

    p = Plain()
    total = 0
    for i in range(200):
        p.n = i
        total += p.n
    del p.n
    return total, hasattr(p, "n")


print(setattr_intercepts())
print(delattr_intercepts())
print(setattr_can_refuse())
print(inherited_setattr())
print(setattr_added_after_instances_exist())
print(setattr_removed_again())
print(slots_still_work())
print(a_class_without_them_is_untouched())

# The dunders are visible where they always were.
class R2:
    def __setattr__(self, k, v):
        object.__setattr__(self, k, v)

    def __delattr__(self, k):
        object.__delattr__(self, k)


print("__setattr__" in R2.__dict__, "__delattr__" in R2.__dict__)
print(R2.__setattr__ is not object.__setattr__)

# Writing to a CLASS still goes through the type's own store, which is what
# `super().__delattr__(name)` from a metaclass has to reach.
class Meta(type):
    def __delattr__(cls, name):
        super().__delattr__(name)


class K(metaclass=Meta):
    doomed = 1


print(K.doomed)
del K.doomed
print(hasattr(K, "doomed"))
K.renamed = 2
print(K.renamed)
