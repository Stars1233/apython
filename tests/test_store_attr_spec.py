# STORE_ATTR rewrites itself into an in-place write of the instance dict entry
# once it has seen an ordinary attribute store.  The guard is a type VERSION --
# four bytes, because STORE_ATTR has only eight cache bytes where LOAD_ATTR has
# eighteen -- and the version is stamped fresh by type_refresh_attr_flags every
# time a class or one of its bases is written to, and pushed down every
# subclass.
#
# So the cases that matter are the ones that change a class AFTER instances of
# it exist and a store site has already specialized.  A missed invalidation
# here is a silently wrong answer, not a crash, which is the worst kind.


def plain_store_and_load():
    class P:
        def __init__(self, x, y):
            self.x = x
            self.y = y

    p = P(1, 2)
    total = 0
    for i in range(500):
        p.x = i
        p.y = p.x + 1
        total += p.y
    return total, p.x, p.y


def property_added_after_specializing():
    """The site specializes on a plain attribute, then the class grows a
    property with the same name.  Every later store must run the setter."""
    class C:
        def __init__(self):
            self.v = 0

    c = C()
    seen = []
    for i in range(6):
        if i == 3:
            # A data descriptor now outranks the instance dict.
            C.v = property(lambda self: self._v * 100,
                           lambda self, n: setattr(self, "_v", n))
        c.v = i
        seen.append(c.v)
    return seen


def base_gains_a_property():
    """The property arrives on a BASE after the subclass's site specialized.
    The invalidation has to reach the subclass, which is what the walk over
    subclasses in type_refresh_attr_flags is for."""
    class Base:
        pass

    class Derived(Base):
        def __init__(self):
            self.w = 0

    d = Derived()
    seen = []
    for i in range(6):
        if i == 3:
            Base.w = property(lambda self: self._w + 1000,
                              lambda self, n: setattr(self, "_w", n))
        d.w = i
        seen.append(d.w)
    return seen


def slots_never_specialize():
    class S:
        __slots__ = ("a", "b")

    s = S()
    total = 0
    for i in range(200):
        s.a = i
        s.b = s.a * 2
        total += s.b
    return total, s.a, s.b


def delete_then_restore():
    class U:
        pass

    u = U()
    u.p = 0
    out = []
    for i in range(5):
        u.p = i
        out.append(u.p)
        del u.p
        out.append(hasattr(u, "p"))
        u.p = i * 100
        out.append(u.p)
    return out


def name_built_at_runtime():
    """dict_set keeps the first writer's key object, so an attribute created
    under a name that is not the interned constant can never satisfy the
    handler's pointer guard.  The site must refuse to specialize rather than
    specialize and deopt on every execution."""
    class T:
        pass

    t = T()
    n = "".join(["a", "b", "c"])
    setattr(t, n, 0)
    total = 0
    for i in range(300):
        t.abc = i          # the constant name; the entry's key is not it
        total += t.abc
    return total, t.abc


def two_classes_one_site():
    class A:
        def __init__(self):
            self.k = 0

    class B:
        def __init__(self):
            self.k = 0

    objs = [A(), B(), A(), B()]
    out = []
    for i in range(20):
        for o in objs:
            o.k = i
            out.append(o.k)
    return sum(out), [o.k for o in objs]


def inherited_attribute():
    class Base:
        def __init__(self):
            self.n = 0

    class Mid(Base):
        pass

    class Leaf(Mid):
        pass

    objs = [Base(), Mid(), Leaf()]
    for _ in range(100):
        for o in objs:
            o.n = o.n + 1
    return [o.n for o in objs]


def method_replaced_mid_loop():
    """Not a data descriptor, but still a class-dict write: the version moves
    and the store site has to survive it."""
    class M:
        def __init__(self):
            self.q = 0

        def get(self):
            return self.q

    m = M()
    out = []
    for i in range(6):
        if i == 3:
            M.get = lambda self: self.q * -1
        m.q = i
        out.append(m.get())
    return out


def dict_written_directly():
    class D:
        def __init__(self):
            self.z = 0

    d = D()
    out = []
    for i in range(6):
        d.z = i
        if i == 2:
            d.__dict__["z"] = 99
        out.append(d.z)
    return out


print(plain_store_and_load())
print(property_added_after_specializing())
print(base_gains_a_property())
print(slots_never_specialize())
print(delete_then_restore())
print(name_built_at_runtime())
print(two_classes_one_site())
print(inherited_attribute())
print(method_replaced_mid_loop())
print(dict_written_directly())

# A store to a module and to a class object must not take the instance path.
class K:
    pass


for _ in range(5):
    K.cls_attr = 7
print(K.cls_attr, "cls_attr" in K.__dict__)

# An object with no dict at all.
try:
    (1).foo = 2
except AttributeError as e:
    print("AttributeError")
