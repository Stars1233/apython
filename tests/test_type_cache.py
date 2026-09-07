# Resolving an attribute through a class walks its MRO probing each tp_dict,
# and type_lookup_cached keeps the answer against the class's version.  The
# cache has no invalidation of its own -- a stale entry fails the version
# compare -- so what has to be tested is that the version really moves on every
# way a class's answer can change, and that the cache cannot confuse two names.


def method_redefined_mid_loop():
    class C:
        def f(self):
            return 1

    c = C()
    out = []
    for i in range(6):
        if i == 3:
            C.f = lambda self: 2
        out.append(c.f())
    return out


def method_added_to_a_base():
    class Base:
        pass

    class Derived(Base):
        pass

    d = Derived()
    out = []
    for i in range(6):
        if i == 3:
            Base.g = lambda self: "from base"
        out.append(getattr(d, "g", "missing"))
    return [x if isinstance(x, str) else x() for x in out]


def method_deleted():
    class C:
        def h(self):
            return "here"

    c = C()
    out = [c.h()]
    del C.h
    out.append(getattr(c, "h", "gone"))
    C.h = lambda self: "back"
    out.append(c.h())
    return out


def negative_then_positive():
    """A miss is cached too, so the version has to invalidate it."""
    class C:
        pass

    c = C()
    out = []
    for i in range(6):
        out.append(hasattr(c, "later"))
        if i == 2:
            C.later = 7
    return out


def two_names_one_class():
    """The cache compares names by pointer and must not confuse two of them."""
    class C:
        a = 1
        b = 2
        c = 3

    o = C()
    total = 0
    for _ in range(200):
        total += o.a * 100 + o.b * 10 + o.c
    return total


def names_built_at_runtime():
    """A name that is not interned is freed after the lookup, and the
    allocator can hand its address to the next one.  The visitor pattern in
    ast.NodeVisitor is exactly this shape, and it dispatched to the wrong
    method until the cache started holding a reference to the name."""
    class Visitor:
        def visit_Alpha(self):
            return "alpha"

        def visit_Beta(self):
            return "beta"

        def visit_Gamma(self):
            return "gamma"

        def generic(self):
            return "generic"

    v = Visitor()
    out = []
    for _ in range(200):
        for kind in ("Alpha", "Beta", "Gamma", "Delta"):
            meth = getattr(v, "visit_" + kind, v.generic)
            out.append(meth())
    return out[:8], len(out), sorted(set(out))


def mixin_inserted_by_metaclass():
    class Meta(type):
        def __new__(mcls, name, bases, ns):
            ns["injected"] = "yes"
            return super().__new__(mcls, name, bases, ns)

    class C(metaclass=Meta):
        pass

    c = C()
    return [c.injected for _ in range(5)]


def subclass_shadows_then_stops():
    class Base:
        def which(self):
            return "base"

    class Sub(Base):
        def which(self):
            return "sub"

    s = Sub()
    out = [s.which()]
    del Sub.which
    out.append(s.which())
    Sub.which = lambda self: "sub again"
    out.append(s.which())
    return out


def dunder_through_the_cache():
    class C:
        def __len__(self):
            return 3

    c = C()
    out = [len(c)]
    C.__len__ = lambda self: 5
    out.append(len(c))
    return out


def instances_of_many_classes():
    classes = []
    for i in range(40):
        classes.append(type("K%d" % i, (), {"tag": i, "get": lambda self: self.tag}))
    objs = [k() for k in classes]
    total = 0
    for _ in range(20):
        for o in objs:
            total += o.get()
    return total


print(method_redefined_mid_loop())
print(method_added_to_a_base())
print(method_deleted())
print(negative_then_positive())
print(two_names_one_class())
print(names_built_at_runtime())
print(mixin_inserted_by_metaclass())
print(subclass_shadows_then_stops())
print(dunder_through_the_cache())
print(instances_of_many_classes())

# A property added to a live class must start intercepting.
class P:
    pass


p = P()
p.v = 1
before = p.v
P.v = property(lambda self: "prop")
print(before, p.v)

# And a static type's attributes still resolve (they are never cached).
print("".join.__name__, (3).bit_length(), [].append.__name__)
