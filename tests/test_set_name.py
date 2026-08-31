# __set_name__ is called on every class-body value that defines it, once the
# class exists, with (owner, name).
#
# It is the hook a descriptor uses to learn what it was assigned to, and enum
# is built on it: each member starts as a _proto_member in the class body and
# __set_name__ is what replaces it with the real member and records the name in
# _member_names_.  Without it an enum class has no members at all, while
# Color.RED still answers -- as the proto object it never stopped being.
class Named:
    def __init__(self, tag):
        self.tag = tag
        self.owner = None
        self.name = None

    def __set_name__(self, owner, name):
        self.owner = owner.__name__
        self.name = name


class Replacing:
    def __init__(self, v):
        self.v = v

    def __set_name__(self, owner, name):
        setattr(owner, name, ("replaced", name, self.v))


class Counter:
    seen = []

    def __set_name__(self, owner, name):
        Counter.seen.append(name)


class C:
    a = Named("A")
    b = Named("B")
    plain = 5
    r = Replacing(1)
    c1 = Counter()
    c2 = Counter()

    def m(self):
        return "m"


print(C.a.owner, C.a.name, C.b.name)
print(C.r)
print(sorted(Counter.seen))
print(C.plain, C().m())

# Inheritance: the hook runs for the class that defines the attribute.
class D(C):
    d = Named("D")


print(D.d.owner, D.d.name, D.a.name)

# A value without __set_name__ is untouched, and so is one whose __set_name__
# lives on the instance rather than the type.
class Inst:
    pass


i = Inst()
i.__set_name__ = lambda o, n: 1 / 0


class E:
    ok = i


print(type(E.ok).__name__)

# The hook runs after the class exists, so it can reach the rest of it.
probe_seen = []


class Probe:
    def __set_name__(self, owner, name):
        probe_seen.append((owner.__name__, name, hasattr(owner, "later")))


class F:
    p = Probe()
    later = 1


print(probe_seen)
