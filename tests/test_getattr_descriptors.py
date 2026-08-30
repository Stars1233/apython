# getattr() must run the descriptor protocol, exactly as attribute syntax does.
#
# It did not: `p.v` invoked a property and `getattr(p, "v")` returned nothing at
# all -- not an AttributeError, an empty value that printed as blank and
# compared equal to nothing.  classmethod and staticmethod went the same way.
# The two spellings have to agree; anything that reaches an attribute by name
# rather than by syntax otherwise sees a different object model, and
# collections.namedtuple is one of those things.
class P:
    plain = 5

    def meth(self):
        return "meth"

    @property
    def prop(self):
        return "prop"

    @classmethod
    def cm(cls):
        return "cm:" + cls.__name__

    @staticmethod
    def sm():
        return "sm"

    lam = property(lambda s: "lam")


class Sub(P):
    pass


class T(tuple):
    first = property(lambda s: s[0])


p = P()
for name in ("plain", "prop", "lam"):
    print(name, repr(getattr(p, name)))

print(getattr(p, "meth")(), getattr(p, "cm")(), getattr(p, "sm")())
print(getattr(P, "cm")(), getattr(P, "sm")())

s = Sub()
print(getattr(s, "prop"), getattr(s, "lam"), getattr(s, "cm")())

t = T((7, 8))
print(t.first, getattr(t, "first"))

# The two spellings agree, whatever the attribute is.
for name in ("plain", "prop", "lam"):
    assert getattr(p, name) == eval("p." + name), name
print("attribute syntax and getattr agree")

# Defaults, hasattr and the error still behave.
print(getattr(p, "nope", "dflt"), hasattr(p, "prop"), hasattr(p, "nope"))
try:
    getattr(p, "nope")
    print("no error")
except AttributeError:
    print("AttributeError")

# A property that raises propagates, rather than reading as absent.
class Boom:
    @property
    def bad(self):
        raise ValueError("boom")


try:
    getattr(Boom(), "bad")
    print("no error")
except ValueError as e:
    print("ValueError", e)
# hasattr only swallows AttributeError; anything else propagates.
try:
    hasattr(Boom(), "bad")
    print("hasattr swallowed it")
except ValueError:
    print("hasattr propagated ValueError")
