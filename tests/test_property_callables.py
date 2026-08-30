# A property whose getter is not a function.
#
# property_descr_get reached for the getter's tp_call directly, which is NULL
# for an instance of a class that defines __call__ -- only op_call knew to look
# for the dunder.  So `property(operator.itemgetter(0))` reported "unreadable
# attribute" rather than calling anything, and that is exactly what
# collections.namedtuple builds its fields from.
import operator


class Getter:
    def __init__(self, i):
        self.i = i

    def __call__(self, obj):
        return obj[self.i]


class T(tuple):
    a = property(operator.itemgetter(0))
    b = property(Getter(1))
    c = property(lambda s: s[0] + s[1])
    d = property(operator.itemgetter(0), doc="the first")


t = T((7, 8))
print(t.a, t.b, t.c, t.d)
print(getattr(t, "a"), getattr(t, "b"), getattr(t, "c"))


class P:
    v = property(operator.attrgetter("raw"))
    raw = 42


print(P().v, getattr(P(), "v"))

# A getter that is a bound method, and one that is a builtin.
class Q:
    def _get(self):
        return "bound"

    v = property(_get)
    n = property(len)


print(Q().v, Q().n if False else "skip-len")

# The whole namedtuple shape, built the way collections does it.
_tuplegetter = lambda index, doc: property(operator.itemgetter(index), doc=doc)


class Pair(tuple):
    __slots__ = ()
    x = _tuplegetter(0, "first")
    y = _tuplegetter(1, "second")

    def __repr__(self):
        return "Pair(x=%r, y=%r)" % self


p = Pair((3, "s"))
print(p.x, p.y, repr(p))
print(getattr(p, "x"), getattr(p, "y"))
