# Builtin type constructors must not make that type's INSTANCES callable.
# A constructor belongs in tp_new; tp_call on a type is what governs whether
# its instances can be called.

def not_callable(thing, label):
    try:
        thing()
    except TypeError:
        print("ok:", label)
    else:
        print("BUG: %s instance is callable" % label)

print("--- instances are not callable ---")
not_callable(42, "int")
not_callable(2**80, "big int")
not_callable("abc", "str")
not_callable([1, 2], "list")
not_callable((1, 2), "tuple")
not_callable({"a": 1}, "dict")
not_callable({1, 2}, "set")
not_callable(frozenset([1]), "frozenset")
not_callable(slice(1, 2), "slice")
not_callable(3.5, "float")
not_callable(True, "bool")
not_callable(b"xy", "bytes")

print("--- constructors still work ---")
print(int(), int("42"), int(7.9))
print(str(), str(42))
print(list(), list((1, 2)))
print(tuple(), tuple([1, 2]))
print(dict(), dict([("a", 1)]))
print(sorted(set([3, 1, 3])), sorted(frozenset([2, 1])))
print(float(), float("1.5"))
print(bool(), bool(1), bool(0))
s = slice(1, 5, 2)
print(s.start, s.stop, s.step)
print(list(range(10))[s])

print("--- exception constructors ---")
e = ValueError("boom")
print(type(e).__name__, str(e))
g = ExceptionGroup("grp", [ValueError("a"), TypeError("b")])
print(type(g).__name__, len(g.exceptions))
not_callable(e, "exception instance")

print("--- descriptors ---")
class C:
    @staticmethod
    def sm():
        return "sm"
    @classmethod
    def cm(cls):
        return "cm"
    @property
    def p(self):
        return "p"

c = C()
print(C.sm(), C.cm(), c.p)

print("--- classes are still callable ---")
print(type(C()).__name__)

class Callable:
    def __call__(self, x):
        return x * 2

print(Callable()(21))
