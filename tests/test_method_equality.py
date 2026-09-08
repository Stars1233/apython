"""Two bound methods for the same function and receiver compare equal.

CPython compares `__self__` by identity and `__func__` by equality, and hashes
`hash(__self__) ^ hash(__func__)`.  Here `method` had neither `tp_richcompare`
nor `tp_hash`, so both fell back to identity -- and every attribute load builds
a fresh wrapper, so `c.m == c.m` was False.

It matters to any code that keeps a callback and later asks whether it already
has one: removing a handler from a list of them is the usual shape, and
`unittest` and `functools` both do it.

A test of `__hash__` alone is not enough and can pass while the bug is live.
`hash(c.m) == hash(c.m)` builds the first wrapper, hashes it, frees it, and the
allocator hands the second wrapper the same address -- so hashing by address
gives the same answer twice for the wrong reason.  Holding both alive at once
is what makes the question real, so every check below binds to a variable
first.

`method_type.tp_dict` was 0 as well, so even with the slots filled `m.__eq__`
and `m.__hash__` did not exist by name.  The stdlib asks by name.
"""


class C:
    def m(self, v):
        return v

    def other(self):
        return 1


class D:
    def m(self, v):
        return v


c = C()
c2 = C()
d = D()

print("--- equality ---")
a, b = c.m, c.m
print("same receiver, same function:", a == b)
print("not identical though:", a is b)
print("different receiver:", c.m == c2.m)
print("different function:", c.m == c.other)
print("different class:", c.m == d.m)
print("against a non-method:", c.m == 1, c.m == None, c.m == c)
print("!= agrees:", a != b, c.m != c2.m)

print("--- hashing ---")
ha, hb = hash(c.m), hash(c.m)
print("equal objects hash equal:", ha == hb)
# The real question: both alive at once, so no address can be reused.
x, y = c.m, c.m
print("both alive:", x == y, hash(x) == hash(y))
print("hash is stable:", hash(x) == hash(x))

print("--- the invariant ---")
pairs = [(c.m, c.m), (c.other, c.other), (c2.m, c2.m), (d.m, d.m)]
print("equal implies equal hash:",
      all((p == q) <= (hash(p) == hash(q)) for p, q in pairs))

print("--- usable as a dict key and in a set ---")
reg = {}
reg[c.m] = "cm"
reg[c2.m] = "c2m"
reg[c.other] = "cother"
print("dict size:", len(reg))
print("lookup by a fresh wrapper:", reg[c.m], reg[c2.m], reg[c.other])
print("set collapses duplicates:", len({c.m, c.m, c.m}))
print("set keeps distinct:", len({c.m, c2.m, c.other, d.m}))

print("--- the callback-removal shape ---")
handlers = []
handlers.append(c.m)
handlers.append(c2.m)
print("in:", c.m in handlers, c2.m in handlers, d.m in handlers)
handlers.remove(c.m)
print("after remove:", len(handlers), c.m in handlers, c2.m in handlers)
print("index:", [c.m, c2.m].index(c2.m))

print("--- reachable by name ---")
print("__eq__:", c.m.__eq__(c.m), c.m.__eq__(c2.m))
print("__hash__:", c.m.__hash__() == c.m.__hash__())
print("__ne__:", c.m.__ne__(c.m))
print("hasattr:", hasattr(c.m, "__eq__"), hasattr(c.m, "__hash__"))

print("--- an unhashable receiver ---")


class Unhashable:
    __hash__ = None

    def m(self):
        return 1


u = Unhashable()
print("equality still works:", u.m == u.m)
try:
    hash(u.m)
    print("hash: accepted - wrong")
except TypeError as e:
    print("hash:", type(e).__name__)

print("--- builtin methods ---")
s = "abc"
print("same builtin method:", s.upper == s.upper)
lst = [1]
print("list method:", lst.append == lst.append)
print("different lists:", [1].append == [1].append)

print("--- a method of a method ---")
print("__func__ identity:", c.m.__func__ is C.m)
print("__self__ identity:", c.m.__self__ is c)

print("done")
