# Two small ones.
#
# A module without a docstring still HAS __doc__ -- CPython binds it to None
# when the module dict is made.  Reading it here was a NameError.
#
# And the container repr's cycle stack was a fixed 64 entries, so a structure
# nested deeper than that reported RecursionError where CPython prints it.
# The bound that belongs there is sys.setrecursionlimit's, which is what the
# repr's own recursion is measured against.

print("=== a module with no docstring ===")
print(__doc__)
print("__doc__" in globals(), __doc__ is None)

print("=== deeply nested reprs ===")
# Built rather than eval'd past a hundred: nesting that deep is a limit of
# the *parser* here, which is a different subsystem's business.
for n in (1, 8, 63, 64, 65, 100):
    v = eval("[" * n + "1" + "]" * n)
    r = repr(v)
    print(n, len(r), r[:3], r[-3:])

for n in (65, 100, 300, 700):
    v = 1
    for _ in range(n):
        v = [v]
    print(n, len(repr(v)))

for n in (65, 300):
    v = (1,)
    for _ in range(n):
        v = (v,)
    print(n, len(repr(v)))

d = {}
cur = d
for i in range(300):
    cur["k"] = {}
    cur = cur["k"]
print("dict", len(repr(d)))

print("=== a cycle is still ... and not a recursion error ===")
a = []
a.append(a)
print(repr(a))
b = {}
b["self"] = b
print(repr(b))
c = [1, [2, [3]]]
c[1][1].append(c)
print(repr(c))

print("=== a cycle nested deeply ===")
outer = []
cur = outer
for i in range(100):
    nxt = []
    cur.append(nxt)
    cur = nxt
cur.append(outer)
r = repr(outer)
print(len(r), r.endswith("]" * 101), "..." in r)

print("=== and the limit is still a limit ===")
import sys
old = sys.getrecursionlimit()
print(old > 100)
