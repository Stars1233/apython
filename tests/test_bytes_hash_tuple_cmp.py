# Two bugs found while checking what posix needs, both of the same shape:
# a type test that was too narrow, and a dict that looked fine at small sizes.
#
# bytes had no tp_hash at all.  obj_hash falls through to the object's ADDRESS
# when a type has none, so two equal byte strings hashed differently and every
# dict and set holding them was silently wrong.  A handful of keys hides it
# completely -- dict_lookup probes and compares keys, so a short linear walk
# still finds the entry -- which is why this survived: at 200 keys, zero
# lookups succeed and a set of 200 distinct values holds 400.  posix.environ
# is a dict[bytes, bytes], which is how it came up.
#
# tuple_richcompare required the RIGHT operand's type to be EXACTLY tuple.  It
# is the one comparison in the family still doing that by hand rather than
# through REQUIRE_TUPLE_TYPE, so T([1,2]) == T([1,2]) was False -- both sides
# declined and the protocol ran out of places to ask.  os.stat() returns a
# tuple subclass, so every comparison of two stat results would have been.

print("--- bytes hash: equal values, equal hashes ---")
print("equal          :", hash(b"abc") == hash(b"ab" + b"c"))
print("empty          :", hash(b"") == hash(b""))
print("long           :", hash(b"x" * 500) == hash(b"x" * 500))
print("differs        :", hash(b"abc") != hash(b"abd"))
print("is an int      :", type(hash(b"abc")).__name__)

print()
print("--- at a size no linear probe can rescue ---")
d = {}
for i in range(200):
    d[("k%03d" % i).encode()] = i
found = sum(1 for i in range(200) if d.get(("k%03d" % i).encode()) == i)
print("dict lookups   :", found, "of 200")
print("dict len       :", len(d))
print("membership     :", sum(1 for i in range(200)
                              if ("k%03d" % i).encode() in d))

s = set()
for i in range(200):
    s.add(("k%03d" % i).encode())
    s.add(("k%03d" % i).encode())
print("set size       :", len(s))
print("set membership :", (b"k042" in s), (b"k999" in s))

fs = frozenset(("v%03d" % i).encode() for i in range(100)) | \
     frozenset(("v%03d" % i).encode() for i in range(100))
print("frozenset size :", len(fs))

print()
print("--- bytes and str are separate keys even so ---")
m = {b"key": "bytes-value", "key": "str-value"}
print("len            :", len(m))
print("bytes key      :", m[b"key"])
print("str key        :", m["key"])

print()
print("--- deleting and re-adding finds the same slot ---")
d2 = {}
for i in range(60):
    d2[("d%02d" % i).encode()] = i
for i in range(0, 60, 2):
    del d2[("d%02d" % i).encode()]
print("after deletes  :", len(d2))
print("odd survives   :", d2.get(b"d07"))
print("even gone      :", d2.get(b"d08", "gone"))
for i in range(0, 60, 2):
    d2[("d%02d" % i).encode()] = i * 10
print("re-added       :", len(d2), d2.get(b"d08"))

print()
print("--- bytearray stays unhashable, as CPython has it ---")
try:
    hash(bytearray(b"x"))
    print("bytearray      : hashable")
except TypeError:
    print("bytearray      : TypeError")


class T(tuple):
    pass


class U(tuple):
    pass


print()
print("--- a tuple subclass compares by contents ---")
a, b = T([1, 2]), T([1, 2])
c = T([1, 3])
print("sub == sub     :", a == b)
print("sub != sub     :", a != c)
print("tuple == sub   :", (1, 2) == a)
print("sub == tuple   :", a == (1, 2))
print("sub < sub      :", a < c)
print("sub > tuple    :", c > (1, 2))
print("sub <= tuple   :", a <= (1, 2))
print("across classes :", T([1, 2]) == U([1, 2]))
print("unequal length :", T([1]) == T([1, 2]))
print("empty          :", T([]) == ())

print()
print("--- and sorts, and works in the containers ---")
print("sorted         :", sorted([T([2]), (1,), T([3])]))
print("min            :", min([T([2]), T([1])]))
print("max            :", max([T([2]), (3,)]))
print("in a list      :", T([1, 2]) in [(1, 2), (3, 4)])
print("as a dict key  :", {T([1, 2]): "found"}[(1, 2)])
print("in a set       :", T([1, 2]) in {(1, 2)})
print("set dedup      :", len({T([1, 2]), (1, 2)}))
print("count          :", [(1, 2), T([1, 2])].count((1, 2)))
print("index          :", [(9,), T([1, 2])].index((1, 2)))

print()
print("--- nested, so the element comparison recurses ---")
print("nested equal   :", T([(1, 2), (3, 4)]) == ((1, 2), (3, 4)))
print("nested sub     :", T([T([1]), T([2])]) == ((1,), (2,)))
print("bytes inside   :", T([b"a", b"b"]) == (b"a", b"b"))
