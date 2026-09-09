# A dict view is set-like, and the operators say so.
#
# The three views had no number methods and no tp_richcompare at all, so
# `d.keys() - ["a"]` was a TypeError and `d.keys() == {"a", "b"}` was False.
# CPython gives them the four set operators over ANY iterable -- which is
# more than a set itself accepts -- and comparison against anything
# set-like.  csv.DictWriter is written against exactly that.

d = {"a": 1, "b": 2}
print(sorted(d.keys() - ["a"]))
print(sorted(d.keys() - {"a"}))
print(sorted(d.keys() | ["c"]))
print(sorted(d.keys() & ["a"]))
print(sorted(d.keys() ^ ["c"]))
print(sorted(d.items() - [("a", 1)]))
print(sorted(d.values()))
print(d.keys() == {"a", "b"}, d.keys() == {"a"}, d.keys() != {"a"})
print(d.keys() == d.keys(), d.keys() == ["a", "b"])
print(d.items() == {("a", 1), ("b", 2)})
print(d.keys() <= {"a", "b", "c"}, d.keys() < {"a", "b"},
      d.keys() >= {"a"}, d.keys() > {"a"})
print(sorted({"a": 1}.keys() | {"b": 2}.keys()))
try:
    d.keys() - 5
except TypeError as e:
    print("TypeError", e)

# --- comparison is lengths, then containment ------------------------------
# It used to be "build a set out of each side and compare those", which is
# wrong twice: an items view's VALUES need not be hashable and hashing them
# is not part of the question, and a VALUES view is not set-like at all --
# CPython gives it no tp_richcompare, so two of them compare by identity.
d = {"a": 1, "b": 2, "c": 3}
k, v, it = d.keys(), d.values(), d.items()

for other in ({"a", "b", "c"}, {"a", "b"}, {"a", "b", "c", "z"}, set(),
              frozenset({"a", "b", "c"}), {"z"}):
    print(sorted(other), k == other, k != other,
          k < other, k <= other, k > other, k >= other)
for other in ({"a": 0, "b": 0, "c": 0}.keys(), {"a": 0}.keys(), {}.keys()):
    print(sorted(other), k == other, k < other, k <= other, k > other, k >= other)
for other in ({("a", 1), ("b", 2), ("c", 3)}, {("a", 1)}, set()):
    print(sorted(other), it == other, it != other,
          it < other, it <= other, it > other, it >= other)
print(it >= {"a": 1}.items(), it > {"a": 1}.items(), it < {"a": 1}.items())

# a values view compares by identity, and has no ordering at all
print(v == v, v != v, d.values() == d.values(), v == {1, 2, 3}, v != {1, 2, 3})
try:
    print(v < d.values())
except TypeError as e:
    print("TypeError", e)
print(k == v, it == v, v == k)

# against something that is not set-like
for other in ([1], "ab", 5, None, (1,)):
    print(repr(other), k == other, k != other)
try:
    print(k < [1])
except TypeError as e:
    print("TypeError", e)

# an items view whose values are unhashable.  Every one of these raised
# TypeError, including the two the LENGTHS alone settle.
u = {"k": [1], "j": {2: 3}}
print(u.items() == {"k": [1], "j": {2: 3}}.items())
print(u.items() == {"k": [1]}.items(), u.items() >= {"k": [1]}.items())
print(u.items() != {"k": [9], "j": {2: 3}}.items())
print(u.items() == set(), u.keys() == {"k", "j"})

print({}.keys() == set(), {}.items() == set(), {}.keys() <= k, {}.items() <= it)
print(k == k, it == it, k <= k, k >= k, k < k, k > k)
print({"a", "b", "c"} == k, {"a"} < k, {"a", "b", "c", "z"} > k)
