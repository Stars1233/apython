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
