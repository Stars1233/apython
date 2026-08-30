# dict.update() takes any mapping, not just a dict.
#
# CPython's rule is one test: if the argument has a `keys` method it is a
# mapping and is read through keys() and indexing; otherwise it is a sequence
# of pairs.  Only the pair form was implemented, so updating from a
# mappingproxy -- which is what a type's __dict__ is -- reported
# "dictionary update sequence element has length != 2", and enum builds its
# classes with `classdict.update(enum_class.__dict__)`.
class C:
    a = 1
    b = 2


d = {}
d.update(C.__dict__)
print(sorted(k for k in d if not k.startswith("__")))
print(d["a"], d["b"])


class Mapping:
    def keys(self):
        return ["x", "y"]

    def __getitem__(self, k):
        return k.upper()


m = {}
m.update(Mapping())
print(sorted(m.items()))


class SubDict(dict):
    pass


s = {}
s.update(SubDict(p=1, q=2))
print(sorted(s.items()))

# The pair form, the keyword form, and both at once still work.
f = {}
f.update([("k", 1), ("m", 2)])
print(sorted(f.items()))
f.update({"n": 3})
f.update(o=4)
f.update([("p", 5)], r=6)
print(sorted(f.items()))
f.update()
print(len(f))

# A malformed pair is still an error, and so is a non-iterable.
try:
    # The wording differs: CPython names the element index and its length,
    # which needs a formatted message.
    {}.update([(1, 2, 3)])
except ValueError:
    print("ValueError")
try:
    {}.update(5)
except TypeError as e:
    print("TypeError")

# The constructor takes the same shapes.
print(sorted(dict(Mapping()).items()))
print(sorted(dict(C.__dict__).keys())[:0], "ctor ok")
