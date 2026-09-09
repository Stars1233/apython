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

# --- a pair that is already a list or a tuple is read where it lies -------
# dict(pairs) used to copy the whole sequence into a tuple AND allocate a
# fresh two-element tuple for every pair in it -- one allocation per element,
# to read two words that were already contiguous.  CPython's PySequence_Fast
# hands a list or a tuple straight back, and this does the same.
#
# What the shortcut must not change: a pair of any other shape is still
# materialised, a two-element ITERABLE is still accepted, a wrong length is
# still a ValueError naming the element, something not iterable at all is
# still a TypeError, and a list or tuple SUBCLASS with its own __iter__ must
# take the slow path, because its __iter__ is what the answer depends on.
print(dict([(1, 2), (3, 4)]), dict(((1, 2), (3, 4))), dict([[1, 2], [3, 4]]))
print(dict([(1, 2), [3, 4]]), dict(x for x in [(1, 2), (3, 4)]))
print(dict(zip("ab", [1, 2])), dict([("k", "v")]), dict([]), dict(()), dict(""))
acc = {}
acc.update([(1, 2)])
acc.update(((3, 4),))
acc.update([[5, 6]])
acc.update(x for x in [(7, 8)])
print(acc)


class Pair:
    def __init__(self, a, b):
        self.a = a
        self.b = b

    def __iter__(self):
        return iter((self.a, self.b))


print(dict([Pair("i", 1)]), dict([Pair("i", 1), ("t", 2), ["l", 3]]))
print(dict(["ab", "cd"]))

for bad in ([(1,)], [(1, 2, 3)], [[1]], [[]], [()], ["abc"], [range(3)]):
    try:
        dict(bad)
    except ValueError as e:
        print("ValueError", e)
for bad in ([1], [None], [1.5], [object()]):
    try:
        dict(bad)
    except TypeError as e:
        print("TypeError", e)


class ListSub(list):
    def __iter__(self):
        return iter([9, 9])


class TupleSub(tuple):
    def __iter__(self):
        return iter([8, 8])


print(dict([ListSub([1, 2])]), dict([TupleSub((1, 2))]))
# ... and as the OUTER sequence too: their __iter__ yields 9 and 8, which are
# not pairs, so the refusal is the proof that the fast arm was not taken.
for outer in (ListSub([(1, 2)]), TupleSub(((1, 2),))):
    try:
        dict(outer)
    except TypeError as e:
        print("TypeError", e)

seen8 = []


class Watch8:
    def __init__(self, tag):
        self.tag = tag

    def __del__(self):
        seen8.append(self.tag)


def churn8():
    pairs = [("k", Watch8("v"))]
    a = dict(pairs)
    b = {}
    b.update(pairs)
    del a, b, pairs


churn8()
print(seen8)

src = [(1, 2), (3, 4)]
made = dict(src)
made[5] = 6
print(src, made)
