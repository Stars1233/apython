# __build_class__ left every tp_as_*, tp_iter, tp_iternext, tp_hash, tp_call
# and tp_richcompare at zero on a heaptype, so dispatch to a user class was
# wired ad-hoc, one operation at a time, wherever somebody remembered.  Of the
# 163 slot reads in the tree, 130 had no dunder fallback: sorted(MyIterator())
# called through a NULL tp_iternext, and any(MyIterable()) raised TypeError,
# while list() and a for-loop over the same object worked fine.
#
# Slots are now installed from the class's dunders at creation, so every
# reader is correct without being edited.


class Counter:
    def __init__(self, n):
        self.n = n
        self.i = 0

    def __iter__(self):
        return self

    def __next__(self):
        self.i += 1
        if self.i > self.n:
            raise StopIteration
        return self.i


class Wrapper:
    def __init__(self, items):
        self.items = items

    def __iter__(self):
        return iter(self.items)


w = Wrapper([10, 20, 30])

# Every consumer of the iterator protocol
print(list(Counter(3)), tuple(w), sorted(w), sorted(w, reverse=True))
print(sum(w), min(w), max(w), any(w), all(w), len(list(w)))
print(list(zip(Counter(3), "abc")), list(map(str, Counter(3))))
print(list(filter(None, Counter(3))), list(enumerate(w)))
print(set(w) == {10, 20, 30}, dict.fromkeys(w) == {10: None, 20: None, 30: None})

acc = []
acc.extend(w)
print(acc)
acc2 = [0]
acc2 += w
print(acc2)

print([x for x in w], {x for x in w}, {x: 1 for x in w})
for x in Counter(2):
    print("loop", x)

a, b, c = w
print(a, b, c)
first, *rest = w
print(first, rest)

# Unpacking now works for every iterable, not just exact tuple/list/str
p, q = {1, 2} if len({1, 2}) == 2 else (1, 2)
print(sorted([p, q]))
r, s = range(2)
print(r, s)
t, u = (i * i for i in range(2))
print(t, u)
v, x = b"ab"
print(v, x)
y, z = {"k": 1, "j": 2}
print(sorted([y, z]))


def raises(fn, *a):
    try:
        fn(*a)
    except Exception as e:
        return type(e).__name__
    return "no error"


print(raises(lambda: [1, 2] == [1]), raises(list, 5))
print([raises(lambda v=v: iter(v)) for v in (5, 1.5, None, object())])


# A __next__ that raises something other than StopIteration propagates
class Angry:
    def __iter__(self):
        return self

    def __next__(self):
        raise ValueError("angry")


print(raises(list, Angry()), raises(sorted, Angry()))


# An __iter__ that raises reaches the caller
class BadIter:
    def __iter__(self):
        raise KeyError("baditer")


print(raises(list, BadIter()), raises(sum, BadIter()))


# A dunder set to None disables the protocol rather than falling back
class Blocked:
    def __iter__(self):
        while False:
            yield None

    __contains__ = None


print(raises(lambda: 1 in Blocked()))


# A dunder must not inherit the keywords of the call that triggered it
class Keyworded:
    def __iter__(self):
        return iter([3, 1, 2])


print(sorted(Keyworded(), reverse=True))
