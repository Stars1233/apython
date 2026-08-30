# A builtin container's __len__ and __iter__ have to be reachable by name.
# They were slots and nothing else, so `cache.__len__` -- how functools'
# lru_cache reads its size without going through len() -- raised
# AttributeError, and re could not import.
for t in (dict, list, tuple, str, set, frozenset, bytes):
    print(t.__name__, hasattr(t, "__len__"), hasattr(t, "__iter__"))

d = {"a": 1, "b": 2}
print(d.__len__(), len(d))
print(sorted(d.__iter__()))

l = [1, 2, 3]
print(l.__len__(), list(l.__iter__()))

t = (4, 5)
print(t.__len__(), list(t.__iter__()))

s = "abc"
print(s.__len__(), list(s.__iter__()))

st = {7, 8}
print(st.__len__(), sorted(st.__iter__()))

fs = frozenset([9])
print(fs.__len__(), list(fs.__iter__()))

b = b"xyz"
print(b.__len__(), list(b.__iter__()))

# Unbound, through the type, is how the stdlib often reaches them.
print(dict.__len__(d), list.__len__(l), str.__len__(s))

# The bound method is a real object that can be stashed and called later.
cache_len = d.__len__
d["c"] = 3
print(cache_len())

# Empty containers.
print({}.__len__(), [].__len__(), "".__len__(), set().__len__())

# A subclass inherits both.
class L(list):
    pass


x = L([1, 2])
print(x.__len__(), list(x.__iter__()))
