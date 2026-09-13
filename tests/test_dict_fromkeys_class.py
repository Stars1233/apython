# dict.fromkeys() answers the class it was called on.
#
# It is a classmethod, and its first argument is the class -- which it ignored,
# building a plain dict every time.  So `OrderedDict.fromkeys("ab")` was an
# ordinary dict, and so was every subclass's, silently: the contents are right
# and only the type is wrong, which is the kind of answer that surfaces
# somewhere else entirely.
#
# CPython builds the exact-dict case directly and calls the class with no
# arguments otherwise, which is why a subclass whose __init__ demands an
# argument cannot be fromkeys'd there either.


class Plain(dict):
    pass


class Counted(dict):
    made = 0

    def __init__(self, *a, **k):
        type(self).made += 1
        super().__init__(*a, **k)


print("dict:", type(dict.fromkeys("ab")).__name__, dict.fromkeys("ab"))
print("dict with value:", dict.fromkeys("ab", 0))
print("subclass type:", type(Plain.fromkeys("ab", 1)).__name__)
print("subclass contents:", dict(Plain.fromkeys("ab", 1)))
print("subclass is a dict:", isinstance(Plain.fromkeys("ab"), dict))

# The class is CONSTRUCTED, so its __init__ runs exactly once.
Counted.made = 0
c = Counted.fromkeys("abc", 0)
print("constructed once:", Counted.made, type(c).__name__, dict(c))

# The iterable may be anything iterable, and the value defaults to None.
print("from list:", dict.fromkeys([1, 2, 3]))
print("from generator:", dict.fromkeys(x for x in "xy"))
print("from dict:", dict.fromkeys({"k": "ignored"}, 5))
print("from range:", dict.fromkeys(range(3), "v"))
print("empty:", dict.fromkeys([]), type(Plain.fromkeys([])).__name__)

# Duplicate keys collapse, keeping the first position and the last value.
print("duplicates:", dict.fromkeys("aabbc", 1))

# The value is SHARED, not copied -- the documented trap.
shared = dict.fromkeys("ab", [])
shared["a"].append(1)
print("shared value:", shared)

# A non-iterable is a TypeError, and it names the type.
try:
    dict.fromkeys(5)
except TypeError as e:
    print("non-iterable:", "not iterable" in str(e))

# An iterator that raises leaves the exception, rather than a short dict.
def angry():
    yield "a"
    raise ValueError("halfway")


try:
    dict.fromkeys(angry())
except ValueError as e:
    print("raising iterable:", e)

# collections' own subclasses, which is what this is for.
import collections

od = collections.OrderedDict.fromkeys("abc", 0)
print("OrderedDict:", type(od).__name__, list(od.items()))

dd = collections.defaultdict.fromkeys("ab", 0)
print("defaultdict:", type(dd).__name__, dict(dd))

# fromkeys is a classmethod, so it is reachable through an instance too.
print("through an instance:", type(Plain().fromkeys("a")).__name__)

print("done")
