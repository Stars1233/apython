# A dict SUBCLASS answers its own misses, through __missing__.
#
# dict_subscript raised KeyError on any miss, whatever the mapping was, so a
# subclass defining __missing__ never saw one.  CPython's asks __missing__
# whenever the object is not EXACTLY a dict, which is what makes
# collections.defaultdict work -- and, less obviously, urllib.parse.quote,
# which memoises its per-character table in a dict subclass and so raised
# KeyError for any character it had not already quoted.
#
# Only __getitem__ consults it: .get(), `in` and .setdefault() do not, in
# CPython either.

import collections


class Missing(dict):
    def __missing__(self, key):
        return "made %r" % (key,)


m = Missing()
print(m["absent"], "a miss goes to __missing__")
m["present"] = 1
print(m["present"], "a hit does not")
print(m.get("absent"), ".get() does not consult it")
print("absent" in m, "`in` does not consult it")
print(m.setdefault("s", 2), ".setdefault() does not consult it")
print(sorted(m), "and nothing was added by the miss")

# Reached through the unbound slot too, since it is the same one.
print(dict.__getitem__(Missing(), "k"), "dict.__getitem__ consults it")

# A plain dict never does, and a subclass without one still raises.
try:
    {}["q"]
    print(False, "a plain dict must raise")
except KeyError as e:
    print(True, "a plain dict raises KeyError", e)


class Plain(dict):
    pass


try:
    Plain()["q"]
    print(False, "a subclass with no __missing__ must raise")
except KeyError as e:
    print(True, "a subclass with no __missing__ raises", e)


# Whatever __missing__ raises is what the caller sees.
class Angry(dict):
    def __missing__(self, key):
        raise ValueError("no such %s" % key)


try:
    Angry()["q"]
    print(False, "a raising __missing__ must propagate")
except ValueError as e:
    print(True, "a raising __missing__ propagates:", e)


# What __missing__ RETURNS is a Value, and an int or a float is an immediate
# rather than a pointer -- unpacking one on the way out turned 42 into a
# pointer to address 42.  Every kind, therefore, not just the str this test
# used to check.
class Kinds(dict):
    def __init__(self, answer):
        self.answer = answer

    def __missing__(self, key):
        return self.answer


for answer in (42, -7, 2 ** 70, 1.5, -0.0, None, True, False, "s", (1, 2), [3]):
    got = Kinds(answer)["k"]
    print(repr(got), got == answer, "__missing__ returning %s" % type(answer).__name__)


# The one in the standard library.
d = collections.defaultdict(list)
d["a"].append(1)
d["b"].append(2)
print(dict(d), "defaultdict builds its default")

counts = collections.defaultdict(int)
for ch in "abracadabra":
    counts[ch] += 1
print(sorted(counts.items()), "defaultdict(int) counts")

# And a __missing__ that is inherited rather than defined on the class itself.
class Base(dict):
    def __missing__(self, key):
        return "base"


class Derived(Base):
    pass


print(Derived()["k"], "an inherited __missing__")
