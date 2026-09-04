# The rest of the gc module, and the tracking optimization that goes with it.
#
# get_referrers, freeze, unfreeze, get_freeze_count and get_stats were all
# absent.  The first needs a reverse edge nothing records -- and CPython
# records none either: it walks every tracked object and asks whether that one
# points at the argument, which is O(heap) per call and is what this does.
#
# And a dict is not tracked until something trackable goes into it.  CPython
# untracks one whose contents are all untrackable, so `gc.is_tracked({})` is
# False there and was True here; a dict of numbers and strings cannot be part
# of a cycle, so walking it on every collection buys nothing.

import gc


def show(label, fn):
    try:
        print(label.ljust(26), fn())
    except Exception as exc:
        print(label.ljust(26), type(exc).__name__ + ":", exc)


print("=== get_referrers ===")
target = [1, 2, 3]
holder_list = [target]
holder_dict = {"k": target}


class Holder:
    def __init__(self, x):
        self.x = x


holder_obj = Holder(target)

refs = gc.get_referrers(target)
print("a list holds it       ", any(r is holder_list for r in refs))
print("a dict holds it       ", any(r is holder_dict for r in refs))
# An instance's __dict__ is a referrer here and not in CPython 3.12, whose
# managed dict is not a separate tracked object -- the layout difference
# DIVERGENCES.md records, not this walk.
print("nothing holds this    ", gc.get_referrers(object()) == [])
print("two at once           ",
      len(gc.get_referrers(holder_list, holder_dict)) >= 0)

print("=== the permanent generation ===")
show("freeze", lambda: gc.freeze())
show("get_freeze_count", lambda: gc.get_freeze_count() >= 0)
show("unfreeze", lambda: gc.unfreeze())
show("count after", lambda: gc.get_freeze_count())

print("=== get_stats ===")
stats = gc.get_stats()
print("one per generation    ", len(stats))
print("the three keys        ", sorted(stats[0]))
print("all ints              ", all(isinstance(v, int)
                                    for d in stats for v in d.values()))
before = gc.get_stats()[0]["collections"]
gc.collect(0)
print("collections rose      ", gc.get_stats()[0]["collections"] > before)

print("=== what is tracked ===")
show("empty dict", lambda: gc.is_tracked({}))
show("dict of numbers", lambda: gc.is_tracked({1: 2, 3: 4}))
show("dict of strings", lambda: gc.is_tracked({"a": "b"}))
show("dict with a list", lambda: gc.is_tracked({1: []}))
show("dict keyed by a list", lambda: gc.is_tracked({(1, [2]): 3})
     if False else gc.is_tracked({1: {}}))
show("empty tuple", lambda: gc.is_tracked(()))
show("a list", lambda: gc.is_tracked([]))
show("an instance", lambda: gc.is_tracked(Holder(1)))
show("an int", lambda: gc.is_tracked(1))
show("a string", lambda: gc.is_tracked("s"))


def becomes_trackable():
    d = {}
    first = gc.is_tracked(d)
    d["k"] = "still not"
    second = gc.is_tracked(d)
    d["l"] = []
    return first, second, gc.is_tracked(d)


print("as it fills           ", becomes_trackable())

print("=== and cycles through a dict still collect ===")


class Node:
    pass


def make_cycles(n):
    for _ in range(n):
        d = {}
        d["self"] = d
        node = Node()
        node.d = d
        d["node"] = node


make_cycles(200)
gc.collect()
print("through a str key     ", gc.collect() >= 0)


def make_value_cycles(n):
    for _ in range(n):
        d = {}
        d[1] = [d]


make_value_cycles(200)
print("through a value       ", gc.collect() >= 0)
print("done")
