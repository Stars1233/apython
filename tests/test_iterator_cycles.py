# An iterator holds the thing it walks, so a container holding its own
# iterator is a cycle -- and every one of these types was allocated with a
# plain malloc, had tp_flags 0 and no traverse or clear, so the collector
# could not see it.  `a = []; a.append(iter(a))` leaked, and so did
# `d["k"] = d.keys()`.

import gc


def cycle(make):
    gc.collect()
    make()
    return gc.collect() > 0


print("=== a container holding its own iterator ===")

def list_iter():
    x = []
    x.append(iter(x))

def tuple_iter():
    x = []
    x.append(iter(tuple([x])))

def dict_iter():
    d = {}
    d["k"] = iter(d)

def dict_reversed():
    d = {"a": 1}
    d["k"] = reversed(d)

print("list", cycle(list_iter))
print("tuple", cycle(tuple_iter))
print("dict", cycle(dict_iter))
print("dict reversed", cycle(dict_reversed))

print("=== and its own view ===")

def keys():
    d = {}
    d["k"] = d.keys()

def values():
    d = {}
    d["k"] = d.values()

def items():
    d = {}
    d["k"] = d.items()

print("keys", cycle(keys))
print("values", cycle(values))
print("items", cycle(items))

print("=== the sequence-protocol iterator ===")

class Seq:
    def __init__(self):
        self.d = []

    def __getitem__(self, i):
        return self.d[i]


def seq_iter():
    s = Seq()
    s.d.append(iter(s))

print("seq", cycle(seq_iter))

print("=== longer chains ===")

def chain():
    a = []
    b = [iter(a)]
    a.append(iter(b))

print("chain", cycle(chain))

def through_dict():
    d = {}
    lst = [d.items()]
    d["l"] = iter(lst)

print("through a dict", cycle(through_dict))

print("=== and they still iterate ===")
a = [1, 2, 3]
print(list(iter(a)), list(iter((4, 5))), sorted(iter({6: 7})))
d = {"x": 1, "y": 2}
print(sorted(d.keys()), sorted(d.values()), sorted(d.items()))
print(list(reversed(d)))
s = Seq()
s.d.extend([1, 2, 3])
print(list(iter(s)))
print([x for x in iter(a)])

print("=== an iterator half-consumed is still collectable ===")

def half():
    x = [1, 2, 3, 4]
    it = iter(x)
    next(it)
    next(it)
    x.append(it)

print("half", cycle(half))

print("=== and an exhausted one ===")

def spent():
    x = [1]
    it = iter(x)
    for _ in it:
        pass
    x.append(it)

print("spent", cycle(spent))
print("done")
