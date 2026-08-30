# A subclass's __setitem__ / __getitem__ / __delitem__ must win over the slot
# it inherits from a builtin base.
#
# type_from_parts hands a builtin subclass its base's method table by pointer,
# and installing a wrapper is what makes the Python method reachable at all --
# without it `d["a"] = 1` on a dict subclass went straight into dict's storage
# and the subclass's __setitem__ never ran.  CPython's collections.OrderedDict
# and enum's _EnumDict are both built on exactly that override.
class D(dict):
    def __init__(self):
        super().__init__()
        self.log = []

    def __setitem__(self, k, v):
        self.log.append(("set", k))
        super().__setitem__(k, "w:" + str(v))

    def __getitem__(self, k):
        self.log.append(("get", k))
        return super().__getitem__(k)

    def __delitem__(self, k):
        self.log.append(("del", k))
        super().__delitem__(k)


d = D()
d["a"] = 1
d["b"] = 2
print(sorted(dict(d).items()))
print(d["a"])
del d["b"]
print(sorted(dict(d).items()))
print(d.log)


class L(list):
    def __setitem__(self, i, v):
        super().__setitem__(i, v * 2)


el = L([1, 2, 3])
el[0] = 5
print(list(el))


# Subscripting a plain builtin is unchanged, and a subclass that overrides
# nothing still uses the base's slot.
class Q(dict):
    pass


q = Q()
q["k"] = "v"
print(q["k"], sorted(q.items()))
p = {"x": 1}
p["y"] = 2
del p["x"]
print(sorted(p.items()))
lst = [1, 2]
lst[0] = 9
print(lst)


# A class that defines only __getitem__ is subscriptable.
class G:
    def __getitem__(self, k):
        return ("got", k)


print(G()[3])
