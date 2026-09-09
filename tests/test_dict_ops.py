# Test dict operators

# dict | dict (merge)
a = {"x": 1, "y": 2}
b = {"y": 3, "z": 4}
c = a | b
print(c["x"])    # 1 (from a)
print(c["y"])    # 3 (from b, overrides a)
print(c["z"])    # 4 (from b)
print(len(c))    # 3

# dict | dict (disjoint)
d = {"a": 1} | {"b": 2}
print(d["a"])    # 1
print(d["b"])    # 2
print(len(d))    # 2

# dict == dict
print({"a": 1, "b": 2} == {"a": 1, "b": 2})  # True
print({"a": 1, "b": 2} == {"a": 1, "b": 3})  # False
print({"a": 1} == {"a": 1, "b": 2})            # False
print({} == {})                                  # True

# dict != dict
print({"a": 1} != {"a": 2})  # True
print({"a": 1} != {"a": 1})  # False

# --- get / pop / setdefault / d[k] over every value encoding ---------------
# These five funnels tested for a miss by looking at the TAG half of the old
# (payload, tag) pair, and took the Value apart and put it back together to
# do it.  A Value is one word and 0 is the only miss, so the encodings that
# matter are the ones whose payload is zero but which are present: int 0,
# False, None, "" and 0.0 all had to be distinguished from absent.
enc = {"z": 0, "f": False, "n": None, "e": "", "l": [], "big": 2 ** 70,
       "fl": 0.0}
for k in ("z", "f", "n", "e", "l", "big", "fl", "missing"):
    print(repr(k), repr(enc.get(k)), repr(enc.get(k, "DEF")), k in enc)
for k in ("z", "f", "n", "e", "big", "fl"):
    print(repr(enc[k]))
try:
    enc["nope"]
except KeyError as e:
    print("KeyError", e)

popped = dict(enc)
for k in ("z", "f", "n", "e", "fl"):
    print(repr(popped.pop(k)), len(popped))
print(repr(popped.pop("nope", "DEF")))
try:
    popped.pop("nope")
except KeyError as e:
    print("KeyError", e)

sd = {"z": 0}
print(repr(sd.setdefault("z")), repr(sd.setdefault("z", 9)), sd)
print(repr(sd.setdefault("new")), sd)
print(repr(sd.setdefault("new2", 5)), sd)
sd2 = {}
print(sd2.setdefault("k", []), sd2)
sd2.setdefault("k").append(1)
print(sd2)

seen6 = []


class Watch6:
    def __init__(self, tag):
        self.tag = tag

    def __del__(self):
        seen6.append(self.tag)


def churn6():
    m = {"a": Watch6("a")}
    x = m.get("a")
    y = m.setdefault("a")
    z = m.pop("a")
    del m, x, y, z


churn6()
print(seen6)


def churn7():
    m = {}
    w = Watch6("b")
    m.setdefault("k", w)
    del w
    print(len(seen6))
    del m


churn7()
print(seen6)
