# A dict preserves insertion order -- guaranteed language semantics since 3.7,
# and previously not true here: the table was open-addressed and iteration
# walked hash slots, so {"b":1,"a":2} printed as {'a':1,'b':2}.  It is
# CPython's compact layout now: a sparse index array over a dense entry array
# appended in insertion order.

d = {}
for k in "zebra":
    d[k] = 1
print(list(d))

d2 = {"b": 1, "a": 2, "c": 3}
print(d2, list(d2), list(d2.keys()), list(d2.values()), list(d2.items()))
print(list(reversed(d2)))

# kwargs, dict() and ** all keep caller order
print(list(dict(x=1, m=2, a=3)))
print({**d2, "z": 9})


def f(**kw):
    return list(kw)


print(f(q=1, b=2, a=3))

# popitem is LIFO
d3 = {"one": 1, "two": 2, "three": 3}
print(d3.popitem(), d3.popitem(), list(d3))

# a delete leaves the surviving order intact, and a later insert appends
d4 = {"a": 1, "b": 2, "c": 3, "d": 4}
del d4["b"]
print(list(d4))
d4["e"] = 5
print(list(d4), d4)
d4["a"] = 99          # updating keeps position
print(list(d4), d4["a"])

# copy, clear and rebuild
print(list(d4.copy()), d4.copy() == d4)
d4.clear()
print(d4, len(d4), list(d4))
d4["z"] = 1
d4["y"] = 2
print(list(d4))

# survives growth well past the initial capacity, and stays ordered
big = {}
for i in range(200):
    big["k%03d" % i] = i
print(list(big)[:5], list(big)[-3:], len(big))
print(list(big) == ["k%03d" % i for i in range(200)])

# delete-heavy churn then re-add: order is by (re)insertion
churn = {}
for i in range(60):
    churn[i] = i
for i in range(0, 60, 2):
    del churn[i]
print(list(churn)[:6], len(churn))
for i in range(0, 6, 2):
    churn[i] = i
print(list(churn)[-3:], len(churn))

# lookups still work through all of that
print(churn[1], churn[59], churn.get(0), churn.get(2, "gone"))
print(1 in churn, 0 in churn, len([k for k in churn]))

# class bodies and __dict__ keep definition order
class C:
    b = 1
    a = 2
    def m(self): pass


print([k for k in C.__dict__ if not k.startswith("_")])

o = C()
o.z = 1
o.y = 2
o.x = 3
print(list(o.__dict__), o.__dict__)

# nested and mixed key types
mixed = {1: "i", "s": "str", (1, 2): "tup", 2.5: "float", True: "bool"}
print(list(mixed))
print(mixed[1], mixed["s"], mixed[(1, 2)], mixed[2.5])

# equality is order-independent
print({"a": 1, "b": 2} == {"b": 2, "a": 1})
