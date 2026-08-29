# None, True and False have a single representation: the immortal heap
# singleton.  Anything that used to distinguish an "inline" None or bool from
# a pointer to the singleton must behave identically now.

print("--- identity ---")
x = None
y = None
print(x is None, y is None, x is y, (x is not None))
print(True is True, False is False, True is not False)
d = {"k": None}
print(d["k"] is None)
lst = [None, True, False]
print(lst[0] is None, lst[1] is True, lst[2] is False)
t = (None, True, False)
print(t[0] is None, t[1] is True, t[2] is False)

def f(a=None, b=True):
    return a is None, b is True
print(f())
print(f(None, True))

print("--- repr / str / bool / hash ---")
print(repr(None), str(None), repr(True), repr(False))
print(bool(None), bool(True), bool(False))
print(hash(None) == hash(None), hash(True) == hash(1), hash(False) == hash(0))
print("%s %s %s" % (None, True, False))
print("{} {} {}".format(None, True, False))
print(f"{None} {True} {False}")
print(type(None).__name__, type(True).__name__)

print("--- as dict keys and set members ---")
m = {None: "n", True: "t", False: "f"}
print(len(m), m[None], m[True], m[False])
print(m[1], m[0])
s = {None, True, False, 1, 0}
print(len(s))
print(None in m, True in m, False in m)

print("--- comparisons ---")
print(None == None, None != None)
print(True == 1, False == 0, True != 0)
print(True + True, True * 3, False - 1)
print(True & False, True | False, True ^ True)
print(type(True & False).__name__, type(True & 1).__name__)
print(sorted([True, False, True]))

print("--- None in containers and control flow ---")
vals = [1, None, 2, None]
print([v for v in vals if v is not None])
print(vals.count(None), vals.index(None))
print(None in vals, 3 in vals)
vals.remove(None)
print(vals)

print("--- default arguments and returns ---")
def g():
    pass
print(g() is None)
print([].append(1) is None)
print(getattr(object(), "nope", None) is None)
print({}.get("missing") is None)
print({}.get("missing", "dflt"))

print("--- dict equality with singleton values ---")
# NOTE: dict value comparison only consults tp_richcompare, so a user class
# whose __eq__ is a plain method, and cross-type values like True vs 1, are
# still reported unequal.  Both gaps predate the singleton change.
print({"a": None} == {"a": None})
print({"a": True} == {"a": True})
print({"a": False} == {"a": False})
print({"a": None} == {"a": False})
print({"a": True} == {"a": False})

print("--- slices with None components ---")
r = list(range(10))
print(r[None:None], r[2:None], r[None:3], r[::None], r[None:None:2])
print(r[::-1][:3])
sl = slice(None, 5, None)
print(sl.start, sl.stop, sl.step, r[sl])
