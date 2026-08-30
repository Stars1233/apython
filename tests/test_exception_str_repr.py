# str(exc) and repr(exc) are defined by exc.args, not by a single stored
# value.  apython derived both from one value, so str(ValueError()) was
# "ValueError", str(ValueError("a", "b")) was "a", and repr dropped the
# quotes and every argument after the first.

for e in (ValueError("a"), ValueError("a", "b"), ValueError(),
          TypeError(1), IndexError(1, 2, 3), RuntimeError(None),
          KeyError("k"), KeyError(1), KeyError()):
    print(type(e).__name__, repr(str(e)), repr(e), e.args)

# The same objects through the formatting paths
print("%s|%r" % (ValueError("x"), ValueError("x")))
print(f"{TypeError('y')}", str(StopIteration()))

# A missing key reports the key, not a fixed message
def missing(fn):
    try:
        fn()
    except KeyError as e:
        return repr(e), e.args, str(e)

d = {"a": 1}
print(missing(lambda: d["z"]))
print(missing(lambda: {1: 2}[9]))
print(missing(lambda: d[(1, 2)]))
print(missing(lambda: d.pop("z")))

def _del(m, k):
    del m[k]

print(missing(lambda: _del(d, "z")))
print(d.pop("z", "dflt"), d.pop("a"), d)

# A user subclass keeps its own name and the base behaviour
class MyError(Exception):
    pass

print(repr(MyError("m")), str(MyError("m", 2)), MyError().args)
