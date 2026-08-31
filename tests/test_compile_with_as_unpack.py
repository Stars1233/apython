# The `as` target is stored inside the with's protected region: CPython's
# exception-table range starts right after BEFORE_WITH, so a failing unpack
# still runs __exit__.  Ours opened the region after the store, and the region
# depth was read at that point -- so opening it earlier needs the handler to
# record how many items are live there that it unwinds away.
log = []


class CM:
    def __init__(self, value):
        self.value = value

    def __enter__(self):
        log.append("enter")
        return self.value

    def __exit__(self, *a):
        log.append("exit " + (a[0].__name__ if a[0] else "clean"))
        return False


try:
    with CM((1, 2, 3)) as (a, b):
        print("NOT REACHED")
except ValueError as e:
    print("caught", e)
print(log)

log.clear()
try:
    with CM((1,)) as (a, b):
        print("NOT REACHED")
except ValueError as e:
    print("caught", e)
print(log)

# The ordinary forms still work.
log.clear()
with CM((1, 2)) as (a, b):
    print("body", a, b)
print(log)

log.clear()
with CM(5) as v:
    print("body", v)
print(log)

log.clear()
with CM(5):
    print("body, no target")
print(log)

# Nested, and with the target used by an inner with.
log.clear()
with CM((1, 2)) as (a, b), CM(a + b) as c:
    print("nested", a, b, c)
print(log)

# The unpack messages themselves.
for pair in ((1, 2, 3), (1,), [1, 2, 3, 4], "xyz", "x"):
    try:
        p, q = pair
    except ValueError as e:
        print(e)

# An exception from the body still reaches __exit__.
log.clear()
try:
    with CM((1, 2)) as (a, b):
        raise KeyError("k")
except KeyError:
    print("body raised")
print(log)
