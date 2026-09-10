# dict.clear() releases what the dict held, and releasing a value runs its
# __del__ -- which is arbitrary Python and may call clear() on the same dict.
#
# The walk released each entry in place, leaving the table installed, so the
# second call walked the very same entries and released every one of them a
# second time.  The object whose __del__ was running got freed underneath its
# own dealloc.  CPython's PyDict_Clear installs the empty table before it drops
# a single reference, for exactly this reason.

import gc

log = []


class X:
    def __init__(self, n):
        self.n = n

    def __del__(self):
        log.append(self.n)
        d.clear()


d = {i: X(i) for i in range(8)}
d.clear()
print("cleared:", len(d), sorted(log))

# The same shape with the objects as KEYS rather than values.
log.clear()


class K:
    def __init__(self, n):
        self.n = n

    def __hash__(self):
        return self.n

    def __del__(self):
        log.append(self.n)
        e.clear()


e = {K(i): i for i in range(8)}
e.clear()
print("cleared keys:", len(e), sorted(log))

# A __del__ that REFILLS the dict it is clearing keeps what it put back.
class R:
    def __del__(self):
        if len(f) < 3:
            f["refilled"] = 1


f = {"a": R()}
f.clear()
print("refilled:", f)

# clear() on an already-empty dict, and twice in a row.
g = {}
g.clear()
g.clear()
print("empty:", g, len(g))

# The dict is usable again afterwards.
d[1] = "one"
d[2] = "two"
print("reused:", d, len(d))
del d, e, f, g
gc.collect()
print("done")
