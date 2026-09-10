# PEP 442: the collector runs __del__ on the whole unreachable set BEFORE it
# clears any of it, and anything a finalizer resurrects is not cleared at all.
#
# Without a finalize phase, __del__ only ran as a side effect of phase 5's
# tp_clear dropping the last reference -- so it was handed an object whose
# attributes had already been dropped, and an object it stored somewhere
# outside the cycle was cleared and freed underneath the reference it had just
# been given.  CPython's own test_gc tests both halves.

import gc

gc.collect()
gc.disable()

# --- a finalizer sees an INTACT object ------------------------------------
seen = []


class Intact:
    def __init__(self):
        self.payload = "here"
        self.me = self          # a cycle, so only the collector frees it

    def __del__(self):
        seen.append(getattr(self, "payload", "<gone>"))


Intact()
gc.collect()
print("finalizer saw:", seen)

# --- resurrection is transitive -------------------------------------------
class Cargo:
    def __init__(self):
        self.me = self


class Lazarus:
    resurrected = []

    def __del__(self):
        Lazarus.resurrected.append(self)


laz = Lazarus()
cargo = Cargo()
cargo_id = id(cargo)
laz.cargo = cargo
cargo.laz = laz
del laz, cargo
gc.collect()
print("resurrected:", len(Lazarus.resurrected))
inst = Lazarus.resurrected.pop()
print("kept its attribute:", hasattr(inst, "cargo"))
print("and the same object:", id(inst.cargo) == cargo_id)
print("cargo still whole:", inst.cargo.me is inst.cargo)

# --- a finalizer runs at most ONCE, even across two collections ------------
calls = []


class Once:
    def __init__(self, n):
        self.n = n
        self.me = self

    def __del__(self):
        calls.append(self.n)
        held.append(self)       # resurrect


held = []
Once(1)
gc.collect()
print("after first collect:", calls)
held.clear()                    # drop it again
gc.collect()
gc.collect()
print("after dropping it:", calls)

# --- an ordinary cycle with a finalizer is still collected -----------------
count = []


class Plain:
    def __del__(self):
        count.append(1)


for i in range(5):
    a = Plain()
    b = Plain()
    a.other = b
    b.other = a
    del a, b
n = gc.collect()
print("plain cycles finalized:", len(count))

# --- and a cycle with NO finalizer is unaffected ---------------------------
class Quiet:
    pass


for i in range(5):
    a = Quiet()
    a.me = a
    del a
gc.collect()
print("quiet ok")

gc.enable()
print("done")
