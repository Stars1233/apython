# LOAD_ATTR's method cache guarded on the type's ADDRESS.
#
# A class that is freed leaves its address free, and the next class allocated
# lands on it -- so the pointer compare passed for a DIFFERENT class, with
# only sixteen bits of dk_version behind it and a cached descriptor belonging
# to the dead class's dict.  A warm site then called the wrong class's method,
# or a freed one, and where it landed depended on the heap: CPython's
# test_copy segfaulted, valgrind was clean over it, and a build that laid the
# heap out differently did not fail at all.
#
# Versions come from one global counter and are never reused, which is what
# every other cache in load_ic.asm already guards on.
#
# It takes a WARM site -- the same call site run over many short-lived classes
# -- to see, so the driver below is one function called in a loop.


def drive(o):
    return o.tag()


results = []
for i in range(200):
    # A fresh class per turn, each dropped before the next is made, so the
    # allocator hands the same address out again and again.
    C = type("C%d" % i, (), {"tag": lambda self, i=i: ("tag", i)})
    results.append(drive(C()))
    del C

print(len(results), results[0], results[-1])
print(all(r == ("tag", n) for n, r in enumerate(results)))


# Enough turns that the SECOND guard -- sixteen bits of the tp_dict's version
# -- comes round to the same value as well.  Both guards then passed for a
# class the site had never seen, and the cached descriptor belonged to one
# that had been freed: this loop segfaulted.
wrong = 0
N = 70000
for i in range(N):
    C = type("C", (), {"tag": lambda self, i=i: i})
    if drive(C()) != i:
        wrong += 1
    del C
print("wrong answers:", wrong, "of", N)


# The same with a different method NAME resolving through each class, which is
# what makes the cached descriptor visibly wrong rather than merely stale.
def call_setstate(o, v):
    return o.__setstate__(v)


out = []
for i in range(200):
    ns = {"__setstate__": lambda self, v, i=i: ("set", i, v)}
    C = type("D%d" % i, (), ns)
    out.append(call_setstate(C(), i * 2))
    del C

print(all(r == ("set", n, n * 2) for n, r in enumerate(out)))


# A class REPLACED at the same name, which is the shape that must still see
# the new method rather than the cached old one.
def get(o):
    return o.m()


for _ in range(3):
    class E:
        def m(self):
            return "first"

    print(get(E()))

    class E:
        def m(self):
            return "second"

    print(get(E()))


# Adding a method to a live class invalidates the site too.
class F:
    def m(self):
        return "F.m"


f = F()
print(get(f))
F.m = lambda self: "F.m rebound"
print(get(f))


# ...and shadowing it on the instance.
f.m = lambda: "instance"
print(get(f))
del f.m
print(get(f))

# A subclass reached through the same site.
class G(F):
    pass


print(get(G()), get(f))

print("done")
