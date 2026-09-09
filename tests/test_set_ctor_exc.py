# set(x) and frozenset(x) must not care whether an exception is already in
# flight when they are called.
#
# Both constructors decide "did the iteration raise?" by snapshotting
# current_exception on the way in and comparing on the way out -- NULL from
# tp_iternext is exhaustion and a raise alike, and only the snapshot tells
# them apart.  The fast paths that copy a source table wholesale, or presize
# from a known length, do not iterate at all, so they must not reach that
# comparison: the snapshot was never taken and the slot holds whatever the
# frame had in it.
#
# An exception IS in flight in ordinary code more often than it looks: every
# __del__ that runs while a frame unwinds, every cleanup in an `except`
# block, every generator being closed.
r = []


class D:
    def __del__(self):
        # each of these takes a different arm of the two constructors
        r.append(("set/list", sorted(set([1, 2, 3]))))
        r.append(("set/set", sorted(set({1, 2, 3}))))
        r.append(("set/frozen", sorted(set(frozenset({1, 2, 3})))))
        r.append(("set/tuple", sorted(set((1, 2, 3)))))
        r.append(("set/gen", sorted(set(x for x in (1, 2, 3)))))
        r.append(("set/str", sorted(set("abc"))))
        r.append(("frozen/set", sorted(frozenset({1, 2, 3}))))
        r.append(("frozen/list", sorted(frozenset([1, 2, 3]))))
        r.append(("frozen/tuple", sorted(frozenset((1, 2, 3)))))
        r.append(("frozen/frozen", sorted(frozenset(frozenset({1, 2, 3})))))
        r.append(("set/empty", sorted(set(set()))))
        r.append(("frozen/empty", sorted(frozenset(frozenset()))))
        r.append(("copy", sorted({1, 2, 3}.copy())))
        r.append(("union", sorted({1, 2} | {3})))


def raiser():
    d = D()
    raise ValueError("boom")


try:
    raiser()
except ValueError:
    pass
for name, val in r:
    print(name, val)
print(len(r))

# the same thing inside an except block, where the exception is being handled
# rather than propagating
out = []
try:
    raise KeyError("k")
except KeyError:
    out.append(sorted(set({4, 5})))
    out.append(sorted(frozenset({4, 5})))
    out.append(sorted(set([4, 5])))
print(out)

# and while a generator is being closed
def gen():
    try:
        yield 1
        yield 2
    finally:
        out.append(sorted(set({6, 7})))


g = gen()
next(g)
g.close()
print(out[-1])

# the constructors still report a genuine failure from the iterator itself
class BadIter:
    def __iter__(self):
        raise RuntimeError("no iter")


class BadNext:
    def __iter__(self):
        return self

    def __next__(self):
        raise RuntimeError("no next")


for ctor in (set, frozenset):
    for src in (BadIter(), BadNext()):
        try:
            ctor(src)
            print("NO RAISE")
        except RuntimeError as e:
            print(ctor.__name__, type(src).__name__, e)
