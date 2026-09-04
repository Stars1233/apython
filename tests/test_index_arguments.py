# An argument that is an index must be taken as one.
#
# Seven builtin methods handed theirs straight to int_to_i64, which reads a
# PyIntObject's fields off whatever it is given: `"a\tb".expandtabs("x")`
# read a str's header as a number and answered a string, and
# `[1, 2].insert("x", 9)` wrote through it.  obj_as_index is the funnel that
# names the type and that takes an object with __index__, which all of these
# accept in CPython and none of them did here.


def show(label, fn, *a):
    try:
        print("%-32s %r" % (label, fn(*a)))
    except Exception as e:
        print("%-32s %s: %s" % (label, type(e).__name__, e))


class Idx:
    def __index__(self):
        return 1


print("-- a type that is not an index")
show("expandtabs('x')", "a\tb".expandtabs, "x")
show("center('x')", "a".center, "x")
show("ljust('x')", "a".ljust, "x")
show("rjust('x')", "a".rjust, "x")
show("zfill('x')", "a".zfill, "x")
show("tuple.index(1, 'x')", (1, 2).index, 1, "x")
show("tuple.index(1, 0, 'x')", (1, 2).index, 1, 0, "x")
show("list.pop('x')", [1, 2].pop, "x")
show("list.insert('x', 9)", [1, 2].insert, "x", 9)
show("list.index(1, 'x')", [1, 2].index, 1, "x")
show("list.index(1, 0, 'x')", [1, 2].index, 1, 0, "x")
show("bytes.splitlines('x')", b"a\nb".splitlines, "x")

print()
print("-- an object with __index__")
show("expandtabs(Idx())", "a\tb".expandtabs, Idx())
show("center(Idx())", "a".center, Idx())
show("zfill(Idx())", "a".zfill, Idx())
show("list.pop(Idx())", [1, 2].pop, Idx())
show("list.insert(Idx(), 9)", [1, 2].insert, Idx(), 9)
show("tuple.index(2, Idx())", (1, 2).index, 2, Idx())
show("list.index(2, Idx())", [1, 2].index, 2, Idx())

print()
print("-- the ordinary calls still work")
print(repr("a\tb".expandtabs(4)))
print(repr("a".center(5, "-")))
print(repr("1".zfill(3)))
print([1, 2, 3].pop(1), [1, 2, 3].index(3, 1), (1, 2, 3).index(3, 1, 3))
x = [1, 2]
x.insert(1, 9)
print(x)
