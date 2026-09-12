# repr_depth must not go negative.
#
# eval_exception_unwind zeroes repr_depth, because a raise from inside a
# nested __repr__ skips repr_pop and leaves the entries on repr_stack stale.
# But an unwind can land INSIDE an outer container repr rather than outside
# it -- a Python __repr__ that calls another repr and catches what that one
# raises -- and the outer repr then still pops on its way out.  Unclamped the
# counter went to -1, and repr_check_active's `dec rcx` loop walked backwards
# out of the array from there.  It is the shape eval.asm records for
# gc_collecting: the unwinder cannot tell "escaped the C routine" from "landed
# inside it" from the fact of the raise alone.
#
# CPython's test_wsgiref segfaulted in tuple_repr on this, several tests after
# the repr that unbalanced the count.


class Bad:
    def __repr__(self):
        raise ValueError("boom")


class Outer:
    def __repr__(self):
        try:
            return repr((Bad(),))
        except ValueError:
            return "caught"


print(repr((Outer(),)))

# Every container repr shares the stack, so each has to survive it.
print(repr((1, 2)), repr([1, 2]), repr({1: 2}), repr({1, 2}))
print(repr({1: 2}.keys()), repr({1: 2}.values()), repr({1: 2}.items()))

for i in range(4):
    print(repr((Outer(),)), repr((i,)), repr([i]))


# The same through a list, a dict and a set, and through two levels.
class OuterList:
    def __repr__(self):
        try:
            return repr([Bad()])
        except ValueError:
            return "L"


class OuterDict:
    def __repr__(self):
        try:
            return repr({1: Bad()})
        except ValueError:
            return "D"


class OuterSet:
    def __repr__(self):
        try:
            return repr({(Bad(),)})
        except ValueError:
            return "S"


class Twice:
    def __repr__(self):
        try:
            return repr((Outer(), Bad()))
        except ValueError:
            return "T"


print(repr([OuterList()]), repr({1: OuterDict()}), repr((OuterSet(),)))
print(repr((Twice(),)))
print(repr((1, [2, {3: (4,)}])))

# And the recursion guard still works afterwards: a self-referential container
# is the thing repr_depth exists for.
a = [1]
a.append(a)
print(repr(a))
d = {}
d["self"] = d
print(repr(d))
t = ([],)
t[0].append(t)
print(repr(t))
