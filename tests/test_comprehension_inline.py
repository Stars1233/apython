"""Comprehensions, and the frame each of them keeps.

PEP 709 inlines a list, set or dict comprehension into the block it is
written in.  This does not, and the reason is in DIVERGENCES.md: inlining
needs the target to be a fast local of the ENCLOSING scope while every other
meaning of that name stays what it was, and CPython gets that by giving one
name two storages at once.

What that costs is visible only through introspection -- `co_name` read from
inside one, and one extra traceback entry through one -- and this pins both,
so a future symbol table that can express two storages has something to
measure against.  Everything ELSE about a comprehension has to match, and
most of this file is that.
"""

import sys


def kinds():
    r = range(4)
    return ([x * 2 for x in r], {x % 2 for x in r}, {x: x * x for x in r},
            list(x + 1 for x in r))


print("--- the four kinds ---")
print(kinds())

print("--- the target does not leak, and does not clobber ---")
i = 100
print([i for i in range(3)], i)
j = "outer"


def shadowing():
    j = "inner"
    got = [j for j in range(3)]
    return got, j


print(shadowing(), j)


def never_bound():
    got = [k for k in range(3)]
    try:
        return got, k
    except NameError as e:
        return got, "NameError"


print(never_bound())


def declared_global():
    global gval
    gval = 5
    return [gval for gval in range(3)], gval


gval = None
print(declared_global(), gval)

print("--- a nested function may capture the target ---")


def captures():
    return [(lambda: n)() for n in range(3)]


print(captures())


def captures_later():
    fns = [(lambda v=n: v) for n in range(3)]
    return [f() for f in fns]


print(captures_later())


def captures_by_reference():
    fns = []
    for n in range(3):
        fns.append(lambda: n)
    return [f() for f in fns]


print(captures_by_reference())

print("--- nesting, conditions, tuple targets, starred ---")
m = [[1, 2], [3, 4]]
print([[y * 10 for y in row] for row in m])
print([x for x in range(10) if x % 3 == 0 if x])
print([a + b for a, b in [(1, 2), (3, 4)]])
print({k: v for k, v in zip("ab", [1, 2])})
print([*[x for x in range(3)], 9])
print([y for x in m for y in x])

print("--- what the frame costs: zero-argument super() inside one ---")
#
# A comprehension with a frame of its own is a nested function, and
# zero-argument super() needs two things the frame does not have: __class__
# as a free variable, and the method's `self` as the first argument -- the
# first argument here is the implicit `.0` iterator.  CPython 3.11 raised
# the same NameError for the same reason; 3.12 made it work by inlining.
# The two-argument form works either way, and is what to write here.


class Base:
    def name(self):
        return "base"


class Child(Base):
    def explicit(self):
        return [super(Child, self).name() for _ in range(2)]

    def zero_arg(self):
        try:
            return [super().name() for _ in range(2)]
        except NameError:
            return "NameError"


c = Child()
print(c.explicit())
print(c.zero_arg() in (["base", "base"], "NameError"))

print("--- and the divergence itself ---")
#
# Both answers are written down here because both are reachable: running a
# .pyc runs CPython's bytecode, where the comprehension IS inlined, and
# running the source runs ours, where it is not.  What the test pins is that
# it is one of the two and nothing else -- the day the symbol table can hold
# two storages for a name, the second answer goes away.


def whose_frame():
    return [sys._getframe().f_code.co_name for _ in range(1)][0]


print(whose_frame() in ("whose_frame", "<listcomp>"))


def raises_inside():
    return [1 / 0 for _ in range(1)]


try:
    raises_inside()
except ZeroDivisionError:
    depth = 0
    tb = sys.exc_info()[2]
    while tb is not None:
        depth += 1
        tb = tb.tb_next
    print("traceback entries in 2..3:", 2 <= depth <= 3)

print("--- a generator expression keeps its own frame in both ---")


def genexp_frame():
    return next(sys._getframe().f_code.co_name for _ in range(1))


print(genexp_frame())
print("done")
