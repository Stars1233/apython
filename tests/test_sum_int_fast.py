# sum() over integers ran every addition through the whole numeric protocol:
# obj_binary_op, then int_binop_unpack on each operand, then int_add.  Over a
# list of ordinary integers that was 45% of the call.
#
# When both the running total and the item are integer immediates the addition
# is now three instructions on the Values themselves, and anything else falls
# into the protocol exactly as before.  What has to keep working:
#
#   - the total crossing +-2^50, where the answer stops being an immediate and
#     the protocol has to box it
#   - a bool, an int subclass, a compact heap int and a float appearing part
#     way through, none of which the fast arm may take
#   - an explicit start of every one of those shapes
#   - the float phase, which must still be entered on the first float item and
#     must still compensate -- the fast arm sits in front of it
#
# CPython has the same fast path (a Py_ssize_t running total that bails on
# overflow, in bltinmodule.c), and it too refuses a bool: PyLong_CheckExact.

import math


def show(label, value):
    print("%-40s %r" % (label, value))


# --- ordinary integer sums -------------------------------------------------
show("range(1000)", sum(range(1000)))
show("range(1000) start 5", sum(range(1000), 5))
show("empty", sum([]))
show("empty start 7", sum([], 7))
show("one item", sum([42]))
show("negatives", sum(range(-500, 500)))
show("all zeros", sum([0] * 100))

# --- crossing the immediate boundary, both ways ----------------------------
BIG = 2 ** 49
for n in (1, 2, 3, 4, 8, 100):
    show("%d * 2**49" % n, sum([BIG] * n))
    show("%d * -2**49" % n, sum([-BIG] * n))
show("2**50 - 1 then 1", sum([2 ** 50 - 1, 1]))
show("2**50 then -2**50", sum([2 ** 50, -(2 ** 50)]))
show("-(2**50) then -1", sum([-(2 ** 50), -1]))
show("back and forth", sum([2 ** 50, -(2 ** 50), 3, 2 ** 51, -(2 ** 51), 4]))
show("start past the range", sum([1, 2, 3], 2 ** 60))
show("start past int64", sum([1, 2, 3], 10 ** 30))
show("items past int64", sum([10 ** 30, 10 ** 30, -1]))

# --- shapes the fast arm must refuse ---------------------------------------
show("bools", sum([True, False, True]))
show("bools with start", sum([True, True], 10))
show("ints and bools", sum([1, True, 2, False, 3]))
show("compact heap ints", sum([2 ** 55, 2 ** 55, 1]))
show("int then float", sum([1, 2, 3.5, 4]))
show("float then int", sum([1.5, 2, 3]))
show("float start", sum([1, 2, 3], 0.0))
show("start -0.0", sum([], -0.0))
show("int start float items", sum([1.5, 2.5], 3))


# An int subclass is a heap object, so it is never an immediate and the fast
# arm cannot take it.  Its own __add__ has to run.
#
# NOT tested here: a subclass that overrides only __radd__, where Python gives
# the right operand's reflected slot priority over the left's.  obj_binary_op,
# which is what sum() adds with, does not implement that rule -- a separate,
# pre-existing divergence from the interpreter's own BINARY_OP, which does.
class MyInt(int):
    def __add__(self, other):
        return MyInt(int(self) + int(other) + 1000)


show("subclass item", sum([1, 2, MyInt(3)]))
show("subclass start", sum([1, 2], MyInt(3)))
show("subclass only", sum([MyInt(1), MyInt(2)]))


class Addable:
    def __radd__(self, other):
        return "radd from %r" % (other,)


show("object item", sum([1, 2, Addable()]))

# --- the float phase is still compensated ----------------------------------
show("[1e100, 1.0, -1e100]", sum([1e100, 1.0, -1e100]))
show("int first, then that", sum([0, 1e100, 1.0, -1e100]))
show("[0.1] * 10", sum([0.1] * 10))
show("nan", sum([1.0, float("nan")]))
show("inf", sum([1.0, float("inf"), -1.0]))
show("[inf, -inf]", sum([float("inf"), float("-inf")]))

# --- generators, so the items are not a list -------------------------------
show("generator", sum(x for x in range(100)))
show("generator with start", sum((x * 2 for x in range(100)), 1))
show("generator crossing", sum(2 ** 49 for _ in range(10)))

# --- errors still surface --------------------------------------------------
try:
    sum([1, 2, "x"])
except TypeError:
    print("TypeError for a str item")
try:
    sum([1, 2], "")
except TypeError:
    print("TypeError for a str start")
try:
    sum(5)
except TypeError:
    print("TypeError for a non-iterable")


def raiser():
    yield 1
    yield 2
    raise ValueError("from the generator")


try:
    sum(raiser())
except ValueError as exc:
    print("ValueError propagates:", exc)

# --- the results are ordinary integers -------------------------------------
vals = [sum(range(1000)), sum([2 ** 49] * 4), sum([1, True]), sum([], 0)]
for v in vals:
    print(v, repr(v), type(v).__name__, v + 1, v * 2, hash(v) == hash(v), bool(v))
print(sorted(vals))

# --- a wide sweep, so a single-bit slip shows up ---------------------------
total = []
for k in range(0, 56, 3):
    base = 1 << k
    for extra in (0, 1, -1, 7, -7):
        total.append(sum([base, extra]))
        total.append(sum([base, base, extra]))
        total.append(sum([-base, extra, base]))
print(len(total))
for v in total:
    print(v)
