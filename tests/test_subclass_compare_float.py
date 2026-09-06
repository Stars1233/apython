# A subclass of a builtin NUMBER compared against a float had its own
# comparison bypassed:
#
#     class F(float):
#         def __lt__(self, other): return "F.__lt__"
#     F(1.5) < 2.0        -> True        (CPython: "F.__lt__")
#     2.0 > F(1.5)        -> True        (CPython: "F.__lt__", reflected)
#
# op_compare_op opens the general path with a float-coercion shortcut: if
# either operand is a float immediate and float_binop_accepts says the other
# is something float_compare can take, it goes straight to float_compare.
# float_binop_accepts says yes to a float subclass and to an int subclass --
# they ARE numbers -- so the shortcut fired and float_compare answered by
# value, and the subclass's __lt__ never ran.
#
# The shortcut now declines when the probed operand is a HEAPTYPE, which is
# exactly the case where a user __lt__ can exist.  Nothing legitimate is lost:
# the only heaptypes float_binop_accepts accepts are int and float subclasses,
# and CPython consults their tp_richcompare first too.
#
# Why this was invisible: it needs a FLOAT on one side.  A subclass compared
# against an int, a str or a list took the general path and was always right,
# which is why `class I(int): __lt__` worked for `I(1) < 2` and failed only
# for `I(1) < 2.0`.


def show(label, value):
    print("%-34s %r" % (label, value))


class F(float):
    def __lt__(self, other):
        return "F.__lt__"

    def __le__(self, other):
        return "F.__le__"

    def __eq__(self, other):
        return "F.__eq__"

    def __gt__(self, other):
        return "F.__gt__"

    def __hash__(self):
        return 0


class I(int):
    def __lt__(self, other):
        return "I.__lt__"

    def __gt__(self, other):
        return "I.__gt__"

    def __hash__(self):
        return 0


class PlainF(float):
    pass


class PlainI(int):
    pass


def all_six(a, b):
    return (a < b, a <= b, a == b, a != b, a > b, a >= b)


# --- the subclass on the left, a float on the right -----------------------
for _ in range(4):
    show("F(1.5) vs 2.0", all_six(F(1.5), 2.0))
    show("2.0 vs F(1.5)", all_six(2.0, F(1.5)))
    show("I(1) vs 2.0", all_six(I(1), 2.0))
    show("2.0 vs I(1)", all_six(2.0, I(1)))

# --- and against an int, which always worked ------------------------------
show("F(1.5) vs 2", all_six(F(1.5), 2))
show("2 vs F(1.5)", all_six(2, F(1.5)))
show("I(1) vs 2", all_six(I(1), 2))
show("2 vs I(1)", all_six(2, I(1)))

# --- a subclass that defines nothing must still compare by value ----------
show("PlainF(1.5) vs 2.0", all_six(PlainF(1.5), 2.0))
show("2.0 vs PlainF(1.5)", all_six(2.0, PlainF(1.5)))
show("PlainI(1) vs 2.0", all_six(PlainI(1), 2.0))
show("2.0 vs PlainI(1)", all_six(2.0, PlainI(1)))
show("PlainF(1.5) vs PlainF(2.5)", all_six(PlainF(1.5), PlainF(2.5)))
show("PlainF(2.0) vs 2", all_six(PlainF(2.0), 2))
show("PlainI(2) vs 2.0", all_six(PlainI(2), 2.0))

# --- the ordinary pairs the shortcut exists for must not slow OR change ---
show("1.5 vs 2.0", all_six(1.5, 2.0))
show("1 vs 2.0", all_six(1, 2.0))
show("2.0 vs 1", all_six(2.0, 1))
show("2 ** 60 vs 2.0", all_six(2 ** 60, 2.0))
show("2.0 vs 2 ** 60", all_six(2.0, 2 ** 60))
show("True vs 2.0", all_six(True, 2.0))
show("2.0 vs True", all_six(2.0, True))

nan = float("nan")
show("F(1.5) vs nan", all_six(F(1.5), nan))
show("nan vs F(1.5)", all_six(nan, F(1.5)))
show("PlainF(1.5) vs nan", all_six(PlainF(1.5), nan))

# --- sorting and min/max drive the same slot ------------------------------
print(sorted([PlainF(3.0), 1.5, PlainI(2), 2.5, 1]))
print(max(PlainF(3.0), 2.0), min(PlainF(3.0), 2.0))
print(PlainF(2.0) == 2.0, PlainF(2.0) == 2, PlainI(2) == 2.0)
print(sorted([PlainF(1.0), PlainF(-1.0), 0.0, -0.0]))

# --- a subclass with only a REFLECTED comparison --------------------------
class R(float):
    def __gt__(self, other):
        return "R.__gt__"

    def __hash__(self):
        return 0


for _ in range(4):
    show("1.0 < R(2.0)", 1.0 < R(2.0))
    show("R(2.0) > 1.0", R(2.0) > 1.0)
