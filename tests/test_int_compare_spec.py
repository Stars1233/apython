# The six comparison operators over integer immediates, and the two forms
# fused with the jump that follows them.
#
# op_compare_op_int unpacked both Values into (payload, tag) pairs before it
# could compare them -- twenty instructions to synthesise a TAG_SMALLINT that
# the next two compared against.  It does not any more: the immediate encoding
# is `i - 2^50` modulo 2^64, which over [-2^50, 2^50) is strictly increasing
# as an UNSIGNED 64-bit number and never wraps, so an unsigned `cmp` on the raw
# Values IS the signed comparison of the integers.
#
# The answer is then taken from a byte table indexed by
# `op*4 + (2*(x>=y) + (x<=y))` -- CPython's COMPARISON_BIT, without the shift
# -- so all six operators share one load and no branch, and the bool is chosen
# with cmov rather than a jump.
#
# What has to keep working:
#
#   - the operand order.  A table indexed the wrong way round makes `<` answer
#     `>` for every unequal pair and stays right for equal ones.
#   - the boundaries of the immediate range, where the unsigned trick is only
#     valid because the encoding does not wrap.
#   - the deopt.  A heap int, a bool, a float or a str on either side must
#     reach the generic path with the stack as it was; these handlers no
#     longer pop, so an operand pushed back by mistake would be invisible
#     until the stack depth mattered.
#
# Each site is run repeatedly, because a site only specializes once it has run.

VALUES = [0, 1, -1, 2, -2, 7, -7, 255, -255, 1000, -1000,
          2 ** 20, -(2 ** 20), 2 ** 49, -(2 ** 49),
          2 ** 50 - 1, -(2 ** 50), 2 ** 50 - 2, -(2 ** 50) + 1]


def lt(a, b): return a < b
def le(a, b): return a <= b
def eq(a, b): return a == b
def ne(a, b): return a != b
def gt(a, b): return a > b
def ge(a, b): return a >= b


OPS = [lt, le, eq, ne, gt, ge]

for f in OPS:
    for a in VALUES:
        for b in VALUES:
            r = None
            for _ in range(4):
                r = f(a, b)
            print(f.__name__, a, b, r)

# --- fused with POP_JUMP_IF_FALSE ------------------------------------------
def jump_false(a, b, op):
    if op == 0:
        if a < b:
            return "lt"
    elif op == 1:
        if a <= b:
            return "le"
    elif op == 2:
        if a == b:
            return "eq"
    elif op == 3:
        if a != b:
            return "ne"
    elif op == 4:
        if a > b:
            return "gt"
    else:
        if a >= b:
            return "ge"
    return "-"


for op in range(6):
    for a in VALUES:
        for b in VALUES:
            print("jf", op, a, b, jump_false(a, b, op))

# --- fused with POP_JUMP_IF_TRUE (an `or` short-circuits on true) ----------
def jump_true(a, b):
    out = []
    if a < b or a == b:
        out.append("le")
    if a > b or a == b:
        out.append("ge")
    if a != b or a < b:
        out.append("ne")
    if not (a < b) or a > b:
        out.append("nlt")
    return out


for a in VALUES:
    for b in VALUES:
        print("jt", a, b, jump_true(a, b))

# --- a while loop, where the fused form does its real work ------------------
def countdown(n, limit):
    c = 0
    while n > limit:
        n -= 1
        c += 1
    return c


for limit in (-5, 0, 5):
    print("cd", limit, countdown(100, limit))

# --- everything the guard has to refuse ------------------------------------
def all_six(a, b):
    return (a < b, a <= b, a == b, a != b, a > b, a >= b)


# NaN against a WIDE int is left out on purpose: it answers wrongly today,
# in the generic float/int comparison and not in anything specialized here.
# tests/test_int_nan_compare.py is where that lives.
OTHERS = [1, 0, -1, 2 ** 60, -(2 ** 60), 2 ** 200, 1.5, -0.5, float("inf"),
          True, False, "x"]

for a in OTHERS:
    for b in OTHERS:
        try:
            print("m", repr(a), repr(b), all_six(a, b))
        except TypeError:
            print("m", repr(a), repr(b), "TypeError")


# --- a subclass with its own comparisons must not be bypassed --------------
class Odd(int):
    def __lt__(self, other):
        return "Odd.__lt__"

    def __eq__(self, other):
        return "Odd.__eq__"

    def __hash__(self):
        return 0


for _ in range(4):
    print(all_six(Odd(3), 5))
    print(all_six(5, Odd(3)))

# --- once a site has specialized, an ordinary int must still work ----------
def mixed(seq):
    out = []
    for v in seq:
        out.append(v < 10)
        out.append(v == 10)
    return out


print(mixed([1, 2, 3, 2 ** 60, 4, 1.5, 5, True, 6]))
print(mixed([1, 2, 3]))
