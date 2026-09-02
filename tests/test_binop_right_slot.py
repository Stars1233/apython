# The right operand's slot gets a turn when the LEFT type has none.
#
# op_binary_op used to reach .binop_try_right_slot only after a left slot that
# was present and *declined*.  When the left type had no tp_as_number at all,
# or a NULL entry in it, the dispatcher jumped straight to the dunder arm --
# which requires TYPE_FLAG_HEAPTYPE and so refuses every builtin static type.
# CPython's binary_op1 asks the right operand's slot in both cases.
#
# The visible symptom was `None | int`: NoneType's nb_or is 0 and type's is
# union_type_or, so nothing was ever asked and it came out a TypeError.

u = None | int
print(type(u).__name__)
print(u == (int | None))
print(u == (None | int))

# The same shape one level up: a union on the right of a None.
print(type(None | (int | str)).__name__)

# What must NOT change: a pair neither side can serve is still a TypeError,
# and it still names no operator (bugs.md records that wording gap).
for expr in ("None + 1", "1 + None", "None * 2", "[] - []", "'a' - 'b'"):
    try:
        eval(expr)
        print(expr, "=> no error")
    except TypeError as e:
        print(expr, "=> TypeError")

# Ordinary arithmetic is untouched.
print(1 + 2, 1.5 * 2, "ab" * 2, [1] + [2], 7 // 2, 7 % 2, 2 ** 10)
print(3 | 5, 3 & 5, 3 ^ 5, 1 << 4, 32 >> 2)
