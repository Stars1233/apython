# The operator TypeErrors name the operator and both operand types.
#
# RAISE can only carry a .rodata literal, so ours were fixed strings:
# "unsupported operand type(s)" where CPython says "unsupported operand
# type(s) for +: 'int' and 'str'", and "'<' not supported between instances"
# where CPython names both types.  raise_binop_type_error had been in the tree
# all along with a single caller in divmod, and nothing mapped an op index to
# the symbol the operator is written with.

exprs = [
    "1 + 'a'", "'a' + 1", "1 - None", "1 * 'a' * 'b'", "1 / 'a'", "1 // 'a'",
    "1 % None", "1 ** 'a'", "1 & 'a'", "1 | 'a'", "1 ^ 'a'", "1 << 'a'",
    "1 >> 'a'", "1 @ 2", "[] - []", "{} - {}", "b'a' + 'b'", "(1,) + [2]",
    "[1] + (2,)", "'a' + b'b'", "None + None", "1.5 + 'a'", "'a' + 1.5",
    "[1] * 1.5", "1.5 * [1]", "'ab' * 'cd'", "(1,) * None", "b'a' * 1.5",
]
for e in exprs:
    try:
        eval(e)
        print(e, "=> no error")
    except TypeError as t:
        print(e, "=>", t)

# The inplace forms name the inplace spelling.
for code in ("x = 1\nx += 'a'", "x = 'a'\nx -= 1", "x = []\nx |= 5",
             "x = 1\nx **= 'a'", "x = 1\nx <<= None", "x = [1]\nx *= 1.5"):
    try:
        exec(code)
        print(code.replace("\n", " ; "), "=> no error")
    except TypeError as t:
        print(code.replace("\n", " ; "), "=>", t)

# The comparisons name both types too.
cmps = ["1 < 'a'", "'a' < 1", "None < 1", "[] < 5", "{} < []", "1 <= 'a'",
        "1 > 'a'", "1 >= 'a'", "object() < object()", "(1,) < [1]",
        "b'a' < 'a'", "1.5 < 'a'", "range(1) < range(2)"]
for e in cmps:
    try:
        eval(e)
        print(e, "=> no error")
    except TypeError as t:
        print(e, "=>", t)

# What must not change: the ones that are legal stay legal, and the type of
# the exception is still TypeError.
print(1 + 2, "a" + "b", [1] + [2], (1,) + (2,), b"a" + b"b")
print("ab" * 2, [1] * 2, (1,) * 2, b"a" * 2, 2 * "ab", True * "ab")
for e in exprs[:4]:
    try:
        eval(e)
    except Exception as t:
        print(type(t).__name__)
