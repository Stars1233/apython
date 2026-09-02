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

# An extended-slice length mismatch names both sizes.  They were known thirty
# lines above the raise and gone by the time it happened, and naming them also
# needed an int-to-decimal helper another file could reach -- there were six
# copies of one, every one of them file-local.
for stmt in ("l = [1, 2, 3, 4]\nl[0:4:2] = [1, 2, 3]",
             "l = [1, 2, 3, 4]\nl[::2] = []",
             "l = [1, 2, 3, 4]\nl[::-1] = [1]",
             "b = bytearray(b'abcd')\nb[0:4:2] = b'abc'"):
    try:
        exec(stmt)
        print(stmt.replace("\n", " ; "), "=> no error")
    except ValueError as e:
        print(stmt.replace("\n", " ; "), "=>", e)

# str() of a Unicode error renders its five fields rather than printing them
# as a tuple.
for args in (("ascii", b"abc", 1, 2, "ordinal not in range(128)"),
             ("utf-8", b"a\xffb", 1, 2, "invalid start byte"),
             ("ascii", b"abcdef", 1, 4, "ordinal not in range(128)")):
    print(str(UnicodeDecodeError(*args)))
for args in (("ascii", "aሴb", 1, 2, "ordinal not in range(128)"),
             ("ascii", "a\xe9b", 1, 2, "ordinal not in range(128)"),
             ("ascii", "a\U0001F600b", 1, 2, "ordinal not in range(128)"),
             ("ascii", "aZb", 1, 2, "ordinal not in range(128)"),
             ("ascii", "abcdef", 1, 4, "ordinal not in range(128)")):
    print(str(UnicodeEncodeError(*args)))

# A five-argument exception that is not one of those two still prints its
# tuple, and the other arities are unchanged.
print(str(ValueError(1, 2, 3, 4, 5)))
print(str(ValueError()), "|", str(ValueError("one")), "|", str(ValueError(1, 2)))
print(str(KeyError("k")))
try:
    b"a\xffb".decode("utf-8")
except UnicodeDecodeError as e:
    print(str(e))
