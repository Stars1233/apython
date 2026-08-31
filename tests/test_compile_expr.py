# The source compiler, phase 1: expressions through eval() and compile().
#
# Every case here is evaluated by apython's own compiler when this file runs
# under apython, and by CPython's when it runs under python3.  The test runner
# diffs the two, so the oracle is CPython itself.

CASES = [
    # precedence and associativity
    "1+2*3", "(1+2)*3", "2*3+4*5", "10 - 3 - 2", "7//2", "7%3",
    # ** is right-associative AND binds tighter than unary minus, so -2**2
    # is -(2**2); its right operand is a `factor`, so 2**-1 parses.
    "2**3**2", "-2**2", "2**-1", "(-2)**2",
    # unary
    "~5", "+5", "-5", "- -5", "not 0", "not 1", "not not 5",
    # bitwise and shifts
    "5 & 3", "5 | 3", "5 ^ 3", "1 << 10", "1024 >> 3", "1 | 2 & 3", "(1 | 2) & 3",
    # comparisons, including chains that must evaluate each operand once
    "1<2", "2<=2", "3>2", "3>=4", "1==1", "1!=2",
    "1 < 2 < 3", "1 < 2 > 3", "3 > 2 > 1", "1 < 2 <= 2 < 3", "1 > 2 < 3",
    # short-circuiting yields the deciding operand, not a bool
    "1 and 2", "0 and 2", "1 or 2", "0 or 3", "0 or 0 or 7",
    "1 and 2 and 3", "1 and 0 and 3", "0 or 0 or 0",
    # conditional expression, right-associative
    "1 if 2 else 3", "1 if 0 else 3", "1 if 0 else 2 if 0 else 3",
    # literals
    "0x1f", "0b1011", "0o17", "1_000_000", "0", "-0",
    "1.5 + 2", "1.5 * 2.0", "1e3", ".5 + .25", "3.0 // 2", "2.5 % 1",
    "True", "False", "None", "True and False", "None is None",
    "True is True", "True is not False",
    # mixed
    "(1 + 2) * 3 == 9", "not (1 > 2)", "1 < 2 and 3 > 2",
]

for c in CASES:
    print(c, "=>", repr(eval(c)))

# Names resolve against the namespace eval is given.  When globals is supplied
# but locals is not, the two are the same mapping -- which is what makes this
# work at all.
ns = {"a": 10, "b": 3}
print(eval("a + b", ns), eval("a * b - 1", ns), eval("a > b", ns))
print(eval("a if b else 0", ns))

# With no namespace at all, eval sees the caller's.
outer = 42
print(eval("outer + 1"))

# eval() strips leading spaces and tabs from a source string.
print(eval("   1 + 1"), eval("\t2 + 2"))

# compile() hands back a real code object, and eval() accepts one.
code = compile("a - b", "<t>", "eval")
print(type(code).__name__, code.co_name, code.co_names, code.co_flags, code.co_nlocals)
print(eval(code, ns))
print(eval(compile("2 ** 10", "<t>", "eval")))

# Syntax errors are reported, not crashed on.
for bad in ["1 +", "1 2", "(1", "*", "1 if 2", "and 3"]:
    try:
        eval(bad)
        print("no error for", repr(bad))
    except SyntaxError:
        print("SyntaxError for", repr(bad))
