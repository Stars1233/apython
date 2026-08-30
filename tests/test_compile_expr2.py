# The source compiler, phase 2: literals, containers, subscripts,
# attributes and calls -- everything an expression can be short of a
# lambda or a comprehension.
#
# Run under python3 the oracle compiles these; run under apython our own
# compiler does, and the runner diffs the two.

CASES = [
    "'hello'", "'a' 'b' 'c'", '"x\\ty"', "'\\x41\\x42'", "'\\u00e9'", "r'a\\nb'",
    "b'bytes'", "b'\\x00\\xff'", "len('abcd')", "'abc'[1]", "'abcdef'[1:4]",
    "[1,2,3]", "[]", "[1,[2,3],4]", "(1,2)", "(1,)", "()", "(5)",
    "{1,2,3}", "{}", "{1:'a',2:'b'}", "{'k':1}['k']",
    "[1,2,3][0]", "[1,2,3][-1]", "[1,2,3][::2]", "[1,2,3][1:]", "[1,2,3][:2]",
    "list((1,2))", "sorted([3,1,2])", "max(1,2)", "min([4,2,9])",
    "abs(-5)", "str(12)", "int('42')", "len([1,2,3])",
    "'a,b,c'.split(',')", "'  x '.strip()", "'ab'.upper()", "'A'.lower()",
    "sum([1,2,3])", "sum([1,2,3], 10)",
    "dict(a=1,b=2)", "sorted([3,1,2], reverse=True)",
    "[*[1,2], 3]", "[*[1,2], *[3,4]]", "(*[1,2],)", "{**{'a':1}, 'b':2}",
    "max(*[1,5,3])", "dict(**{'x':9})",
    "'{}-{}'.format(1,2)", "[1,2,3].count(1)",
    "isinstance(1, int)", "tuple([1,2])",
]
for c in CASES:
    print(c, "=>", repr(eval(c)))

# Method calls take the LOAD_ATTR self-form rather than PUSH_NULL, so they are
# worth exercising through a few shapes.
print(eval("'a-b-c'.split('-')"))
print(eval("[3,1,2].__len__()"))
print(eval("(1,2).index(2)"))
print(eval("{'a':1}.get('a')"), eval("{'a':1}.get('z', 7)"))

# Keyword arguments arrive through KW_NAMES, and only after the positional
# ones; the reverse is a syntax error.
print(eval("sorted([2,1,3], reverse=True)"))
try:
    eval("f(a=1, 2)")
    print("no error")
except SyntaxError:
    print("SyntaxError for f(a=1, 2)")

# Nested containers and chained postfix operators.
print(eval("[[1,2],[3,4]][1][0]"))
print(eval("{'k': [1,2,3]}['k'][1:]"))
print(eval("'abcdef'[::-1]"))
