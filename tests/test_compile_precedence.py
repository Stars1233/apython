# Binding powers the Pratt driver got one step wrong.
#
# The driver continues while lbp > min_bp, so an operand parsed AT an
# operator's own power stops before it.  Three places wanted one level below:
#
#   * a conditional's else branch.  At BP_TERNARY, `a if p else b if q else c`
#     came out left-nested, so `"pos" if x > 0 else "zero" if x == 0 else "neg"`
#     answered "neg" for a positive x.
#   * a lambda body.  At BP_TERNARY the `if` stopped, leaving the lambda as the
#     conditional's body and its parameter out of scope -- a NameError, not a
#     wrong answer.
#   * a starred element.  Its operand is an or_expr: `[*a | b]` is `[*(a | b)]`
#     while `[*a in b]` is a syntax error.  Recursing below BP_COMPARE swallowed
#     the `in` of a for statement, so `for a, *b in z` had no loop keyword left
#     -- while `for *a, b in z` parsed, because there the star was not the
#     element the `in` followed.
#
# The walrus is here because it had no row in the table at all.
SRC = '''
def classify(x):
    return "pos" if x > 0 else "zero" if x == 0 else "neg"


print(classify(5), classify(0), classify(-5))
print([classify(v) for v in (2, 0, -2)])

f = lambda x: "pos" if x > 0 else "non-pos"
g = lambda a, b=2: a if a > b else b
print(f(1), f(0), g(1), g(5))
print((lambda: 1 if 0 else 2)())
print(sorted([3, 1, 2], key=lambda v: -v if v > 1 else v))


def gen():
    yield 1 if 0 else 2
    yield from (3 if 1 else 4,)


print(list(gen()))

for a, *b in [(1, 2, 3), (4, 5, 6)]:
    print(a, b)
for *a, b in [(1, 2, 3)]:
    print(a, b)
x, *y, z = 1, 2, 3, 4
print(x, y, z)
print([*[1, 2], *[3]], {**{"a": 1}, **{"b": 2}})

if (n := 10) > 5:
    print("walrus", n)
print(m := 3, m)
print([y for v in (1, 2, 3) if (y := v * 2) > 2])
print((w := 7) + w)

# A comma still ends a lambda body and a conditional.
print(1 if 1 else 2, 3)
h = lambda: 1, 2
print(type(h[0]).__name__, h[1])
'''
ns = {}
exec(compile(SRC, "<t>", "exec"), ns)

# The syntax errors these levels must keep rejecting.
for bad in ["[*a in b]", "[*a or b]", "(a.b := 1)", "(a[0] := 1)"]:
    try:
        compile(bad, "<t>", "eval")
        print("no error for", bad)
    except SyntaxError:
        print("SyntaxError for", bad)
