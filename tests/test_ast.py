# `ast`, and the _ast module under it.
#
# apython has had a complete parser for a long time and no way to see what it
# produced: the arena is 32-byte records addressed by a u32 index, freed the
# moment a compile ends.  compile(src, name, mode, ast.PyCF_ONLY_AST) now
# walks it and hands back CPython's node classes.
#
# ast.dump is the oracle for nearly all of this on purpose: it is a total
# function of every node's _fields, so one diff covers the whole model -- the
# class names, the field names, the field ORDER, and the shape of every list.
#
# The positions are compared too, with include_attributes=True: all four of
# lineno, col_offset, end_lineno and end_col_offset.
#
# Four things are left out because the parser, not this, is what is missing:
# a return annotation (`def f() -> int`) and the type-parameter syntax, which
# it reads and discards; the empty literal CPython puts after the last field
# of a nested format spec; and whether an annotated target was parenthesised,
# which is AnnAssign.simple.  All four are in bugs.md.

import ast

EXPRS = [
    "1 + 2", "a - b * c", "x // y % z", "a ** b ** c", "a | b ^ c & d",
    "a << 1 >> 2", "a @ b", "-x", "+x", "~x", "not x", "a and b or c",
    "a < b <= c", "a is not b", "a not in b", "x if y else z",
    "lambda: 1", "lambda a: a", "lambda a, b=1, *c, d, e=2, **f: a",
    "lambda a, /, b, *, c: a",
    "(1, 2)", "[1, 2]", "{1, 2}", "{1: 2}", "{1: 2, **d}", "()", "[]", "{}",
    "f(1, 2, k=3, *a, **b)", "f()", "f(*a)", "f(**b)",
    "a.b.c", "a[1]", "a[1:2]", "a[1:2:3]", "a[::2]", "a[:]", "a[b, c]",
    "(x := 1)", "[*a, *b]", "{**a, **b}",
    "[i for i in r]", "[i for i in r if i]", "[i for i in r if i if j]",
    "{i for i in r}", "{i: j for i, j in r}", "(i for i in r)",
    "[i for a in b for i in a]",
    "f'{x}'", "f'{x!r}'", "f'{x!s}'", "f'{x!a}'", "f'{x:>10}'",
    "f'a{x}b{y}c'", "f''",
    "'a' 'b'", "b'xy'", "None", "True", "False", "...", "1.5", "1j", "0x10",
]

STMTS = [
    "pass", "break" if False else "x = 1", "x = y = 1", "x, y = 1, 2",
    "[x, y] = z", "x += 1", "x **= 2", "x: int = 1", "x: int",
    "del x", "del x, y",
    "def f():\n    return", "def f():\n    return 1",
    "raise", "raise E", "raise E from C", "assert x", "assert x, 'm'",
    "global a, b",
    "import os", "import os.path", "import os.path as p", "import a, b",
    "from a import b", "from a import b as c", "from a import (b, c)",
    "from . import b", "from ..a import b as c", "from a import *",
    "if x: pass", "if x: pass\nelse: pass",
    "if x: pass\nelif y: pass\nelse: pass",
    "while x: pass", "while x: pass\nelse: pass",
    "while x:\n    break\n    continue",
    "for i in r: pass", "for i in r: pass\nelse: pass",
    "for i, j in r: pass",
    "with a: pass", "with a as b: pass", "with a as b, c as d: pass",
    "with (a as b, c as d): pass",
    "try: pass\nexcept: pass", "try: pass\nexcept E: pass",
    "try: pass\nexcept (E, F) as e: pass",
    "try: pass\nexcept E as e: pass\nelse: pass\nfinally: pass",
    "try: pass\nfinally: pass",
    "try: pass\nexcept* E: pass",
    "def f(): pass", "def f(a, b=1, *c, d, **e): return a",
    "def f(a, /, b, *, c): pass", "def f(a: int, b: str = 'x'): pass",
    "@d\ndef f(): pass", "@d1\n@d2(x)\ndef f(): pass",
    "class C: pass", "class C(B): pass", "class C(B, metaclass=M): pass",
    "class C(*bases, **kw): pass", "@d\nclass C: pass",
    "async def f(): pass", "async def f():\n    await x",
    "async def f():\n    async for i in r: pass",
    "async def f():\n    async with a: pass",
    "async def f():\n    return [i async for i in r]",
    "def g():\n    yield", "def g():\n    yield 1", "def g():\n    yield from r",
    "def f():\n    nonlocal_placeholder = 1\n    def g():\n        nonlocal nonlocal_placeholder",
    "def f():\n    'doc'\n    pass",
    "match x:\n    case 1: pass",
    "match x:\n    case 'a' | 'b': pass",
    "match x:\n    case [1, 2]: pass",
    "match x:\n    case [1, *rest]: pass",
    "match x:\n    case (1, 2): pass",
    "match x:\n    case {'a': b}: pass",
    "match x:\n    case {'a': b, **r}: pass",
    "match x:\n    case C(): pass",
    "match x:\n    case C(1, k=2): pass",
    "match x:\n    case [1] as y: pass",
    "match x:\n    case None: pass",
    "match x:\n    case True: pass",
    "match x:\n    case _: pass",
    "match x:\n    case y if y > 1: pass",
    "match x:\n    case 1: pass\n    case 2: pass",
]

print("=== expressions, in eval mode ===")
for src in EXPRS:
    print(repr(src), ast.dump(ast.parse(src, mode="eval")))

print("=== statements, in exec mode ===")
for src in STMTS:
    print(repr(src), ast.dump(ast.parse(src)))

print("=== the same expressions as statements ===")
for src in EXPRS[:20]:
    print(repr(src), ast.dump(ast.parse(src)))

print("=== compile() takes the flag positionally and by keyword ===")
print(ast.dump(compile("1+1", "<s>", "eval", ast.PyCF_ONLY_AST)))
print(ast.dump(compile("1+1", "<s>", "eval", flags=ast.PyCF_ONLY_AST)))
print(type(compile("1+1", "<s>", "eval")).__name__)
print(ast.PyCF_ONLY_AST)

print("=== literal_eval ===")
for src in ["1", "-1", "1.5", "'s'", "b'b'", "True", "None", "...",
            "(1, 2)", "[1, [2]]", "{1: 2}", "{1, 2}", "1 + 2j", "-1 - 2j"]:
    print(repr(src), repr(ast.literal_eval(src)))
for bad in ["f()", "x", "1 if x else 2"]:
    try:
        ast.literal_eval(bad)
        print(repr(bad), "NO ERROR")
    except ValueError:
        print(repr(bad), "ValueError")

print("=== walk, iter_fields, iter_child_nodes ===")
tree = ast.parse("def f(a):\n    return a + 1\n")
print(sorted({type(n).__name__ for n in ast.walk(tree)}))
print([n for n, _ in ast.iter_fields(tree.body[0])])
print([type(n).__name__ for n in ast.iter_child_nodes(tree.body[0])])

print("=== NodeVisitor ===")


class Names(ast.NodeVisitor):
    def __init__(self):
        self.seen = []

    def visit_Name(self, node):
        self.seen.append((node.id, type(node.ctx).__name__))
        self.generic_visit(node)


v = Names()
v.visit(ast.parse("a = b + c[d]\nfor e in f: del g\n"))
print(v.seen)

print("=== the class model ===")
print(ast.AST._fields, ast.expr._fields)
print(ast.Name._fields, ast.Name._attributes)
print(ast.arguments._fields)
print(ast.FunctionDef._fields)
print(issubclass(ast.Name, ast.expr), issubclass(ast.expr, ast.AST))
print(issubclass(ast.Add, ast.operator), issubclass(ast.Load, ast.expr_context))
n = ast.Name("x", ast.Load())
print(n.id, type(n.ctx).__name__, sorted(n.__dict__))
try:
    ast.Name().id
except AttributeError:
    print("an unset field is an AttributeError")
# ast.get_docstring is not called here: it imports inspect for cleandoc, and
# inspect needs _imp.
print(ast.parse("'doc'\npass").body[0].value.value)

print("=== positions, on every node ===")
POS = [
    "x = a + 1", "a.b.c", "f(x, y)", "a[1:2]", "x if y else z",
    "a and b or c", "a < b < c", "[1, 2]", "{'k': v}", "-x",
    "def f(a, b=1):\n    return a\n",
    "class C(B):\n    x = 1\n",
    "if a:\n    b\nelse:\n    c\n",
    "for i in r:\n    pass\n",
    "while a:\n    b\n",
    "with a as b:\n    pass\n",
    "try:\n    a\nexcept E as e:\n    b\nfinally:\n    c\n",
    "lambda a: a + 1",
    "[i for i in r]",
    "x: int = 1",
    "x += 1",
    "import a.b as c",
    "from x import y",
    "assert a, b",
    "del a, b",
    "@d\ndef f(): pass\n",
    "a = (\n    1 +\n    2\n)\n",
    "def f():\n    return (a,\n            b)\n",
    "f(x for x in y)",
    "sum(i * 2 for i in r if i)",
    "(x for x in y)",
    "{i for i in r}",
    "{i: j for i, j in r}",
    "async def f():\n    async for i in r:\n        await g()\n",
    "async def f():\n    async with a as b:\n        pass\n",
    "try:\n    a\nexcept A:\n    b\nexcept B as e:\n    c\nelse:\n    d\n",
    "try:\n    a\nexcept* E:\n    b\n",
    "from . import x",
    "from ..p import a as b, c",
    "import a.b, c as d",
    "def f(a, /, b, *c, d=1, **e):\n    pass\n",
    "def f(a: int = 1, *, b: str) -> bool:\n    pass\n",
    "x = *a, *b",
    "a[b], c = d",
    "a[::2, ...]",
    "global g\nnonlocal_ = 1\n",
    "raise E('m') from f",
    "x = f'{a!r:>{w}} tail'",
    "x = 'a' 'b'",
    "match p:\n    case [1, *r] if r:\n        pass\n    case {'k': v, **w}:\n        pass\n    case C(a, b=2) | None:\n        pass\n    case _:\n        pass\n",
    "@a.b(c)\nclass C(B, metaclass=M):\n    pass\n",
    "yield_ = lambda: (yield)",
    "def f():\n    yield 1\n    x = yield from g()\n",
    "if a:\n    pass\nelif b:\n    pass\nelse:\n    pass\n",
    "while a:\n    break\nelse:\n    continue\n",
    "for i in r:\n    pass\nelse:\n    pass\n",
    "with (a as b, c as d):\n    pass\n",
    "x = a if b else c if d else e",
    "print(*a, **b)",
    "x = not a is not b",
    "x = ~-+1",
    "x = b'ab' b'cd'",
    "x = (\n)",
    "x = [\n    1,\n]",
]
for src in POS:
    print(repr(src), ast.dump(ast.parse(src), include_attributes=True))

print("=== a syntax error is still a syntax error ===")
for bad in ["1 +", "def", "class 1:", "for x in: pass"]:
    try:
        ast.parse(bad)
        print(repr(bad), "NO ERROR")
    except SyntaxError as e:
        # Only the type: SyntaxError carries none of CPython's .msg,
        # .filename, .lineno or .offset attributes here -- see bugs.md.
        print(repr(bad), type(e).__name__)
