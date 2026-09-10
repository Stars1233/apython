# Every location a code object reports has to come from the source.
#
# cg_unit_init set the four `cur*` location fields and not the four `last*`
# ones beside them, and the implicit `return None` takes its line from
# .lastline -- which nothing has written when a unit emits no lined
# instruction before it.  An empty module, a module that is only a comment and
# an empty function body all reported a line number read off the stack: 13, 14,
# whatever happened to be there.  Under valgrind it is a conditional jump on an
# uninitialised value in cg_emit, on every compile in the tree.
#
# The runs are compared by PROPERTY rather than against CPython's: two
# compilers may split a line table differently and both be right, but neither
# may name a line the file does not have.


def lines_of(src):
    c = compile(src, "<t>", "exec")
    return [ln for (_, _, ln) in c.co_lines() if ln is not None]


def sane(src):
    n = src.count("\n") + 1
    return all(0 <= ln <= n for ln in lines_of(src))


cases = [
    "",
    "pass",
    "# just a comment\n",
    "\n\n\n",
    "'''doc'''",
    "\n\n\npass\n",
    "if False:\n    pass\n",
    "def f(): pass",
    "def f():\n    pass\n",
    "class C: pass",
    "class C:\n    '''d'''\n",
    "def f():\n    '''d'''\n",
    "lambda: None",
    "[x for x in ()]",
    "async def f(): pass",
    "try:\n    pass\nfinally:\n    pass\n",
]
for src in cases:
    print(repr(src), sane(src))

# The nested units get their own line tables, and the same rule.
src = "def outer():\n    def inner():\n        pass\n    return inner\n"
top = compile(src, "<t>", "exec")


def walk(code, depth=0):
    yield code
    for k in code.co_consts:
        if hasattr(k, "co_lines"):
            yield from walk(k, depth + 1)


ok = True
for c in walk(top):
    for _, _, ln in c.co_lines():
        if ln is not None and not (0 <= ln <= 4):
            ok = False
print("nested units sane:", ok)

# And the implicit return really is attributed to the last statement, which is
# what .lastline is for.
src = "x = 1\ny = 2\n"
last = [ln for (_, _, ln) in compile(src, "<t>", "exec").co_lines() if ln is not None]
print("implicit return line:", last[-1])
print("done")
