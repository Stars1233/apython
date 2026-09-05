"""`# type:` comments, and the two token kinds that carry them.

The statement dispatch bounds against TOK_COUNT and indexes stmt_table, and
the table was two rows short of it: the two type-comment tokens were added to
the token set without rows here, so one in statement position read past the
end of the table and called whatever followed it.  The assembler checks the
length now, since nothing catches a short jump table at run time.

A `# type: ignore` on a line of its own also reaches statement position,
where nothing was taking it -- ast_take_typecomment collects one only when a
statement precedes it.  CPython records it on the Module wherever it appears,
and carries the line terminator in its tag when the comment is the whole
line but not when it follows a statement.
"""

import ast

CASES = [
    "# type: ignore\nx = 1\n",
    "x = 1\n# type: ignore\n",
    "x = 1  # type: ignore\n",
    "x = 1  # type: ignore[abc]\n",
    "# type: ignore[code] and more\ny = 2\n",
    "# type: ignore\n# type: ignore\nz = 3\n",
    "x = 1  # type: int\n",
    "def f(a):\n    # type: (int) -> str\n    return 'x'\n",
    "for i in []:  # type: int\n    pass\n",
    "with open('f') as fh:  # type: object\n    pass\n",
    "x = 1\n",
]

print("--- with type comments asked for ---")
for src in CASES:
    tree = ast.parse(src, type_comments=True)
    ignores = [(t.lineno, t.tag) for t in tree.type_ignores]
    comments = [getattr(node, "type_comment", None) for node in tree.body]
    print(repr(src), ignores, comments)

print("--- and without, where they are only comments ---")
for src in CASES:
    tree = ast.parse(src)
    print(repr(src), tree.type_ignores,
          [getattr(node, "type_comment", None) for node in tree.body])

print("--- a type comment that belongs to nothing is a syntax error ---")
for src in ("# type: int\nx = 1\n", "x = 1\n# type: (int) -> str\n"):
    try:
        ast.parse(src, type_comments=True)
        print(repr(src), "accepted")
    except SyntaxError as e:
        print(repr(src), "SyntaxError", e.msg)

print("--- and compiling the same source still runs it ---")
ns = {}
exec(compile("# type: ignore\nresult = 6 * 7  # type: int\n", "<t>", "exec"), ns)
print(ns["result"])
print("done")
