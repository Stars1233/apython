# compile(..., "single") has a start symbol of its own.
#
# CPython's is `NEWLINE | simple_stmt | compound_stmt NEWLINE`, and this ran
# the full module grammar and then relabelled the root -- so it accepted three
# things CPython refuses.  The tree it built for them was right; nothing but
# the grammar was wrong.
#
#   * nothing at all: "" and "\n" and a comment on its own are all a
#     SyntaxError there, despite NEWLINE being one of the three alternatives.
#   * a second statement, which has a message of its own.
#   * a compound statement whose last suite sits on the same line as its
#     header, with no trailing newline: `def f(): pass` is an error and
#     `def f(): pass\n` is not.

import ast


def single(src):
    try:
        compile(src, "<s>", "single")
        return "ok"
    except SyntaxError as exc:
        return "SyntaxError: " + str(exc).split(" (")[0]


CASES = [
    # nothing
    "", "\n", "  \n", "# just a comment\n", "\n\n\n",
    # one simple statement, with and without the newline
    "x = 1", "x = 1\n", "x = 1\n\n", "pass", "print",
    # a semicolon-joined simple statement is ONE statement
    "x = 1; y = 2", "x = 1; y = 2\n", "x = 1; y = 2; z = 3",
    # two logical lines are two statements
    "x = 1\ny = 2", "x = 1\ny = 2\n", "x = 1\n\ny = 2\n",
    # a compound statement, block on its own lines
    "if 1:\n    pass", "if 1:\n    pass\n", "if 1:\n    pass\n\n",
    "while 0:\n    pass", "for i in []:\n    pass\n",
    "def f():\n    return 1\n", "class C:\n    pass\n",
    "try:\n    pass\nexcept:\n    pass\n",
    "with open:\n    pass\n",
    "if 1:\n    pass\nelse:\n    pass\n",
    # ...and with the suite inline, which needs the trailing newline
    "if 1: pass", "if 1: pass\n",
    "def f(): pass", "def f(): pass\n",
    "class C: pass", "class C: pass\n",
    "while 0: pass", "while 0: pass\n",
    "for i in []: pass", "for i in []: pass\n",
    # the last suite is the one that counts
    "if 1:\n    pass\nelse: pass",
    "try:\n    pass\nexcept: pass",
    # a compound statement followed by another statement
    "if 1:\n    pass\nx = 1\n",
]

for src in CASES:
    print(repr(src).ljust(34), single(src))

print("=== the tree is still Interactive ===")
tree = ast.parse("x = 1", mode="single")
print(type(tree).__name__, len(tree.body), type(tree.body[0]).__name__)
tree = ast.parse("x = 1; y = 2", mode="single")
print(type(tree).__name__, len(tree.body))
tree = ast.parse("if 1:\n    pass\n", mode="single")
print(type(tree).__name__, type(tree.body[0]).__name__)

print("=== and it still runs as exec does ===")
ns = {}
exec(compile("x = 40 + 2", "<s>", "single"), ns)
print(ns["x"])
exec(compile("def g(): return 7\n", "<s>", "single"), ns)
print(ns["g"]())
print("done")
