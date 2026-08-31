# The scan that decides whether a block needs SETUP_ANNOTATIONS.
#
# It walked a node's child list for any kind it did not recognise, but
# AST_GLOBAL and AST_NONLOCAL keep OBJECT indices there, and the object and
# node arenas overlap freely.  Walking one as a node landed on whatever
# happened to sit at that index, and `global a` at module scope recursed about
# a hundred thousand deep and segfaulted the compiler.  An allow-list of the
# kinds whose children really are blocks is the fix, the same shape sym_visit
# uses.
#
# The other half: a for/else keeps its else block in clist with nchild at 0, so
# no child walk reaches it, and an annotation there never created
# __annotations__.
ns = {}
exec("global a\na = 1\n", ns)
print("global at module scope", ns["a"])

ns = {}
exec("def f():\n    global b\n    b = 2\nf()\n", ns)
print("global in a function", ns["b"])

ns = {}
exec("if True:\n    global c\n    c = 3\n", ns)
print("global under an if", ns["c"])

ns = {}
exec("try:\n    global d\n    d = 4\nexcept Exception:\n    pass\n", ns)
print("global under a try", ns["d"])

ns = {}
exec("def g():\n    x = 1\n    def h():\n        nonlocal x\n        x = 2\n"
     "    h()\n    return x\nout = g()\n", ns)
print("nonlocal", ns["out"])

# A comparison chain interleaves raw operator codes with node indices in the
# same child list -- the same trap one size smaller.
ns = {}
exec("v = 1 == 1 == 1\n", ns)
print("compare chain", ns["v"])

# for/else, at module scope and in a class body.
ns = {}
exec("for i in []:\n    pass\nelse:\n    x: int = 1\n", ns)
print("for-else", sorted(ns["__annotations__"]))

ns = {}
exec("class C:\n    for i in []:\n        pass\n    else:\n        y: int = 1\n", ns)
print("class for-else", sorted(ns["C"].__annotations__))

# while/else is the ordinary shape, and still works.
ns = {}
exec("while False:\n    pass\nelse:\n    z: int = 1\n", ns)
print("while-else", sorted(ns["__annotations__"]))

# An annotation nested in the compound statements really does still count.
ns = {}
exec("if True:\n    p: int = 1\n", ns)
print("if", sorted(ns["__annotations__"]))

ns = {}
exec("for i in [1]:\n    q: int = 1\n", ns)
print("for body", sorted(ns["__annotations__"]))

ns = {}
exec("with open('/dev/null') as fh:\n    r: int = 1\n", ns)
print("with", sorted(ns["__annotations__"]))

# ...and a module with no annotation anywhere still has no __annotations__.
ns = {}
exec("global e\ne = 5\nfor i in []:\n    pass\nelse:\n    pass\n", ns)
print("none", "__annotations__" in ns, ns["e"])
