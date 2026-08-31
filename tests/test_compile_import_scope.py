# An import binds its name through the symbol table, like any other assignment.
#
# cg_s_import and cg_s_importfrom always emitted STORE_NAME, ignoring what the
# symbol table said -- so an import inside a function wrote into a locals
# mapping a function frame does not have, and one captured by a nested function
# never reached the cell.
#
# The symbol table was wrong too: it bound the whole dotted name for
# `import a.b`, where CPython binds `a`.  And the bound head is mangled while
# the imported name is not, which can only be decided in the parser -- the
# class that does the mangling is gone by the time the symbol table runs.
ns = {}
exec("def f():\n    import sys\n    return sys.maxsize > 0\nout = f()\n", ns)
print(ns["out"])

ns = {}
exec("def f():\n    import sys as s\n    def g():\n        return s.maxsize > 0\n"
     "    return g()\nout = f()\n", ns)
print(ns["out"])

ns = {}
exec("def f():\n    from sys import maxsize\n    return maxsize > 0\nout = f()\n", ns)
print(ns["out"])

ns = {}
exec("def f():\n    from sys import maxsize as m\n    def g():\n        return m > 0\n"
     "    return g()\nout = f()\n", ns)
print(ns["out"])

# A dotted import binds the top package, not the dotted name.
ns = {}
exec("def f():\n    import sys\n    return sys.maxsize > 0\nout = f()\n"
     "names = f.__code__.co_varnames\n", ns)
print(ns["out"], ns["names"])

# In a class body the import lands in the class namespace.
ns = {}
exec("class C:\n    import sys\n", ns)
print(ns["C"].sys.maxsize > 0, "sys" in ns["C"].__dict__)

# ...and a private name in a class body is mangled, as any other binding is.
ns = {}
exec("class C:\n    import sys as __s\n", ns)
print(sorted(k for k in ns["C"].__dict__ if k.endswith("__s")))

# A comprehension can capture an imported name.
ns = {}
exec("def f():\n    import sys\n    return [sys.maxsize > 0 for _ in range(2)]\n"
     "out = f()\n", ns)
print(ns["out"])

# Module scope is unchanged: still by name, against the module dict.
d = {}
exec("import sys", d)
print(d["sys"].maxsize > 0)

d = {}
exec("from sys import maxsize", d)
print(d["maxsize"] > 0)

# `import *` still binds nothing statically and is still a module-scope thing.
d = {}
exec("from sys import *", d)
print("maxsize" in d)
