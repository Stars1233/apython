# A recorded compile error must not read as success.
#
# cg_e_attribute's exit label was shared between the success and failure paths
# and never zeroed eax, so cg_super_attr's -1 -- which means "an error is
# already recorded" -- was handed back and every caller's `test eax, eax` read
# it as success.  The SyntaxError was dropped on the floor and the module
# assembled with the offending expression simply missing from the bytecode:
# `def f(): return super().__repr__` compiled clean, printed an empty line and
# exited 0.
#
# The two interpreters disagree about WHEN a zero-argument super() outside a
# method is refused -- CPython defers it to call time as a RuntimeError, this
# one refuses it at compile time -- but they agree that it must be refused, and
# that is what this pins.  Silently running is the bug.
src = "def f():\n    return super().__repr__\n"
try:
    ns = {}
    exec(src, ns)
    ns["f"]()
    print("ran silently")
except (SyntaxError, RuntimeError, TypeError):
    print("refused")

src = "class C:\n    @staticmethod\n    def m():\n        return super().x\n"
try:
    ns = {}
    exec(src, ns)
    ns["C"].m()
    print("ran silently")
except (SyntaxError, RuntimeError, TypeError):
    print("refused")


# The ordinary forms still compile and still work.
ns = {}
exec("class A:\n    def r(self):\n        return 'A'\n"
     "class B(A):\n    def r(self):\n        return 'B' + super().r()\n"
     "out = B().r()\n", ns)
print(ns["out"])

ns = {}
exec("class A:\n    def r(self):\n        return 'A'\n"
     "class B(A):\n    def r(self):\n        return 'B' + super(B, self).r()\n"
     "out = B().r()\n", ns)
print(ns["out"])


# A super() attribute that is called, rather than merely fetched, goes through
# the other of the two call sites.
ns = {}
exec("class A:\n    def v(self):\n        return 7\n"
     "class B(A):\n    def v(self):\n        return super().v() + 1\n"
     "out = B().v()\n", ns)
print(ns["out"])


# And an error the compiler records for an ordinary reason still surfaces.
try:
    compile("def f():\n    return 1 +\n", "<t>", "exec")
    print("accepted")
except SyntaxError:
    print("SyntaxError")
