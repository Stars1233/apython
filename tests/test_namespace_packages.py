# PEP 420 implicit namespace packages -- a directory with no __init__.py.
#
# `import nspkg` for a directory that carries no __init__.py was
# ModuleNotFoundError: import_search_syspath tried five patterns, four of
# which want an __init__ and one of which wants a .py of the leaf's own name,
# and a bare directory matched none.  CPython has made such a directory a
# package since 3.3.
#
# It is not an exotic corner.  CPython's own Lib/test/test_warnings/data/ has
# no __init__.py, so `from test.test_warnings.data import package_helper` --
# the second line of that test module -- failed, and all of test_warnings
# with it.  This tree's own tests/ directory is one too.
#
# Two rules make it more than "fall back to the directory":
#
#   - The portions ACCUMULATE.  A namespace package's __path__ is every
#     matching directory on sys.path, not the first, so two distributions can
#     each ship part of one package.  That is the whole point of the PEP.
#   - A regular package or module WINS, wherever it is on the path.  A real
#     package on sys.path[1] beats a namespace portion on sys.path[0], which
#     is the opposite of the usual first-match rule and the reason the search
#     has to record portions and keep looking rather than return.
import os
import sys

# Run from the .pyc, __file__ is the .pyc -- so the directory beside it is
# tests/__pycache__ rather than tests/.  CPython does the same when handed a
# .pyc directly; the fixture is found relative to the source either way.
HERE = os.path.dirname(os.path.abspath(__file__))
if os.path.basename(HERE) == "__pycache__":
    HERE = os.path.dirname(HERE)
A = os.path.join(HERE, "nspkg_a")
B = os.path.join(HERE, "nspkg_b")
sys.path.insert(0, B)
sys.path.insert(0, A)

# --- a namespace package whose portions are in two directories ---------
import alpha.one
import alpha.two

print("portion in A:", alpha.one.WHO)
print("portion in B:", alpha.two.WHO)

import alpha

print("__file__ is None:", getattr(alpha, "__file__", "ABSENT"))
print("__path__ has both portions:",
      [os.path.basename(os.path.dirname(p)) for p in alpha.__path__])
print("__name__:", alpha.__name__)
print("is a module:", type(alpha).__name__)

# --- a namespace package nested inside a namespace package ------------
import alpha.deep.three

print("nested:", alpha.deep.three.WHO)
print("nested __path__:",
      [os.path.basename(p) for p in alpha.deep.__path__])

# --- one with a single portion ----------------------------------------
import solo.only

print("single portion:", solo.only.WHO)
print("single __path__ length:", len(solo.__path__))

# --- from-import off a namespace package ------------------------------
from alpha import one as one_again
from alpha.deep import three as three_again

print("from-import:", one_again.WHO, three_again.WHO)
print("identity with sys.modules:", one_again is sys.modules["alpha.one"])

# --- a REGULAR package still works, and still wins --------------------
import regular
import regular.inside

print("regular package:", regular.WHO, regular.inside.WHO)
print("regular has a __file__:", os.path.basename(regular.__file__))

# `shadowed` is a bare directory in A -- which comes FIRST on sys.path --
# and a real package in B.  CPython takes the real one anyway.
import shadowed

print("a real package beats an earlier portion:", shadowed.WHO)
try:
    import shadowed.ghost
    print("the shadowed portion is reachable: WRONG")
except ImportError:
    print("the shadowed portion is not reachable: right")

# --- what must still fail ---------------------------------------------
try:
    import no_such_namespace
    print("a missing name: NOT REFUSED")
except ModuleNotFoundError as exc:
    print("a missing name:", exc)
try:
    import alpha.no_such_submodule
    print("a missing submodule: NOT REFUSED")
except ModuleNotFoundError as exc:
    print("a missing submodule:", exc)
try:
    from alpha import no_such_name
    print("a missing from-name: NOT REFUSED")
except ImportError as exc:
    print("a missing from-name:", type(exc).__name__)

# --- reimporting is the same object -----------------------------------
import alpha as alpha2

print("cached:", alpha2 is alpha, sys.modules["alpha"] is alpha)
print("survived")
