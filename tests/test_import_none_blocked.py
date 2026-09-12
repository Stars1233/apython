# `sys.modules[name] = None` is how a program BLOCKS an import.
#
# CPython's import system treats a None already sitting in sys.modules as a
# deliberate refusal rather than as a cached module: `importlib/_bootstrap.py`
# checks for it right after the sys.modules hit and raises
# ModuleNotFoundError('import of NAME halted; None in sys.modules').
#
# We took the dict hit and handed the None back as if it were the module, so
# `import x` bound None and raised NOTHING at all.  That is what
# `test.support.import_fresh_module(..., blocked=[...])` is built on -- it
# blocks a module by name and expects the import to fail -- so test_hashlib
# stopped on its first line, and so does anything else that fences off an
# optional accelerator in order to test the pure-Python path underneath.
#
# Note the exception TYPE: ModuleNotFoundError, not a bare ImportError.  It is
# a subclass, so `except ImportError` still catches it.
#
# The halted message is raised for the name actually being IMPORTED and only
# for that name.  `import a.b` with the PARENT `a` blocked is a different
# CPython path -- the None makes _gcd_import skip importing the parent, and
# then reading `parent_module.__path__` off the None raises AttributeError,
# which becomes "No module named 'a.b'; 'a' is not a package".  That case lives
# in tests/test_import_not_a_package.py, with the message it needs.

import sys


def show(label, fn):
    try:
        fn()
    except BaseException as e:
        print("%-24s %s: %s" % (label, type(e).__name__, e))
    else:
        print("%-24s NO RAISE" % label)


def plain():
    sys.modules["blocked_a"] = None
    import blocked_a


def from_form():
    sys.modules["blocked_b"] = None
    from blocked_b import anything


def from_star():
    sys.modules["blocked_c"] = None
    exec("from blocked_c import *", {})


def dotted_name_itself_blocked():
    # The blocked name is the full dotted one and its parent is a real
    # package we ship, so this is the halted message rather than the
    # not-a-package one.
    import collections
    sys.modules["collections.blockedsub"] = None
    import collections.blockedsub


def twice():
    # Raising must not consume the block: a second attempt fails the same way.
    sys.modules["blocked_f"] = None
    try:
        import blocked_f
    except ImportError:
        pass
    import blocked_f


def dunder_import():
    # The shape test.support.import_fresh_module actually goes through.
    sys.modules["blocked_g"] = None
    __import__("blocked_g")


def already_real():
    # A module that is genuinely loaded is still returned from the cache.
    import collections
    sys.modules["blocked_h"] = None
    import collections as again
    print("%-24s cache still works: %s" % ("", again is collections))


show("import x", plain)
show("from x import y", from_form)
show("from x import *", from_star)
show("import pkg.sub (sub blkd)", dotted_name_itself_blocked)
show("twice", twice)
show("__import__", dunder_import)
show("real module beside", already_real)

# A None does not poison its neighbours, and is not itself disturbed.
print("blocked_a still None:", sys.modules["blocked_a"] is None)

# Removing the None makes the name importable again -- the refusal must not
# have been cached as a negative result.
sys.modules["blocked_i"] = None
try:
    import blocked_i
except ImportError as e:
    print("blocked:", e)
del sys.modules["blocked_i"]
try:
    import blocked_i
except ImportError as e:
    print("after unblocking:", type(e).__name__, e)

# Blocking a module that is ALREADY imported shadows it for later importers.
import collections

sys.modules["collections"] = None
try:
    import collections as shadowed
except ImportError as e:
    print("shadowing a live module:", e)
finally:
    sys.modules["collections"] = collections
print("restored:", sys.modules["collections"] is collections)
