# PEP 562: a module's own __getattr__ and __dir__.
#
# A module-level `def __getattr__(name)` runs when ordinary resolution of a
# module attribute misses -- it is the module namespace's version of the hook
# a class gets.  module_getattr had no such fallback, so every one of them was
# ignored and the attribute was a plain AttributeError.
#
# What that cost was not the feature itself but the modules written against
# it.  CPython's unittest/__init__.py publishes IsolatedAsyncioTestCase this
# way and its concurrent/futures/__init__.py publishes ProcessPoolExecutor;
# both were AttributeError here, and each one kills a whole test module at
# collection rather than failing a test.  lib/ast.py, which is CPython's own
# file unmodified, publishes the five deprecated node aliases the same way.
import sys
import warnings

sys.path.insert(0, "tests")

import pep562helper as m

# A name that IS in the module dict does not reach the hook.
print("visible:", m.visible)
print("asked after a hit:", m.asked)

# A name that is not reaches it.
print("lazy:", m.lazy)
print("built:", m.built)
print("asked:", m.asked)

# getattr() and its default, and hasattr(), all have to see an AttributeError
# raised by the hook as "absent" rather than as a failure.
print("getattr hit:", getattr(m, "lazy"))
print("getattr default:", getattr(m, "nope", "<default>"))
print("hasattr present:", hasattr(m, "lazy"))
print("hasattr absent:", hasattr(m, "nope"))

# A bare attribute access reports what the hook raised, wording included.
try:
    m.nope
except AttributeError as e:
    print("AttributeError:", e)

# Anything that is NOT an AttributeError propagates unchanged; it is a failure
# in the middle of a lookup, not the protocol saying "absent".
try:
    m.boom
except RuntimeError as e:
    print("RuntimeError:", e)
except AttributeError as e:
    print("wrongly turned into AttributeError:", e)

# ...and hasattr must not swallow it either.
try:
    hasattr(m, "boom")
except RuntimeError as e:
    print("hasattr propagates:", e)

# The hook may reach back into its own module for another name.
print("recurse:", m.recurse)

# from-import goes through the same miss.
from pep562helper import lazy as imported_lazy

print("from-import:", imported_lazy)

try:
    from pep562helper import nope
except ImportError as e:
    print("from-import miss is an ImportError")

# __dir__ replaces the module's answer wholesale, and dir() sorts it.
print("dir:", dir(m))

# A module with no hook at all is unchanged: the miss is still a plain
# AttributeError and dir() still reports the dict.
import myhelper

try:
    myhelper.definitely_not_there
except AttributeError as e:
    print("no-hook AttributeError:", e)
print("no-hook dir has module name:", "__name__" in dir(myhelper))

# Setting a name the hook would have answered shadows it, because the dict is
# consulted first.
m.lazy = "shadowed"
print("shadowed:", m.lazy)
del m.lazy
print("unshadowed:", m.lazy)

# Assigning a non-callable __getattr__ is CPython's business to refuse at call
# time, not ours to treat as absent.
import types

hookless = types.ModuleType("hookless")
hookless.__getattr__ = 5
try:
    hookless.anything
except TypeError as e:
    print("non-callable hook:", type(e).__name__)
except AttributeError as e:
    print("non-callable hook wrongly absent:", type(e).__name__)

# A hook installed at runtime works, and one removed stops working.
live = types.ModuleType("live")
try:
    live.x
except AttributeError:
    print("live: absent before")
live.__getattr__ = lambda name: "answered " + name
print("live:", live.x)
del live.__getattr__
try:
    live.x
except AttributeError:
    print("live: absent again")

# lib/ast.py is CPython's own file and publishes its deprecated aliases
# through the hook, so this is the shipped tree exercising it.
import ast

with warnings.catch_warnings():
    warnings.simplefilter("ignore")
    print("ast.Num:", ast.Num.__name__)
    print("ast.Str:", ast.Str.__name__)
try:
    ast.NotANode
except AttributeError:
    print("ast miss is still an AttributeError")

print("done")
