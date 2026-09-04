# A module's __file__ is the source, not the cache file it came out of.
#
# CPython records the .py even when it executes a cached .pyc: it is what a
# module's repr prints, what inspect.getsource opens, and what a traceback
# through the module names.  This recorded whichever file the import search
# matched, so every import through __pycache__ answered
# ".../__pycache__/m.cpython-312.pyc".
#
# The rewrite is textual and needs no stat: "<dir>/__pycache__/<name>
# .cpython-312.pyc" is "<dir>/<name>.py", and a .pyc that is NOT under a
# __pycache__ directory is CPython's sourceless form, whose __file__ is the
# .pyc and is left alone.

import os
import sys

sys.path.insert(0, "tests")

# A plain module and a package, both from this directory, both imported the
# ordinary way -- which means from their .pyc, since make check compiles the
# whole tree first.
import myhelper
import srcpkg

for mod in (myhelper, srcpkg):
    print(mod.__name__.ljust(10),
          os.path.basename(mod.__file__),
          mod.__file__.endswith(".py"),
          "__pycache__" in mod.__file__)

# A package's __file__ is its __init__.py, and its directory is the package's.
print("package   ", os.path.basename(srcpkg.__file__),
      os.path.basename(os.path.dirname(srcpkg.__file__)))

# The repr prints it, which is the most visible place it shows.
print("repr      ", repr(srcpkg).split(" from ")[0] + " from ...",
      repr(srcpkg).endswith(".py'>"))

# A submodule, and one nested a level deeper.
import srcpkg.inner
import srcpkg.sub.deep
print("submodule ", os.path.basename(srcpkg.inner.__file__),
      srcpkg.inner.__file__.endswith(".py"))
print("nested    ", os.path.basename(srcpkg.sub.deep.__file__),
      srcpkg.sub.deep.__file__.endswith(".py"))

# A module of the interpreter's own library.
import posixpath
print("stdlib    ", os.path.basename(posixpath.__file__),
      posixpath.__file__.endswith(".py"))

# A builtin has no __file__ at all, which is how a program tells the two
# apart -- and the reason to check is that the rewrite must not invent one.
import sys as _sys
print("builtin   ", hasattr(_sys, "__file__") and _sys.__file__ or "none")
print("done")
