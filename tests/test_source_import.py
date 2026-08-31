# Phase 10: running and importing .py source.
#
# The interpreter reads a .pyc when one is there and compiles the .py when one
# is not.  The source patterns are searched last, so a directory that already
# has a __pycache__ behaves exactly as it did before; this file exercises the
# fallback by importing a package that deliberately has none.
#
# There is no .pyc writer: compiling is cheap, and a cache would need mtime
# comparison and atomic replacement to be anything but a source of stale
# bytecode.
import sys
sys.path.insert(0, "tests")

import srcpkg
from srcpkg import GREETING, shout
from srcpkg.inner import helper
import srcpkg.sub
from srcpkg.sub import deep
from srcpkg.sub.deep import DEPTH

print(srcpkg.NAME, GREETING, shout("hi"))
print(helper(), srcpkg.sub.LEVEL, DEPTH)
print(srcpkg.__name__, srcpkg.inner.__name__, deep.__name__)
print(srcpkg.__package__, deep.__package__)

# The package's __path__ points at the package directory, not at a __pycache__
# one level down -- the two layouts strip a different number of components.
print(srcpkg.__path__[0].replace("\\", "/").split("/")[-1])

# Everything the compiler can build, imported rather than exec'd.
print(srcpkg.mixed.run())
