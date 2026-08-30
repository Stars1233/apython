# Relative imports inside a package.  The level operand of IMPORT_NAME was
# read off the stack and then ignored, so `from . import x` looked like an
# import of the empty name.
import sys
sys.path.insert(0, "tests")

import relpkg
print(relpkg.VALUE, relpkg.mod.VALUE, relpkg.sub.deep.D)
print(relpkg.__package__, relpkg.mod.__package__, relpkg.sub.deep.__package__)

from relpkg import mod
print(mod.VALUE)
from relpkg.sub import deep
print(deep.D)
