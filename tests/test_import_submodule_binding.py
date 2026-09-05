"""Two things about importing a submodule that a package's __init__ relies on.

A package's `__init__` shares its globals with the package MODULE, so binding
a submodule as an attribute of the package is what makes the bare name
visible inside `__init__`.  CPython does that wherever a submodule is loaded;
here the dotted walk did it and the `from ... import name` fallback did not,
so a submodule that some OTHER submodule had already pulled in was never
bound.  asyncio's __init__ is written on exactly that: it reads
`coroutines.__all__` having written only `from .coroutines import *`, and it
is base_events, imported one line earlier, that actually imports coroutines.

The same fallback pushed a BORROWED reference onto the value stack, which
owns what it holds -- so the module lost a reference every time.
"""

import os
import sys

# A directory of its own under the working directory, made here rather than
# with tempfile: this has to run the same way from a .pyc and from source,
# and the point of it is the import machinery, not the rest of the library.
root = "_bindtree"
pkg = os.path.join(root, "bindpkg")
for d in (root, pkg):
    try:
        os.mkdir(d)
    except FileExistsError:
        pass

with open(os.path.join(pkg, "leaf.py"), "w") as fh:
    fh.write('__all__ = ["leaf_value"]\nleaf_value = "leaf"\n')

with open(os.path.join(pkg, "first.py"), "w") as fh:
    fh.write('__all__ = ["first_value"]\nfrom . import leaf\nfirst_value = "first"\n')

with open(os.path.join(pkg, "__init__.py"), "w") as fh:
    fh.write(
        "from .first import *\n"
        "from .leaf import *\n"
        "names = sorted(k for k in dir() if not k.startswith('_'))\n"
        "combined = first.__all__ + leaf.__all__\n"
    )

sys.path.insert(0, root)
import bindpkg

print("--- the submodule is bound on the package ---")
print(bindpkg.names)
print(bindpkg.combined)
print(bindpkg.first_value, bindpkg.leaf_value)
print(hasattr(bindpkg, "first"), hasattr(bindpkg, "leaf"))
print(bindpkg.leaf is sys.modules["bindpkg.leaf"])
print(bindpkg.first.leaf is bindpkg.leaf)

print("--- and importing it again does not disturb it ---")
from bindpkg import leaf as leaf_again
print(leaf_again is bindpkg.leaf, leaf_again.leaf_value)
for _ in range(50):
    from bindpkg import leaf as repeatedly
print(repeatedly is bindpkg.leaf, repeatedly.leaf_value)
print(sys.modules["bindpkg.leaf"].leaf_value)

print("--- a name the package really does not have ---")
try:
    from bindpkg import nothing_here
except ImportError:
    print("ImportError")

for name in os.listdir(pkg):
    p = os.path.join(pkg, name)
    if os.path.isdir(p):
        for inner in os.listdir(p):
            os.unlink(os.path.join(p, inner))
        os.rmdir(p)
    else:
        os.unlink(p)
os.rmdir(pkg)
os.rmdir(root)
print("done")
