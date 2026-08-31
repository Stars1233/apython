# `import a.b.c as n` binds the submodule, not the top package: after
# IMPORT_NAME leaves `a`, each remaining component is walked with IMPORT_FROM.
# Storing the IMPORT_NAME result straight into `n` gave `import os.path as p`
# the `os` module.
#
# Underneath it, the import machinery imported only the first component and
# then the whole name, skipping the packages in between -- so anything three
# deep failed outright -- and hung each submodule off the *top* package rather
# than its own parent.  A cache hit on the full dotted name also handed back
# the leaf where an empty fromlist calls for the top.
import sys

sys.path.insert(0, "tests/imppkg")

import aa.bb.cc as m

print(m.NAME, m.V)

import aa.dd as d

print(d.NAME)

import aa.bb.cc

print(aa.NAME, aa.bb.NAME, aa.bb.cc.NAME, aa.bb.cc.V)

import aa.bb as n

print(n.NAME)

import aa

print(aa.NAME)

from aa.bb import cc

print(cc.NAME)

from aa import bb

print(bb.NAME)

from aa.bb.cc import V

print(V)

# The names really are separate modules, and the parents carry the children.
print(m is aa.bb.cc, n is aa.bb, cc is aa.bb.cc)
print(sorted(k for k in sys.modules if k.startswith("aa")))
print("bb" in aa.__dict__, "cc" in aa.bb.__dict__, "dd" in aa.__dict__)

# A plain single-component import still binds its own name.
import sys as system

print(system is sys)
