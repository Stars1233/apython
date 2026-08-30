# Calling a plain function through a module attribute.
#
# LOAD_ATTR's method form (`NULL|self + name`) asks the interpreter to decide
# whether the attribute is a method that should be bound.  For an attribute
# reached through tp_getattr the decision was made by asking whether the object
# is a heaptype instance -- true for a user class, false for a module -- so a
# module fell into the built-in case and its functions were called with the
# module as their first argument.  A module's namespace is not a type's, and
# nothing in it is ever bound.
import sys
sys.path.insert(0, "tests")

import attrpkg
import myhelper
from attrpkg import leaf

alias = leaf
print(leaf.run(), alias.run(), attrpkg.leaf.run())
print(leaf.one(21), alias.one(21), attrpkg.leaf.one(21))

# The same attribute reached without the method form still works.
f = leaf.run
print(f(), getattr(leaf, "run")())

# Binding does still happen where it should: a class attribute on the module.
print(leaf.K.s(), leaf.K().m())

# And a builtin module's methods are bound as before.
print(sys.path.count("tests") >= 1)
