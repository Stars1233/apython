"""types.ModuleType(name[, doc]), and the sys constants that came with it.

module_type had no tp_new and no tp_init, so type_call fell through to
instance_new: it allocated PyModuleObject_size bytes and handed back a module
whose mod_name and mod_dict held whatever the allocator left, and only then did
the arity check refuse the arguments with "module() takes no arguments" -- a
message naming a method nobody wrote.  A constructor goes in tp_new, which is
what type_call consults; tp_call on a type is what makes that type's INSTANCES
callable.

The sys constants below are the ones whose value is the same on both sides.
sys.hexversion, sys.dont_write_bytecode and sys.path_importer_cache are not:
the first tracks whichever 3.12.x is running, and the other two are honest
about a tree that never writes a .pyc and whose finders are not path hooks.
They are checked for shape and for agreement with sys.version_info instead.
"""

import sys
import types

m = types.ModuleType("mymod")
print("name:", m.__name__)
print("doc:", m.__doc__)
print("dict keys:", sorted(m.__dict__))
print("type is ModuleType:", type(m) is types.ModuleType)
print("type is type(sys):", type(m) is type(sys))

d = types.ModuleType("withdoc", "the docstring")
print("with doc:", d.__name__, repr(d.__doc__))

n = types.ModuleType("nodoc", None)
print("explicit None doc:", n.__name__, repr(n.__doc__))

# It is an ordinary namespace afterwards.
m.value = 42
m.fn = lambda: "called"
print("attrs:", m.value, m.fn())
print("keys after:", sorted(m.__dict__))
del m.value
print("after del:", sorted(m.__dict__))

# Each one is its own object with its own dict.
a = types.ModuleType("a")
b = types.ModuleType("b")
a.x = 1
print("independent:", hasattr(b, "x"), a.__dict__ is not b.__dict__)

# A name that is not a str is refused, and so is the wrong count.
for args in ((), (5,), (None,), ([],), ("a", "b", "c")):
    try:
        types.ModuleType(*args)
        print("accepted", args, "- wrong")
    except TypeError:
        print("refused:", args)

# Reading an attribute it does not have.
try:
    a.nosuch
except AttributeError as e:
    print("missing attr:", type(e).__name__)

# It can be put in sys.modules and imported from.
sys.modules["mymod"] = m
import mymod
print("importable:", mymod is m, mymod.fn())
del sys.modules["mymod"]


print("--- sys constants ---")
print("maxunicode:", sys.maxunicode)
print("float_repr_style:", sys.float_repr_style)
print("hexversion is an int:", isinstance(sys.hexversion, int))
print("hexversion agrees with version_info:",
      (sys.hexversion >> 24) == sys.version_info[0],
      ((sys.hexversion >> 16) & 0xFF) == sys.version_info[1])
print("dont_write_bytecode is a bool:", isinstance(sys.dont_write_bytecode, bool))
print("path_importer_cache is a dict:", isinstance(sys.path_importer_cache, dict))
sys.path_importer_cache.clear()
print("clearable:", sys.path_importer_cache == {})

print("stdout alias:", sys.__stdout__ is sys.stdout)
print("stderr alias:", sys.__stderr__ is sys.stderr)
print("stdin alias:", sys.__stdin__ is sys.stdin)
print("maxunicode round trip:", chr(sys.maxunicode) == "\U0010FFFF")

print("done")
