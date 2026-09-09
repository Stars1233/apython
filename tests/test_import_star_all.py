"""`from mod import *` checks that every name in `__all__` exists.

CPython raises `AttributeError: module 'm' has no attribute 'missing'`; this
tree bound what it found and silently skipped the rest, so a module could
promise a name it never defined and nothing noticed.  `lib/copyreg.py` did
exactly that for three functions until a reviewer read the file.

`__all__` was also assumed to be a list or a tuple.  A set has no `ob_item`,
so `__all__ = {"a"}` read a tuple's field off a set header and segfaulted.
CPython indexes `__all__` as a sequence, which accepts a str (a sequence of
one-character names, and it really does bind them) and refuses a set with
`TypeError: 'set' object does not support indexing`.

The helper modules live in `tests/importstar_mods/`, which `make check` does
not discover -- the names do not start with `test_`.
"""

import os
import sys

# Run from the .py this is tests/, run from the .pyc it is tests/__pycache__/.
_here = os.path.dirname(os.path.abspath(__file__))
if os.path.basename(_here) == "__pycache__":
    _here = os.path.dirname(_here)
sys.path.insert(0, os.path.join(_here, "importstar_mods"))


def star(mod, names):
    """`from mod import *`, reporting what bound or what was raised."""
    ns = {}
    try:
        exec("from %s import *" % mod, ns)
    except Exception as e:                      # noqa: BLE001
        return "%s: %s" % (type(e).__name__, e)
    return sorted(n for n in names if n in ns)


print("--- __all__ names something that is not there ---")
print("missing:", star("all_missing", ["present", "missing"]))

print("--- an honest __all__ ---")
print("ok:", star("all_ok", ["shown", "hidden"]))
print("tuple:", star("all_tuple", ["x", "y"]))

print("--- __all__ that is not a list or a tuple ---")
print("set:", star("all_set", ["a"]))
print("str:", star("all_str", ["a", "b"]))
print("int:", star("all_int", ["a"]))

print("--- no __all__ at all ---")
print("none:", star("no_all", ["pub", "_priv"]))

print("--- the module still imports normally ---")
import all_missing
print("attribute:", all_missing.present)
print("__all__ readable:", all_missing.__all__)
try:
    all_missing.missing
    print("missing attribute: found - wrong")
except AttributeError as e:
    print("missing attribute:", e)

print("--- a named import of the missing name ---")
try:
    from all_missing import missing
    print("named: bound - wrong")
except ImportError as e:
    print("named:", type(e).__name__)

print("--- a named import of one that is there ---")
from all_missing import present
print("named ok:", present)

print("--- a package whose __all__ names a submodule ---")
_ns = {}
exec("from all_pkg import *", _ns)
print("here:", _ns.get("here"))
print("leaf:", _ns["leaf"].__name__, _ns["leaf"].value)
import all_pkg
print("bound on the package:", all_pkg.leaf.value)

print("done")


# sys.modules is an ordinary dict and a program may put anything in it --
# importlib's own import_fresh_module does, and CPython's test suite is full
# of it.  `from x import *` and `from x import y` both read mod_dict off
# whatever was there, which for a class is the field that happens to sit at
# that offset: a string's characters, handed to dict_get as a pointer.
import sys
import types


class WithAll:
    __all__ = ["shown"]
    shown = 1
    hidden = 2


sys.modules["apy_fake_all"] = WithAll
ns = {}
exec("from apy_fake_all import *", ns)
print(sorted(k for k in ns if not k.startswith("__")))


class WithoutAll:
    visible = 1
    _private = 2


sys.modules["apy_fake_plain"] = WithoutAll
ns = {}
exec("from apy_fake_plain import *", ns)
print(sorted(k for k in ns if not k.startswith("__")))

# ...and by name, which goes down a different path.
ns = {}
exec("from apy_fake_plain import visible", ns)
print(ns["visible"])
try:
    exec("from apy_fake_plain import nope", {})
    print("NOT REFUSED")
except ImportError as e:
    print("missing name:", type(e).__name__)


class NoDict:
    __slots__ = ()


sys.modules["apy_fake_nodict"] = NoDict()
try:
    exec("from apy_fake_nodict import *", {})
    print("NOT REFUSED")
except ImportError as e:
    print("no dict:", e)

# A real module still works, and so does a ModuleType subclass.
real = types.ModuleType("apy_real")
real.a = 1
sys.modules["apy_real"] = real
ns = {}
exec("from apy_real import *", ns)
print(sorted(k for k in ns if not k.startswith("__")))


class SubModule(types.ModuleType):
    pass


sub = SubModule("apy_sub")
sub.b = 2
sys.modules["apy_sub"] = sub
ns = {}
exec("from apy_sub import *", ns)
print(sorted(k for k in ns if not k.startswith("__")))


# __dict__ is whatever the object's TYPE answers with, and a property that
# builds one on every read hands back the only reference there is -- so the
# walk has to hold it.  Releasing it and keeping a borrowed pointer, which is
# safe for an instance whose __dict__ is a real field, freed the dict this
# loop was about to read.
class FreshDict:
    @property
    def __dict__(self):
        return {"alpha": 1, "beta": 2, "_hidden": 3}


sys.modules["apy_fresh"] = FreshDict()
try:
    exec("from apy_fresh import *", {})
    print("NOT REFUSED")
except AttributeError as e:
    # CPython takes the NAMES from __dict__ and then getattr's each off the
    # object, so a __dict__ that is not the object's namespace fails here.
    print("fresh dict:", e)


class FreshDictThatAnswers:
    alpha = 1
    beta = 2

    @property
    def __dict__(self):
        return {"alpha": 0, "beta": 0}


sys.modules["apy_fresh2"] = FreshDictThatAnswers()
ns = {}
exec("from apy_fresh2 import *", ns)
print("names from __dict__, values from the object:",
      sorted((k, ns[k]) for k in ns if not k.startswith("__")))

# An instance whose __dict__ IS its namespace is the ordinary case, and the
# two agree there.
class Ordinary:
    pass


ordinary = Ordinary()
ordinary.one = 1
ordinary._two = 2
sys.modules["apy_ordinary"] = ordinary
ns = {}
exec("from apy_ordinary import *", ns)
print("ordinary:", sorted(k for k in ns if not k.startswith("__")))
