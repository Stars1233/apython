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

print("done")
