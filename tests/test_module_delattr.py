# `del module.attr` did not delete the entry.  It NULLED it.
#
# DELETE_ATTR calls tp_setattr(obj, name, NULL) -- a NULL value is the delete
# convention -- and module_setattr handed that straight to dict_set, which
# stored the NULL as the entry's value and left the key in place.
# dict_ass_subscript, which serves `del d[k]`, has always routed a NULL to
# dict_del instead; module_setattr never did.
#
# Two things followed:
#
#   1. dk_version did not change.  A dict's keys version is bumped when the
#      KEY SET changes, and overwriting a value is not that -- so every
#      LOAD_GLOBAL_BUILTIN inline cache guarding on it stayed valid.
#   2. The key was still findable.  So when the cache did notice the NULL
#      value and deopt, op_load_global looked the name up again, FOUND it,
#      re-specialized, loaded the NULL value and pushed it.
#
# A NULL Value on the value stack is not an error anything notices; it
# propagates until something dereferences it.  `del builtins.x` followed by a
# reference to x returned an empty non-object rather than raising NameError,
# and the failure surfaced at whatever touched it next.
#
# `builtins.__dict__.pop('x')` was always right -- that goes through dict_del
# -- which is why this needed the attribute form to show.

import builtins


# --- a builtin, the case that showed the bug ------------------------------
builtins.probe_one = 11


def read_one():
    return probe_one


for _ in range(6):                      # enough to form the inline cache
    print(read_one())

del builtins.probe_one
print("gone from dict:", "probe_one" in builtins.__dict__)
print("gone from module:", hasattr(builtins, "probe_one"))
try:
    print("still readable:", read_one())
except NameError:
    print("read_one -> NameError")

# --- and it must work again if it comes back ------------------------------
builtins.probe_one = 22
for _ in range(6):
    print(read_one())
del builtins.probe_one
try:
    read_one()
    print("second delete missed")
except NameError:
    print("second delete -> NameError")

# --- an ordinary module, not just builtins --------------------------------
import sys as _sys
_sys.probe_two = 33
print(_sys.probe_two, "probe_two" in _sys.__dict__, hasattr(_sys, "probe_two"))
del _sys.probe_two
print("probe_two" in _sys.__dict__, hasattr(_sys, "probe_two"))
try:
    _sys.probe_two
    print("attribute survived")
except AttributeError:
    print("_sys.probe_two -> AttributeError")
try:
    del _sys.probe_two
    print("second delete succeeded")
except AttributeError:
    print("second delete -> AttributeError")

# --- deleting something that was never there ------------------------------
try:
    del _sys.never_set
except AttributeError:
    print("never_set -> AttributeError")

# --- the dict really is smaller, and iteration does not see a hole --------
_sys.zz1 = 1
_sys.zz2 = 2
_sys.zz3 = 3
before = len(_sys.__dict__)
del _sys.zz2
after = len(_sys.__dict__)
print("length dropped by", before - after)
print("zz1" in _sys.__dict__, "zz2" in _sys.__dict__, "zz3" in _sys.__dict__)
print(sorted(k for k in _sys.__dict__ if k.startswith("zz")))
print([(k, _sys.__dict__[k]) for k in sorted(_sys.__dict__) if k.startswith("zz")])
del _sys.zz1
del _sys.zz3

# --- set, delete, set again, in a loop, so tombstones get reused ----------
for i in range(50):
    _sys.churn = i
    got = _sys.churn
    del _sys.churn
    if got != i:
        print("churn mismatch", i, got)
print("churn ok", "churn" in _sys.__dict__)

# --- a module-level global, which was always right, as the control --------
G = 1


def read_g():
    return G


for _ in range(6):
    read_g()
del G
try:
    read_g()
    print("global delete missed")
except NameError:
    print("global delete -> NameError")
