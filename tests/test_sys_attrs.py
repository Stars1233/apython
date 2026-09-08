# The sys and time attributes the stdlib reads without asking first.
#
# CPython's getopt, gettext and optparse open with sys.base_prefix, timeit
# with time.perf_counter, and site.py with sys.copyright.  None of them
# guards the read, so an absent name is an AttributeError at import rather
# than a feature the program can do without -- five modules that could not be
# imported for want of four strings and a clock.
#
# The values are not compared against CPython's: prefix is an installation
# path, copyright is a licence, and perf_counter's origin is undefined by
# definition.  What has to agree is that they exist and what shape they are.

import sys
import time

print("=== the prefixes ===")
for name in ("prefix", "exec_prefix", "base_prefix", "base_exec_prefix"):
    value = getattr(sys, name)
    print(name, type(value).__name__)

# With no virtualenv, base_prefix is prefix.  CPython says so too, and this is
# the invariant getopt and gettext are really relying on.
print("base is prefix:", sys.base_prefix == sys.prefix)
print("base_exec is exec:", sys.base_exec_prefix == sys.exec_prefix)

print("=== copyright ===")
print("type:", type(sys.copyright).__name__)
print("non-empty:", len(sys.copyright) > 0)
print("names a year:", any(c.isdigit() for c in sys.copyright))

print("=== perf_counter ===")
first = time.perf_counter()
second = time.perf_counter()
print("type:", type(first).__name__)
print("does not go backwards:", second >= first)
# It measures something: a busy loop has to take a non-negative time.
start = time.perf_counter()
total = 0
for i in range(10000):
    total += i
print("elapsed is non-negative:", time.perf_counter() - start >= 0.0)
print("total:", total)
try:
    time.perf_counter(1)
except TypeError:
    print("argument => TypeError")

print("=== the import machinery's own attributes ===")
# importlib._bootstrap walks sys.meta_path on every import and sys.path_hooks
# when a path entry has no finder cached.  Neither is guarded, so their absence
# was an AttributeError from inside `import`.  The finders here are assembly
# rather than importlib hooks, so both are empty -- what matters is that they
# exist, that they are lists, and that a program can append to them.
# The CONTENTS differ by design -- CPython's hold its own finders and these
# hold nothing -- so the lengths are compared against themselves, not printed.
print("meta_path:", type(sys.meta_path).__name__)
print("path_hooks:", type(sys.path_hooks).__name__)
before = len(sys.meta_path)
sys.meta_path.append("sentinel")
print("append works:", sys.meta_path[-1], len(sys.meta_path) - before)
sys.meta_path.pop()
print("and pop:", len(sys.meta_path) - before)
print("still a list:", isinstance(sys.meta_path, list))

print("=== getrefcount ===")
# The count includes getrefcount's own argument reference, which is why
# CPython documents it as one higher than expected.  Two names for one object
# is one more than one name for it: that difference is the invariant, not the
# absolute number.
obj = []
one = sys.getrefcount(obj)
alias = obj
two = sys.getrefcount(obj)
print("an alias adds one:", two - one)
del alias
print("and dropping it takes it back:", sys.getrefcount(obj) - one)
print("type:", type(one).__name__)
try:
    sys.getrefcount()
except TypeError:
    print("no argument => TypeError")

print("=== displayhook ===")
print("is the default:", sys.displayhook is sys.__displayhook__)
print("callable:", callable(sys.displayhook))
# None prints nothing and does not bind _; anything else prints its repr and
# does, which is what an interactive prompt is.
import builtins
if hasattr(builtins, "_"):
    del builtins._
sys.displayhook(None)
print("None does not bind _:", not hasattr(builtins, "_"))
sys.displayhook("hi")
print("bound _:", builtins._)
del builtins._
