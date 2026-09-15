# A PEP 562 hook that unbinds itself while it is running.
#
# module_getattr found `__getattr__` with dict_get, which hands back a
# BORROWED Value, and called it without taking a reference -- the code even
# said so: "Nothing in this frame is owned at that point."  A hook whose body
# is `del globals()["__getattr__"]` therefore dropped the last reference to
# the function that was executing, and the interpreter went on using the
# freed object.  A SIGSEGV, from four lines of ordinary Python.
#
# module_dunder_dir had the identical defect one door over, so __dir__ is
# exercised here too.
#
# CPython's own suite covers this -- Lib/test/test_module/bad_getattr3.py,
# whose comment is "these lookups should not crash" -- which is where it was
# found.  Nothing in this tree had ever run that file.
import sys

sys.path.insert(0, "tests")

import pep562delhelper as m

# A miss that is NOT the self-deleting name: the ordinary path, and the hook
# survives it.
try:
    m.one
except AttributeError as e:
    print("one ->", type(e).__name__)
print("hook still there:", "__getattr__" in vars(m))

# The self-deleting name.  The hook unbinds itself and then raises, so the
# answer is an AttributeError and the process is still running.
try:
    m.delgetattr
except AttributeError as e:
    print("delgetattr ->", type(e).__name__)
print("hook gone:", "__getattr__" not in vars(m))
print("calls:", m.calls)

# With the hook gone, a miss is a plain module AttributeError again -- and the
# freed function must not be consulted a second time.
try:
    m.two
except AttributeError as e:
    print("two after deletion ->", type(e).__name__)
print("calls unchanged:", m.calls)

# The same shape through __dir__, which is looked up and called the same way.
print("dir:", m.__dir__())
print("__dir__ gone:", "__dir__" not in vars(m))
# dir() over the module now falls back to the module dict, which must not
# reach the freed function either.
names = dir(m)
print("dir() has calls:", "calls" in names)

# A garbage collection here would fault on a freed-but-still-linked object if
# the reference counting above were wrong, so it is part of the test rather
# than tidiness.
import gc

gc.collect()
print("survived")
