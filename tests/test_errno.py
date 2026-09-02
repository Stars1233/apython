# The errno module: every name, its number, and errorcode's inverse mapping.
#
# These are Linux x86-64 kernel ABI constants, written out in
# src/errnomod.asm rather than generated at build time.  This file is what
# makes that safe: run_tests.sh diffs our stdout against the system CPython's,
# so a mistyped number cannot survive a test run.
#
# The names come from dir(), which also makes this file a test of dir() on a
# module: the set has to match CPython's exactly, name for name, or the very
# first line differs.  It used to be a literal list, because dir() on a module
# answered with object's dunders instead of the module's own contents.

import errno

# Underscored names are skipped: our modules carry no __doc__, __loader__,
# __package__ or __spec__, and those four are the whole difference.
NAMES = [n for n in sorted(dir(errno)) if not n.startswith("_")]

print("names:", len(NAMES))
for n in NAMES:
    v = getattr(errno, n)
    # errorcode is one of those names, and it is a dict whose insertion order
    # differs between the two interpreters; its contents are printed sorted
    # just below, so only its type belongs here.
    print(n, v if isinstance(v, int) else type(v).__name__)

print("errorcode:", len(errno.errorcode))
for k in sorted(errno.errorcode):
    print(k, errno.errorcode[k])

# The ones the OSError subclass mapping depends on.
for n in ("ENOENT", "EEXIST", "EACCES", "EPERM", "EISDIR", "ENOTDIR", "EINTR",
          "EAGAIN", "EPIPE", "ECHILD", "ESRCH", "ETIMEDOUT"):
    print(n, getattr(errno, n), errno.errorcode[getattr(errno, n)])
