# posix.putenv() and posix.unsetenv().
#
# os.environ.__setitem__ calls putenv and __delitem__ calls unsetenv, so
# without them `os.environ["X"] = "1"` is a NameError -- and the setUp of
# CPython's test_argparse does exactly that on every one of its 400 tests,
# which is why the whole module errored.  os_helper.EnvironmentVarGuard, which
# a great many of CPython's test modules use, is built on the same two.
#
# They go through libc's setenv/unsetenv rather than a syscall because there is
# no syscall: glibc's `environ` is what execve is handed here, so a child sees
# the change for free.  That half is not testable in-tree -- subprocess needs a
# real stdlib -- and is checked against CPython's own Lib instead.

import os
import posix

NAME = "APYTHON_PUTENV_PROBE"
os.environ.pop(NAME, None)

# --- through os.environ, which is what the stdlib uses ---------------------
os.environ[NAME] = "one"
print("set:", os.environ[NAME], NAME in os.environ)
os.environ[NAME] = "two"
print("overwrite:", os.environ[NAME])
del os.environ[NAME]
print("deleted:", NAME in os.environ, os.environ.get(NAME))

# --- and directly ----------------------------------------------------------
posix.putenv(NAME, "three")
print("putenv:", os.environb.get(NAME.encode()))
posix.unsetenv(NAME)
print("unsetenv:", os.environb.get(NAME.encode()))

# bytes are accepted, as CPython accepts them
posix.putenv(NAME.encode(), b"four")
print("bytes:", os.environb.get(NAME.encode()))
posix.unsetenv(NAME.encode())

# --- the refusals ----------------------------------------------------------
for expr in ('posix.putenv("A=B", "x")', 'posix.putenv("", "x")',
             'posix.putenv("A")', 'posix.unsetenv()',
             'posix.putenv(1, "x")'):
    try:
        eval(expr)
        print(expr, "-> ok")
    except (ValueError, TypeError, OSError) as e:
        print(expr, "->", type(e).__name__)

print("done")
