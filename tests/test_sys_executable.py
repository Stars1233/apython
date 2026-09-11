# sys.executable names the interpreter, and the interpreter takes -c.
#
# Both were missing together, and the pair is what test.support's script_helper
# needs before it will let a module run at all:
# interpreter_requires_environment() does
#     subprocess.check_call([sys.executable, '-E', '-c', 'import sys; sys.exit(0)'])
# and catches only CalledProcessError.  With sys.executable == "" that is an
# uncaught FileNotFoundError raised while the helper is being imported, so the
# module dies before its first test -- 40 of CPython's test modules did.
#
# The path comes from /proc/self/exe, not argv[0]: main() shifts argv past -t,
# so argv[0] is sometimes "-t", and a bare "./apython" would not survive a
# chdir.  prefix and the three names beside it follow from its directory.

import os
import sys

exe = sys.executable
print("is str:", isinstance(exe, str))
print("non-empty:", bool(exe))
print("absolute:", os.path.isabs(exe))
print("exists:", os.path.exists(exe))
print("is a file:", os.path.isfile(exe))
print("executable:", os.access(exe, os.X_OK))

# _base_executable is the same interpreter when there is no virtualenv, which
# is always here.  CPython's sysconfig and venv both read it.
print("_base_executable:", sys._base_executable == exe)

# prefix, exec_prefix and the base_ pair all name the same place with no
# virtualenv, and that place is where the interpreter lives.
print("prefix non-empty:", bool(sys.prefix))
print("prefix is a dir:", os.path.isdir(sys.prefix))
print("exec_prefix:", sys.exec_prefix == sys.prefix)
print("base_prefix:", sys.base_prefix == sys.prefix)
print("base_exec_prefix:", sys.base_exec_prefix == sys.exec_prefix)
print("exe under prefix:", os.path.dirname(exe).startswith(sys.prefix))

# argv[0] is the SCRIPT, not the interpreter -- that is CPython's rule and the
# reason sys.executable has to come from somewhere else.
print("argv0 is not exe:", sys.argv[0] != exe)
import os as _os
print("argv0 names this file:",
      _os.path.basename(sys.argv[0]).startswith("test_sys_executable"))

# The flags script_helper passes must all be accepted.  They are no-ops here:
# apython has no site machinery and no environment-driven configuration, so
# ignoring them is the correct behaviour rather than a stub.
for name in ("ignore_environment", "isolated", "no_site", "no_user_site",
             "dont_write_bytecode", "verbose", "quiet", "optimize"):
    print("flag", name, isinstance(getattr(sys.flags, name), int))
print("dev_mode:", sys.flags.dev_mode is False)
print("done")
