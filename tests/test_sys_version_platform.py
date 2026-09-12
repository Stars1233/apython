# sys.version has to be parseable by platform.
#
# `platform._sys_version` matches sys.version against a regex that requires a
# BRACKETED compiler field:
#
#     ([\w.+]+)\s*\(#?([^,]+)(?:,\s*([\w ]*)(?:,\s*([\w :]*))?)?\)\s*\[([^\]]+)\]?
#
# '3.12.0 (apython 0.6.0)' has no bracket, so every call raised
# "ValueError: failed to parse CPython sys.version" -- and platform is reached
# by a great deal of ordinary code, test_wsgiref's thirty-five tests among it.
#
# python_implementation() has no hook for a name of its own: it is hardcoded to
# Jython when sys.platform starts with 'java', to PyPy when "PyPy" is a
# substring of sys.version, and to CPython otherwise.  So it answers 'CPython'
# here.  sys.implementation.name is the honest answer and stays 'apython'.
# DIVERGENCES.md records the choice.
#
# The oracle for this file is python3, whose own sys.version differs, so what
# is printed is what the two must AGREE about: that the regex matches, and
# what it yields.

import re
import sys

sys_version_parser = re.compile(
    r"([\w.+]+)\s*"
    r"\(#?([^,]+)"
    r"(?:,\s*([\w ]*)"
    r"(?:,\s*([\w :]*))?)?\)\s*"
    r"\[([^\]]+)\]?",
    re.ASCII,
)

m = sys_version_parser.match(sys.version)
print("regex matches:", m is not None)
if m is not None:
    version, buildno, builddate, buildtime, compiler = m.groups()
    print("version field is the three-part version:",
          version == "%d.%d.%d" % sys.version_info[:3])
    print("build field is non-empty:", bool(buildno.strip()))
    print("compiler field is non-empty:", bool(compiler.strip()))

print("version_info:", tuple(sys.version_info)[:2],
      sys.version_info.releaselevel in ("alpha", "beta", "candidate", "final"))
print("version starts with the version_info:",
      sys.version.startswith("%d.%d." % sys.version_info[:2]))
print("hexversion agrees:",
      (sys.hexversion >> 24) == sys.version_info[0],
      ((sys.hexversion >> 16) & 0xFF) == sys.version_info[1])
print("no newline in sys.version:", "\n" not in sys.version)

# python_implementation()'s three probes, reimplemented here rather than
# imported: `platform` is pure Python and lives in the stdlib rather than in
# lib/, so a run with no stdlib on the path could not print the same lines.
# The real module is exercised by CPython's own test_platform and test_wsgiref.
if sys.platform.startswith("java"):
    impl = "Jython"
elif "PyPy" in sys.version:
    impl = "PyPy"
else:
    impl = "CPython"
print("python_implementation:", impl)

if m is not None:
    print("python_version:", m.group(1) == "%d.%d.%d" % sys.version_info[:3])
    print("python_build:", bool(m.group(2).strip()),
          isinstance(m.group(3) or "", str))
    print("python_compiler:", bool(m.group(5).strip()))

# sys.implementation is where the real name lives, and it is not platform's.
print("implementation is a namespace with a name:",
      isinstance(sys.implementation.name, str) and sys.implementation.name.islower())
print("implementation.version agrees with version_info:",
      tuple(sys.implementation.version)[:3] == tuple(sys.version_info)[:3])
print("implementation.cache_tag:", sys.implementation.cache_tag)
