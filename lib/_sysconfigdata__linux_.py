"""The build-time variables `sysconfig` expects to import.

CPython writes this file out at build time from its Makefile: one dict,
several hundred entries, describing the compiler, the flags and the paths
that built the interpreter.  `sysconfig._init_posix` imports it by a name it
assembles from `sys.abiflags`, `sys.platform` and the multiarch tuple --
here that comes to `_sysconfigdata__linux_` -- and a ModuleNotFoundError
there is fatal to `sysconfig` and to everything behind it: `pydoc`, `cgitb`
and `zoneinfo` all stopped at this import and nowhere else.

apython has no build-time Makefile to render, and most of what CPython's
copy records is about a C build that did not happen: CC, CFLAGS, LIBS,
the object list, the configure switches.  Inventing values for those would
be a lie a program could act on.  What is here instead is the subset that
describes THIS interpreter and is true of it -- the version, the paths, the
extension suffix, the platform -- with the C-build entries either absent or
empty, which is the honest answer to "what compiler built this".

`sysconfig.get_config_var` returns None for a name that is not here, which
is what a caller must already handle: the set differs between platforms and
between CPython builds.
"""

import sys as _sys

_prefix = _sys.prefix
_version = "%d.%d" % _sys.version_info[:2]

build_time_vars = {
    # --- identity -------------------------------------------------------
    "VERSION": _version,
    "py_version": "%d.%d.%d" % _sys.version_info[:3],
    "py_version_short": _version,
    "py_version_nodot": _version.replace(".", ""),
    "ABIFLAGS": "",
    "SOABI": "cpython-%s-x86_64-linux-gnu" % _version.replace(".", ""),
    "EXT_SUFFIX": ".cpython-%s-x86_64-linux-gnu.so"
                  % _version.replace(".", ""),
    "SHLIB_SUFFIX": ".so",
    "MULTIARCH": "x86_64-linux-gnu",
    "HOST_GNU_TYPE": "x86_64-pc-linux-gnu",
    "MACHDEP": "linux",
    "PLATFORM_TRIPLET": "x86_64-linux-gnu",

    # --- the layout sysconfig's posix_prefix scheme is written against ---
    "prefix": _prefix,
    "exec_prefix": _sys.exec_prefix,
    "base": _prefix,
    "platbase": _sys.exec_prefix,
    "installed_base": _sys.base_prefix,
    "installed_platbase": _sys.base_exec_prefix,
    "BINDIR": _prefix + "/bin",
    "BINLIBDEST": _prefix + "/lib/python" + _version,
    "LIBDIR": _prefix + "/lib",
    "LIBDEST": _prefix + "/lib/python" + _version,
    "INCLUDEPY": _prefix + "/include/python" + _version,
    "INCLUDEDIR": _prefix + "/include",
    "CONFINCLUDEDIR": _prefix + "/include",
    "SCRIPTDIR": _prefix + "/lib",
    "DESTLIB": _prefix + "/lib/python" + _version,
    "DESTSHARED": _prefix + "/lib/python" + _version + "/lib-dynload",
    "LIBPL": _prefix + "/lib/python" + _version + "/config",
    "platlibdir": "lib",
    "projectbase": _prefix + "/bin",
    "srcdir": _prefix,
    "abs_srcdir": _prefix,
    "abs_builddir": _prefix,

    # --- what a C build would have recorded ------------------------------
    #
    # Empty rather than invented.  A program that reads CC to compile an
    # extension has nothing to compile it with, and should find out here
    # rather than from a compiler that does not exist.
    "CC": "",
    "CXX": "",
    "CFLAGS": "",
    "CCSHARED": "",
    "LDSHARED": "",
    "LDFLAGS": "",
    "LDLIBRARY": "",
    "LIBRARY": "",
    "LIBS": "",
    "SYSLIBS": "",
    "OPT": "",
    "BLDSHARED": "",
    "BLDLIBRARY": "",
    "PY_CFLAGS": "",
    "PY_CORE_CFLAGS": "",
    "PY_LDFLAGS": "",
    "AR": "",
    "ARFLAGS": "",
    "EXE": "",

    # --- flags that describe the build's shape ---------------------------
    "Py_ENABLE_SHARED": 0,
    "Py_DEBUG": 0,
    "Py_GIL_DISABLED": 0,
    "WITH_PYMALLOC": 1,
    "WITH_DOC_STRINGS": 1,
    "HAVE_DYNAMIC_LOADING": 0,
    "SIZEOF_VOID_P": 8,
    "SIZEOF_LONG": 8,
    "SIZEOF_SIZE_T": 8,
    "SIZEOF_INT": 4,
    "SIZEOF_SHORT": 2,
    "SIZEOF_FLOAT": 4,
    "SIZEOF_DOUBLE": 8,
    "DOUBLE_IS_LITTLE_ENDIAN_IEEE754": 1,
    "userbase": None,
}

del _sys
