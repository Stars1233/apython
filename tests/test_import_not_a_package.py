# A module is not a package, and asking for a submodule of one says so.
#
# CPython's `_bootstrap._find_and_load_unlocked` imports the parent, then reads
# `parent_module.__path__`; an AttributeError there becomes
#
#     No module named 'sys.foo'; 'sys' is not a package
#
# We reported only the first half, so `import sys.foo` was indistinguishable
# from a genuinely missing `sys.foo` -- and the reason the name cannot exist
# (sys has no submodules at all) was not in the message.
#
# The error names the FIRST prefix that cannot be resolved and its immediate
# parent, not the whole dotted name that was requested: `import sys.foo.bar`
# stops at `sys.foo`, because the walk never gets as far as `bar`.
#
# The ORDER of the two checks is what this file mostly exists to pin down, and
# getting it wrong broke `import os.path`.  CPython's
# _find_and_load_unlocked does three things, in this order:
#
#     if parent not in sys.modules:
#         _call_with_frames_removed(import_, parent)
#     # Crazy side-effects!
#     if name in sys.modules:
#         return sys.modules[name]
#     parent_module = sys.modules[parent]
#     try:
#         path = parent_module.__path__
#     except AttributeError:
#         ... 'x' is not a package
#
# So a submodule the PARENT installed in sys.modules itself wins, and the
# parent is never asked whether it is a package.  `os` is not a package and
# `lib/os.py` ends with `sys.modules['os.path'] = path`, which is exactly the
# arrangement that comment is about -- asking about `os.__path__` first makes
# `import os.path` a ModuleNotFoundError.
#
# The blocked-parent case belongs here too.  `sys.modules['x'] = None` makes
# CPython skip importing the parent, so `parent_module` IS the None and
# reading `__path__` off it lands in exactly this arm -- which is why the
# "halted" message of tests/test_import_none_blocked.py deliberately does not
# cover it.

import sys


def show(label, code):
    try:
        exec(code, {"__name__": "t"})
    except BaseException as e:
        print("%-34s %s: %s" % (label, type(e).__name__, e))
        print("%-34s   name=%r path=%r" % ("", getattr(e, "name", "ABSENT"),
                                            getattr(e, "path", "ABSENT")))
    else:
        print("%-34s NO RAISE" % label)


# A plain module as the parent.
show("import sys.foo", "import sys.foo")
show("import sys.foo.bar", "import sys.foo.bar")
show("from sys.foo import x", "from sys.foo import x")
show("import sys.foo as f", "import sys.foo as f")

# Two deep, where the failing parent is itself a submodule of a real package.
show("import collections.abc.zz", "import collections.abc.zz")
show("import collections.abc.zz.yy", "import collections.abc.zz.yy")

# A real package as the parent is still just a missing submodule -- no suffix.
# (`from pkg import missing` is an ImportError whose message carries the
# package's file PATH, so it cannot be diffed against python3 here; it lives in
# tests/test_import_from_name.py, which prints the portable parts of it.)
show("import collections.nosuch", "import collections.nosuch_zzz")
show("import nosuch_top", "import nosuch_zzz")

# A None parent reaches the same arm, because the parent import is skipped.
sys.modules["blocked_p"] = None
show("import blocked_p.sub (None parent)", "import blocked_p.sub")
show("from blocked_p.sub import x", "from blocked_p.sub import x")

# And the parent itself is untouched by any of it.
print("sys is still a module:", type(sys).__name__)
print("blocked_p still None:", sys.modules["blocked_p"] is None)

print()
print("== a submodule the parent installed itself is found, not refused ==")
# os is NOT a package; lib/os.py registers os.path in sys.modules by hand.
show("import os.path", "import os.path")
show("import os.path as p", "import os.path as p")
show("from os.path import join", "from os.path import join")
import os.path

print("os.path works:", os.path.join("a", "b"))
print("and is the registered object:", sys.modules["os.path"] is os.path)

# The same arrangement by hand, so the rule is pinned down without depending
# on how os.py happens to be written.
import sys as _sys

_sys.modules["blocked_q"] = _sys                 # a module, not a package
_sys.modules["blocked_q.sub"] = _sys.modules["os"]
show("parent installed the child", "import blocked_q.sub")
print("resolved to the registered module:",
      _sys.modules["blocked_q.sub"] is _sys.modules["os"])
# But a DIFFERENT child of the same non-package parent is still refused.
show("a sibling it did not install", "import blocked_q.other")

print()
print("== a genuine package keeps working after all of the above ==")
import collections.abc

print("collections.abc ok:", collections.abc.__name__)
print("has __path__:", hasattr(collections, "__path__"), hasattr(sys, "__path__"))
