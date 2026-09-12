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

# A genuine package keeps working after all of the above.
import collections.abc

print("collections.abc ok:", collections.abc.__name__)
print("has __path__:", hasattr(collections, "__path__"), hasattr(sys, "__path__"))
