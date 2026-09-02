"""__import__ with keyword arguments, and the fromlist it was ignoring.

Two bugs met here, and only together did they show up -- as
`TypeError: __init_subclass__() takes no keyword arguments` raised from a
class statement in a module that has no keyword arguments anywhere in it.

  * KW_NAMES writes a global that the next CALL consumes.  A builtin that
    ignores it leaves it set, and `__import__` runs a whole module body
    before returning, so the first class statement in that module read the
    caller's `fromlist=`/`level=` as ITS class keywords.  The global is
    per-call-site state and must not cross a frame boundary.
  * `__import__` ignored fromlist entirely and always returned the top-level
    package.  `encodings.search_function` does
    `__import__('encodings.' + modname, fromlist=['*']).getregentry()`,
    which is how codecs.lookup() found nothing.
"""

# posix, not os: os is CPython's own Python module and needs $CPYTHON_LIB,
# and this belongs in the default gate.
import posix
import sys

tmp = "/tmp/apython_import_kwargs_pkg"
for d in (tmp, tmp + "/pkg", tmp + "/kwpkg"):
    try:
        posix.mkdir(d, 0o755)
    except FileExistsError:
        pass

with open(tmp + "/pkg/__init__.py", "w") as f:
    f.write("VALUE = 'package'\n")

# A class statement is the tripwire: it is the one call that reads pending
# keyword names and complains about them.
with open(tmp + "/pkg/sub.py", "w") as f:
    f.write(
        "class Base:\n"
        "    pass\n"
        "class Derived(Base):\n"
        "    pass\n"
        "VALUE = 'submodule'\n"
        "def getvalue():\n"
        "    return VALUE\n"
    )

# A package imported ONLY by the keyword form, so its body runs while the
# caller's keyword names are pending.  Reusing pkg would not reach the body:
# the second import finds it in sys.modules and never executes it.
with open(tmp + "/kwpkg/__init__.py", "w") as f:
    f.write("class Marker:\n    pass\nVALUE = 'kwpkg'\n")

sys.path.insert(0, tmp)


def check_first(label, fn):
    try:
        got = fn()
    except Exception as exc:
        got = type(exc).__name__ + ": " + str(exc)
    print(label.ljust(32), repr(got))


# This is the original failure, in three lines: a class statement inside a
# module body imported by a call that had keyword arguments of its own.
check_first("body run under kwargs", lambda: __import__(
    "kwpkg", globals(), locals(), fromlist=["*"], level=0).Marker.__name__)


def check(label, fn):
    try:
        got = fn()
    except Exception as exc:
        got = type(exc).__name__ + ": " + str(exc)
    print(label.ljust(32), repr(got))


# Positional, no fromlist: the top-level package, as CPython does.
check("no fromlist", lambda: __import__("pkg.sub", globals(), locals(), None, 0).__name__)

# With a fromlist, the leaf.  Keyword form, which is what encodings uses.
check("kw fromlist", lambda: __import__(
    "pkg.sub", globals(), locals(), fromlist=["*"], level=0).__name__)
check("kw fromlist value", lambda: __import__(
    "pkg.sub", globals(), locals(), fromlist=["*"], level=0).VALUE)
check("kw fromlist call", lambda: __import__(
    "pkg.sub", globals(), locals(), fromlist=["getvalue"], level=0).getvalue())
check("positional fromlist", lambda: __import__(
    "pkg.sub", globals(), locals(), ["*"], 0).__name__)
check("named fromlist", lambda: __import__(
    "pkg.sub", globals(), locals(), ["getvalue"], 0).getvalue())
check("level by keyword", lambda: __import__(
    "pkg.sub", globals(), locals(), ["*"], level=0).__name__)

# The class in the freshly imported module must have been built without any
# of the caller's keywords attached to it.
mod = __import__("pkg.sub", globals(), locals(), ["*"], 0)
check("class built", lambda: mod.Derived.__name__)
check("its base", lambda: mod.Derived.__mro__[1].__name__)
check("instance", lambda: type(mod.Derived()).__name__)


# The same leak, without any import: a builtin that ignores keyword names
# must not leave them for the next call.
def kwuser(**kw):
    return sorted(kw)


check("kwargs still work", lambda: kwuser(a=1, b=2))


class AfterEverything:
    pass


check("class after imports", lambda: AfterEverything.__name__)

for name in list(sys.modules):
    if name in ("pkg", "kwpkg") or name.startswith("pkg."):
        del sys.modules[name]
sys.path.remove(tmp)

for f in ("/pkg/__init__.py", "/pkg/sub.py", "/kwpkg/__init__.py"):
    try:
        posix.unlink(tmp + f)
    except OSError:
        pass
for d in ("/pkg/__pycache__", "/pkg", "/kwpkg/__pycache__", "/kwpkg", ""):
    try:
        posix.rmdir(tmp + d)
    except OSError:
        pass
print("cleaned", True)
