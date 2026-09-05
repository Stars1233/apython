# The warning machinery, and what it does with a filter.
#
# `warnings` was a stub: a warn() that appended to a list nobody read, a
# simplefilter() that took an action and no category -- so silencing one
# category silenced every warning -- and no filterwarnings, resetwarnings or
# _deprecated at all, which ast.py, re and _collections_abc all call by name.
# The real machinery was in `_warnings` next door, unreachable from any of
# them, and even there `warn` knew only "ignore": "error" printed instead of
# raising, "once" printed every time, and every warning came out as ":0:"
# because nothing looked at the caller's frame.
#
# The two halves share one filter list, the way CPython's do:
# importlib._bootstrap reaches for _warnings directly and never imports this
# module, so a filter installed through either has to be seen by both.

import warnings

warnings.resetwarnings()
print("filters start empty", warnings.filters == [])
print("has", sorted(n for n in ("filterwarnings", "simplefilter",
                                "resetwarnings", "catch_warnings", "warn",
                                "warn_explicit", "showwarning",
                                "formatwarning", "_deprecated")
                    if hasattr(warnings, n)))

print("=== recording ===")
with warnings.catch_warnings(record=True) as log:
    warnings.simplefilter("always")
    warnings.warn("hello")
    warnings.warn("dep", DeprecationWarning)
    warnings.warn(UserWarning("an instance"))
print([(str(w.message), w.category.__name__) for w in log])

print("=== a filter for ONE category ===")
with warnings.catch_warnings(record=True) as log:
    warnings.simplefilter("ignore", DeprecationWarning)
    warnings.simplefilter("always", UserWarning)
    warnings.warn("dep", DeprecationWarning)
    warnings.warn("user", UserWarning)
print([str(w.message) for w in log])

print("=== error ===")
with warnings.catch_warnings():
    warnings.simplefilter("error")
    try:
        warnings.warn("boom")
    except Exception as e:
        print(type(e).__name__, e)
    try:
        warnings.warn("boom too", DeprecationWarning)
    except Exception as e:
        print(type(e).__name__, e)

print("=== once ===")
with warnings.catch_warnings(record=True) as log:
    warnings.simplefilter("once")
    for _ in range(3):
        warnings.warn("said once")
print("shown", len(log))

print("=== a message pattern ===")
with warnings.catch_warnings(record=True) as log:
    warnings.simplefilter("always")
    warnings.filterwarnings("ignore", message="^secret")
    warnings.warn("secret thing")
    warnings.warn("public thing")
print([str(w.message) for w in log])

print("=== where it says the warning came from ===")
with warnings.catch_warnings(record=True) as log:
    warnings.simplefilter("always")
    warnings.warn("here")
    here = log[-1]
    print(here.filename.endswith("test_warnings.py"), here.lineno > 0)


def outer():
    warnings.warn("blamed on the caller", stacklevel=2)


with warnings.catch_warnings(record=True) as log:
    warnings.simplefilter("always")
    outer()
    print("stacklevel", log[-1].lineno > 0)

print("=== formatting ===")
print(warnings.formatwarning("m", UserWarning, "f.py", 3).strip())
print(warnings.formatwarning("m", DeprecationWarning, "f.py", 3,
                             "   x = 1  ").strip())

print("=== _deprecated ===")
with warnings.catch_warnings(record=True) as log:
    warnings.simplefilter("always")
    warnings._deprecated("thing", remove=(3, 99))
print(str(log[-1].message), log[-1].category.__name__)

print("=== the two halves share a list ===")
import _warnings

warnings.resetwarnings()
warnings.simplefilter("ignore", DeprecationWarning)
print("same object", warnings.filters is _warnings.filters,
      len(_warnings.filters))
warnings.resetwarnings()

print("=== a bad action ===")
for bad in ("nope", 5):
    for fn in (warnings.simplefilter, warnings.filterwarnings):
        try:
            fn(bad)
            print("accepted", bad)
        except Exception as e:
            print("%-16s %-6r %s" % (fn.__name__, bad, type(e).__name__))
warnings.resetwarnings()
try:
    warnings.warn("x", "not a class")
except Exception as e:
    print("bad category", type(e).__name__)

print("=== nesting ===")
with warnings.catch_warnings():
    warnings.simplefilter("ignore")
    with warnings.catch_warnings(record=True) as log:
        warnings.simplefilter("always")
        warnings.warn("inner")
    print("inner", [str(w.message) for w in log])
    print("outer restored", [f[0] for f in warnings.filters])
print("done")
