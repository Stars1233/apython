# `warnings`, CPython's own.
#
# The stand-in this replaces was written for one reason, and it is still the
# reason the split exists: the interpreter has to be able to warn before
# `warnings` is importable, so the filter list lives in `_warnings` and both
# halves must see THE SAME LIST.  importlib._bootstrap reaches for _warnings
# directly and never imports this module; a `filters` that was a copy would
# mean a filter installed through `warnings` did nothing to an import warning.
#
# CPython's file preserves that by construction -- it does
# `from _warnings import filters, ...` inside a try -- and the first thing
# checked below is that the identity holds here too.
#
# What it gains over the stand-in: catch_warnings as a context manager and a
# decorator, __warningregistry__ and the "once"/"module"/"default" actions
# that depend on it, _filters_mutated's version counter, the message and
# module regex matching, and _deprecated.
import re
import warnings

_original_showwarning = warnings.showwarning
import _warnings

print("one shared filter list:", warnings.filters is _warnings.filters)
print("and the other shared names:",
      warnings._defaultaction == _warnings._defaultaction,
      warnings._onceregistry is _warnings._onceregistry,
      warnings.warn is _warnings.warn,
      warnings.warn_explicit is _warnings.warn_explicit)

# --- recording, which is how every test suite looks at a warning -------
with warnings.catch_warnings(record=True) as caught:
    warnings.simplefilter("always")
    warnings.warn("first", UserWarning)
    warnings.warn("second", DeprecationWarning)
print("recorded:", [(str(w.message), w.category.__name__) for w in caught])
print("a record has the source line:",
      caught[0].filename.endswith(".py"), isinstance(caught[0].lineno, int))

# --- catch_warnings restores the filters it found ----------------------
warnings.resetwarnings()
warnings.simplefilter("error")
before = list(warnings.filters)
with warnings.catch_warnings():
    warnings.simplefilter("ignore")
    warnings.warn("silenced")
print("filters restored:", warnings.filters == before)
print("and it is still the same list object:",
      warnings.filters is _warnings.filters)

# --- the actions -------------------------------------------------------
def count(action, times=3, category=UserWarning):
    with warnings.catch_warnings(record=True) as w:
        warnings.simplefilter(action)
        for _ in range(times):
            warnings.warn("repeated", category)
        return len(w)


print("always:", count("always"))
print("once:", count("once"))
print("default:", count("default"))
print("ignore:", count("ignore"))

with warnings.catch_warnings():
    warnings.simplefilter("error")
    try:
        warnings.warn("raised", UserWarning)
        print("error: NOT RAISED")
    except UserWarning as exc:
        print("error:", exc)

# --- filterwarnings, with its message and module patterns --------------
with warnings.catch_warnings(record=True) as w:
    warnings.resetwarnings()
    warnings.simplefilter("always")
    warnings.filterwarnings("ignore", message="^skip")
    warnings.warn("skip me")
    warnings.warn("keep me")
print("message pattern:", [str(x.message) for x in w])

with warnings.catch_warnings(record=True) as w:
    warnings.resetwarnings()
    warnings.simplefilter("always")
    warnings.filterwarnings("ignore", category=DeprecationWarning)
    warnings.warn("a deprecation", DeprecationWarning)
    warnings.warn("a user warning", UserWarning)
print("category filter:", [str(x.message) for x in w])

# A category filter must not silence the others -- which is what the
# stand-in got wrong: its simplefilter took an action and no category.
with warnings.catch_warnings(record=True) as w:
    warnings.resetwarnings()
    warnings.simplefilter("always")
    warnings.simplefilter("ignore", FutureWarning)
    for cat in (UserWarning, DeprecationWarning, FutureWarning, SyntaxWarning):
        warnings.warn("x", cat)
print("one category silenced, the rest kept:",
      sorted(x.category.__name__ for x in w))

# --- the filter tuple's shape ------------------------------------------
warnings.resetwarnings()
warnings.filterwarnings("ignore", message="m", category=UserWarning,
                        module="mod", lineno=7, append=False)
entry = warnings.filters[0]
print("a filter is a 5-tuple:", len(entry), entry[0], entry[2].__name__, entry[4])
print("message and module are compiled patterns:",
      isinstance(entry[1], re.Pattern), isinstance(entry[3], re.Pattern))
warnings.filterwarnings("error", append=True)
print("append puts it last:", warnings.filters[-1][0])

# --- _filters_mutated invalidates the registry -------------------------
# The counter itself is private and spelled differently in CPython, so what
# is compared is its effect: a "default" filter shows a warning again after
# the filters are declared changed.  tests/test_warning_registry.py is the
# whole of that behaviour.
warnings.resetwarnings()
warnings.simplefilter("default")
seen = []
warnings.showwarning = lambda *a, **k: seen.append(1)
warnings.warn("versioned", UserWarning)
warnings.warn("versioned", UserWarning)
warnings._filters_mutated()
warnings.warn("versioned", UserWarning)
warnings.showwarning = _original_showwarning
print("_filters_mutated invalidates the registry:", len(seen))

# --- warn_explicit and __warningregistry__ -----------------------------
registry = {}
with warnings.catch_warnings(record=True) as w:
    warnings.resetwarnings()
    warnings.simplefilter("once")
    warnings.warn_explicit("explicit", UserWarning, "f.py", 12, registry=registry)
    warnings.warn_explicit("explicit", UserWarning, "f.py", 12, registry=registry)
print("warn_explicit once:", len(w), str(w[0].message))
print("the registry was written:", bool(registry))

# --- formatwarning / showwarning ---------------------------------------
print("formatwarning:",
      repr(warnings.formatwarning("m", UserWarning, "f.py", 3)))
print("with a source line:",
      repr(warnings.formatwarning("m", UserWarning, "f.py", 3, "  code()\n")))

shown = []
with warnings.catch_warnings():
    warnings.resetwarnings()
    warnings.simplefilter("always")
    warnings.showwarning = lambda *a, **k: shown.append(a[:2])
    warnings.warn("through showwarning")
print("showwarning is called:", [(str(m), c.__name__) for m, c in shown])

# --- catch_warnings as a decorator, and its attributes -----------------
warnings.resetwarnings()
warnings.simplefilter("always")
with warnings.catch_warnings(record=True) as w:
    warnings.warn("recorded", DeprecationWarning)
print("record inherits the filters in force:", len(w))

# --- stacklevel --------------------------------------------------------
def inner():
    warnings.warn("from inner", UserWarning, stacklevel=2)


def outer():
    inner()


with warnings.catch_warnings(record=True) as w:
    warnings.simplefilter("always")
    inner()
    outer()
print("stacklevel picks the caller:", w[0].lineno != w[1].lineno)

# --- _deprecated, which ast.py, re and _collections_abc call -----------
print("_deprecated exists:", callable(warnings._deprecated))
with warnings.catch_warnings(record=True) as w:
    warnings.simplefilter("always")
    warnings._deprecated("thing", remove=(3, 14))
print("_deprecated warns:", w[0].category.__name__, str(w[0].message))

# --- what must be refused ----------------------------------------------
for call, what in (
        (lambda: warnings.warn("m", "not a category"), "a str category"),
        (lambda: warnings.simplefilter("bogus"), "an unknown action"),
        (lambda: warnings.filterwarnings("bogus"), "an unknown action (filter)"),
        (lambda: warnings.simplefilter("always", lineno=-1), "a negative lineno")):
    try:
        call()
        print("%-26s NOT REFUSED" % what)
    except (TypeError, ValueError, AssertionError) as exc:
        print("%-26s %s" % (what, type(exc).__name__))

warnings.resetwarnings()
print("resetwarnings empties it:", warnings.filters)
print("__all__ complete:",
      [n for n in warnings.__all__ if not hasattr(warnings, n)])
print("survived")
