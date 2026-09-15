# The warning registry, and the version stamp that invalidates it.
#
# "once", "module" and "default" each show a warning the first time and
# remember that they have, in the __warningregistry__ dict the warning's own
# module carries.  That memory has to be thrown away whenever the FILTERS
# change, or a filter installed after a warning has already been seen can
# never show it -- and every `with warnings.catch_warnings():` block changes
# the filters twice.
#
# CPython stamps the registry with a version counter and clears the whole
# dict when the stamp no longer matches.  This wrote the stamp and never
# compared it, so the registry was permanent:
#
#     count("once")      # 1  -- and the key is now in the registry
#     count("default")   # 0  -- CPython says 1
#
# Nothing raised.  A test suite that turns on a filter to catch a
# DeprecationWarning simply saw no warning, if anything earlier in the
# process had warned from the same line.
import warnings

_original_showwarning = warnings.showwarning


def count(action, times=3, message="repeated", category=UserWarning):
    with warnings.catch_warnings(record=True) as w:
        warnings.simplefilter(action)
        for _ in range(times):
            warnings.warn(message, category)
        return len(w)


# Each of these enters a fresh catch_warnings, which mutates the filters --
# so each starts from an empty registry and sees its own first occurrence.
print("always:", count("always"))
print("once:", count("once"))
print("default:", count("default"))
print("module:", count("module"))
print("ignore:", count("ignore"))
print("default after all of those:", count("default"))
print("once after all of those:", count("once"))

# --- the same, from one line, over and over ----------------------------
for action in ("default", "module", "once", "default"):
    print("%-8s %d" % (action, count(action)))

# --- without a catch_warnings, the registry DOES persist ---------------
# Two warns from the same line under one unchanged filter set is one
# warning, which is what "default" means.
warnings.resetwarnings()
warnings.simplefilter("default")
shown = []
warnings.showwarning = lambda *a, **k: shown.append(str(a[0]))
for _ in range(4):
    warnings.warn("same line", UserWarning)
print("default, same line, no filter change:", len(shown))

# Changing the filters invalidates it, and the same line warns again.
warnings.simplefilter("default")
for _ in range(4):
    warnings.warn("same line", UserWarning)
print("and again after simplefilter:", len(shown))

# resetwarnings is a filter change too.
warnings.resetwarnings()
warnings.simplefilter("default")
warnings.warn("same line", UserWarning)
print("and after resetwarnings:", len(shown))

# --- an explicit registry behaves the same -----------------------------
reg = {}
warnings.resetwarnings()
warnings.simplefilter("default")
shown.clear()
for _ in range(3):
    warnings.warn_explicit("explicit", UserWarning, "f.py", 9, registry=reg)
print("warn_explicit default:", len(shown))
print("the registry carries a version:", "version" in reg)
warnings.simplefilter("default")
warnings.warn_explicit("explicit", UserWarning, "f.py", 9, registry=reg)
print("after a filter change:", len(shown))

# A registry whose stamp is wrong is cleared outright, not merely bypassed.
reg2 = {("x", UserWarning, 1): 1, "version": -1, "stale": True}
warnings.warn_explicit("x", UserWarning, "f.py", 1, registry=reg2)
print("a stale registry is emptied:", "stale" in reg2, sorted(map(str, reg2)))

# --- different lines and different categories are different keys -------
warnings.resetwarnings()
warnings.simplefilter("default")
shown.clear()
warnings.warn_explicit("m", UserWarning, "f.py", 1, registry=reg)
warnings.warn_explicit("m", UserWarning, "f.py", 2, registry=reg)
warnings.warn_explicit("m", DeprecationWarning, "f.py", 1, registry=reg)
warnings.warn_explicit("other", UserWarning, "f.py", 1, registry=reg)
print("distinct keys all show:", len(shown))

# --- "module" ignores the line, "default" does not ---------------------
warnings.resetwarnings()
warnings.simplefilter("module")
shown.clear()
regm = {}
warnings.warn_explicit("m", UserWarning, "f.py", 1, registry=regm)
warnings.warn_explicit("m", UserWarning, "f.py", 2, registry=regm)
print("module ignores the line:", len(shown))

warnings.resetwarnings()
warnings.simplefilter("default")
shown.clear()
regd = {}
warnings.warn_explicit("m", UserWarning, "f.py", 1, registry=regd)
warnings.warn_explicit("m", UserWarning, "f.py", 2, registry=regd)
print("default does not:", len(shown))

warnings.showwarning = _original_showwarning
warnings.resetwarnings()
print("survived")
