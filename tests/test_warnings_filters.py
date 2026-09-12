# `catch_warnings()` plus `simplefilter('error')` must make a warning RAISE.
#
# That pair is how essentially every test suite asserts a warning, including
# CPython's own -- `test_hmac.test_legacy_block_size_warnings` is written on
# it -- and it did nothing here: the warning printed to stderr and execution
# continued.
#
# The action protocol in lib/_warnings.py was fine.  What was wrong is WHERE
# it looked for the filters.  `catch_warnings.__enter__` does
#
#     self._filters = warnings.filters
#     warnings.filters = self._filters[:]
#
# -- it REBINDS the attribute rather than mutating the list -- so from that
# point on `warnings.filters` and `_warnings.filters` are two different lists,
# and every `simplefilter()` call inside the block goes into the one that
# lib/_warnings.py was not reading.  CPython's C half re-reads the attribute
# off the warnings module on every warning (`get_warnings_attr(interp,
# &_Py_ID(filters), ...)`), which is exactly what makes the rebinding work.
#
# Outside a catch_warnings block the two names are the same object, which is
# why `simplefilter('error')` at module level always worked and hid this.

import warnings

print("== error, inside catch_warnings ==")
for label, setup in (
    ("simplefilter('error')",
     lambda: warnings.simplefilter("error")),
    ("simplefilter('error', RuntimeWarning)",
     lambda: warnings.simplefilter("error", RuntimeWarning)),
    ("filterwarnings('error')",
     lambda: warnings.filterwarnings("error")),
    ("filterwarnings('error', category=...)",
     lambda: warnings.filterwarnings("error", category=RuntimeWarning)),
):
    with warnings.catch_warnings():
        setup()
        try:
            warnings.warn("boom", RuntimeWarning)
            print("%-40s NOT raised" % label)
        except Warning as e:
            print("%-40s raised %s: %s" % (label, type(e).__name__, e))

print()
print("== a filter for another category does not catch this one ==")
with warnings.catch_warnings():
    warnings.simplefilter("ignore")
    warnings.simplefilter("error", DeprecationWarning)
    try:
        warnings.warn("not me", RuntimeWarning)
        print("RuntimeWarning: not raised (correct)")
    except Warning as e:
        print("RuntimeWarning: raised %s (wrong)" % type(e).__name__)
    try:
        warnings.warn("me", DeprecationWarning)
        print("DeprecationWarning: not raised (wrong)")
    except Warning as e:
        print("DeprecationWarning: raised %s (correct)" % type(e).__name__)

print()
print("== a subclass is caught by a filter on its base ==")
with warnings.catch_warnings():
    warnings.simplefilter("error", Warning)
    try:
        warnings.warn("sub", FutureWarning)
        print("NOT raised")
    except Warning as e:
        print("raised %s" % type(e).__name__)

print()
print("== ignore still ignores, and the block restores what it found ==")
with warnings.catch_warnings():
    warnings.simplefilter("ignore")
    warnings.warn("silent", RuntimeWarning)
    print("ignored without raising")
    inner = len(warnings.filters)
with warnings.catch_warnings():
    warnings.simplefilter("error")
    print("a second block is independent:", end=" ")
    try:
        warnings.warn("boom2", UserWarning)
        print("NOT raised")
    except Warning as e:
        print("raised", type(e).__name__)

print()
print("== catch_warnings(record=True) collects them ==")
with warnings.catch_warnings(record=True) as log:
    warnings.simplefilter("always")
    warnings.warn("one", UserWarning)
    warnings.warn("two", RuntimeWarning)
    print("recorded:", len(log))
    print("categories:", [w.category.__name__ for w in log])
    print("messages:", [str(w.message) for w in log])
    print("message is an instance:", [isinstance(w.message, Warning) for w in log])

print()
print("== the error action carries the warning INSTANCE, with its args ==")
with warnings.catch_warnings():
    warnings.simplefilter("error")
    try:
        warnings.warn(UserWarning("built", 42))
    except UserWarning as e:
        print("args:", e.args)
    try:
        warnings.warn("from a string")
    except UserWarning as e:
        print("from str:", type(e).__name__, e.args)

print()
print("== nesting: the inner block's filters do not leak out ==")
with warnings.catch_warnings():
    warnings.simplefilter("ignore")
    with warnings.catch_warnings():
        warnings.simplefilter("error")
        try:
            warnings.warn("inner", UserWarning)
            print("inner NOT raised")
        except Warning:
            print("inner raised")
    warnings.warn("outer", UserWarning)
    print("outer still ignored")

print()
print("== once and default remember, inside a block ==")
with warnings.catch_warnings(record=True) as log:
    warnings.simplefilter("once")
    for _ in range(3):
        warnings.warn("repeat", UserWarning)
    print("once showed:", len(log))
