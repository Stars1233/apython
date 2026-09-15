"""_testcapi - the few things CPython's TEST HARNESS reads off it.

Like lib/_testinternalcapi.py, and for the same reason: this is not CPython's
_testcapi, which is a C module exercising the public C API.  It is a stand-in
for what `test.support` looks up while it is being imported or while a
decorator is being evaluated, because those lookups sit at module scope and
an ImportError there takes a whole test module down before any of its tests
run.

test_call is the one that costs the most.  It writes

    _instance = _testcapi.MethInstance()

inside a class body, with the module fetched by a guarded import that leaves
None behind on failure -- so the whole file died on "'NoneType' object has no
attribute 'MethInstance'", and 181 tests with it.  That one cannot be stood
in for: MethInstance exists to exercise the C-level vectorcall protocol,
which this interpreter does not have.  What is here is only what
test.support itself reads, so the modules that merely IMPORT it survive.

Every name answers a true fact about this interpreter or raises
NotImplementedError when called.  Nothing here pretends to exercise a C API
that is not there.
"""

# test.support.requires_limited_api decorates with skipUnless(this), so False
# is what makes those tests skip rather than error.  There is no Limited API
# here: there is no C API at all.
LIMITED_API_AVAILABLE = False

# test.support.with_pymalloc() answers this.  The allocator here is
# src/alloc.asm's size-class pool, which is not pymalloc; the tests that
# branch on it are asking "are object sizes and arenas CPython's", and they
# are not.
WITH_PYMALLOC = False

# What test.support.requires_legacy_unicode_capi checks for.  Deliberately
# absent rather than present-and-lying: the decorator is
# skipUnless(unicode_legacy_string), so an AttributeError on the import is
# exactly how those tests get skipped.  Nothing else reads it.

# Sizes test.support and a few modules read directly.
SIZEOF_PYGC_HEAD = 16
SIZEOF_TIME_T = 8
SIZEOF_WCHAR_T = 4

# Recursion is Python-level here; see lib/_testinternalcapi.py.
Py_C_RECURSION_LIMIT = 10000

# The two subinterpreter entry points test.support wraps.  There are no
# subinterpreters here -- one interpreter state, no per-interpreter GIL -- so
# these raise, and test.support's callers are decorated with skips that read
# the absence correctly when they are not.


def run_in_subinterp(code):
    raise NotImplementedError(
        "_testcapi.run_in_subinterp is not implemented: this interpreter has "
        "one interpreter state and no subinterpreter support")


def run_in_subinterp_with_config(code, **config):
    raise NotImplementedError(
        "_testcapi.run_in_subinterp_with_config is not implemented; see "
        "run_in_subinterp")
