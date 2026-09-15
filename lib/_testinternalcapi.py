"""_testinternalcapi - the few things CPython's TEST HARNESS reads off it.

This is not CPython's _testinternalcapi, which is a C module exposing the
interpreter's internals so its own suite can poke them.  It is a stand-in for
the handful of names `test.support` looks up while it is being IMPORTED, and
it exists because those lookups are unguarded enough to take a whole test
module down before any of its tests run:

    def has_no_debug_ranges():
        try:
            import _testinternalcapi
        except ImportError:
            raise unittest.SkipTest("_testinternalcapi required")
        config = _testinternalcapi.get_config()
        return not bool(config['code_debug_ranges'])

`requires_debug_ranges()` calls that at CLASS-DEFINITION time, so the
SkipTest is raised while the module body is executing and nothing in the file
runs at all.  test_traceback, test_compile, test_marshal, test_code,
test_peepholer and test_types are all decorated with it, and
test.support.bytecode_helper does a module-scope `from _testinternalcapi
import ...` that test_dis and test_bytes depend on.

So each name here answers a TRUE fact about this interpreter, or raises
NotImplementedError when called.  The three compiler entry points are the
second kind: they have to EXIST, because the import that fetches them is at
module scope, and the few tests that actually call them then fail with a
clear message instead of taking the other hundreds with them.
"""

import sys

# The size of the collector's header, which test.support.check_sizeof adds
# back to sys.getsizeof's answer for a tracked object.  It is PyGC_Head in
# src/include/object.inc, two words.
SIZEOF_PYGC_HEAD = 16

# What a Py_buffer costs; test.support reads it the same way.
SIZEOF_PYOBJECT = 16

# The interpreter's own recursion limit, as the C layer sees it.
SIZEOF_TIME_T = 8


def get_config():
    """The subset of PyConfig test.support asks about.

    `code_debug_ranges` is the one that matters: it says whether a code
    object carries column information, which is what co_positions() answers
    from.  This tree's does -- src/traceback.asm holds both side tables and
    tests/test_code_localsplus.py checks them -- so the honest answer is 1,
    and requires_debug_ranges() lets its module run.
    """
    return {
        "code_debug_ranges": 1,
        "use_hash_seed": 0,
        "hash_seed": 0,
        "faulthandler": 0,
        "dev_mode": 0,
        "verbose": 0,
        "quiet": 0,
        "isolated": 0,
        "bytes_warning": 0,
        "optimization_level": 0,
        "parser_debug": 0,
        "write_bytecode": 0,
        "interactive": 0,
        "inspect": 0,
        "install_signal_handlers": 1,
        "config": {},
    }


def get_recursion_depth():
    """How deep the caller is, counted the way test.support's fallback does.

    CPython's C version reads the interpreter's own counter; this walks
    f_back, which is what test.support falls back to when the import fails.
    Supplying it means the fallback is not needed.
    """
    depth = 0
    frame = sys._getframe(1)
    while frame is not None:
        depth += 1
        frame = frame.f_back
    return depth


def get_recursion_limit():
    return sys.getrecursionlimit()


def set_recursion_limit(n):
    sys.setrecursionlimit(n)


def get_c_recursion_remaining():
    """CPython's C stack headroom, in its own units.

    There is no separate C recursion counter here -- a Python call does not
    consume a C frame in this interpreter, which is what the inline frame push
    is for -- so the answer is the Python headroom.
    """
    return sys.getrecursionlimit() - get_recursion_depth()


def optimize_ast(tree, *args, **kwargs):
    raise NotImplementedError(
        "_testinternalcapi.optimize_ast is not implemented: this module is a "
        "stand-in for what test.support reads, not for CPython's compiler "
        "internals")


def compiler_codegen(*args, **kwargs):
    raise NotImplementedError(
        "_testinternalcapi.compiler_codegen is not implemented: this module "
        "is a stand-in for what test.support reads, not for CPython's "
        "compiler internals.  It exists so that the module-scope import in "
        "test.support.bytecode_helper succeeds and the tests that do not use "
        "it can run.")


def optimize_cfg(*args, **kwargs):
    raise NotImplementedError(
        "_testinternalcapi.optimize_cfg is not implemented; see "
        "compiler_codegen")


def assemble_code_object(*args, **kwargs):
    raise NotImplementedError(
        "_testinternalcapi.assemble_code_object is not implemented; see "
        "compiler_codegen")


def set_config(config):
    raise NotImplementedError(
        "_testinternalcapi.set_config is not implemented: there is no "
        "PyConfig to write back.  It is here so the failure says that, "
        "rather than arriving as an AttributeError -- and a version that "
        "accepted the dict and did nothing would be worse than both, since "
        "_test_embed_set_config reads the values straight back off sys and "
        "would report a mismatch instead of a missing feature.")


# ---------------------------------------------------------------------------
# The locale codec round trip, which test_codecs.LocaleCodecTest exercises.
#
# CPython's pair wrap _Py_DecodeLocaleEx/_Py_EncodeLocaleEx -- the C library's
# mbstowcs under the LC_CTYPE locale, or the filesystem encoding.  Both call
# sites pass current_locale=0, which means "use the filesystem encoding", and
# sys.getfilesystemencoding() is always 'utf-8' here (DIVERGENCES.md records
# why), so that is the encoding to use.
#
# The three outcomes are the ones the C switch reports, and the tests read all
# three: a result, a RuntimeError whose text they match with a regex, and a
# ValueError for an error handler the C layer does not offer.  In particular
# 'backslashreplace' has to be REFUSED even though this interpreter's own
# str.encode accepts it -- _Py_GetErrorHandler does not know it.
# ---------------------------------------------------------------------------
_LOCALE_ERROR_HANDLERS = frozenset((
    "strict", "surrogateescape", "surrogatepass", "replace", "ignore",
))


def _locale_encoding(current_locale):
    import sys
    if current_locale:
        # No LC_CTYPE handling here; the filesystem encoding is the only one.
        return sys.getfilesystemencoding()
    return sys.getfilesystemencoding()


def _check_locale_errors(errors):
    if errors is None:
        return "strict"
    if errors not in _LOCALE_ERROR_HANDLERS:
        raise ValueError("unsupported error handler")
    return errors


def EncodeLocaleEx(text, current_locale=0, errors=None):
    if not isinstance(text, str):
        raise TypeError("argument 1 must be str, not %s"
                        % type(text).__name__)
    handler = _check_locale_errors(errors)
    try:
        return text.encode(_locale_encoding(current_locale), handler)
    except UnicodeEncodeError as exc:
        raise RuntimeError("encode error: pos=%d, reason=%s"
                           % (exc.start, exc.reason)) from None


def DecodeLocaleEx(encoded, current_locale=0, errors=None):
    if not isinstance(encoded, (bytes, bytearray)):
        raise TypeError("argument 1 must be bytes, not %s"
                        % type(encoded).__name__)
    handler = _check_locale_errors(errors)
    try:
        return bytes(encoded).decode(_locale_encoding(current_locale), handler)
    except UnicodeDecodeError as exc:
        raise RuntimeError("decode error: pos=%d, reason=%s"
                           % (exc.start, exc.reason)) from None
