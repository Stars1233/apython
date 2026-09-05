"""warnings - the filter list, and what shows a warning.

CPython's `warnings` is Python over a C `_warnings`, and the division is
about bootstrapping: the interpreter itself has to be able to warn before
this module is importable.  The same division is kept here -- `_warnings`
holds the filter list, the default action, the once-registry and the two
`warn` entry points, and this is the part programs actually call -- so that
`importlib._bootstrap`, which reaches for `_warnings` directly, and a caller
that reaches for `warnings` see the same filters.

What was here before was a stub: a `warn` that appended to a list nobody
read, and a `simplefilter` that took an action and no category.  Anything
that installed a filter for one category silenced every warning, and
`filterwarnings`, `resetwarnings` and `_deprecated` -- which ast.py, re and
_collections_abc all call -- were not there at all.
"""

import _warnings
import sys

__all__ = ["warn", "warn_explicit", "showwarning", "formatwarning",
           "filterwarnings", "simplefilter", "resetwarnings",
           "catch_warnings"]

# The one filter list, shared with _warnings: importlib._bootstrap consults
# it through that name and never imports this module.
filters = _warnings.filters

warn = _warnings.warn
warn_explicit = _warnings.warn_explicit


def _filters_mutated():
    _warnings._filters_mutated()


def formatwarning(message, category, filename, lineno, line=None):
    """CPython's wording, which doctests and test suites match against."""
    s = "%s:%s: %s: %s\n" % (filename, lineno, category.__name__, message)
    if line:
        s += "  %s\n" % (line.strip(),)
    return s


def showwarning(message, category=UserWarning, filename="", lineno=0,
                file=None, line=None):
    if file is None:
        file = sys.stderr
        if file is None:
            return
    try:
        file.write(formatwarning(message, category, filename, lineno, line))
    except OSError:
        pass


def _add_filter(*item, append=False):
    if not append:
        try:
            filters.remove(item)
        except ValueError:
            pass
        filters.insert(0, item)
    elif item not in filters:
        filters.append(item)
    _filters_mutated()


def filterwarnings(action, message="", category=Warning, module="",
                   lineno=0, append=False):
    """Insert a filter.  The message and module patterns are regexes, which
    is why this is not the same call as simplefilter."""
    # CPython asserts rather than raising here, and the difference is
    # visible: a test that passes a bad action gets AssertionError.
    assert action in ("error", "ignore", "always", "default", "module",
                      "once"), "invalid action: %r" % (action,)
    assert isinstance(lineno, int) and lineno >= 0, \
        "lineno must be an int >= 0"
    compiled_message = None
    compiled_module = None
    if message:
        import re
        compiled_message = re.compile(message, re.I)
    if module:
        import re
        compiled_module = re.compile(module)
    _add_filter(action, compiled_message, category, compiled_module, lineno,
                append=append)


def simplefilter(action, category=Warning, lineno=0, append=False):
    """The same, with no patterns: everything of that category matches."""
    assert action in ("error", "ignore", "always", "default", "module",
                      "once"), "invalid action: %r" % (action,)
    assert isinstance(lineno, int) and lineno >= 0, \
        "lineno must be an int >= 0"
    _add_filter(action, None, category, None, lineno, append=append)


def resetwarnings():
    filters[:] = []
    _filters_mutated()


def _deprecated(name, message="{name!r} is deprecated and slated for removal "
                              "in Python {remove}", *, remove, _version=None):
    """Warn that something will be removed in a future version.

    CPython's is in _warnings and raises RuntimeError once the interpreter
    has passed the removal version; ast.py and re both call it by this name.
    """
    if _version is None:
        _version = sys.version_info
    remove_formatted = "%d.%d" % remove
    if (_version[:2] > remove) if isinstance(remove, tuple) else False:
        msg = message.format(name=name, remove=remove_formatted)
        raise RuntimeError(msg)
    warn(message.format(name=name, remove=remove_formatted),
         DeprecationWarning, stacklevel=3)


class catch_warnings:
    """Save the filter list and showwarning, and put them back.

    With record=True the warnings raised inside are collected into the list
    it returns, which is how a test asserts that something warned.
    """

    def __init__(self, *, record=False, module=None, action=None,
                 category=Warning, lineno=0, append=False):
        self._record = record
        self._module = sys.modules["warnings"] if module is None else module
        self._entered = False
        if action is None:
            self._filter = None
        else:
            self._filter = (action, category, lineno, append)

    def __repr__(self):
        args = []
        if self._record:
            args.append("record=True")
        if self._module is not sys.modules["warnings"]:
            args.append("module=%r" % (self._module,))
        return "%s(%s)" % (type(self).__name__, ", ".join(args))

    def __enter__(self):
        if self._entered:
            raise RuntimeError("Cannot enter %r twice" % (self,))
        self._entered = True
        # The CONTENTS are saved, not the binding.  _warnings holds the same
        # list object and reads it by its own name, so rebinding
        # warnings.filters to a copy would leave the half that actually
        # decides consulting the original -- which is how a
        # simplefilter("ignore", DeprecationWarning) inside a catch_warnings
        # block silenced nothing.
        self._filters = self._module.filters[:]
        self._module._filters_mutated()
        self._showwarning = self._module.showwarning
        if self._record:
            log = []

            def showarg(message, category=UserWarning, filename="", lineno=0,
                        file=None, line=None):
                log.append(WarningMessage(message, category, filename,
                                          lineno, file, line))

            self._module.showwarning = showarg
        else:
            log = None
        if self._filter is not None:
            action, category, lineno, append = self._filter
            simplefilter(action, category, lineno, append)
        return log

    def __exit__(self, *exc_info):
        if not self._entered:
            raise RuntimeError("Cannot exit %r without entering first" % (self,))
        self._module.filters[:] = self._filters
        self._module._filters_mutated()
        self._module.showwarning = self._showwarning
        return False


class WarningMessage:
    _WARNING_DETAILS = ("message", "category", "filename", "lineno", "file",
                        "line", "source")

    def __init__(self, message, category, filename, lineno, file=None,
                 line=None, source=None):
        self.message = message
        self.category = category
        self.filename = filename
        self.lineno = lineno
        self.file = file
        self.line = line
        self.source = source
        self._category_name = category.__name__ if category else None

    def __str__(self):
        return ("{message : %r, category : %r, filename : %r, lineno : %s, "
                "line : %r}" % (self.message, self._category_name,
                                self.filename, self.lineno, self.line))
