"""_warnings - the warning machinery's C half.

CPython's is C so that the interpreter itself can warn before `warnings` is
importable, and so that the filter list can be consulted from the eval loop.
Neither applies here: nothing in this interpreter raises a warning from
assembly, and `warnings` is an ordinary module.

What is left is the interface `importlib._bootstrap` and `warnings` expect --
the filter list, the default action, the once-registry and `warn` itself --
so that a module which reaches for it finds it rather than falling back.
CPython's `warnings.py` does have that fallback; `importlib._bootstrap`'s
_setup does not, and twenty-one modules of the standard library stop there.
"""

filters = []
_defaultaction = "default"
_onceregistry = {}
_filters_version = 1


def _filters_mutated():
    global _filters_version
    _filters_version += 1


def _is_ignored(category, action):
    return action == "ignore"


def _match(pattern, text):
    """A filter's message and module patterns are compiled regexes in
    CPython; here `None` means "everything", which is what every filter this
    tree installs uses, and anything else is compared with `re.match` when re
    is importable and by equality when it is not."""
    if pattern is None:
        return True
    try:
        return pattern.match(text) is not None
    except AttributeError:
        return pattern == text


def _find_action(message, category, module, lineno):
    for item in filters:
        action, msg, cat, mod, ln = item
        if ((msg is None or _match(msg, str(message)))
                and (cat is None or issubclass(category, cat))
                and (mod is None or _match(mod, module))
                and (ln == 0 or lineno == ln)):
            return action
    return _defaultaction


def warn(message, category=None, stacklevel=1, source=None, *,
         skip_file_prefixes=()):
    """Show a warning, unless a filter says not to.

    Where CPython prints to sys.stderr through the C showwarning, this
    defers to the `warnings` module when it is loaded, so a program that
    installed its own showwarning still sees the call.
    """
    import sys
    if category is None:
        category = UserWarning if not isinstance(message, Warning) \
            else type(message)
    if isinstance(message, Warning):
        category = type(message)
    action = _find_action(message, category, "", 0)
    if action == "ignore":
        return
    mod = sys.modules.get("warnings")
    show = getattr(mod, "showwarning", None) if mod is not None else None
    if show is not None:
        show(message, category, "", 0)
        return
    try:
        sys.stderr.write("%s: %s\n" % (category.__name__, message))
    except Exception:
        pass


def warn_explicit(message, category, filename, lineno, module=None,
                  registry=None, module_globals=None, source=None):
    import sys
    action = _find_action(message, category, module or filename, lineno)
    if action == "ignore":
        return
    if registry is not None:
        key = (str(message), category, lineno)
        if registry.get(key):
            return
        if action == "once":
            registry[key] = 1
    mod = sys.modules.get("warnings")
    show = getattr(mod, "showwarning", None) if mod is not None else None
    if show is not None:
        show(message, category, filename, lineno)
        return
    try:
        sys.stderr.write("%s:%s: %s: %s\n"
                         % (filename, lineno, category.__name__, message))
    except Exception:
        pass


def _acquire_lock():
    return None


def _release_lock():
    return None
