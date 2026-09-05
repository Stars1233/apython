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


def _caller(stacklevel):
    """Where the warning is being reported FROM.

    CPython walks stacklevel frames out and reports that filename, line and
    module, which is what puts a deprecation on the line that used the
    deprecated thing rather than on the line that warned.  Passing "" and 0
    instead, as this did, meant every warning printed as ":0:" and no filter
    with a module or lineno pattern could ever match.
    """
    import sys
    globals_ = None
    filename = "<sys>"
    lineno = 0
    try:
        frame = sys._getframe(1)
        depth = max(stacklevel, 1)
        while depth > 1 and frame is not None:
            frame = frame.f_back
            depth -= 1
        if frame is not None:
            globals_ = frame.f_globals
            filename = frame.f_code.co_filename
            lineno = frame.f_lineno
    except (ValueError, AttributeError):
        pass
    module = filename
    if globals_ is not None:
        module = globals_.get("__name__", filename)
    if module and module.endswith(".py"):
        module = module[:-3]
    registry = None
    if globals_ is not None:
        registry = globals_.setdefault("__warningregistry__", {})
    return filename, lineno, module, registry


def _source_line(filename, lineno):
    """The line the warning is about, which CPython prints under it.

    linecache is not in this tree, so the file is read directly and any
    failure means no line -- which is also what CPython shows for a warning
    from a file it cannot open.
    """
    if not filename or lineno <= 0:
        return None
    try:
        with open(filename, "r") as fh:
            for n, text in enumerate(fh, 1):
                if n == lineno:
                    return text
    except (OSError, UnicodeDecodeError):
        return None
    return None


def warn(message, category=None, stacklevel=1, source=None, *,
         skip_file_prefixes=()):
    """Show a warning, unless a filter says not to."""
    if category is None:
        category = UserWarning if not isinstance(message, Warning) \
            else type(message)
    if isinstance(message, Warning):
        category = type(message)
    if not (isinstance(category, type) and issubclass(category, Warning)):
        raise TypeError("category must be a Warning subclass, not '%s'"
                        % (type(category).__name__,))
    filename, lineno, module, registry = _caller(stacklevel + 1)
    warn_explicit(message, category, filename, lineno, module, registry,
                  source=source)


def warn_explicit(message, category, filename, lineno, module=None,
                  registry=None, module_globals=None, source=None):
    """The whole of the action protocol, which used to be only "ignore".

    "error" RAISES the warning, which is what a test suite turns on to make
    a deprecation fail; "once", "module" and "default" each remember what
    they have shown, in the registry the caller's globals carry.  All four
    were shown unconditionally.
    """
    import sys
    if module is None:
        module = filename
        if module and module.endswith(".py"):
            module = module[:-3]
    text = str(message)
    if isinstance(message, Warning):
        category = type(message)
    else:
        # What is SHOWN is a Warning instance, not the string: CPython wraps
        # it, and a recorded warning's .message is the instance -- which is
        # what a test reads .args off.
        message = category(text)
    action = _find_action(message, category, module, lineno)
    if action == "ignore":
        return
    key = (text, category, lineno)
    if registry is not None:
        if registry.get(key):
            return
        registry["version"] = _filters_version
    if action == "error":
        if isinstance(message, Warning):
            raise message
        raise category(message)
    if action == "once":
        if registry is not None:
            registry[key] = 1
        oncekey = (text, category)
        if _onceregistry.get(oncekey):
            return
        _onceregistry[oncekey] = 1
    elif action == "module":
        if registry is not None:
            registry[key] = 1
    elif action == "default":
        if registry is not None:
            registry[key] = 1
    line = _source_line(filename, lineno)
    mod = sys.modules.get("warnings")
    show = getattr(mod, "showwarning", None) if mod is not None else None
    if show is not None:
        try:
            show(message, category, filename, lineno, None, line)
        except TypeError:
            # A showwarning of the four-argument shape, which is what a
            # program that replaced it may well have written.
            show(message, category, filename, lineno)
        return
    try:
        sys.stderr.write("%s:%s: %s: %s\n"
                         % (filename, lineno, category.__name__, text))
        if line:
            sys.stderr.write("  %s\n" % (line.strip(),))
    except Exception:
        pass


def _acquire_lock():
    return None


def _release_lock():
    return None
