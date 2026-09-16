"""_lsprof - the deterministic profiler cProfile and pstats are written against.

CPython's is C, over a rotating tree keyed by code object; this is the same
bookkeeping in Python over `sys.setprofile`, which already reports all four
events (`call`, `return`, `c_call`, `c_return`) and is the hook CPython's own
profiler installs.  The arithmetic is the part that has to match exactly,
because pstats prints it and `test_profile`/`test_cprofile` compare printed
output:

  * `totaltime` is the wall time between a frame's call and its return,
    counted ONCE per outermost activation -- a recursive call adds to
    `callcount` and to `reccallcount` and not to `totaltime`, or a recursion
    of depth n would report n times the elapsed time.
  * `inlinetime` is that minus the time spent inside the calls it made, which
    is accumulated per context rather than subtracted afterwards: a context
    carries the total of its children, so an exception unwinding several
    frames still leaves each parent's figure right.
  * a subentry is per (caller, callee) pair and carries the same four numbers,
    which is what fills pstats' "callers" column.

A profiler is a real class rather than a factory, because `cProfile.Profile`
SUBCLASSES it and `test_cprofile` instantiates that subclass directly.

Not here: the C profiler's `_lsprof.profiler_entry` and `profiler_subentry`
are structseqs, so they also index and unpack.  These are plain objects with
the same attribute names, which is everything cProfile and pstats read.
"""

import sys
import time

__all__ = ["Profiler", "profiler_entry", "profiler_subentry"]


class profiler_subentry:
    """One (caller -> callee) edge, with this edge's share of the numbers."""

    __slots__ = ("code", "callcount", "reccallcount", "totaltime",
                 "inlinetime")

    def __init__(self, code):
        self.code = code
        self.callcount = 0
        self.reccallcount = 0
        self.totaltime = 0.0
        self.inlinetime = 0.0

    def __repr__(self):
        return ("_lsprof.profiler_subentry(code=%r, callcount=%r, "
                "reccallcount=%r, totaltime=%r, inlinetime=%r)"
                % (self.code, self.callcount, self.reccallcount,
                   self.totaltime, self.inlinetime))


class profiler_entry:
    """One profiled callable, and the edges out of it."""

    __slots__ = ("code", "callcount", "reccallcount", "totaltime",
                 "inlinetime", "calls")

    def __init__(self, code):
        self.code = code
        self.callcount = 0
        self.reccallcount = 0
        self.totaltime = 0.0
        self.inlinetime = 0.0
        self.calls = None

    def __repr__(self):
        return ("_lsprof.profiler_entry(code=%r, callcount=%r, "
                "reccallcount=%r, totaltime=%r, inlinetime=%r, calls=%r)"
                % (self.code, self.callcount, self.reccallcount,
                   self.totaltime, self.inlinetime, self.calls))


class _Context:
    """One activation on the profiler's own stack.

    `t0` is when it started and `subt` the time its children have taken so
    far; the difference at return is its inline time.  `entry` and `subentry`
    are the rows the numbers land in, resolved once at call time so that the
    return path does no lookups.
    """

    __slots__ = ("t0", "subt", "entry", "subentry", "key")

    def __init__(self, t0, entry, subentry, key):
        self.t0 = t0
        self.subt = 0.0
        self.entry = entry
        self.subentry = subentry
        self.key = key


def _display(key):
    """What `getstats()` reports as `.code`.

    A Python frame reports its code object; a builtin reports a descriptive
    STRING, which is what cProfile's `label()` recognises with
    `isinstance(code, str)` and turns into the ('~', 0, name) triple pstats
    prints.  The dict is keyed by the ORIGINAL object either way, so two
    builtins whose descriptions collide still keep separate rows.
    """
    if hasattr(key, "co_name"):
        return key
    return _builtin_label(key)


def _builtin_label(func):
    """CPython's normalizeUserObj, which is not the repr in every case.

    A method bound to an INSTANCE is reported as the unbound descriptor's
    repr -- `"ab".join` becomes "<method 'join' of 'str' objects>" -- so that
    every call of `str.join` lands in one row whatever it was called on.  A
    module-level builtin is bound to its MODULE, whose type has no such
    attribute, and falls through to "<built-in method builtins.len>"; the
    repr, `<built-in function len>`, is NOT what the profiler reports.
    """
    name = getattr(func, "__name__", None)
    if name is None:
        return repr(func)
    module = getattr(func, "__module__", None)
    try:
        receiver = func.__self__
    except AttributeError:
        # CPython's m_self == NULL arm: the bare name, qualified by the
        # module unless that is builtins.
        if isinstance(module, str) and module != "builtins":
            return "<%s.%s>" % (module, name)
        return "<%s>" % name
    for base in type(receiver).__mro__:
        if name in base.__dict__:
            return repr(base.__dict__[name])
    if isinstance(module, str):
        return "<built-in method %s.%s>" % (module, name)
    return "<built-in method %s>" % name


class Profiler:
    """Profiler(timer=None, timeunit=None, subcalls=True, builtins=True)"""

    def __init__(self, timer=None, timeunit=None, subcalls=True,
                 builtins=True):
        self.timer = timer
        # A custom timer may count in integer units; timeunit says how long
        # one is.  CPython's default is 0.0, which means "the timer already
        # answers seconds".
        self.timeunit = 0.0 if timeunit is None else float(timeunit)
        self.subcalls = bool(subcalls)
        self.builtins = bool(builtins)
        self._entries = {}
        self._stack = []
        self._enabled = False
        # cProfile.Profile reads this after create_stats(); it is not part of
        # the C type's surface but nothing here needs it to be.
        self.stats = {}

    # --- the clock ---------------------------------------------------------

    def _now(self):
        timer = self.timer
        if timer is None:
            return time.perf_counter()
        value = timer()
        unit = self.timeunit
        if unit:
            return value * unit
        if isinstance(value, tuple):
            # CPython accepts the (sec, usec) pair `profile` module timers
            # hand back.
            return value[0] + value[1] * 1e-6
        return value

    # --- the hook ----------------------------------------------------------

    def _entry_for(self, key):
        entry = self._entries.get(key)
        if entry is None:
            entry = profiler_entry(_display(key))
            self._entries[key] = entry
        return entry

    def _enter(self, key):
        entry = self._entry_for(key)
        subentry = None
        if self.subcalls and self._stack:
            caller = self._stack[-1].entry
            calls = caller.calls
            if calls is None:
                calls = caller.calls = []
            shown = _display(key)
            for existing in calls:
                # `==`, not `is`: _display builds a NEW string for a builtin,
                # so an identity test never matched and every call appended a
                # subentry of its own -- fifty len() calls became fifty rows of
                # callcount 1, and `calls` grew without bound on a hot loop.
                if existing.code == shown:
                    subentry = existing
                    break
            else:
                subentry = profiler_subentry(shown)
                calls.append(subentry)
        self._stack.append(_Context(self._now(), entry, subentry, key))

    def _leave(self, key):
        stack = self._stack
        if not stack:
            return
        # An exception can leave frames the profiler was never told about --
        # `sys.setprofile` reports a `return` for each, but a generator that
        # is never resumed reports none at all.  Unwind to the matching entry
        # rather than trusting the top blindly.
        for depth in range(len(stack) - 1, -1, -1):
            if stack[depth].key is key:
                break
        else:
            return
        while len(stack) > depth + 1:
            self._close(stack.pop(), stack)
        self._close(stack.pop(), stack)

    def _close(self, context, stack):
        elapsed = self._now() - context.t0
        inline = elapsed - context.subt
        entry = context.entry
        entry.callcount += 1
        entry.inlinetime += inline
        # Recursion: the time is already inside the outer activation's own
        # total, so only the outermost one adds to totaltime.
        recursive = False
        for other in stack:
            if other.entry is entry:
                recursive = True
                break
        if recursive:
            entry.reccallcount += 1
        else:
            entry.totaltime += elapsed

        sub = context.subentry
        if sub is not None:
            sub.callcount += 1
            sub.inlinetime += inline
            if recursive:
                sub.reccallcount += 1
            else:
                sub.totaltime += elapsed

        if stack:
            stack[-1].subt += elapsed

    def _dispatch(self, frame, event, arg):
        if event == "call":
            self._enter(frame.f_code)
        elif event == "return":
            self._leave(frame.f_code)
        elif event == "c_call":
            if self.builtins:
                self._enter(arg)
        elif event == "c_return" or event == "c_exception":
            if self.builtins:
                self._leave(arg)

    # --- the surface cProfile uses -----------------------------------------

    def enable(self, subcalls=None, builtins=None):
        # CPython parses both with a -1 "not given" sentinel and leaves the
        # flag alone; defaulting them to True here discarded what the
        # constructor was told, so Profiler(subcalls=False, builtins=False)
        # followed by a bare enable() profiled builtins after all.
        if subcalls is not None:
            self.subcalls = bool(subcalls)
        if builtins is not None:
            self.builtins = bool(builtins)
        self._enabled = True
        sys.setprofile(self._dispatch)

    def disable(self):
        if self._enabled:
            self._enabled = False
            sys.setprofile(None)
            # Whatever was still on the stack when profiling stopped never
            # got a `return`; close it so its time is not simply lost.
            stack = self._stack
            while stack:
                self._close(stack.pop(), stack)

    def clear(self):
        self._entries = {}
        self._stack = []

    def getstats(self):
        # A live list of the rows themselves, as CPython's is: cProfile reads
        # them straight through and pstats keeps no reference.
        return list(self._entries.values())

    def __enter__(self):
        self.enable()
        return self

    def __exit__(self, *exc_info):
        self.disable()


profiler = Profiler
