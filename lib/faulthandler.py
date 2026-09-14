"""faulthandler - dump the Python traceback on a fault or a timeout.

CPython's is a C module, and it has to be: its whole point is to report a
crash, and by then calling into the interpreter is not safe.  This one is
Python over the `signal` module, and the difference is worth stating plainly
rather than discovering.

What works
----------
`dump_traceback` walks the frame chain with sys._getframe and writes it in
CPython's format.  `register` and `unregister` are signal.signal, so a signal
a program sends itself -- SIGUSR1, SIGTERM -- really does dump a traceback.
`dump_traceback_later` is signal.setitimer with SIGALRM, so a watchdog over
code that keeps running really does fire.  `enable`, `disable` and
`is_enabled` install and remove handlers for the fatal signals.

What does not
-------------
A handler here runs the way every Python-level signal handler in this tree
runs: the C handler records the signal and the eval loop delivers it at the
top of an instruction.  That is fine for a signal a program RAISES and for a
timer, and it is not fine for a genuine SIGSEGV -- the process is already in a
state where the eval loop will not be reached, so the report never comes.
CPython's C handler writes the traceback from the signal context itself and
does not need the interpreter.

So this reports faults a program causes deliberately and hangs that are not
wedged inside a C loop, and it is silent for a real crash.  That is recorded
in DIVERGENCES.md.  `all_threads` is accepted and ignored, because there is
only ever one.
"""

import signal
import sys

__all__ = [
    "enable", "disable", "is_enabled", "dump_traceback",
    "register", "unregister",
    "dump_traceback_later", "cancel_dump_traceback_later",
]

# The signals CPython's enable() takes over, with the words it reports them
# by -- "Segmentation fault", not "SIGSEGV".
_FATAL = (
    ("SIGSEGV", "Segmentation fault"),
    ("SIGFPE", "Floating point exception"),
    ("SIGABRT", "Aborted"),
    ("SIGBUS", "Bus error"),
    ("SIGILL", "Illegal instruction"),
)

_state = {
    "enabled": False,
    "file": None,
    "saved": {},        # signum -> the handler enable() displaced
    "registered": {},   # signum -> (file, chain, previous handler)
    "later": None,      # (file, exit) while a timer is armed
}


def _fileno(file):
    """CPython takes a file object or a raw fd; regrtest passes an fd."""
    if file is None:
        file = sys.stderr
    if isinstance(file, int):
        return file
    fd = file.fileno()
    return fd


def _write(fd, text):
    import os
    data = text.encode("utf-8", "backslashreplace")
    while data:
        n = os.write(fd, data)
        if n <= 0:
            break
        data = data[n:]


def _frames(frame):
    out = []
    while frame is not None:
        out.append(frame)
        frame = frame.f_back
    return out


def _dump(fd, frame, header="Current thread"):
    lines = ["%s 0x%016x (most recent call first):\n" % (header, 0)]
    for f in _frames(frame):
        code = f.f_code
        lines.append('  File "%s", line %d in %s\n'
                     % (code.co_filename, f.f_lineno, code.co_name))
    _write(fd, "".join(lines))


def dump_traceback(file=None, all_threads=True):
    """Write the current thread's traceback to `file` (default sys.stderr)."""
    _dump(_fileno(file), sys._getframe(1))


def _make_handler(fd, name, chain_to=None, exit_after=False):
    # `name` is None for register(), which dumps the traceback and nothing
    # else; only enable()'s fatal handlers announce themselves, and they do it
    # in CPython's words.
    def handler(signum, frame):
        if name is not None:
            _write(fd, "Fatal Python error: %s\n\n" % (name,))
        _dump(fd, frame)
        if chain_to is not None and callable(chain_to):
            chain_to(signum, frame)
        if exit_after:
            import os
            os._exit(1)
    return handler


def enable(file=None, all_threads=True):
    """Install a handler for each fatal signal.

    See the module docstring: a real SIGSEGV will not reach it.
    """
    fd = _fileno(file)
    disable()
    saved = {}
    for name, reported in _FATAL:
        signum = getattr(signal, name, None)
        if signum is None:
            continue
        try:
            saved[signum] = signal.signal(signum, _make_handler(fd, reported))
        except (OSError, ValueError, RuntimeError):
            continue
    _state["saved"] = saved
    _state["enabled"] = True
    _state["file"] = fd


def disable():
    """Remove the handlers enable() installed."""
    for signum, previous in _state["saved"].items():
        try:
            signal.signal(signum, previous if previous is not None
                          else signal.SIG_DFL)
        except (OSError, ValueError, RuntimeError):
            pass
    _state["saved"] = {}
    _state["enabled"] = False
    _state["file"] = None


def is_enabled():
    return _state["enabled"]


def register(signum, file=None, all_threads=True, chain=False):
    """Dump a traceback when `signum` is received, then optionally chain."""
    signum = int(signum)
    fd = _fileno(file)
    previous = signal.getsignal(signum)
    handler = _make_handler(fd, None, chain_to=previous if chain else None)
    signal.signal(signum, handler)
    _state["registered"][signum] = (fd, chain, previous)


def unregister(signum):
    """Undo register(); True when it had been registered."""
    signum = int(signum)
    entry = _state["registered"].pop(signum, None)
    if entry is None:
        return False
    previous = entry[2]
    try:
        signal.signal(signum, previous if previous is not None
                      else signal.SIG_DFL)
    except (OSError, ValueError, RuntimeError):
        pass
    return True


def dump_traceback_later(timeout, repeat=False, file=None, exit=False):
    """Dump a traceback after `timeout` seconds, as a watchdog.

    signal.setitimer with SIGALRM, so it fires wherever a Python-level signal
    handler can run -- which is at the top of an instruction, and therefore
    not inside a C loop that never returns.
    """
    if timeout <= 0:
        raise ValueError("timeout must be greater than 0")
    fd = _fileno(file)

    def fired(signum, frame):
        _write(fd, "Timeout (%s)!\n" % (_format_timeout(timeout),))
        _dump(fd, frame)
        if exit:
            import os
            os._exit(1)
        if not repeat:
            cancel_dump_traceback_later()

    signal.signal(signal.SIGALRM, fired)
    interval = timeout if repeat else 0.0
    signal.setitimer(signal.ITIMER_REAL, timeout, interval)
    _state["later"] = (fd, exit)


def cancel_dump_traceback_later():
    """Disarm the watchdog dump_traceback_later armed."""
    if _state["later"] is None:
        return
    try:
        signal.setitimer(signal.ITIMER_REAL, 0.0, 0.0)
        signal.signal(signal.SIGALRM, signal.SIG_DFL)
    except (OSError, ValueError, RuntimeError):
        pass
    _state["later"] = None


def _format_timeout(seconds):
    seconds = int(seconds)
    hours, seconds = divmod(seconds, 3600)
    minutes, seconds = divmod(seconds, 60)
    return "%d:%02d:%02d" % (hours, minutes, seconds)
