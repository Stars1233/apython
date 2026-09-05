"""signal - the Python face of _signal.

_signal has the numbers and the two entry points; this is the layer CPython's
signal.py adds over them, which is almost entirely enum: the signal numbers
become `Signals` members and the two sentinels become `Handlers`, so that
`signal.SIGINT` reprs as `<Signals.SIGINT: 2>` and `getsignal()` answers a
member rather than a bare int.

What CPython's has and this does not is the thread-mask family --
pthread_sigmask, sigpending, sigwait and their timed forms.  CPython's own
signal.py guards each with `if 'x' in _globals`, because they are absent on
platforms without them; this interpreter has one thread, so they are absent
here for the same reason and by the same mechanism.
"""

import _signal
from _signal import *
from enum import IntEnum as _IntEnum

_globals = globals()

_IntEnum._convert_(
    "Signals", __name__,
    lambda name: name.isupper() and name.startswith("SIG")
    and not name.startswith("SIG_"))

_IntEnum._convert_(
    "Handlers", __name__,
    lambda name: name in ("SIG_DFL", "SIG_IGN"))

_IntEnum._convert_(
    "Sigmasks", __name__,
    lambda name: name in ("SIG_BLOCK", "SIG_UNBLOCK", "SIG_SETMASK"))


def _int_to_enum(value, enum_klass):
    """-> the enum member for `value`, or `value` itself when there is none.

    A signal number the platform does not name -- a realtime signal, say --
    stays an int rather than becoming a member that does not exist.
    """
    if not isinstance(value, int):
        return value
    try:
        return enum_klass(value)
    except ValueError:
        return value


def _enum_to_int(value):
    """-> `value` as a plain int if it is an enum member, else unchanged."""
    try:
        return int(value)
    except (ValueError, TypeError):
        return value


def signal(signalnum, handler):
    """-> the handler that was installed before, as a Handlers member or a
    callable."""
    handler = _signal.signal(_enum_to_int(signalnum), _enum_to_int(handler))
    return _int_to_enum(handler, Handlers)


def getsignal(signalnum):
    """-> the handler currently installed for `signalnum`."""
    handler = _signal.getsignal(_enum_to_int(signalnum))
    return _int_to_enum(handler, Handlers)


def strsignal(signalnum):
    """-> the platform's description of the signal, or None."""
    return _signal.strsignal(_enum_to_int(signalnum))


def valid_signals():
    """-> the set of signal numbers this platform accepts.

    Every number from 1 to NSIG-1 except the two glibc keeps for its own
    thread machinery, which is what sigfillset leaves behind and so what
    CPython answers.  A number the platform names comes back as a Signals
    member; a realtime signal, which has no name, stays an int.
    """
    return {_int_to_enum(n, Signals)
            for n in range(1, NSIG) if n not in (32, 33)}


del _IntEnum, _globals
