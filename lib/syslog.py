"""syslog - the system logger, spoken to directly rather than through libc.

CPython's module is a wrapper on openlog(3)/syslog(3)/closelog(3), which are
themselves a few dozen lines around a datagram socket: connect to /dev/log,
write "<priority>tag[pid]: message", and drop the message if nothing is
listening.  That is what this does, so no new C entry point is needed and the
behaviour a program can see -- the priority arithmetic, the mask, the ident
and the option flags -- is the same.

Two things libc does that this does not: the SOCK_STREAM fallback for the few
systems whose /dev/log is a stream socket, and LOG_CONS, which writes to
/dev/console when the socket cannot be reached.  Neither is observable from
Python, and a message that cannot be delivered is silently dropped either way,
which is syslog(3)'s own contract.
"""

import os
import time

__all__ = ["openlog", "syslog", "closelog", "setlogmask",
           "LOG_MASK", "LOG_UPTO"]

# --- priorities, lowest number is most urgent -------------------------------
LOG_EMERG = 0
LOG_ALERT = 1
LOG_CRIT = 2
LOG_ERR = 3
LOG_WARNING = 4
LOG_NOTICE = 5
LOG_INFO = 6
LOG_DEBUG = 7

# --- facilities, already shifted left by three ------------------------------
LOG_KERN = 0
LOG_USER = 8
LOG_MAIL = 16
LOG_DAEMON = 24
LOG_AUTH = 32
LOG_SYSLOG = 40
LOG_LPR = 48
LOG_NEWS = 56
LOG_UUCP = 64
LOG_CRON = 72
LOG_AUTHPRIV = 80
LOG_LOCAL0 = 128
LOG_LOCAL1 = 136
LOG_LOCAL2 = 144
LOG_LOCAL3 = 152
LOG_LOCAL4 = 160
LOG_LOCAL5 = 168
LOG_LOCAL6 = 176
LOG_LOCAL7 = 184

# --- openlog() options ------------------------------------------------------
LOG_PID = 1
LOG_CONS = 2
LOG_ODELAY = 4
LOG_NDELAY = 8
LOG_NOWAIT = 16
LOG_PERROR = 32

_PATH = "/dev/log"

_ident = None           # None until openlog(); syslog() then uses sys.argv[0]
_logoption = 0
_facility = LOG_USER
_logmask = 0xFF         # every priority, which is what libc starts with
_sock = None
_connected = False


def LOG_MASK(pri):
    """The mask bit for one priority."""
    return 1 << pri


def LOG_UPTO(pri):
    """The mask for every priority at least as urgent as `pri`."""
    return (1 << (pri + 1)) - 1


def _default_ident():
    import sys
    argv0 = (sys.argv[0] if getattr(sys, "argv", None) else "") or ""
    return os.path.basename(argv0) or "python"


def _open():
    """Connect the datagram socket, or leave it None if nothing is there.

    A missing /dev/log is not an error: syslog(3) drops the message, and so
    must this, or a program that logs would fail in a container.
    """
    global _sock, _connected
    if _connected:
        return _sock
    _connected = True
    try:
        import socket
        s = socket.socket(socket.AF_UNIX, socket.SOCK_DGRAM)
        s.connect(_PATH)
    except (OSError, ImportError):
        _sock = None
        return None
    _sock = s
    return s


def openlog(ident=None, logoption=0, facility=LOG_USER):
    """Set the tag, the options and the default facility for later calls."""
    global _ident, _logoption, _facility
    if ident is not None and not isinstance(ident, str):
        raise TypeError("openlog() argument 'ident' must be str, not %s"
                        % type(ident).__name__)
    _ident = _default_ident() if ident is None else ident
    _logoption = logoption
    _facility = facility
    if logoption & LOG_NDELAY:
        _open()


def closelog():
    """Drop the connection and forget the ident, as CPython's does."""
    global _sock, _connected, _ident, _logoption, _facility
    if _sock is not None:
        _sock.close()
    _sock = None
    _connected = False
    _ident = None
    _logoption = 0
    _facility = LOG_USER


def setlogmask(maskpri):
    """Install a new priority mask and answer the old one.  A zero mask is a
    QUERY, which is syslog(3)'s rule and the one a caller relies on to read
    the mask without changing it."""
    global _logmask
    old = _logmask
    if maskpri:
        _logmask = maskpri
    return old


def syslog(priority, message=None):
    """syslog(message) or syslog(priority, message).

    A priority with no facility bits takes the one openlog() set, which is
    what the `priority | facility` arithmetic in every caller assumes.
    """
    if message is None:
        message = priority
        priority = LOG_INFO
    if not isinstance(message, str):
        raise TypeError("syslog() argument 'message' must be str, not %s"
                        % type(message).__name__)
    if not _logmask & LOG_MASK(priority & 7):
        return None
    if not priority & ~7:
        priority |= _facility

    ident = _ident if _ident is not None else _default_ident()
    tag = ident
    if _logoption & LOG_PID:
        tag = "%s[%d]" % (ident, os.getpid())
    stamp = time.strftime("%b %e %H:%M:%S") if hasattr(time, "strftime") else ""
    line = "<%d>%s %s: %s" % (priority, stamp, tag, message)

    if _logoption & LOG_PERROR:
        try:
            import sys
            sys.stderr.write("%s: %s\n" % (tag, message))
        except Exception:
            pass

    s = _open()
    if s is None:
        return None
    try:
        s.send(line.encode("utf-8", "replace"))
    except OSError:
        # The daemon restarted, or the socket filled.  libc reconnects once
        # and then gives up; so does this.
        global _connected, _sock
        try:
            s.close()
        except OSError:
            pass
        _sock = None
        _connected = False
        s = _open()
        if s is not None:
            try:
                s.send(line.encode("utf-8", "replace"))
            except OSError:
                pass
    return None
