"""select - waiting on descriptors, over poll().

CPython's select module wraps four different multiplexers; this one is
poll(), which _socketcore provides, with select() written on top of it.  Both
of the interfaces Lib/selectors.py knows how to use are here, so its
DefaultSelector picks PollSelector and nothing has to fall back.

epoll, kqueue and devpoll are absent, and their absence is the supported way
to say so: selectors and asyncio both ask with hasattr.
"""

import _socketcore as _c

POLLIN = _c.POLLIN
POLLPRI = _c.POLLPRI
POLLOUT = _c.POLLOUT
POLLERR = _c.POLLERR
POLLHUP = _c.POLLHUP
POLLNVAL = _c.POLLNVAL

error = OSError

__all__ = ["select", "poll", "error", "POLLIN", "POLLPRI", "POLLOUT",
           "POLLERR", "POLLHUP", "POLLNVAL"]


def _fileno(obj):
    if isinstance(obj, int):
        return obj
    fd = obj.fileno()
    if not isinstance(fd, int):
        raise TypeError("fileno() returned a non-integer")
    return fd


def select(rlist, wlist, xlist, timeout=None):
    """The BSD interface, answered by poll().

    Duplicates are not merged: one pollfd per entry keeps the answer in the
    order the caller gave, which is what the return value is indexed by.
    """
    entries = []
    flat = []
    for obj in rlist:
        entries.append((obj, 0))
        flat.append(_fileno(obj))
        flat.append(POLLIN)
    for obj in wlist:
        entries.append((obj, 1))
        flat.append(_fileno(obj))
        flat.append(POLLOUT)
    for obj in xlist:
        entries.append((obj, 2))
        flat.append(_fileno(obj))
        flat.append(POLLPRI)

    if timeout is None:
        ms = -1
    else:
        timeout = float(timeout)
        if timeout < 0:
            raise ValueError("timeout must be non-negative")
        ms = int(timeout * 1000)

    revents = _c.poll(flat, ms)
    out = ([], [], [])
    for i in range(len(entries)):
        obj, which = entries[i]
        got = revents[i]
        if which == 0:
            if got & (POLLIN | POLLHUP | POLLERR | POLLNVAL):
                out[0].append(obj)
        elif which == 1:
            if got & (POLLOUT | POLLERR | POLLNVAL):
                out[1].append(obj)
        else:
            if got & (POLLPRI | POLLNVAL):
                out[2].append(obj)
    return out


class poll:
    """The poll object: a registry of descriptors and the events wanted."""

    def __init__(self):
        self._fds = {}

    def register(self, fd, eventmask=None):
        if eventmask is None:
            eventmask = POLLIN | POLLPRI | POLLOUT
        self._fds[_fileno(fd)] = eventmask

    def modify(self, fd, eventmask):
        fd = _fileno(fd)
        if fd not in self._fds:
            raise OSError(2, "No such file or directory")
        self._fds[fd] = eventmask

    def unregister(self, fd):
        fd = _fileno(fd)
        if fd not in self._fds:
            raise KeyError(fd)
        del self._fds[fd]

    def poll(self, timeout=None):
        fds = list(self._fds.items())
        flat = []
        for fd, mask in fds:
            flat.append(fd)
            flat.append(mask)
        if timeout is None or timeout < 0:
            ms = -1
        else:
            ms = int(timeout)
        revents = _c.poll(flat, ms)
        out = []
        for i in range(len(fds)):
            if revents[i]:
                out.append((fds[i][0], revents[i]))
        return out
