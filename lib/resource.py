"""resource - the process's limits and its accumulated usage.

CPython's is C; here `posix.prlimit` and `posix.getrusage` are the two
syscalls (src/modules/posixproc.asm) and everything else is Python, which is
the split the rest of the tree uses.  `getrlimit` and `setrlimit` are not
syscalls of their own: prlimit(2) does both, reading into `old` and writing
from `new`, so one entry point in assembly serves all three.

The constants are Linux's asm-generic/resource.h numbers.  RLIM_INFINITY is
(unsigned)-1 to the kernel and -1 here, which is what CPython reports too, so
nothing has to convert between the two views.
"""

import posix

__all__ = ["error", "getrlimit", "setrlimit", "prlimit", "getrusage",
           "getpagesize", "struct_rusage",
           "RLIM_INFINITY", "RUSAGE_SELF", "RUSAGE_CHILDREN", "RUSAGE_THREAD",
           "RLIMIT_CPU", "RLIMIT_FSIZE", "RLIMIT_DATA", "RLIMIT_STACK",
           "RLIMIT_CORE", "RLIMIT_RSS", "RLIMIT_NPROC", "RLIMIT_NOFILE",
           "RLIMIT_OFILE", "RLIMIT_MEMLOCK", "RLIMIT_AS", "RLIMIT_SIGPENDING",
           "RLIMIT_MSGQUEUE", "RLIMIT_NICE", "RLIMIT_RTPRIO", "RLIMIT_RTTIME"]

# CPython raises its own error class, which is an alias of OSError.
error = OSError

RLIM_INFINITY = -1

RUSAGE_SELF = 0
RUSAGE_CHILDREN = -1
RUSAGE_THREAD = 1

RLIMIT_CPU = 0
RLIMIT_FSIZE = 1
RLIMIT_DATA = 2
RLIMIT_STACK = 3
RLIMIT_CORE = 4
RLIMIT_RSS = 5
RLIMIT_NPROC = 6
RLIMIT_NOFILE = 7
RLIMIT_OFILE = RLIMIT_NOFILE        # the BSD spelling, which CPython keeps
RLIMIT_MEMLOCK = 8
RLIMIT_AS = 9
RLIMIT_LOCKS = 10
RLIMIT_SIGPENDING = 11
RLIMIT_MSGQUEUE = 12
RLIMIT_NICE = 13
RLIMIT_RTPRIO = 14
RLIMIT_RTTIME = 15


class struct_rusage(tuple):
    """The sixteen fields, in CPython's order -- a tuple first, as its is.

    `ru_utime` and `ru_stime` are floats of seconds; everything after them is
    an integer, and the seven Linux does not maintain are 0 there too.
    """

    __slots__ = ()
    n_fields = 16
    n_sequence_fields = 16
    n_unnamed_fields = 0

    _FIELDS = ("ru_utime", "ru_stime", "ru_maxrss", "ru_ixrss", "ru_idrss",
               "ru_isrss", "ru_minflt", "ru_majflt", "ru_nswap", "ru_inblock",
               "ru_oublock", "ru_msgsnd", "ru_msgrcv", "ru_nsignals",
               "ru_nvcsw", "ru_nivcsw")

    def __new__(cls, sequence):
        values = tuple(sequence)
        if len(values) != 16:
            raise TypeError("resource.struct_rusage() takes a 16-sequence")
        return tuple.__new__(cls, values)

    ru_utime = property(lambda self: self[0])
    ru_stime = property(lambda self: self[1])
    ru_maxrss = property(lambda self: self[2])
    ru_ixrss = property(lambda self: self[3])
    ru_idrss = property(lambda self: self[4])
    ru_isrss = property(lambda self: self[5])
    ru_minflt = property(lambda self: self[6])
    ru_majflt = property(lambda self: self[7])
    ru_nswap = property(lambda self: self[8])
    ru_inblock = property(lambda self: self[9])
    ru_oublock = property(lambda self: self[10])
    ru_msgsnd = property(lambda self: self[11])
    ru_msgrcv = property(lambda self: self[12])
    ru_nsignals = property(lambda self: self[13])
    ru_nvcsw = property(lambda self: self[14])
    ru_nivcsw = property(lambda self: self[15])

    def __repr__(self):
        return "resource.struct_rusage(%s)" % ", ".join(
            "%s=%r" % (name, value)
            for name, value in zip(self._FIELDS, self))


def getrlimit(resource):
    """The (soft, hard) pair for one limit."""
    return posix.prlimit(0, resource)


def _pair(limits):
    """CPython's three refusals, which are not one exception type.

    A non-sequence is a TypeError from the iteration itself; a sequence of the
    wrong LENGTH is "expected a tuple of 2 integers"; and a member that is not
    an integer is named by `operator.index`, which is where "'float' object
    cannot be interpreted as an integer" comes from.
    """
    import operator
    values = tuple(limits)              # TypeError for a non-iterable
    if len(values) != 2:
        raise ValueError("expected a tuple of 2 integers")
    return operator.index(values[0]), operator.index(values[1])


def _set(pid, resource, limits):
    soft, hard = _pair(limits)
    try:
        return posix.prlimit(pid, resource, soft, hard)
    except OSError as e:
        # CPython turns the kernel's two refusals into the sentences a caller
        # can act on; EINVAL here is almost always soft > hard.
        import errno as _errno
        if e.errno == _errno.EINVAL:
            raise ValueError("current limit exceeds maximum limit") from None
        if e.errno == _errno.EPERM:
            raise ValueError("not allowed to raise maximum limit") from None
        raise


def setrlimit(resource, limits):
    """Set one limit from a (soft, hard) pair."""
    _set(0, resource, limits)
    return None


def prlimit(pid, resource, limits=None):
    """getrlimit and setrlimit for another process, and the call both are
    built on: with limits given it sets them and answers what they WERE."""
    if limits is None:
        return posix.prlimit(pid, resource)
    return _set(pid, resource, limits)


def getrusage(who):
    if who not in (RUSAGE_SELF, RUSAGE_CHILDREN, RUSAGE_THREAD):
        raise ValueError("invalid who parameter")
    return struct_rusage(posix.getrusage(who))


def getpagesize():
    # posix.sysconf is not here; the page size is 4096 on every x86-64 Linux
    # this runs on, and mmap already assumes it.
    return 4096
