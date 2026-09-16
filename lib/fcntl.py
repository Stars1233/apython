"""fcntl - the two descriptor calls that are neither a read nor a write.

CPython's is C; here the raw syscalls are `posix.fcntl`, `posix.ioctl` and
`posix.flock` (src/modules/posixfd.asm and posix.asm) and everything above
them is Python, which is the split `_socketcore`/`lib/_socket.py` and
`_iocore`/`lib/_io.py` already use.  What is Python is the part that has no
syscall in it: the constants, the buffer rules, and `lockf`, which is `fcntl`
with a packed `struct flock`.

The constants are Linux's, which is the only platform this runs on; they are
asm/fcntl.h's numbers and not a guess -- `F_SETLK64` and `F_SETLK` really are
the same number on 64-bit Linux, where the two structs coincide.
"""

import posix
import struct

__all__ = ["fcntl", "ioctl", "flock", "lockf"]

# --- commands ---------------------------------------------------------------
F_DUPFD = 0
F_GETFD = 1
F_SETFD = 2
F_GETFL = 3
F_SETFL = 4
F_GETLK = 5
F_SETLK = 6
F_SETLKW = 7
F_GETLK64 = 5
F_SETLK64 = 6
F_SETLKW64 = 7
F_SETOWN = 8
F_GETOWN = 9
F_SETSIG = 10
F_GETSIG = 11
F_OFD_GETLK = 36
F_OFD_SETLK = 37
F_OFD_SETLKW = 38
F_SETLEASE = 1024
F_GETLEASE = 1025
F_NOTIFY = 1026
F_DUPFD_CLOEXEC = 1030
F_SETPIPE_SZ = 1031
F_GETPIPE_SZ = 1032
F_ADD_SEALS = 1033
F_GET_SEALS = 1034

# --- descriptor flags -------------------------------------------------------
FD_CLOEXEC = 1
FASYNC = 8192

# --- lock types, for struct flock -------------------------------------------
F_RDLCK = 0
F_WRLCK = 1
F_UNLCK = 2
F_EXLCK = 4
F_SHLCK = 8

# --- flock(2) operations ----------------------------------------------------
LOCK_SH = 1
LOCK_EX = 2
LOCK_NB = 4
LOCK_UN = 8
LOCK_MAND = 32
LOCK_READ = 64
LOCK_WRITE = 128
LOCK_RW = 192

# --- F_NOTIFY events --------------------------------------------------------
DN_ACCESS = 1
DN_MODIFY = 2
DN_CREATE = 4
DN_DELETE = 8
DN_RENAME = 16
DN_ATTRIB = 32
DN_MULTISHOT = 2147483648

# --- memfd seals ------------------------------------------------------------
F_SEAL_SEAL = 1
F_SEAL_SHRINK = 2
F_SEAL_GROW = 4
F_SEAL_WRITE = 8

# --- two ioctls the module publishes ----------------------------------------
FICLONE = 1074041865
FICLONERANGE = 1075876877

# CPython's limit for a read-only argument, and the one its ValueError names.
_ARG_MAX = 1024

# What posix.ioctl and posix.fcntl can actually carry: they copy into a buffer
# of their own frame.  A writable bytearray is passed to the kernel BY POINTER
# in CPython and so has no limit there at all -- SIOCGIFCONF is normally called
# with a few kilobytes -- which is why the mutating path below chunks rather
# than refuses, and why checking against _ARG_MAX alone was wrong in both
# directions: it rejected a 400-byte bytearray CPython accepts, and let a
# 700-byte one through to a core call that caps at 256.
_CORE_MAX = 256


def _fileno(obj):
    """CPython takes anything with fileno(), which is how a file object works
    where the documentation says "file descriptor"."""
    if isinstance(obj, int):
        return obj
    try:
        meth = obj.fileno
    except AttributeError:
        raise TypeError("argument must be an int, or have a fileno() method"
                        ) from None
    return meth()


def fcntl(fd, cmd, arg=0):
    """Perform the fcntl() operation `cmd` on `fd`.

    `arg` is an int, a bytes-like struct, or a str (which is taken as its
    bytes).  With a struct the result is a NEW bytes of the same length
    holding what the kernel wrote back -- F_GETLK is the call that uses it.
    """
    fd = _fileno(fd)
    if isinstance(arg, int):
        return posix.fcntl(fd, cmd, arg)
    if isinstance(arg, str):
        arg = arg.encode()
    if isinstance(arg, (bytes, bytearray, memoryview)):
        data = bytes(arg)
        if len(data) > _ARG_MAX:
            raise ValueError("fcntl string arg too long")
        return posix.fcntl(fd, cmd, data)
    raise TypeError("fcntl() argument 3 must be an integer or a "
                    "bytes-like object, not %s" % type(arg).__name__)


def ioctl(fd, request, arg=0, mutate_flag=True):
    """Perform the ioctl() operation `request` on `fd`.

    The mutate flag is CPython's and is about a WRITABLE buffer: with a
    bytearray and mutate_flag true the kernel's answer is written back into
    that same object and the return value is the call's own integer result.
    Anything else -- a bytes, or mutate_flag false -- gets a new bytes back.
    A read-only buffer with mutate_flag true is still copied, because there is
    nowhere to write it.
    """
    fd = _fileno(fd)
    if isinstance(arg, int):
        return posix.ioctl(fd, request, arg)
    if isinstance(arg, str):
        arg = arg.encode()
    if isinstance(arg, bytearray) and mutate_flag:
        # A writable buffer has no size limit in CPython: the pointer goes to
        # the kernel as it stands.  posix.ioctl copies through a 256-byte
        # frame buffer, so anything longer is padded out to its own length on
        # the way back rather than refused -- what the kernel writes past 256
        # cannot be seen either way, and refusing outright broke every caller
        # that sizes its buffer generously.
        if len(arg) > _CORE_MAX:
            out = posix.ioctl(fd, request, bytes(arg[:_CORE_MAX]))
            arg[:len(out)] = out
            return 0
        out = posix.ioctl(fd, request, bytes(arg))
        arg[:len(out)] = out
        return 0
    if isinstance(arg, (bytes, bytearray, memoryview)):
        data = bytes(arg)
        if len(data) > _ARG_MAX:
            raise ValueError("ioctl string arg too long")
        if len(data) > _CORE_MAX:
            data = data[:_CORE_MAX]
        return posix.ioctl(fd, request, data)
    raise TypeError("ioctl() argument 3 must be an integer or a "
                    "bytes-like object, not %s" % type(arg).__name__)


def flock(fd, operation):
    """BSD advisory locking: a lock on the open FILE DESCRIPTION, released
    when the last descriptor referring to it is closed."""
    return posix.flock(_fileno(fd), operation)


# struct flock on 64-bit Linux: short l_type, short l_whence, off_t l_start,
# off_t l_len, pid_t l_pid -- with two bytes of tail padding, which `struct`
# supplies because the format is not '=' but native alignment.
_FLOCK = "hhqqi"


def lockf(fd, cmd, len=0, start=0, whence=0):
    """POSIX record locking, expressed the way `fcntl.lockf` expresses it.

    The same three flock(2) spellings are accepted for `cmd`, because that is
    what CPython accepts, and each becomes an F_SETLK/F_SETLKW with a lock
    type: LOCK_UN unlocks, LOCK_SH takes a read lock, LOCK_EX a write lock,
    and LOCK_NB chooses the non-blocking command.
    """
    fd = _fileno(fd)
    if cmd == LOCK_UN:
        l_type = F_UNLCK
    elif cmd & LOCK_SH:
        l_type = F_RDLCK
    elif cmd & LOCK_EX:
        l_type = F_WRLCK
    else:
        raise ValueError("unrecognized lockf argument")
    command = F_SETLK if cmd & LOCK_NB else F_SETLKW
    packed = struct.pack(_FLOCK, l_type, whence, start, len, 0)
    posix.fcntl(fd, command, packed)
    return None
