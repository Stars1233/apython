# The posix calls a program asks for by name, and the constants it spells
# them in terms of.
#
# Thirty-two of the everyday names were missing -- getuid and its eight
# siblings, cpu_count, sched_getaffinity, times, pread, pwrite, sendfile,
# fchdir, get_terminal_size -- along with the O_*, EX_*, ST_*, SCHED_*,
# PRIO_*, SEEK_*, GRND_*, RTLD_*, POSIX_FADV_*, RWF_*, P_*, CLD_* and the
# NGROUPS_MAX group.  lib/os.py is CPython's verbatim file, so each one
# surfaces under its own name the moment it is here.
#
# One trap worth naming: sched_getaffinity answers how many BYTES of the mask
# it wrote and leaves the rest of the buffer alone.  Scanning the whole
# buffer counts whatever was on the stack -- this machine reported 267
# processors on a 24-processor box, and cpu_count, which is the same scan,
# reported 284.
import os
import posix

# --- the credentials, which cannot fail --------------------------------
print("uid == euid:", posix.getuid() == posix.geteuid())
print("gid == egid:", posix.getgid() == posix.getegid())
print("ppid and pgrp are positive:", posix.getppid() > 0, posix.getpgrp() > 0)
print("they are ints:", all(isinstance(v, int) for v in
                            (posix.getuid(), posix.geteuid(), posix.getgid(),
                             posix.getegid(), posix.getppid(), posix.getpgrp())))
groups = posix.getgroups()
print("getgroups:", type(groups).__name__,
      all(isinstance(g, int) for g in groups))

# --- the processors ----------------------------------------------------
affinity = posix.sched_getaffinity(0)
print("affinity is a set of ints:", type(affinity).__name__,
      all(isinstance(c, int) for c in affinity), min(affinity) >= 0)
print("cpu_count agrees with it:", posix.cpu_count() == len(affinity))
print("and with os:", os.cpu_count() == posix.cpu_count())
print("affinity of self and of 0 agree:",
      posix.sched_getaffinity(os.getpid()) == affinity)

# --- times -------------------------------------------------------------
t = posix.times()
print("times has five fields:", len(t), all(isinstance(x, float) for x in t))
print("they are not negative:", all(x >= 0.0 for x in t))
print("it is a times_result:", type(t).__name__)
ot = os.times()
print("os.times names them:", ot.user >= 0, ot.system >= 0,
      ot.children_user >= 0, ot.children_system >= 0, ot.elapsed >= 0)
print("and it IS posix.times:", os.times is posix.times)

# --- pread and pwrite, which leave the position alone -------------------
path = "@test_posix_identity_tmp"
fd = os.open(path, os.O_RDWR | os.O_CREAT | os.O_TRUNC, 0o600)
try:
    os.write(fd, b"hello world")
    os.lseek(fd, 3, 0)
    print("pread:", posix.pread(fd, 5, 0), posix.pread(fd, 5, 6))
    print("the position did not move:", os.lseek(fd, 0, 1) == 3)
    print("pread past the end:", posix.pread(fd, 100, 6))
    print("pread at the end:", posix.pread(fd, 5, 11))
    print("pwrite:", posix.pwrite(fd, b"HELLO", 0))
    print("and it took:", posix.pread(fd, 11, 0))
    print("the position still did not move:", os.lseek(fd, 0, 1) == 3)
    print("pwrite from a bytearray:",
          posix.pwrite(fd, bytearray(b"xy"), 0), posix.pread(fd, 2, 0))
    try:
        posix.pread(fd, -1, 0)
        print("a negative length: NOT REFUSED")
    except (ValueError, OSError) as exc:
        print("a negative length:", type(exc).__name__)
    try:
        posix.pwrite(fd, "a str", 0)
        print("a str to pwrite: NOT REFUSED")
    except TypeError:
        print("a str to pwrite: TypeError")
finally:
    os.close(fd)
    os.unlink(path)

# --- fchdir -------------------------------------------------------------
here = os.getcwd()
d = os.open("/tmp", os.O_RDONLY)
try:
    posix.fchdir(d)
    print("fchdir:", os.getcwd() == "/tmp")
finally:
    os.chdir(here)
    os.close(d)
try:
    posix.fchdir(9999)
    print("fchdir on a closed descriptor: NOT REFUSED")
except OSError as exc:
    print("fchdir on a closed descriptor:", exc.errno == 9)

# --- get_terminal_size, which needs a terminal --------------------------
# Under a pipe there is none, and ENOTTY is the answer rather than a guess.
try:
    size = posix.get_terminal_size(1)
    print("terminal size:", type(size).__name__, len(size) == 2,
          size.columns == size[0], size.lines == size[1])
except OSError as exc:
    print("no terminal:", exc.errno in (6, 25))

# --- the constants ------------------------------------------------------
GROUPS = [
    ("O_", ("O_DIRECT", "O_DIRECTORY", "O_NOFOLLOW", "O_NOATIME", "O_PATH",
            "O_TMPFILE", "O_SYNC", "O_DSYNC", "O_ASYNC", "O_LARGEFILE")),
    ("EX_", ("EX_OK", "EX_USAGE", "EX_DATAERR", "EX_NOINPUT", "EX_NOUSER",
             "EX_NOHOST", "EX_UNAVAILABLE", "EX_SOFTWARE", "EX_OSERR",
             "EX_OSFILE", "EX_CANTCREAT", "EX_IOERR", "EX_TEMPFAIL",
             "EX_PROTOCOL", "EX_NOPERM", "EX_CONFIG")),
    ("ST_", ("ST_RDONLY", "ST_NOSUID", "ST_NODEV", "ST_NOEXEC",
             "ST_SYNCHRONOUS", "ST_MANDLOCK", "ST_WRITE", "ST_APPEND",
             "ST_IMMUTABLE", "ST_NOATIME", "ST_NODIRATIME", "ST_RELATIME")),
    ("SCHED_", ("SCHED_OTHER", "SCHED_FIFO", "SCHED_RR", "SCHED_BATCH",
                "SCHED_IDLE", "SCHED_RESET_ON_FORK")),
    ("PRIO_", ("PRIO_PROCESS", "PRIO_PGRP", "PRIO_USER")),
    ("SEEK_", ("SEEK_DATA", "SEEK_HOLE")),
    ("GRND_", ("GRND_RANDOM", "GRND_NONBLOCK")),
    ("RTLD_", ("RTLD_LAZY", "RTLD_NOW", "RTLD_GLOBAL", "RTLD_LOCAL",
               "RTLD_NODELETE", "RTLD_NOLOAD", "RTLD_DEEPBIND")),
    ("POSIX_FADV_", ("POSIX_FADV_NORMAL", "POSIX_FADV_RANDOM",
                     "POSIX_FADV_SEQUENTIAL", "POSIX_FADV_WILLNEED",
                     "POSIX_FADV_DONTNEED", "POSIX_FADV_NOREUSE")),
    ("RWF_", ("RWF_DSYNC", "RWF_HIPRI", "RWF_SYNC", "RWF_NOWAIT",
              "RWF_APPEND")),
    ("P_/W", ("P_ALL", "P_PID", "P_PGID", "P_PIDFD", "WEXITED", "WSTOPPED",
              "WCONTINUED", "WNOWAIT")),
    ("CLD_", ("CLD_EXITED", "CLD_KILLED", "CLD_DUMPED", "CLD_TRAPPED",
              "CLD_STOPPED", "CLD_CONTINUED")),
    ("limits", ("NGROUPS_MAX", "TMP_MAX", "PIPE_BUF")),
]
for label, names in GROUPS:
    missing = [n for n in names if not hasattr(posix, n)]
    wrong = [n for n in names
             if hasattr(posix, n) and getattr(posix, n) != getattr(os, n, None)
             and hasattr(os, n)]
    print("%-12s %d names, missing %s, disagreeing with os %s"
          % (label, len(names), missing, wrong))

# The VALUES are the kernel's, so they are compared against the oracle's own.
import posix as _p

vals = [(n, getattr(_p, n)) for _, names in GROUPS for n in names
        if hasattr(_p, n)]
print("values:", vals)
print("survived")
