# The posix module: the platform module os.py imports.
#
# os.py gates its whole existence on `if 'posix' in sys.builtin_module_names`,
# so the name has to be in the module table as well as in sys.modules -- the
# module can be there and os.py will still raise "no os specific module found"
# without it.
#
# Almost nothing here can print a raw value: pids, working directories,
# directory contents and mtimes all differ between runs and between the two
# interpreters this is diffed against.  Everything is reduced to a predicate.

import posix

print("--- the module exists and is built in ---")
import sys
print("in modules :", "posix" in sys.modules)
print("in builtins:", "posix" in sys.builtin_module_names)
print("error      :", posix.error is OSError)
# CPython's lists thirty HAVE_* names; ours is empty, which is the honest
# answer -- no dir_fd= support here.  os.py needs the attribute to exist
# at all, because supports_dir_fd is defined only inside its _exists guard.
print("_have_funcs:", type(posix._have_functions).__name__)

print()
print("--- the constants, whose values are the kernel's ---")
print("O_RDONLY   :", posix.O_RDONLY)
print("O_WRONLY   :", posix.O_WRONLY)
print("O_RDWR     :", posix.O_RDWR)
print("O_ACCMODE  :", posix.O_ACCMODE)
print("O_CREAT    :", posix.O_CREAT)
print("O_EXCL     :", posix.O_EXCL)
print("O_TRUNC    :", posix.O_TRUNC)
print("O_APPEND   :", posix.O_APPEND)
print("O_NONBLOCK :", posix.O_NONBLOCK)
print("O_DIRECTORY:", posix.O_DIRECTORY)
print("O_NOFOLLOW :", posix.O_NOFOLLOW)
print("O_CLOEXEC  :", posix.O_CLOEXEC)
print("access     :", posix.F_OK, posix.R_OK, posix.W_OK, posix.X_OK)
# SEEK_SET and friends live in os.py in CPython, not in posix; they are
# here as well because posix.lseek takes them.  os.SEEK_SET is 0 either way.
print("wait       :", posix.WNOHANG, posix.WUNTRACED, posix.WCONTINUED)
print("seek       :", 0, 1, 2)

print()
print("--- getpid, getcwd ---")
pid = posix.getpid()
print("pid        :", type(pid).__name__, pid > 0)
print("stable     :", pid == posix.getpid())
cwd = posix.getcwd()
print("cwd        :", type(cwd).__name__, cwd.startswith("/"))
cwdb = posix.getcwdb()
print("cwdb       :", type(cwdb).__name__, cwdb == cwd.encode())

print()
print("--- strerror, whose text is libc's and so CPython's ---")
import errno
print("ENOENT     :", posix.strerror(errno.ENOENT))
print("EEXIST     :", posix.strerror(errno.EEXIST))
print("EACCES     :", posix.strerror(errno.EACCES))
print("type       :", type(posix.strerror(1)).__name__)

print()
print("--- urandom ---")
r = posix.urandom(32)
print("type       :", type(r).__name__)
print("length     :", len(r))
print("differs    :", posix.urandom(32) != posix.urandom(32))
print("zero       :", posix.urandom(0), len(posix.urandom(0)))
try:
    posix.urandom(-1)
    print("negative   : accepted?!")
except ValueError:
    print("negative   : ValueError")

print()
print("--- environ, a dict[bytes, bytes] ---")
env = posix.environ
print("type       :", type(env).__name__)
print("non-empty  :", len(env) > 0)
print("keys bytes :", all(type(k).__name__ == "bytes" for k in env))
print("vals bytes :", all(type(v).__name__ == "bytes" for v in env.values()))
print("HOME       :", type(env.get(b"HOME", b"")).__name__)
# The lookup is the point: bytes had no tp_hash, so every one of these missed.
print("round trip :", all(env[k] == v for k, v in list(env.items())[:20]))
print("no '=' key :", all(b"=" not in k for k in env))

print()
print("--- stat, and the shape it returns ---")
st = posix.stat("/etc/hostname")
print("type       :", type(st).__name__)
print("is a tuple :", isinstance(st, tuple))
print("len        :", len(st))
print("mode>0     :", st.st_mode > 0)
print("size>=0    :", st.st_size >= 0)
print("positional :", st[0] == st.st_mode, st[6] == st.st_size)
print("named tail :", st.st_atime_ns >= 0, st.st_blksize > 0, st.st_blocks >= 0)
print("rdev       :", type(st.st_rdev).__name__)
print("nlink      :", st.st_nlink >= 1)
print("repr shape :", repr(st).startswith("os.stat_result(st_mode="))
print("repr ends  :", repr(st).endswith(")"))
print("repr fields:", repr(st).count(", ") == 9)
# Two stats of the same file agree -- which needs tuple's comparison to
# accept a subclass on the right, the second half of the fix bytes_hash came
# with.
print("equal      :", posix.stat("/etc/hostname") == posix.stat("/etc/hostname"))
print("hashable   :", type(hash(st)).__name__)

print()
print("--- lstat and fstat see the same file ---")
print("lstat mode :", posix.lstat("/etc/hostname").st_mode == st.st_mode)
fd = posix.open("/etc/hostname", posix.O_RDONLY)
print("fd         :", type(fd).__name__, fd >= 0)
print("fstat size :", posix.fstat(fd).st_size == st.st_size)
print("read       :", type(posix.read(fd, 4)).__name__)
print("lseek      :", posix.lseek(fd, 0, 0))
print("lseek end  :", posix.lseek(fd, 0, 2) == st.st_size)
d = posix.dup(fd)
print("dup        :", d != fd, d >= 0)
print("close      :", posix.close(d), posix.close(fd))

print()
print("--- listdir ---")
names = posix.listdir("/")
print("type       :", type(names).__name__)
print("has etc    :", "etc" in names)
print("has usr    :", "usr" in names)
print("no dot     :", "." not in names and ".." not in names)
print("all str    :", all(type(n).__name__ == "str" for n in names))
print("no dups    :", len(set(names)) == len(names))
# The default is ".", as os.listdir()'s is.
print("default    :", sorted(posix.listdir()) == sorted(posix.listdir(".")))
# A big directory needs more than one getdents64 round, which is where a
# mis-sized d_reclen walks off the end of the buffer.
big = posix.listdir("/usr/lib")
print("big dir    :", len(big) > 20, len(set(big)) == len(big))

print()
print("--- access ---")
print("exists     :", posix.access("/etc/hostname", posix.F_OK))
print("readable   :", posix.access("/etc/hostname", posix.R_OK))
print("missing    :", posix.access("/nonexistent-xyz", posix.F_OK))
print("dir        :", posix.access("/etc", posix.X_OK))

print()
print("--- fspath ---")
print("str        :", posix.fspath("/a/b"))
print("bytes      :", posix.fspath(b"/a/b"))


class Pathish:
    def __fspath__(self):
        return "/from/fspath"


print("PathLike   :", posix.fspath(Pathish()))


class HostnamePath:
    def __fspath__(self):
        return "/etc/hostname"


print("stat PathL :", posix.stat(HostnamePath()).st_size == st.st_size)

print()
print("--- the errors, which must be the right OSError subclass ---")
for call, label in (
        (lambda: posix.stat("/nonexistent-xyz"), "stat missing"),
        (lambda: posix.listdir("/nonexistent-xyz"), "listdir missing"),
        (lambda: posix.open("/nonexistent-xyz", posix.O_RDONLY), "open missing"),
        (lambda: posix.rmdir("/nonexistent-xyz"), "rmdir missing"),
        (lambda: posix.unlink("/nonexistent-xyz"), "unlink missing"),
        (lambda: posix.readlink("/nonexistent-xyz"), "readlink missing"),
        (lambda: posix.mkdir("/etc"), "mkdir existing"),
        (lambda: posix.listdir("/etc/hostname"), "listdir a file"),
        (lambda: posix.close(-1), "close a bad fd"),
        (lambda: posix.read(-1, 4), "read a bad fd"),
):
    try:
        call()
        print("%-18s no error?!" % label)
    except OSError as e:
        print("%-18s %s errno=%s" % (label, type(e).__name__, e.errno))

print()
print("--- the errno reaches the right subclass ---")
try:
    posix.stat("/nonexistent-xyz")
except FileNotFoundError as e:
    print("FileNotFound:", e.errno == errno.ENOENT, e.filename)
try:
    posix.mkdir("/etc")
except FileExistsError as e:
    print("FileExists  :", e.errno == errno.EEXIST, e.filename)
try:
    posix.listdir("/etc/hostname")
except NotADirectoryError as e:
    print("NotADir     :", e.errno == errno.ENOTDIR)

print()
print("--- a path with an embedded NUL is refused, not truncated ---")
try:
    posix.stat("/etc/host\x00name")
    print("accepted?!")
except ValueError as e:
    # The type and the fact, not the wording: CPython 3.12 changed this
    # message mid-series, from "embedded null byte" to
    # "stat: embedded null character in path", and the suite diffs against
    # whichever python3 is installed.
    print("ValueError  :", "null" in str(e).lower())

print()
print("--- and a path of the wrong type ---")
for bad in (1, None, [], 1.5):
    try:
        posix.stat(bad)
        print("accepted?!")
    except TypeError:
        print("TypeError   :", type(bad).__name__)

print()
print("--- the wait-status readers, on statuses built by hand ---")
# 0x0000 is exit code 0; 0x0100 is exit code 1; 0x0009 is signal 9.
for status, label in ((0x0000, "exit 0"), (0x0100, "exit 1"),
                      (0x2A00, "exit 42"), (0x0009, "signal 9"),
                      (0x0089, "signal 9 + core"), (0x137F, "stopped 19"),
                      (0xFFFF, "continued")):
    print("%-16s exited=%-5s signalled=%-5s stopped=%-5s continued=%s" % (
        label, posix.WIFEXITED(status), posix.WIFSIGNALED(status),
        posix.WIFSTOPPED(status), posix.WIFCONTINUED(status)))
print("exit code  :", posix.WEXITSTATUS(0x2A00))
print("term sig   :", posix.WTERMSIG(0x0009))
print("stop sig   :", posix.WSTOPSIG(0x137F))
print("core dump  :", posix.WCOREDUMP(0x0089), posix.WCOREDUMP(0x0009))
print("to exitcode:", posix.waitstatus_to_exitcode(0x2A00),
      posix.waitstatus_to_exitcode(0x0009))
