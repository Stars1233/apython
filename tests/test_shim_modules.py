"""The small libc shims: fcntl, resource, syslog, and _lsprof.

Each is the split the rest of the tree uses -- the syscalls in assembly, the
constants and the shape in Python -- so what is worth testing here is the
Python half and the boundary between them, not the kernel's behaviour.

`code._varname_from_oparg` rides along because it is the same kind of gap: a
name four modules read that was simply absent.
"""

import os
import struct
import sys


def test_fcntl_descriptor_flags():
    import fcntl
    fd = os.open("/etc/hostname", os.O_RDONLY)
    try:
        flags = fcntl.fcntl(fd, fcntl.F_GETFD)
        fcntl.fcntl(fd, fcntl.F_SETFD, flags | fcntl.FD_CLOEXEC)
        assert fcntl.fcntl(fd, fcntl.F_GETFD) & fcntl.FD_CLOEXEC
        assert fcntl.fcntl(fd, fcntl.F_GETFL) & os.O_ACCMODE == os.O_RDONLY
        # A struct argument comes back as bytes of the same length: F_GETLK
        # fills in the `struct flock` it was handed.
        packed = struct.pack("hhqqi", fcntl.F_WRLCK, 0, 0, 0, 0)
        answer = fcntl.fcntl(fd, fcntl.F_GETLK, packed)
        assert isinstance(answer, bytes) and len(answer) == len(packed)
        assert struct.unpack("hhqqi", answer)[0] == fcntl.F_UNLCK
        # Anything past the buffer is refused rather than truncated.
        try:
            fcntl.fcntl(fd, fcntl.F_GETLK, b"x" * 2000)
        except ValueError as e:
            assert str(e) == "fcntl string arg too long", e
        else:
            raise AssertionError("no ValueError")
    finally:
        os.close(fd)


def test_fcntl_takes_anything_with_a_fileno():
    import fcntl
    f = open("/etc/hostname", "rb")
    try:
        assert fcntl.fcntl(f, fcntl.F_GETFD) == fcntl.fcntl(f.fileno(),
                                                            fcntl.F_GETFD)
    finally:
        f.close()


def test_flock_and_lockf():
    import fcntl
    path = "/tmp/apython-test-lock-%d" % os.getpid()
    f = open(path, "wb")
    try:
        fcntl.flock(f, fcntl.LOCK_EX | fcntl.LOCK_NB)
        fcntl.flock(f, fcntl.LOCK_UN)
        # lockf is the other family: fcntl with a packed struct flock.
        fcntl.lockf(f, fcntl.LOCK_EX | fcntl.LOCK_NB)
        fcntl.lockf(f, fcntl.LOCK_UN)
        try:
            fcntl.lockf(f, 0)
        except ValueError as e:
            assert str(e) == "unrecognized lockf argument", e
        else:
            raise AssertionError("no ValueError")
    finally:
        f.close()
        os.unlink(path)


def test_resource_limits():
    import resource
    soft, hard = resource.getrlimit(resource.RLIMIT_NOFILE)
    assert isinstance(soft, int) and isinstance(hard, int)
    # Setting what is already set has to be a no-op, not a refusal.
    resource.setrlimit(resource.RLIMIT_NOFILE, (soft, hard))
    assert resource.getrlimit(resource.RLIMIT_NOFILE) == (soft, hard)
    # prlimit is the call the other two are built on, and it answers what the
    # limit WAS.
    assert resource.prlimit(0, resource.RLIMIT_NOFILE) == (soft, hard)
    # CPython's two refusals, both ValueError.
    # CPython's three refusals, which are not one exception type.
    try:
        resource.setrlimit(resource.RLIMIT_NOFILE, 5)
    except TypeError:
        pass
    else:
        raise AssertionError("no TypeError")
    try:
        resource.setrlimit(resource.RLIMIT_NOFILE, (1,))
    except ValueError as e:
        assert str(e) == "expected a tuple of 2 integers", e
    else:
        raise AssertionError("no ValueError")
    try:
        resource.setrlimit(resource.RLIMIT_NOFILE, (1.5, 2))
    except TypeError as e:
        assert "float" in str(e), e
    else:
        raise AssertionError("no TypeError")
    if hard != resource.RLIM_INFINITY:
        try:
            resource.setrlimit(resource.RLIMIT_NOFILE, (hard + 1000000, hard))
        except ValueError:
            pass
        else:
            raise AssertionError("no ValueError for soft > hard")


def test_resource_usage():
    import resource
    u = resource.getrusage(resource.RUSAGE_SELF)
    assert len(u) == 16
    assert isinstance(u.ru_utime, float) and isinstance(u.ru_stime, float)
    assert u.ru_utime >= 0.0
    assert u[2] is u.ru_maxrss
    assert isinstance(u.ru_maxrss, int) and u.ru_maxrss > 0
    assert "ru_maxrss=" in repr(u)
    try:
        resource.getrusage(17)
    except ValueError as e:
        assert str(e) == "invalid who parameter", e
    else:
        raise AssertionError("no ValueError")


def test_syslog_arithmetic():
    import syslog
    assert syslog.LOG_MASK(syslog.LOG_ERR) == 1 << syslog.LOG_ERR
    assert syslog.LOG_UPTO(syslog.LOG_ERR) == 0b1111
    old = syslog.setlogmask(syslog.LOG_UPTO(syslog.LOG_ERR))
    try:
        # A zero mask is a QUERY, not "mask nothing".
        assert syslog.setlogmask(0) == syslog.LOG_UPTO(syslog.LOG_ERR)
    finally:
        syslog.setlogmask(old)
    # Delivery is best-effort -- no /dev/log is not an error -- so what is
    # asserted is that a call returns rather than what arrives.
    syslog.openlog("apython-test", syslog.LOG_PID, syslog.LOG_LOCAL1)
    assert syslog.syslog(syslog.LOG_INFO, "test message") is None
    assert syslog.syslog("default priority") is None
    syslog.closelog()
    for bad in (lambda: syslog.openlog(1),
                lambda: syslog.syslog(syslog.LOG_INFO, 5)):
        try:
            bad()
        except TypeError:
            pass
        else:
            raise AssertionError("no TypeError")


def test_lsprof_counts_calls():
    import _lsprof

    def fib(n):
        return n if n < 2 else fib(n - 1) + fib(n - 2)

    p = _lsprof.Profiler()
    p.enable()
    fib(10)
    len([1, 2, 3])
    p.disable()
    rows = {}
    for entry in p.getstats():
        key = entry.code if isinstance(entry.code, str) else entry.code.co_name
        rows[key] = entry
    assert "fib" in rows, sorted(rows)
    # fib(10) is 177 calls, all but the outermost recursive.
    assert rows["fib"].callcount == 177, rows["fib"].callcount
    assert rows["fib"].reccallcount == 176, rows["fib"].reccallcount
    assert rows["fib"].totaltime >= rows["fib"].inlinetime
    # A builtin is reported by the description CPython's normalizeUserObj
    # builds, not by its repr.
    assert "<built-in method builtins.len>" in rows, sorted(rows)
    assert rows["<built-in method builtins.len>"].callcount == 1
    p.clear()
    assert p.getstats() == []


def test_varname_from_oparg():
    def f(a, b=1):
        c = a
        return c
    names = [f.__code__._varname_from_oparg(i) for i in range(3)]
    assert names == ["a", "b", "c"], names

    def g(a):
        def inner():
            return a                    # `a` is a CELL, not a plain local
        return inner
    # The oparg indexes co_localsplusnames, which is locals, then cells, then
    # free variables -- so a cell is reachable by the same number and a plain
    # co_varnames lookup would miss it.
    assert g.__code__._varname_from_oparg(0) == "a"
    assert g(1).__code__._varname_from_oparg(0) == "a"
    try:
        f.__code__._varname_from_oparg(99)
    except IndexError:
        pass
    else:
        raise AssertionError("no IndexError")
    try:
        f.__code__._varname_from_oparg("x")
    except TypeError:
        pass
    else:
        raise AssertionError("no TypeError")


for fn in (test_fcntl_descriptor_flags,
           test_fcntl_takes_anything_with_a_fileno,
           test_flock_and_lockf,
           test_resource_limits,
           test_resource_usage,
           test_syslog_arithmetic,
           test_lsprof_counts_calls,
           test_varname_from_oparg):
    fn()
    print(fn.__name__, 'ok')
print('OK')
