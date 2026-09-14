# The interpreter's own command line: -m, -, and the flags it ignores.
#
# apython took a file, -c, and the selftests, and nothing else.  `-m` in
# particular is how CPython's own test suite, pip, unittest, http.server and
# most modern tooling are invoked -- and it is how the package-style test
# directories in tests/pkg_probe.sh have to be driven, which is why that
# probe was reduced to spelling `unittest.main(module=None)` by hand.
#
# `m` was not even in the accept-and-ignore list, so `apython -m os` printed
# the usage error.
#
# Everything here runs the interpreter as a subprocess, because what is under
# test is what happens when it is HANDED arguments -- which a program running
# inside one cannot ask.
import posix
import sys

HERE = posix.getcwd() + "/tests"
ROOT = posix.getcwd()
# Everything runs from tests/, so the package root of the fixture is climod,
# which has an __init__.py.  `tests` itself has none: CPython treats it as a
# PEP 420 namespace package and walks it, and this interpreter does not --
# which is a separate gap and not what is under test here.


def _drain(fd):
    out = b""
    while True:
        chunk = posix.read(fd, 65536)
        if not chunk:
            return out
        out += chunk


def run(*args, stdin=None, cwd=HERE):
    """fork, exec, and collect both streams.

    `subprocess` is not used because it does not ship in lib/ -- it is
    CPython's own Python module, and make check runs without a real stdlib on
    the path.  posix.fork/execv/waitpid are in both interpreters, and this is
    the shape subprocess itself is built on.
    """
    op, oc = posix.pipe()
    ep, ec = posix.pipe()
    ip, ic = posix.pipe()
    pid = posix.fork()
    if pid == 0:
        posix.dup2(ip, 0)
        posix.dup2(oc, 1)
        posix.dup2(ec, 2)
        for fd in (op, oc, ep, ec, ip, ic):
            try:
                posix.close(fd)
            except OSError:
                pass
        try:
            posix.chdir(cwd)
            posix.execv(sys.executable, [sys.executable] + list(args))
        except BaseException:
            pass
        posix._exit(127)
    posix.close(oc)
    posix.close(ec)
    posix.close(ip)
    if stdin:
        posix.write(ic, stdin)
    posix.close(ic)
    out = _drain(op)
    err = _drain(ep)
    posix.close(op)
    posix.close(ep)
    _, status = posix.waitpid(pid, 0)
    rc = status >> 8 if (status & 0xFF) == 0 else -(status & 0x7F)
    return (rc, out.decode().replace("\r\n", "\n"),
            err.decode().replace("\r\n", "\n"))


# --- -c, the one that already worked -----------------------------------
rc, out, err = run("-c", "import sys; print('c:', sys.argv)")
print(rc, out.strip())

# --- -m over a plain module inside a package ---------------------------
rc, out, err = run("-m", "climod.plain", "one", "two")
print("rc", rc)
print(out, end="")

# --- -m over a package, which runs its __main__ ------------------------
rc, out, err = run("-m", "climod", "three")
print("rc", rc)
print(out, end="")

# --- -m over a top-level module ----------------------------------------
rc, out, err = run("-m", "plain", cwd=HERE + "/climod")
print("rc", rc)
print(out, end="")

# --- the two failures, which are worded differently --------------------
rc, out, err = run("-m", "climod.sub")
# The message, not the exception name: CPython's runpy catches it and prints
# a one-line "<exe>: No module named ..." where this lets the ImportError
# reach the traceback.
print("no __main__:", rc, "cannot be directly executed" in err,
      "climod.sub.__main__" in err)
rc, out, err = run("-m", "climod.definitely_not_there")
print("no module:", rc, "ImportError" in err or "No module" in err)
rc, out, err = run("-m")
print("-m with nothing:", rc, "usage" in err.lower())

# --- a program on standard input ---------------------------------------
rc, out, err = run("-", stdin=b"import sys\nprint('stdin:', sys.argv)\n")
print(rc, out.strip())
rc, out, err = run("-", "x", "y", stdin=b"import sys\nprint(sys.argv)\n")
print(rc, out.strip())
# The filename a traceback shows is CPython's.
rc, out, err = run("-", stdin=b"x = 1\nraise ValueError('boom')\n")
print("stdin traceback:", rc, '"<stdin>", line 2' in err, "boom" in err)
# Empty input is an empty program, not an error.
rc, out, err = run("-", stdin=b"")
print("empty stdin:", rc, out == "", err == "")

# --- the flags that are accepted and ignored ---------------------------
for flags in (["-E"], ["-I"], ["-S"], ["-B"], ["-u"], ["-b"], ["-d"], ["-q"],
              ["-v"], ["-O"], ["-OO"], ["-P"], ["-R"], ["-bb"],
              ["-E", "-S"], ["-X", "faulthandler"], ["-W", "ignore"],
              ["-I", "-X", "dev"]):
    rc, out, err = run(*(flags + ["-c", "print('ok')"]))
    print("%-24s %s %s" % (" ".join(flags), rc, out.strip()))

# --- and one that is not -----------------------------------------------
rc, out, err = run("-z", "-c", "print('no')")
print("unknown flag:", rc != 0, "usage" in err.lower())

# --- a file still works, and so does a nonexistent one -----------------
script = HERE + "/climod/plain.py"
rc, out, err = run(script, "a")
print("as a file:", rc, "plain module" in out)
rc, out, err = run(HERE + "/no_such_file_here.py")
print("missing file:", rc != 0)
print("survived")
