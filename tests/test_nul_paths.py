# An embedded NUL is refused, everywhere a string reaches a syscall.
#
# A C string ends at the first NUL; a Python str does not.  Handing one
# straight to a syscall therefore acts on a PREFIX of what the caller asked
# for, silently, and that is a classic way to turn a validated path into a
# different one.  CPython raises ValueError("embedded null byte") rather than
# truncating, and posix_path_arg does the same here -- it compares ap_strlen
# against ob_size, which is exactly why every PyBytesObject is
# NUL-terminated.
#
# Two paths bypassed that check and acted on the truncated string:
#
#   * open(), whose FileIO.__init__ handed [str + data] to sys_open with no
#     length check at all, so open("a\0b") opened "a".
#
#   * the exec family.  pxv_string_vector points argv INTO the strings
#     uncopied, and pev_append copies an environment entry up to its first
#     NUL -- so os.execv(p, ["sh", "-c", "x\0y"]) passed "x", and a NUL in an
#     environment KEY produced a malformed entry with no "=" in it at all.
#
# The exec one was not theoretical.  CPython's test_os.test_execve_invalid_env
# does exactly this with sys.executable, so the interpreter execed ITSELF --
# `apython -c pass` -- and exited 0 at test 70 of 344.  No traceback, no
# failure, no summary; test_os simply reported that it had run no tests.
import os
import sys

BAD = "a\0b"
BADB = b"a\0b"


def normalise(msg):
    """Collapse the one message CPython 3.12 changed mid-series.

    3.12.3 says "embedded null byte" for every path that reaches a syscall;
    3.12.14 says "stat: embedded null character in path", naming the function
    and the parameter.  The suite diffs against whichever python3 is installed
    -- 3.12.3 on this box, 3.12.14 in CI -- so the wording cannot be compared
    and the fact can.  Anything else is printed as it stands, which is what
    keeps "illegal environment variable name" pinned.
    """
    if "null" in msg.lower():
        return "embedded null"
    return msg


def check(label, fn):
    try:
        fn()
        print("%-30s NOT REFUSED" % label)
    except ValueError as e:
        print("%-30s ValueError: %s" % (label, normalise(str(e))))
    except TypeError as e:
        print("%-30s TypeError: %s" % (label, normalise(str(e))))


# The two that were missing.
check("open(str)", lambda: open("/etc/hostname\0x"))
check("open(bytes)", lambda: open(b"/etc/hostname\0x"))
check("os.execv path", lambda: os.execv("/bin/ec\0ho", ["echo"]))
check("os.execv argv", lambda: os.execv("/bin/echo", ["echo", BAD]))
check("os.execv argv0", lambda: os.execv("/bin/echo", [BAD]))
check("os.execve path", lambda: os.execve("/bin/ec\0ho", ["echo"], {}))
check("os.execve argv", lambda: os.execve("/bin/echo", ["echo", BAD], {}))
check("os.execve env key", lambda: os.execve("/bin/echo", ["echo"], {BAD: "v"}))
check("os.execve env value", lambda: os.execve("/bin/echo", ["echo"], {"K": BAD}))

# An '=' in the NAME is the same failure wearing a different byte: an entry
# is built as "<key>=<value>", so {"FRUIT=ORANGE": "lemon"} would set FRUIT
# to "ORANGE=lemon" rather than the variable that was asked for.
check("os.execve env '=' in key", lambda: os.execve("/bin/echo", ["echo"], {"A=B": "v"}))

# The ones that already refused, so the wording stays one family.
check("os.stat", lambda: os.stat("/etc/hostname\0x"))
check("os.open", lambda: os.open("/etc/hostname\0x", os.O_RDONLY))
check("os.listdir", lambda: os.listdir("/tmp\0x"))
check("os.mkdir", lambda: os.mkdir("/tmp/zz\0x"))
check("os.unlink", lambda: os.unlink("/tmp/zz\0x"))
check("os.chdir", lambda: os.chdir("/tmp\0x"))
check("os.rename", lambda: os.rename("/tmp/a\0b", "/tmp/c"))
check("os.putenv", lambda: os.putenv(BAD, "v"))

# bytes paths take the same route and are refused too.  Only the refusal is
# compared, for the reason normalise() above gives.
def refused(label, fn):
    try:
        fn()
        print("%-30s NOT REFUSED" % label)
    except ValueError:
        print("%-30s refused" % label)


refused("os.stat(bytes)", lambda: os.stat(b"/etc/hostname\0x"))
refused("os.listdir(bytes)", lambda: os.listdir(b"/tmp\0x"))

# A NUL anywhere in the argument, not only in the middle, and a NUL alone.
check("open leading NUL", lambda: open("\0/etc/hostname"))
check("open only a NUL", lambda: open("\0"))

# And the control: a path with no NUL still opens, which is what the check
# must not have broken.
with open("/etc/hostname") as f:
    print("control open:", len(f.read()) > 0)
print("control stat:", os.stat("/etc/hostname").st_size > 0)

# The process must still be this one -- if an exec had gone through above,
# nothing below would run at all.
print("still ourselves:", os.getpid() == os.getpid())
print("survived")
