# SIG_IGN SURVIVES execve -- only installed handlers are reset by it -- and
# this interpreter ignores SIGPIPE at start-up the way CPython does.  So every
# child inherited a SIGPIPE it could not receive: `yes | head -1` never ended,
# and any program in a pipeline that relies on being killed by it ran for ever.
#
# CPython's fork_exec resets SIGPIPE, SIGXFZ and SIGXFSZ to SIG_DFL in the
# child when restore_signals is true, which is Popen's default.  That is what
# the flag means; this tree documented it as having nothing to restore.
#
# _posixsubprocess.fork_exec is the level tested here, because `subprocess`
# itself is CPython's and not on the path of an in-tree run -- and CPython's
# own _posixsubprocess is a C module whose signature this one only matches
# where subprocess.py calls it, so there is no CPython to diff against.  The
# expected output is recorded in tests/expected/, and every step below
# asserts, so the recording is a transcript of a verified run.

import os
import signal

import _posixsubprocess

SIGPIPE_BIT = 1 << (signal.SIGPIPE - 1)


def child_sigign(restore):
    r, w = os.pipe()
    er, ew = os.pipe()
    pid = _posixsubprocess.fork_exec(
        [b"sh", b"-c", b"grep SigIgn /proc/self/status"],
        [b"/bin/sh"], True, (w, ew), None, None,
        -1, -1, -1, w, -1, -1,
        er, ew, restore, False,
        -1, -1, None, -1, -1, None)
    os.close(w)
    os.close(ew)
    out = b""
    while True:
        chunk = os.read(r, 4096)
        if not chunk:
            break
        out += chunk
    os.close(r)
    err = b""
    while True:
        chunk = os.read(er, 4096)
        if not chunk:
            break
        err += chunk
    os.close(er)
    os.waitpid(pid, 0)
    if err:
        return "error: " + err.decode()[:60]
    return int(out.decode().split()[1], 16)


ign = child_sigign(True)
assert isinstance(ign, int), ign
assert ign & SIGPIPE_BIT == 0, "child still ignores SIGPIPE: %x" % ign
print("restore_signals=True clears SIGPIPE")

ign = child_sigign(False)
assert isinstance(ign, int), ign
assert ign & SIGPIPE_BIT != 0, "restore_signals=False should keep it: %x" % ign
print("restore_signals=False keeps it")

# The parent still ignores it: the reset happens in the CHILD.
assert signal.getsignal(signal.SIGPIPE) is signal.SIG_IGN
print("parent still ignores SIGPIPE")

# ...and the parent's own write to a closed pipe still raises rather than
# killing it.
rd, wr = os.pipe()
os.close(rd)
try:
    os.write(wr, b"x" * 100)
    raise AssertionError("write to a closed pipe did not raise")
except BrokenPipeError as e:
    assert e.errno == 32, e.errno
    print("parent write raises BrokenPipeError")
os.close(wr)

print("done")
