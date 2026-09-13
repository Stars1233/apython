# print() to a broken pipe raises, rather than discarding the output.
#
# builtin_print checks every write -- all eight call sites test
# print_sink_write's answer -- but print_sink_write's fast arm for the start-up
# streams called fileobj_emit and threw the result away.  sys.stdout IS a
# file_type object, so the DEFAULT case was the unchecked one: `apython x.py |
# head` exited 0 with the tail of its output silently gone.  The generic arm,
# for a Python-level sys.stdout, has propagated all along.
#
# The probe has to make fd 1 ITSELF the dead end, which rules out the two easy
# tests: assigning sys.stdout an open() file routes the write through the arm
# that already worked, and redirecting fd 1 in this process leaves the buffered
# bytes to be flushed to the RESTORED fd at exit.  So: a fork, with the verdict
# coming back on a pipe of its own.

import os


def probe(body, catch):
    """Run body() in a child whose fd 1 is a pipe with no reader."""
    vr, vw = os.pipe()
    pr, pw = os.pipe()
    pid = os.fork()
    if pid == 0:
        os.close(vr)
        os.close(pr)
        os.dup2(pw, 1)
        os.close(pw)
        try:
            body()
            verdict = b"no error"
        except BrokenPipeError as e:
            verdict = b"BrokenPipeError errno=%d" % e.errno
        except BaseException as e:
            verdict = (type(e).__name__ + ": " + str(e)).encode()
        try:
            os.write(vw, verdict)
        except OSError:
            pass
        os._exit(0 if catch else 7)
    os.close(vw)
    os.close(pw)
    os.close(pr)            # the reader is gone: every write to fd 1 is EPIPE
    out = b""
    while True:
        chunk = os.read(vr, 4096)
        if not chunk:
            break
        out += chunk
    os.close(vr)
    _, status = os.waitpid(pid, 0)
    code = os.WEXITSTATUS(status) if os.WIFEXITED(status) else -1
    return out.decode(), code


def many_prints():
    for i in range(200000):
        print(i)


def one_flush():
    print("x")
    import sys
    sys.stdout.flush()


def print_with_flush():
    print("x", flush=True)


def stdout_write():
    import sys
    for i in range(200000):
        sys.stdout.write("%d\n" % i)


def stdout_write_then_flush():
    import sys
    sys.stdout.write("x\n")
    sys.stdout.flush()


def writelines():
    import sys
    sys.stdout.writelines("%d\n" % i for i in range(200000))


for name, body in (("print loop", many_prints),
                   ("print+flush()", one_flush),
                   ("print(flush=True)", print_with_flush),
                   ("stdout.write loop", stdout_write),
                   ("stdout.write+flush", stdout_write_then_flush),
                   ("writelines", writelines)):
    verdict, code = probe(body, catch=True)
    print("%-22s %s" % (name, verdict))

# The child's exit status comes back, which the exit-time case below needs.
verdict, code = probe(many_prints, catch=False)
print("status propagates:", code)


# A print too small to leave the buffer is a failure no Python can see: the
# write happens in the exit-time flush, which must NOT raise -- two of its
# callers are inside the traceback printer.  CPython reports it through the
# unraisable hook and exits 120, overriding whatever status was asked for.
def exit_time(asked):
    pr, pw = os.pipe()
    pid = os.fork()
    if pid == 0:
        os.close(pr)
        os.dup2(pw, 1)
        os.close(pw)
        # stderr would carry the "Exception ignored" report, whose first line
        # names sys.stdout and so differs between the two interpreters.
        devnull = os.open("/dev/null", os.O_WRONLY)
        os.dup2(devnull, 2)
        print("x")
        if asked is not None:
            import sys
            sys.exit(asked)
    os.close(pw)
    os.close(pr)
    _, status = os.waitpid(pid, 0)
    return os.WEXITSTATUS(status) if os.WIFEXITED(status) else -1


print("exit-time flush:", exit_time(None))
print("exit-time overrides sys.exit(3):", exit_time(3))

# A write to a LIVE stdout still answers the way it always did.
import sys

print("alive:", sys.stdout.write("") == 0, sys.stdout.flush() is None)

print("done")
