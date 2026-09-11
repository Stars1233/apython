# SIGPIPE is ignored at start-up, as CPython ignores it.
#
# Without that, a write to a closed pipe killed the interpreter where it
# stood: `apython foo.py | head` exited 141 with no message, and every test in
# CPython's test_subprocess that closes a pipe early took the whole process
# with it.  With it, the write fails with EPIPE and the program sees a
# BrokenPipeError it can catch.

import os
import signal

print("SIGPIPE:", signal.getsignal(signal.SIGPIPE) is signal.SIG_IGN)

r, w = os.pipe()
os.close(r)
try:
    os.write(w, b"x" * 100)
    print("wrote")
except BrokenPipeError as e:
    print("os.write:", e.errno, type(e).__name__)
os.close(w)

# ...and through a file object, which is the same errno one layer up.
r, w = os.pipe()
os.close(r)
f = open(w, "wb", closefd=False)
try:
    f.write(b"y" * 100)
    f.flush()
    print("wrote")
except BrokenPipeError as e:
    print("file.write:", e.errno, type(e).__name__)
os.close(w)

# BrokenPipeError is an OSError and a ConnectionError.
print(issubclass(BrokenPipeError, ConnectionError),
      issubclass(BrokenPipeError, OSError))

# The program is still running, which is the whole point.
print("alive")

# A program may still ask for the default back.
old = signal.signal(signal.SIGPIPE, signal.SIG_DFL)
print("restored:", old is signal.SIG_IGN)
signal.signal(signal.SIGPIPE, old)
print("re-ignored:", signal.getsignal(signal.SIGPIPE) is signal.SIG_IGN)

print("done")
