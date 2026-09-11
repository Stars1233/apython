# A Python signal handler has to run while a blocking read is blocked.
#
# Handlers were installed with SA_RESTART, so an interrupted read was
# restarted by the KERNEL and never returned -- and the Python handler could
# not run until the read finished.  A program whose handler is what UNBLOCKS
# the read therefore waited for ever: `signal.alarm(1)` during `f.read(6)`,
# with a handler that writes the rest of the data, is exactly that shape, and
# it is CPython's own test_io.check_interrupted_read_retry.  Two of CPython's
# test modules hung on it rather than failing.
#
# CPython passes sa_flags = 0 and does the retry PEP 475 promises in software.
# So does this now: the syscall funnels retry EINTR themselves, and the file
# object's read and write run the pending handler in between.

import os
import signal

r, w = os.pipe()
ran = []


def alarm_handler(sig, frame):
    ran.append(sig)
    os.write(w, b"bar")


old = signal.signal(signal.SIGALRM, alarm_handler)
rio = open(r, "rb", closefd=False)
os.write(w, b"foo")
signal.alarm(1)
data = rio.read(6)
signal.alarm(0)
print(data, ran == [signal.SIGALRM])
rio.close()
os.close(w)
os.close(r)

# A handler that RAISES comes out of the read rather than being swallowed.
r, w = os.pipe()


def raising_handler(sig, frame):
    raise KeyboardInterrupt("from the handler")


signal.signal(signal.SIGALRM, raising_handler)
rio = open(r, "rb", closefd=False)
os.write(w, b"foo")
signal.alarm(1)
try:
    rio.read(6)
    print("no raise")
except KeyboardInterrupt as e:
    print("raised:", e)
signal.alarm(0)
rio.close()
os.close(w)
os.close(r)

# ...and readall(), which is the other loop.
r, w = os.pipe()
ran = []


def closing_handler(sig, frame):
    ran.append(sig)
    os.write(w, b"bar")
    os.close(w)


signal.signal(signal.SIGALRM, closing_handler)
rio = open(r, "rb", closefd=False)
os.write(w, b"foo")
signal.alarm(1)
print(rio.read(), ran == [signal.SIGALRM])
signal.alarm(0)
rio.close()
os.close(r)

# os.read and os.write are the other pair: CPython's _pyio builds its FileIO
# on them, so its half of test_io hung for the same reason the C half did.
r, w = os.pipe()
ran = []


def writing_handler(sig, frame):
    ran.append(sig)
    os.write(w, b"bar")


signal.signal(signal.SIGALRM, writing_handler)
os.write(w, b"foo")
signal.alarm(1)
got = b""
while len(got) < 6:
    got += os.read(r, 6 - len(got))
signal.alarm(0)
print(got, ran == [signal.SIGALRM])
os.close(w)
os.close(r)

signal.signal(signal.SIGALRM, old)

# An ordinary read is unaffected.
path = "test_signal_eintr_tmp.txt"
with open(path, "wb") as f:
    f.write(b"hello")
with open(path, "rb") as f:
    print(f.read(), f.read())
os.unlink(path)

print("done")
