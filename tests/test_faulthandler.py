# faulthandler, which is a lib/ stand-in here rather than a C module.
#
# CPython's has to be C: its whole point is to report a crash, and by then
# calling into the interpreter is not safe.  This one is Python over the
# `signal` module, which reports faults a program causes DELIBERATELY and
# hangs that are not wedged inside a C loop, and is silent for a real
# SIGSEGV -- the eval loop that delivers a Python-level handler is never
# reached.  DIVERGENCES.md records that.
#
# What the tests below compare is the API and the report's FORMAT, both of
# which are CPython's exactly.  The thread id in the header is not: CPython
# prints a real pointer and this prints zero, because there is only ever one
# thread, so the header is normalised away.
import os
import signal
import sys

import faulthandler

# tempfile is not in the shipped lib/, and this needs a file only so that the
# report can be read back.  A pid-unique name under TMPDIR is enough.
_TMPDIR = os.environ.get("TMPDIR", "/tmp")
_counter = [0]


def mktemp():
    _counter[0] += 1
    return os.path.join(_TMPDIR, "apython-fh-%d-%d" % (os.getpid(), _counter[0]))


def captured(fn, *args, **kwargs):
    """Run fn with a file it writes into, and hand back the normalised text."""
    path = mktemp()
    with open(path, "w+") as fh:
        kwargs["file"] = fh
        fn(*args, **kwargs)
        fh.flush()
        fh.seek(0)
        text = fh.read()
    os.unlink(path)
    out = []
    for line in text.splitlines():
        if line.startswith("Current thread 0x"):
            line = "Current thread 0xTHREAD (most recent call first):"
        out.append(line)
    return out


# --- the surface -------------------------------------------------------------

for name in ("enable", "disable", "is_enabled", "dump_traceback",
             "register", "unregister", "dump_traceback_later",
             "cancel_dump_traceback_later"):
    print("%-28s %s" % (name, callable(getattr(faulthandler, name))))

# --- dump_traceback ----------------------------------------------------------

print()


def inner():
    return captured(faulthandler.dump_traceback)


def outer():
    return inner()


lines = outer()
print("header:", lines[0])
print("frames:", len(lines) - 1 >= 3)
print("names:", [l.rsplit(" in ", 1)[1] for l in lines[1:]][:3])
print("format:", lines[1].startswith('  File "') and '", line ' in lines[1])

# --- enable / disable / is_enabled -------------------------------------------

print()
print("initially:", faulthandler.is_enabled())
faulthandler.enable()
print("after enable:", faulthandler.is_enabled())
faulthandler.enable()                       # idempotent
print("twice:", faulthandler.is_enabled())
faulthandler.disable()
print("after disable:", faulthandler.is_enabled())
faulthandler.disable()                      # also idempotent
print("twice off:", faulthandler.is_enabled())

# enable() must not disturb a handler it does not own.
seen = []
signal.signal(signal.SIGUSR2, lambda s, f: seen.append(s))
faulthandler.enable()
os.kill(os.getpid(), signal.SIGUSR2)
for _ in range(100):
    pass
faulthandler.disable()
print("unrelated handler intact:", seen == [signal.SIGUSR2])
signal.signal(signal.SIGUSR2, signal.SIG_DFL)

# --- register / unregister ---------------------------------------------------

print()
path = mktemp()
with open(path, "w+") as fh:
    faulthandler.register(signal.SIGUSR1, file=fh)
    os.kill(os.getpid(), signal.SIGUSR1)
    for _ in range(100):
        pass
    fh.flush()
    fh.seek(0)
    text = fh.read()
os.unlink(path)
print("register dumped:", "most recent call first" in text)
print("register has no header line:", "Fatal Python error" not in text)
print("unregister known:", faulthandler.unregister(signal.SIGUSR1))
print("unregister unknown:", faulthandler.unregister(signal.SIGUSR2))

# After unregister the signal is the default again, so it must not dump.
signal.signal(signal.SIGUSR1, lambda s, f: seen.append("mine"))
os.kill(os.getpid(), signal.SIGUSR1)
for _ in range(100):
    pass
print("handler restored:", seen[-1] == "mine")
signal.signal(signal.SIGUSR1, signal.SIG_DFL)

# chain=True runs what was there before.
chained = []
signal.signal(signal.SIGUSR1, lambda s, f: chained.append(s))
path = mktemp()
with open(path, "w+") as fh:
    faulthandler.register(signal.SIGUSR1, file=fh, chain=True)
    os.kill(os.getpid(), signal.SIGUSR1)
    for _ in range(100):
        pass
os.unlink(path)
print("chained:", chained == [signal.SIGUSR1])
faulthandler.unregister(signal.SIGUSR1)
signal.signal(signal.SIGUSR1, signal.SIG_DFL)

# --- the watchdog ------------------------------------------------------------

print()
try:
    faulthandler.dump_traceback_later(0)
except ValueError:
    print("refuses a zero timeout")
try:
    faulthandler.dump_traceback_later(-1)
except ValueError:
    print("refuses a negative timeout")

# Armed and cancelled before it can fire: the point is that cancelling works,
# not that the timer does, since a test that waits for it would take seconds.
faulthandler.dump_traceback_later(30)
faulthandler.cancel_dump_traceback_later()
print("armed and cancelled")
faulthandler.cancel_dump_traceback_later()      # idempotent
print("cancel twice")

# It really does fire, which is the half a no-op stand-in would get wrong.
path = mktemp()
with open(path, "w+") as fh:
    faulthandler.dump_traceback_later(0.05, file=fh)
    deadline = 0
    while deadline < 40000000:
        deadline += 1
    faulthandler.cancel_dump_traceback_later()
    fh.flush()
    fh.seek(0)
    fired = fh.read()
os.unlink(path)
print("watchdog fired:", "most recent call first" in fired)
print("watchdog said Timeout:", fired.startswith("Timeout ("))

# --- an fd works where a file object does ------------------------------------

print()
path = mktemp()
fd = os.open(path, os.O_RDWR | os.O_CREAT)
faulthandler.dump_traceback(file=fd)
os.close(fd)
with open(path) as fh:
    by_fd = fh.read()
os.unlink(path)
print("raw fd accepted:", "most recent call first" in by_fd)

print("done")
