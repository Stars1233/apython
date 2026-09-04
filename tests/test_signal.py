"""signal: installing a handler, and having it run.

A C signal handler cannot run Python -- it has no frame, no value stack, and
it interrupted something half-finished -- so what it does is record that the
signal arrived, and the eval loop runs the Python handler at the top of the
next loop iteration.  Everything below turns on that delivery actually
happening, which is why each case ends in a loop rather than a bare call.
"""

import signal
import os

print("--- the numbers and the sentinels ---")
print(signal.SIGINT, signal.SIGTERM, signal.SIGUSR1, signal.SIGUSR2)
print(repr(signal.SIGINT), repr(signal.SIG_DFL), repr(signal.SIG_IGN))
print(int(signal.SIGHUP), int(signal.SIGALRM), int(signal.SIGCHLD))
print(signal.NSIG, signal.SIG_DFL == 0, signal.SIG_IGN == 1)
print(signal.Signals.SIGINT is signal.SIGINT)
print(sorted(int(x) for x in signal.valid_signals())[:5])
print(len(signal.valid_signals()))

print("--- what is installed to begin with ---")
print(signal.getsignal(signal.SIGINT))
print(signal.getsignal(signal.SIGUSR1))
print(signal.getsignal(signal.SIGTERM))

print("--- installing one, and running it ---")
seen = []


def handler(signum, frame):
    seen.append(int(signum))


previous = signal.signal(signal.SIGUSR1, handler)
print("previous:", previous)
print("installed:", signal.getsignal(signal.SIGUSR1) is handler)

os.kill(os.getpid(), signal.SIGUSR1)
for _ in range(3):
    pass
print("delivered:", seen)

print("--- raise_signal reaches the same place ---")
seen.clear()
signal.raise_signal(signal.SIGUSR1)
for _ in range(3):
    pass
print("delivered:", seen)

print("--- two different signals ---")
seen.clear()
signal.signal(signal.SIGUSR2, handler)
signal.raise_signal(signal.SIGUSR1)
signal.raise_signal(signal.SIGUSR2)
for _ in range(3):
    pass
print("delivered:", sorted(seen))

print("--- SIG_IGN and SIG_DFL round trip ---")
print(signal.signal(signal.SIGUSR1, signal.SIG_IGN) is handler)
print(signal.getsignal(signal.SIGUSR1))
print(signal.signal(signal.SIGUSR1, signal.SIG_DFL))
print(signal.getsignal(signal.SIGUSR1))
seen.clear()
signal.signal(signal.SIGUSR1, signal.SIG_IGN)
signal.raise_signal(signal.SIGUSR1)
for _ in range(3):
    pass
print("ignored:", seen)

print("--- a handler that raises unwinds the loop it interrupted ---")


def raiser(signum, frame):
    raise RuntimeError("from a handler")


signal.signal(signal.SIGUSR1, raiser)
n = 0
try:
    while True:
        n += 1
        if n == 5:
            signal.raise_signal(signal.SIGUSR1)
        if n > 1000:
            break
except RuntimeError as e:
    print("RuntimeError", e, "after", n < 1000)

print("--- KeyboardInterrupt is what SIGINT does by default ---")
signal.signal(signal.SIGUSR1, signal.SIG_DFL)
try:
    m = 0
    while True:
        m += 1
        if m == 5:
            signal.raise_signal(signal.SIGINT)
        if m > 1000:
            break
except KeyboardInterrupt:
    print("KeyboardInterrupt caught")

print("--- and it can be replaced and put back ---")
old_int = signal.getsignal(signal.SIGINT)
signal.signal(signal.SIGINT, handler)
seen.clear()
signal.raise_signal(signal.SIGINT)
for _ in range(3):
    pass
print("delivered:", seen)
signal.signal(signal.SIGINT, old_int)
print(signal.getsignal(signal.SIGINT) is old_int)

print("--- what it refuses ---")
for num, hnd in ((signal.SIGKILL, handler), (signal.SIGSTOP, handler)):
    try:
        signal.signal(num, hnd)
    except OSError as e:
        print("OSError", e.errno)
for num in (0, -1, 1234, signal.NSIG):
    try:
        signal.signal(num, handler)
    except ValueError as e:
        print("ValueError", e)
for bad in ("nope", 2, 3.5, [], None):
    try:
        signal.signal(signal.SIGUSR1, bad)
    except TypeError as e:
        print("TypeError", e)
for num in (0, -1, 1234):
    try:
        signal.getsignal(num)
    except ValueError as e:
        print("ValueError", e)

print("--- strsignal ---")
print(signal.strsignal(signal.SIGINT))
print(signal.strsignal(signal.SIGTERM))
print(signal.strsignal(signal.SIGUSR1))

print("--- alarm returns what was left, and cancels ---")
print(signal.alarm(0))
signal.alarm(100)
left = signal.alarm(0)
print(0 < left <= 100)
print(signal.alarm(0))

signal.signal(signal.SIGUSR1, signal.SIG_DFL)
signal.signal(signal.SIGUSR2, signal.SIG_DFL)
print("done")
