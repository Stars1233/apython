# When a Python signal handler actually runs.
#
# A C signal handler cannot run Python -- it has no frame and no value stack,
# and it interrupted something half-finished -- so signal_trampoline sets a
# flag and the eval loop reads it.  It read it in exactly ONE place: the top
# of a loop.  CPython reads it in two, and the missing one is the one that
# matters:
#
#     os.kill(os.getpid(), signal.SIGINT)
#     print(...)                            # handler has NOT run
#
# A signal arrives while the process is inside a syscall, and the instruction
# that was running is the CALL that made it -- so CPython checks the eval
# breaker when a call returns.  Without that check the flag sat set until the
# program happened to reach a backward jump, and a program with no loop after
# the signal never delivered at all.  `signal.signal(SIGINT, h)` followed by
# Ctrl-C ran nothing.
#
# This is test_unittest.test_break, which hung on it.
import os
import signal

seen = []


def record(signum, frame):
    seen.append(signum)


signal.signal(signal.SIGINT, record)

# --- straight-line code after the call ---------------------------------
os.kill(os.getpid(), signal.SIGINT)
print("immediately after the call:", seen)

os.kill(os.getpid(), signal.SIGINT)
x = 1 + 1
print("after an arithmetic instruction:", len(seen))

os.kill(os.getpid(), signal.SIGINT)
for _ in range(2):
    pass
print("after a loop, which was the only place it worked:", len(seen))


def nothing():
    return 1


os.kill(os.getpid(), signal.SIGINT)
nothing()
print("after a Python call:", len(seen))

# --- inside a function, with no jump of any kind ------------------------
def probe():
    n = len(seen)
    os.kill(os.getpid(), signal.SIGINT)
    return len(seen) - n


print("delivered inside one frame with no jump:", probe())

# --- a handler that raises reaches the caller ---------------------------
def raiser(signum, frame):
    raise KeyboardInterrupt("from the handler")


signal.signal(signal.SIGINT, raiser)
try:
    os.kill(os.getpid(), signal.SIGINT)
    print("a raising handler: NOT RAISED")
except KeyboardInterrupt as exc:
    print("a raising handler:", exc)

# The exception is raised at the instruction that was interrupted, so an
# enclosing try sees it and a finally still runs.
order = []
try:
    try:
        os.kill(os.getpid(), signal.SIGINT)
    finally:
        order.append("finally")
except KeyboardInterrupt:
    order.append("except")
print("unwinds normally:", order)

# --- the default handler is KeyboardInterrupt --------------------------
signal.signal(signal.SIGINT, signal.default_int_handler)
try:
    os.kill(os.getpid(), signal.SIGINT)
    print("default_int_handler: NOT RAISED")
except KeyboardInterrupt:
    print("default_int_handler: KeyboardInterrupt")

# --- a handler installed over another, calling it ----------------------
# test_unittest.test_break's testHandlerReplacedButCalled, reduced.
calls = []
signal.signal(signal.SIGINT, lambda s, f: calls.append("first"))
first = signal.getsignal(signal.SIGINT)
signal.signal(signal.SIGINT, lambda s, f: (calls.append("second"), first(s, f)))
os.kill(os.getpid(), signal.SIGINT)
print("a handler delegating to the one it replaced:", calls)

# --- getsignal / SIG_IGN / SIG_DFL --------------------------------------
print("getsignal returns what was set:",
      signal.getsignal(signal.SIGINT) is not None)
signal.signal(signal.SIGINT, signal.SIG_IGN)
os.kill(os.getpid(), signal.SIGINT)
print("SIG_IGN drops it:", signal.getsignal(signal.SIGINT) is signal.SIG_IGN)

# --- a second signal, to show the flag is per-signal --------------------
usr = []
signal.signal(signal.SIGUSR1, lambda s, f: usr.append(s))
signal.signal(signal.SIGUSR2, lambda s, f: usr.append(s))
os.kill(os.getpid(), signal.SIGUSR1)
os.kill(os.getpid(), signal.SIGUSR2)
print("two different signals:", usr == [signal.SIGUSR1, signal.SIGUSR2], usr)

# The same signal twice before either handler runs is delivered once, which
# is CPython's rule and why the flag is a byte rather than a counter.  It
# cannot be shown without blocking the delivery point, so what is checked is
# the ordinary case: two kills with a delivery point between them run twice.
usr.clear()
os.kill(os.getpid(), signal.SIGUSR1)
os.kill(os.getpid(), signal.SIGUSR1)
print("twice with a delivery point between:", len(usr))

signal.signal(signal.SIGINT, signal.SIG_DFL)
print("survived")
