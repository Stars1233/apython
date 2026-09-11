# poll() has to RETURN on EINTR, run the Python handler, and wait out what is
# LEFT of its timeout.
#
# Linux never restarts poll() whatever SA_RESTART says -- the timeout is
# absolute to the call -- so retrying it inside the syscall funnel is not what
# the flag used to do: it re-issues the wait with the WHOLE timeout again and
# runs no handler at all.  A `poll(600000)` under a repeating timer then never
# returned, and Ctrl-C during one was swallowed.

import os
import select
import signal
import time

r, w = os.pipe()
p = select.poll()
p.register(r, select.POLLIN)

# A handler runs, and the total wait stays near the original timeout rather
# than being restarted by each signal.
ran = []


def h(sig, frame):
    ran.append(sig)


old = signal.signal(signal.SIGALRM, h)
signal.setitimer(signal.ITIMER_REAL, 0.1, 0.1)
t = time.time()
res = p.poll(700)
signal.setitimer(signal.ITIMER_REAL, 0)
elapsed = time.time() - t
print(res, 0.6 <= elapsed < 3.0, len(ran) >= 3)

# A handler that RAISES comes out of the poll rather than being swallowed.
def boom(sig, frame):
    raise KeyboardInterrupt("during poll")


signal.signal(signal.SIGALRM, boom)
signal.alarm(1)
t = time.time()
try:
    p.poll(30000)
    print("no raise")
except KeyboardInterrupt as e:
    print("raised:", e, time.time() - t < 5)
signal.alarm(0)
signal.signal(signal.SIGALRM, old)

# A ready descriptor ends an infinite wait.
os.write(w, b"x")
print(p.poll(-1) != [])
print(os.read(r, 1))

# A zero timeout returns at once.
t = time.time()
p.poll(0)
print(time.time() - t < 0.5)

os.close(w)
os.close(r)

# select() over the same machinery.
r, w = os.pipe()
os.write(w, b"y")
print(select.select([r], [], [], 1.0)[0] == [r])
print(select.select([], [], [], 0.0))
os.close(w)
os.close(r)

print("done")
