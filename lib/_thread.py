"""_thread - the single-threaded stand-in apython ships.

CPython's _thread is a C module.  apython has no threads, so the pieces the
stdlib actually reaches for at import time -- get_ident, allocate_lock, RLock
-- are provided here with the only behaviour a single thread can observe: the
identity is constant and the locks are uncontended.  Anything that would
actually start a thread raises, rather than pretending to.
"""

TIMEOUT_MAX = 10000000.0


class error(RuntimeError):
    """Raised on thread-specific errors.  _thread.error is RuntimeError in 3.x."""


def get_ident():
    """The one thread there is.  CPython guarantees only that it is a nonzero int."""
    return 1


def get_native_id():
    return 1


def _count():
    return 0


def stack_size(size=None):
    return 0


def interrupt_main(signum=2):
    raise KeyboardInterrupt


def start_new_thread(function, args, kwargs=None):
    raise error("apython does not support threads")


start_joinable_thread = start_new_thread


class LockType:
    """An uncontended lock.  With one thread, acquire always succeeds."""

    __slots__ = ("_locked",)

    def __init__(self):
        self._locked = False

    def acquire(self, blocking=True, timeout=-1):
        if self._locked:
            # A single thread that blocks on its own lock would never wake.
            if blocking:
                raise error("deadlock: lock already held by this thread")
            return False
        self._locked = True
        return True

    __enter__ = acquire

    def release(self):
        if not self._locked:
            raise error("release unlocked lock")
        self._locked = False

    def __exit__(self, t, v, tb):
        self.release()

    def locked(self):
        return self._locked

    def acquire_lock(self, blocking=True, timeout=-1):
        return self.acquire(blocking, timeout)

    def release_lock(self):
        self.release()

    def locked_lock(self):
        return self._locked

    def __repr__(self):
        state = "locked" if self._locked else "unlocked"
        return "<_thread.lock %s>" % state


class RLock:
    """A reentrant lock: the same thread may acquire it repeatedly."""

    __slots__ = ("_count",)

    def __init__(self):
        self._count = 0

    def acquire(self, blocking=True, timeout=-1):
        self._count += 1
        return True

    __enter__ = acquire

    def release(self):
        if self._count == 0:
            raise RuntimeError("cannot release un-acquired lock")
        self._count -= 1

    def __exit__(self, t, v, tb):
        self.release()

    def _is_owned(self):
        return self._count > 0

    def __repr__(self):
        return "<_thread.RLock count=%d>" % self._count


def allocate_lock():
    return LockType()


allocate = allocate_lock


class _local:
    """thread._local, which with one thread is an ordinary attribute holder."""


local = _local
