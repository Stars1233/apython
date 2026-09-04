# fork, execv, _exit, kill and the fork hooks.
#
# `subprocess` is built on exactly these, through _posixsubprocess.fork_exec,
# and seven of CPython's own Lib/ modules stop at that import.  None of them
# is more than a syscall wrapper; what they needed was to exist.
#
# The child's side of every case below ends in _exit rather than falling off
# the end, because a forked child that unwinds would run the parent's atexit
# handlers and flush buffers the parent still owns.
import os
import posix


def run_child(body):
    """Fork, run `body` in the child, and answer what it wrote and its status."""
    r, w = posix.pipe()
    pid = posix.fork()
    if pid == 0:
        posix.close(r)
        try:
            body(w)
        finally:
            posix._exit(0)
    posix.close(w)
    out = b""
    while True:
        chunk = posix.read(r, 4096)
        if not chunk:
            break
        out += chunk
    posix.close(r)
    return out, posix.waitpid(pid, 0)[1]


print("-- fork sees a different pid on each side")
mine = posix.getpid()
out, status = run_child(lambda w: posix.write(w, str(posix.getpid()).encode()))
print("child reported its own pid:", out.decode() != str(mine), "status:", status)

print()
print("-- execv replaces the child")
out, status = run_child(lambda w: (posix.dup2(w, 1),
                                   posix.execv("/bin/echo", ["echo", "execed"])))
print(out.decode().strip(), "status:", status)

print()
print("-- _exit's code reaches waitpid")
r, w = posix.pipe()
pid = posix.fork()
if pid == 0:
    posix.close(r)
    posix.close(w)
    posix._exit(7)
posix.close(r)
posix.close(w)
st = posix.waitpid(pid, 0)[1]
print("exited:", os.WIFEXITED(st), "code:", os.WEXITSTATUS(st))

print()
print("-- kill, and the status that reports it")
pid = posix.fork()
if pid == 0:
    while True:
        pass
posix.kill(pid, 9)
st = posix.waitpid(pid, 0)[1]
print("signalled:", os.WIFSIGNALED(st), "signal:", os.WTERMSIG(st))

print()
print("-- the fork hooks, in CPython's order")
seen = []
os.register_at_fork(before=lambda: seen.append("before1"))
os.register_at_fork(before=lambda: seen.append("before2"))
os.register_at_fork(after_in_parent=lambda: seen.append("parent"))
pid = posix.fork()
if pid == 0:
    posix._exit(0)
posix.waitpid(pid, 0)
print(seen)
try:
    os.register_at_fork()
except TypeError as e:
    print("TypeError:", e)
