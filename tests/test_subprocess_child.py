# What a forked child does and does not inherit.
#
# _posixsubprocess.fork_exec takes twenty-two arguments and the ones it cannot
# honour matter more than the ones it can: a caller who passed `env=` to keep
# a secret from the child, or `user=` to drop privileges, must not be handed a
# child that quietly did neither.  So the environment is real, close_fds is
# real, and everything to do with credentials raises.
import _posixsubprocess
import posix

print("-- the environment is the caller's, not the parent's")
r, w = posix.pipe()
pid = posix.fork()
if pid == 0:
    posix.close(r)
    posix.dup2(w, 1)
    posix.close(w)
    posix.execve("/bin/sh", ["sh", "-c", "echo ${MARKER:-absent} ${SECRET:-absent}"],
                 {"MARKER": "given", "PATH": "/bin:/usr/bin"})
    posix._exit(1)
posix.close(w)
out = b""
while True:
    chunk = posix.read(r, 4096)
    if not chunk:
        break
    out += chunk
posix.close(r)
posix.waitpid(pid, 0)
print(out.decode().strip())

print()
print("-- close_range shuts what the child should not see")
keep_r, keep_w = posix.pipe()
r, w = posix.pipe()
pid = posix.fork()
if pid == 0:
    posix.close(r)
    posix.dup2(w, 1)
    posix.close(w)
    posix.closerange(3, 1024)
    posix.execv("/bin/sh",
                ["sh", "-c",
                 "if [ -e /proc/self/fd/%d ]; then echo LEAKED; else echo closed; fi"
                 % keep_r])
    posix._exit(1)
posix.close(w)
out = b""
while True:
    chunk = posix.read(r, 4096)
    if not chunk:
        break
    out += chunk
posix.close(r)
posix.waitpid(pid, 0)
posix.close(keep_r)
posix.close(keep_w)
print(out.decode().strip())

print()
print("-- and what cannot be honoured is refused, not dropped")
ORDER = ("args", "executable_list", "close_fds", "fds_to_keep", "cwd",
         "env_list", "p2cread", "p2cwrite", "c2pread", "c2pwrite", "errread",
         "errwrite", "errpipe_read", "errpipe_write", "restore_signals",
         "start_new_session", "process_group", "gid", "gids", "uid", "umask",
         "preexec_fn")
BASE = dict(args=["/bin/true"], executable_list=[b"/bin/true"],
            close_fds=False, fds_to_keep=(), cwd=None, env_list=None,
            p2cread=-1, p2cwrite=-1, c2pread=-1, c2pwrite=-1,
            errread=-1, errwrite=-1, errpipe_read=-1, errpipe_write=-1,
            restore_signals=0, start_new_session=0, process_group=-1,
            gid=None, gids=None, uid=None, umask=-1, preexec_fn=None)
for field, value in (("uid", 0), ("gid", 0), ("gids", [0]), ("umask", 0o22),
                     ("process_group", 0)):
    kw = dict(BASE)
    kw[field] = value
    try:
        # CPython raises for these too -- PermissionError from the setuid it
        # actually attempts, NotImplementedError here -- so only the fact of
        # a refusal is compared.
        _posixsubprocess.fork_exec(*[kw[n] for n in ORDER])
        print(field, "was ACCEPTED")
    except Exception:
        print(field, "refused")
