"""_posixsubprocess - the fork-and-exec `subprocess` is built on.

CPython's is C for a reason that does not apply here: between fork and exec a
process may call almost nothing, because any lock another thread was holding
is held forever in the child.  There is one thread in this interpreter, so
there is no such lock -- but the discipline is kept anyway, and everything
this does after the fork is a syscall.

`subprocess` calls one function, `fork_exec`, and reads the result the same
way CPython's does: the child writes an exception to the error pipe and
_exits, the parent reads it and re-raises.

The arguments that are NOT honoured raise rather than being ignored.  That is
the whole rule here: a caller who asked to drop privileges, or to run with a
scrubbed environment, must not be handed a child that quietly did neither.
"""

import posix

# What `subprocess` may ask for that this interpreter cannot do.  Each is
# refused by name rather than dropped: silently running a child as root when
# `user=` said otherwise, or with the parent's environment when `env=` said
# otherwise, is worse than not running it at all.
_UNSUPPORTED = (
    ("uid", "user="),
    ("gid", "group="),
    ("gids", "extra_groups="),
    ("umask", "umask="),
    ("process_group", "process_group="),
)


def _write_error(errpipe_write, exc_type, errno_num, what):
    """The child's half of the error protocol.

    CPython's format is `<class>:<hex errno>:<message>` on one line, and
    subprocess.py splits on the colons and reads the middle field with
    `int(hex_errno, 16)`.  Writing it in decimal is not a smaller mistake
    than writing the wrong number: EACCES, 13, came back to the caller as 19,
    which is ENODEV.  Anything that goes wrong writing it is dropped: the
    child is about to _exit, and the parent will report a bare failure rather
    than nothing.
    """
    try:
        msg = ("%s:%x:%s" % (exc_type, errno_num, what)).encode("utf-8",
                                                                "replace")
        posix.write(errpipe_write, msg)
    except Exception:
        pass


def _close_inherited(fds_to_keep):
    """Shut every descriptor above 2 that is not on the keep list.

    `subprocess` asks for this by DEFAULT, and without it every open file,
    socket and pipe in the parent is inherited by the child -- which is the
    thing close_fds exists to prevent.  close_range takes an inclusive span,
    so the keep list is walked in order and the gaps between are closed.
    """
    keep = sorted(set(int(fd) for fd in (fds_to_keep or ())) | {0, 1, 2})
    low = 3
    for fd in keep:
        if fd >= low:
            if fd > low:
                posix.closerange(low, fd)
            low = fd + 1
    posix.closerange(low, 0x7FFFFFFF)


def fork_exec(args, executable_list, close_fds, fds_to_keep, cwd, env_list,
              p2cread, p2cwrite, c2pread, c2pwrite, errread, errwrite,
              errpipe_read, errpipe_write, restore_signals, start_new_session,
              process_group, gid, gids, uid, umask, preexec_fn,
              allow_vfork=False):
    """Fork, wire the three pipes up, and exec.  Answers the child's pid.

    The arguments are CPython's, in CPython's order, because subprocess.py
    passes them positionally.
    """
    if args is None:
        raise ValueError("no args to exec")
    for value, spelling in _UNSUPPORTED:
        got = locals()[value]
        if got is not None and got != -1:
            raise NotImplementedError(
                "%s is not supported: this interpreter has no way to change "
                "the child's credentials, and running it without the change "
                "would not be what was asked for" % (spelling,))
    argv = [a if isinstance(a, bytes) else _fsencode(a) for a in args]
    envv = None
    if env_list is not None:
        envv = [e.decode("utf-8", "surrogateescape") if isinstance(e, bytes)
                else e for e in env_list]

    pid = posix.fork()
    if pid != 0:
        return pid

    # ---- the child ----------------------------------------------------
    try:
        # The three standard descriptors, in CPython's order: each is dup2'd
        # into place and the original closed, and a -1 means "leave it".
        for src, dst in ((p2cread, 0), (c2pwrite, 1), (errwrite, 2)):
            if src is not None and src != -1 and src != dst:
                posix.dup2(src, dst)

        if close_fds:
            _close_inherited(tuple(fds_to_keep or ()) + (errpipe_write,))
        else:
            for fd in (p2cread, p2cwrite, c2pread, c2pwrite, errread, errwrite):
                if fd is not None and fd != -1 and fd > 2 and fd != errpipe_write:
                    try:
                        posix.close(fd)
                    except OSError:
                        pass

        if start_new_session:
            posix.setsid()
        if cwd is not None:
            posix.chdir(cwd if isinstance(cwd, str) else cwd.decode("utf-8"))

        # restore_signals is honoured by having nothing to restore: this
        # interpreter installs no handlers, so a child starts with the
        # dispositions it inherited, which is what the flag asks for.
        if preexec_fn is not None:
            preexec_fn()

        last = OSError(2, "No such file or directory")
        for exe in (executable_list or ()):
            path = exe if isinstance(exe, str) else exe.decode("utf-8",
                                                              "surrogateescape")
            names = [a.decode("utf-8", "surrogateescape") for a in argv]
            try:
                if envv is None:
                    posix.execv(path, names)
                else:
                    posix.execve(path, names, envv)
            except OSError as e:
                last = e
        raise last
    except BaseException as e:
        errno_num = getattr(e, "errno", 0) or 0
        _write_error(errpipe_write, type(e).__name__, errno_num, str(e))
    posix._exit(255)


def _fsencode(s):
    if isinstance(s, bytes):
        return s
    return s.encode("utf-8", "surrogateescape")
