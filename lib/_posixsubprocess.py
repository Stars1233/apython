"""_posixsubprocess - the fork-and-exec `subprocess` is built on.

CPython's is C for a reason that does not apply here: between fork and exec a
process may call almost nothing, because any lock another thread was holding
is held forever in the child.  There is one thread in this interpreter, so
there is no such lock -- but the discipline is kept anyway, and everything
this does after the fork is a syscall.

`subprocess` calls one function, `fork_exec`, and reads the result the same
way CPython's does: the child writes an exception to the error pipe and
_exits, the parent reads it and re-raises.  Seven modules of CPython's Lib/
stop at this import.
"""

import posix


def _write_error(errpipe_write, exc_type, errno_num, what):
    """The child's half of the error protocol.

    CPython's format is `OSError:<errno>:<message>` on one line, with a
    leading "OSError" for the class subprocess re-raises.  Anything that goes
    wrong writing it is dropped: the child is about to _exit, and the parent
    will report a bare failure rather than nothing.
    """
    try:
        msg = ("%s:%d:%s" % (exc_type, errno_num, what)).encode("utf-8",
                                                                "replace")
        posix.write(errpipe_write, msg)
    except Exception:
        pass


def fork_exec(args, executable_list, close_fds, fds_to_keep, cwd, env_list,
              p2cread, p2cwrite, c2pread, c2pwrite, errread, errwrite,
              errpipe_read, errpipe_write, restore_signals, start_new_session,
              process_group, gid, gids, uid, umask, preexec_fn,
              allow_vfork=False):
    """Fork, wire the three pipes up, and exec.  Answers the child's pid.

    The arguments are CPython's, in CPython's order, because subprocess.py
    passes them positionally.  Several are accepted and ignored, and each
    says so where it is dropped.
    """
    if args is None:
        raise ValueError("no args to exec")
    argv = [a if isinstance(a, bytes) else _fsencode(a) for a in args]

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
        if c2pwrite != -1 and errwrite != -1 and errwrite == c2pwrite:
            pass
        for fd in (p2cread, p2cwrite, c2pread, c2pwrite, errread, errwrite):
            if fd is not None and fd != -1 and fd > 2:
                try:
                    posix.close(fd)
                except OSError:
                    pass

        if start_new_session:
            posix.setsid()
        if cwd is not None:
            posix.chdir(cwd if isinstance(cwd, str) else cwd.decode("utf-8"))

        # close_fds, restore_signals, process_group, gid, gids, uid, umask
        # and preexec_fn are accepted and dropped: this interpreter has no
        # signal handlers to restore, no credential syscalls, and no way to
        # enumerate open descriptors.  A caller that asked for one of them
        # gets a child that ran without it rather than a failure, which is
        # what DIVERGENCES.md records.
        if preexec_fn is not None:
            preexec_fn()

        for exe in (executable_list or ()):
            path = exe if isinstance(exe, str) else exe.decode("utf-8",
                                                              "surrogateescape")
            names = [a.decode("utf-8", "surrogateescape") for a in argv]
            try:
                posix.execv(path, names)
            except OSError as e:
                last = e
        raise last if executable_list else OSError(2, "no executable given")
    except BaseException as e:
        errno_num = getattr(e, "errno", 0) or 0
        _write_error(errpipe_write, type(e).__name__, errno_num, str(e))
    posix._exit(255)


def _fsencode(s):
    if isinstance(s, bytes):
        return s
    return s.encode("utf-8", "surrogateescape")
