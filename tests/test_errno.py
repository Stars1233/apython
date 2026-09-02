# The errno module: every name, its number, and errorcode's inverse mapping.
#
# These are Linux x86-64 kernel ABI constants, written out in
# src/errnomod.asm rather than generated at build time.  This file is what
# makes that safe: run_tests.sh diffs our stdout against the system CPython's,
# so a mistyped number cannot survive a test run.
#
# The names are listed literally rather than discovered with dir(): dir() on a
# module does not report the module's own contents here (bugs.md).

import errno

NAMES = [
    "E2BIG", "EACCES", "EADDRINUSE", "EADDRNOTAVAIL", "EADV",
    "EAFNOSUPPORT", "EAGAIN", "EALREADY", "EBADE", "EBADF", "EBADFD",
    "EBADMSG", "EBADR", "EBADRQC", "EBADSLT", "EBFONT", "EBUSY",
    "ECANCELED", "ECHILD", "ECHRNG", "ECOMM", "ECONNABORTED",
    "ECONNREFUSED", "ECONNRESET", "EDEADLK", "EDEADLOCK", "EDESTADDRREQ",
    "EDOM", "EDOTDOT", "EDQUOT", "EEXIST", "EFAULT", "EFBIG", "EHOSTDOWN",
    "EHOSTUNREACH", "EIDRM", "EILSEQ", "EINPROGRESS", "EINTR", "EINVAL",
    "EIO", "EISCONN", "EISDIR", "EISNAM", "EKEYEXPIRED", "EKEYREJECTED",
    "EKEYREVOKED", "EL2HLT", "EL2NSYNC", "EL3HLT", "EL3RST", "ELIBACC",
    "ELIBBAD", "ELIBEXEC", "ELIBMAX", "ELIBSCN", "ELNRNG", "ELOOP",
    "EMEDIUMTYPE", "EMFILE", "EMLINK", "EMSGSIZE", "EMULTIHOP",
    "ENAMETOOLONG", "ENAVAIL", "ENETDOWN", "ENETRESET", "ENETUNREACH",
    "ENFILE", "ENOANO", "ENOBUFS", "ENOCSI", "ENODATA", "ENODEV", "ENOENT",
    "ENOEXEC", "ENOKEY", "ENOLCK", "ENOLINK", "ENOMEDIUM", "ENOMEM",
    "ENOMSG", "ENONET", "ENOPKG", "ENOPROTOOPT", "ENOSPC", "ENOSR",
    "ENOSTR", "ENOSYS", "ENOTBLK", "ENOTCONN", "ENOTDIR", "ENOTEMPTY",
    "ENOTNAM", "ENOTRECOVERABLE", "ENOTSOCK", "ENOTSUP", "ENOTTY",
    "ENOTUNIQ", "ENXIO", "EOPNOTSUPP", "EOVERFLOW", "EOWNERDEAD", "EPERM",
    "EPFNOSUPPORT", "EPIPE", "EPROTO", "EPROTONOSUPPORT", "EPROTOTYPE",
    "ERANGE", "EREMCHG", "EREMOTE", "EREMOTEIO", "ERESTART", "ERFKILL",
    "EROFS", "ESHUTDOWN", "ESOCKTNOSUPPORT", "ESPIPE", "ESRCH", "ESRMNT",
    "ESTALE", "ESTRPIPE", "ETIME", "ETIMEDOUT", "ETOOMANYREFS", "ETXTBSY",
    "EUCLEAN", "EUNATCH", "EUSERS", "EWOULDBLOCK", "EXDEV", "EXFULL",
]

print("names:", len(NAMES))
for n in NAMES:
    print(n, getattr(errno, n))

print("errorcode:", len(errno.errorcode))
for k in sorted(errno.errorcode):
    print(k, errno.errorcode[k])

# The ones the OSError subclass mapping depends on.
for n in ("ENOENT", "EEXIST", "EACCES", "EPERM", "EISDIR", "ENOTDIR", "EINTR",
          "EAGAIN", "EPIPE", "ECHILD", "ESRCH", "ETIMEDOUT"):
    print(n, getattr(errno, n), errno.errorcode[getattr(errno, n)])
