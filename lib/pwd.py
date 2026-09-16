"""pwd - the password database, read from /etc/passwd.

CPython's is a C module over getpwnam(3), which goes through NSS and so can
answer from LDAP, SSSD or anything else nsswitch.conf names.  This reads the
file, which is what NSS resolves to on an ordinary machine and nothing more.
DIVERGENCES.md records the limit; what it costs is a host whose users are not
in /etc/passwd, where every lookup here is a KeyError and CPython's is not.

What it unblocks is mostly not itself: `getpass`, `shutil.chown`, `tarfile`'s
ownership restore and `os.path.expanduser` all reach for it, and
`import pwd` failing took each of them with it.
"""

import os

__all__ = ["struct_passwd", "getpwuid", "getpwnam", "getpwall"]


class struct_passwd(tuple):
    """The seven fields, as a named tuple.

    CPython's is a structseq, so it is a tuple first and a record second --
    `pwd.getpwnam(n)[0]` is the name and `len(entry)` is 7.  Code in the
    stdlib reads it both ways.
    """

    __slots__ = ()
    n_fields = 7
    n_sequence_fields = 7
    n_unnamed_fields = 0

    _FIELDS = ("pw_name", "pw_passwd", "pw_uid", "pw_gid", "pw_gecos",
               "pw_dir", "pw_shell")

    def __new__(cls, sequence):
        values = tuple(sequence)
        if len(values) != 7:
            raise TypeError("pwd.struct_passwd() takes a 7-sequence")
        return tuple.__new__(cls, values)

    pw_name = property(lambda self: self[0])
    pw_passwd = property(lambda self: self[1])
    pw_uid = property(lambda self: self[2])
    pw_gid = property(lambda self: self[3])
    pw_gecos = property(lambda self: self[4])
    pw_dir = property(lambda self: self[5])
    pw_shell = property(lambda self: self[6])

    def __repr__(self):
        return ("pwd.struct_passwd(pw_name=%r, pw_passwd=%r, pw_uid=%r, "
                "pw_gid=%r, pw_gecos=%r, pw_dir=%r, pw_shell=%r)" % self)


def _entries():
    try:
        with open("/etc/passwd", "r", encoding="utf-8", errors="surrogateescape") as f:
            lines = f.readlines()
    except OSError:
        return
    for line in lines:
        line = line.rstrip("\n")
        if not line or line.startswith("#"):
            continue
        parts = line.split(":")
        if len(parts) != 7:
            continue
        try:
            uid = int(parts[2])
            gid = int(parts[3])
        except ValueError:
            continue
        yield struct_passwd((parts[0], parts[1], uid, gid,
                             parts[4], parts[5], parts[6]))


def getpwall():
    return list(_entries())


def getpwnam(name):
    if not isinstance(name, str):
        raise TypeError("getpwnam() argument must be str, not %s"
                        % type(name).__name__)
    # A C string ends at its first NUL, so a name carrying one would silently
    # become a different name -- CPython refuses it rather than truncating.
    if "\0" in name:
        raise ValueError("embedded null character")
    for entry in _entries():
        if entry.pw_name == name:
            return entry
    raise KeyError("getpwnam(): name not found: %r" % (name,))


def getpwuid(uid):
    # __index__, not int(): CPython refuses a float here, and int() would
    # accept 3.14 and look up user 3.
    import operator
    try:
        uid = operator.index(uid)
    except TypeError:
        raise TypeError("integer argument expected, got %s"
                        % type(uid).__name__) from None
    for entry in _entries():
        if entry.pw_uid == uid:
            return entry
    raise KeyError("getpwuid(): uid not found: %d" % uid)
