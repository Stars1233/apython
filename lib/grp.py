"""grp - the group database, read from /etc/group.

The same arrangement lib/pwd.py explains, and the same limit: CPython's C
module goes through NSS and this reads the file.

`shutil.chown`, `tarfile`'s ownership restore and `os.path.expanduser`
reach for it beside pwd, so the two travel together.
"""

__all__ = ["struct_group", "getgrgid", "getgrnam", "getgrall"]


class struct_group(tuple):
    """The four fields, as a named tuple -- a tuple first, as CPython's is."""

    __slots__ = ()
    n_fields = 4
    n_sequence_fields = 4
    n_unnamed_fields = 0

    _FIELDS = ("gr_name", "gr_passwd", "gr_gid", "gr_mem")

    def __new__(cls, sequence):
        values = tuple(sequence)
        if len(values) != 4:
            raise TypeError("grp.struct_group() takes a 4-sequence")
        return tuple.__new__(cls, values)

    gr_name = property(lambda self: self[0])
    gr_passwd = property(lambda self: self[1])
    gr_gid = property(lambda self: self[2])
    gr_mem = property(lambda self: self[3])

    def __repr__(self):
        return ("grp.struct_group(gr_name=%r, gr_passwd=%r, gr_gid=%r, "
                "gr_mem=%r)" % self)


def _entries():
    try:
        with open("/etc/group", "r", encoding="utf-8", errors="surrogateescape") as f:
            lines = f.readlines()
    except OSError:
        return
    for line in lines:
        line = line.rstrip("\n")
        if not line or line.startswith("#"):
            continue
        parts = line.split(":")
        if len(parts) != 4:
            continue
        try:
            gid = int(parts[2])
        except ValueError:
            continue
        members = [m for m in parts[3].split(",") if m]
        yield struct_group((parts[0], parts[1], gid, members))


def getgrall():
    return list(_entries())


def getgrnam(name):
    if not isinstance(name, str):
        raise TypeError("getgrnam() argument must be str, not %s"
                        % type(name).__name__)
    # A C string ends at its first NUL, so a name carrying one would silently
    # become a different name -- CPython refuses it rather than truncating.
    if "\0" in name:
        raise ValueError("embedded null character")
    for entry in _entries():
        if entry.gr_name == name:
            return entry
    raise KeyError("getgrnam(): name not found: %r" % (name,))


def getgrgid(gid):
    # __index__, not int(): CPython refuses a float here, and int() would
    # accept 3.14 and look up group 3.
    import operator
    try:
        gid = operator.index(gid)
    except TypeError:
        raise TypeError("integer argument expected, got %s"
                        % type(gid).__name__) from None
    for entry in _entries():
        if entry.gr_gid == gid:
            return entry
    raise KeyError("getgrgid(): gid not found: %d" % gid)
