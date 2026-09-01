# open() failures now carry the real errno.
#
# sys_open is a bare syscall, so its return is -errno and the filename is still
# in hand; both used to be discarded and the message hardcoded to "No such file
# or directory" whatever had gone wrong.  A directory reported ENOENT, and so
# did an unreadable file.
#
# Linux also lets open(2) succeed on a directory -- it is read(2) that fails
# with EISDIR -- so open("/tmp") handed back a file object that failed later
# and somewhere else.  CPython fstats the descriptor; so do we now.

import errno

for path in ["/nonexistent-directory-xyz/file", "/tmp", "/etc/shadow"]:
    try:
        f = open(path)
        f.close()
        print(path, "-> opened")
    except OSError as e:
        print(path, "->", type(e).__name__)
        print("   errno:", e.errno, errno.errorcode[e.errno])
        print("   strerror:", repr(e.strerror))
        print("   filename:", repr(e.filename))
        print("   str:", repr(str(e)))

# The subclass is what `except` clauses actually name.
try:
    open("/nonexistent-directory-xyz/file")
except FileNotFoundError as e:
    print("FileNotFoundError caught, errno", e.errno)

try:
    open("/tmp")
except IsADirectoryError as e:
    print("IsADirectoryError caught, errno", e.errno)

# A file that does open still opens, and reads.
with open("/etc/hostname") as f:
    print("read ok:", isinstance(f.read(), str))
