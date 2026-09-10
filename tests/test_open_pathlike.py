# open() takes anything os.fspath() takes.
#
# It accepted an int and a str and refused everything else, so
# `open(pathlib.Path(...))` and `open(b"name")` were both
# "TypeError: invalid file: ...".  shutil.copytree hands it the DirEntry that
# scandir produced and stopped there; CPython's suite has 48 of these.

import posix

NAME = "@open_pathlike_probe"


class P:
    def __init__(self, s):
        self._s = s

    def __fspath__(self):
        return self._s


class BadP:
    def __fspath__(self):
        return 42


def cleanup():
    try:
        posix.unlink(NAME)
    except OSError:
        pass


cleanup()

with open(NAME, "w") as f:
    f.write("payload")

# --- str, the case that always worked --------------------------------------
with open(NAME) as f:
    print("str:", f.read())

# --- bytes -----------------------------------------------------------------
with open(NAME.encode()) as f:
    print("bytes:", f.read())

# --- os.PathLike -----------------------------------------------------------
with open(P(NAME)) as f:
    print("fspath str:", f.read())
with open(P(NAME.encode())) as f:
    print("fspath bytes:", f.read())

# --- binary mode reaches the same place ------------------------------------
with open(P(NAME), "rb") as f:
    print("binary:", f.read())

# --- writing through one, and appending ------------------------------------
with open(P(NAME), "a") as f:
    f.write("!")
with open(NAME) as f:
    print("appended:", f.read())

# --- an int is still an int, not a path ------------------------------------
fd = posix.open(NAME, posix.O_RDONLY)
with open(fd, closefd=True) as f:
    print("fd:", f.read())

# --- and the refusals ------------------------------------------------------
for bad in (None, 3.5, [], BadP()):
    try:
        open(bad)
        print(type(bad).__name__, "accepted")
    except TypeError:
        print(type(bad).__name__, "TypeError")

cleanup()
print("done")
