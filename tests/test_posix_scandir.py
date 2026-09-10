# posix.scandir() and posix.DirEntry.
#
# os.py's walk(), and through it shutil, glob, pathlib and tempfile's cleanup,
# reach for scandir and nothing else; without it every one of them ends on
# `NameError: name 'scandir' is not defined`.
#
# Written against `posix` directly rather than through `os`, so it says the
# same thing on either interpreter, and everything is sorted: a directory has
# no order of its own.

import posix
import stat as st_mod

BASE = "@scandir_probe"


def cleanup():
    for name in ("d/inner.txt", "f.txt", "link", "dangling", "d"):
        p = BASE + "/" + name
        try:
            posix.unlink(p)
        except OSError:
            try:
                posix.rmdir(p)
            except OSError:
                pass
    try:
        posix.rmdir(BASE)
    except OSError:
        pass


cleanup()
posix.mkdir(BASE)
posix.mkdir(BASE + "/d")
fd = posix.open(BASE + "/f.txt", posix.O_WRONLY | posix.O_CREAT, 0o644)
posix.write(fd, b"hello")
posix.close(fd)
fd = posix.open(BASE + "/d/inner.txt", posix.O_WRONLY | posix.O_CREAT, 0o644)
posix.close(fd)
posix.symlink("f.txt", BASE + "/link")
posix.symlink("nowhere", BASE + "/dangling")

# --- the entries, by name -------------------------------------------------
entries = sorted(posix.scandir(BASE), key=lambda e: e.name)
print("names:", [e.name for e in entries])
print("paths:", [e.path for e in entries])
print("repr:", [repr(e) for e in entries])

for e in entries:
    print(e.name,
          "dir", e.is_dir(),
          "dir!", e.is_dir(follow_symlinks=False),
          "file", e.is_file(),
          "file!", e.is_file(follow_symlinks=False),
          "link", e.is_symlink())

# --- stat() ---------------------------------------------------------------
by_name = {e.name: e for e in entries}
print("f size:", by_name["f.txt"].stat().st_size)
print("f is reg:", st_mod.S_ISREG(by_name["f.txt"].stat().st_mode))
print("d is dir:", st_mod.S_ISDIR(by_name["d"].stat().st_mode))
print("link follows:", by_name["link"].stat().st_size)
print("link itself:", st_mod.S_ISLNK(by_name["link"].stat(follow_symlinks=False).st_mode))
print("inode matches:", by_name["f.txt"].inode() == by_name["f.txt"].stat().st_ino)

# A dangling symlink: following it fails, not following it does not.
d = by_name["dangling"]
print("dangling is_symlink:", d.is_symlink())
print("dangling is_file:", d.is_file())
print("dangling is_dir:", d.is_dir())
try:
    d.stat()
    print("dangling stat: no error")
except OSError as e:
    print("dangling stat: OSError")
print("dangling lstat ok:", st_mod.S_ISLNK(d.stat(follow_symlinks=False).st_mode))

# --- os.PathLike ----------------------------------------------------------
print("fspath:", sorted(posix.fspath(e) for e in entries))

# --- the iterator is an iterator, a context manager, and closeable --------
it = posix.scandir(BASE)
print("is self-iter:", iter(it) is it)
first = next(it)
print("one entry has a name:", isinstance(first.name, str))
it.close()
print("closed, then exhausted:", list(it))

with posix.scandir(BASE) as it2:
    print("with-block count:", len(list(it2)))

# Closing twice, and closing an exhausted one, are both fine.
it3 = posix.scandir(BASE)
list(it3)
it3.close()
it3.close()
print("double close ok")

# --- the default path is "." ----------------------------------------------
here = sorted(e.name for e in posix.scandir(BASE))
print("explicit == default:", here == sorted(posix.listdir(BASE)))

# --- errors ---------------------------------------------------------------
try:
    posix.scandir(BASE + "/nosuchdir")
    print("missing: no error")
except FileNotFoundError:
    print("missing: FileNotFoundError")
try:
    posix.scandir(BASE + "/f.txt")
    print("not a dir: no error")
except NotADirectoryError:
    print("not a dir: NotADirectoryError")
try:
    posix.DirEntry()
    print("DirEntry(): constructed")
except TypeError:
    print("DirEntry(): TypeError")

cleanup()
print("done")
