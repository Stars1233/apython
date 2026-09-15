"""Importing a .py writes its __pycache__ entry, and a stale one is refused.

The module imported here has a name nothing else uses, so this process's first
import of it is the one under test.  The temporary directory is built by hand:
`tempfile` is not among the modules this interpreter ships.
"""

import os
import sys


SUFFIX = ".cpython-312.pyc"
MAGIC = (3531).to_bytes(2, "little") + b"\r\n"


def write(path, text, mtime=None):
    with open(path, "w") as f:
        f.write(text)
    if mtime is not None:
        os.utime(path, (mtime, mtime))


def main():
    tmp = os.path.join(os.path.dirname(os.path.abspath(__file__)),
                       "pycache_probe_dir")
    src = os.path.join(tmp, "pycachedprobe.py")
    cache = os.path.join(tmp, "__pycache__", "pycachedprobe" + SUFFIX)
    for path in (cache, src):
        if os.path.exists(path):
            os.unlink(path)
    for d in (os.path.join(tmp, "__pycache__"), tmp):
        if os.path.isdir(d):
            os.rmdir(d)
    os.mkdir(tmp)

    try:
        # Both sources exist before the directory is ever searched: CPython's
        # FileFinder caches a directory's listing by its mtime, so a file
        # created after the first import from it may or may not be seen,
        # depending on the filesystem's timestamp resolution.
        write(src, "VALUE = 1\n", 1600000000)
        src2 = os.path.join(tmp, "pycachedprobe2.py")
        cache2 = os.path.join(tmp, "__pycache__", "pycachedprobe2" + SUFFIX)
        write(src2, "VALUE = 3\n", 1600000000)
        sys.path.insert(0, tmp)
        print("before:", os.path.exists(cache))

        import pycachedprobe
        print("first:", pycachedprobe.VALUE)
        print("written:", os.path.exists(cache))

        with open(cache, "rb") as f:
            header = f.read(16)
        st = os.stat(src)
        print("magic:", header[:4] == MAGIC)
        print("flags:", int.from_bytes(header[4:8], "little"))
        print("mtime:", int.from_bytes(header[8:12], "little")
              == int(st.st_mtime) & 0xffffffff)
        print("size:", int.from_bytes(header[12:16], "little") == st.st_size)

        # Re-importing reads the cache back rather than the source.
        del sys.modules["pycachedprobe"]
        import pycachedprobe as again
        print("second:", again.VALUE)

        # An edited source makes the cache stale, and a stale one is skipped.
        write(src, "VALUE = 2\n", 1600000010)
        del sys.modules["pycachedprobe"]
        import pycachedprobe as edited
        print("edited:", edited.VALUE)

        with open(cache, "rb") as f:
            header = f.read(16)
        print("refreshed:", int.from_bytes(header[8:12], "little")
              == 1600000010)

        # sys.dont_write_bytecode is read on every write, not once at startup.
        # It starts False here; -B is what makes it True.
        print("default flag:", sys.dont_write_bytecode)
        sys.dont_write_bytecode = True
        import pycachedprobe2
        print("no-write value:", pycachedprobe2.VALUE)
        print("no-write wrote:", os.path.exists(cache2))
        sys.dont_write_bytecode = False
    finally:
        sys.path.remove(tmp)
        for name in ("pycachedprobe", "pycachedprobe2"):
            sys.modules.pop(name, None)
        pyc = os.path.join(tmp, "__pycache__")
        if os.path.isdir(pyc):
            for name in os.listdir(pyc):
                os.unlink(os.path.join(pyc, name))
            os.rmdir(pyc)
        for name in os.listdir(tmp):
            os.unlink(os.path.join(tmp, name))
        os.rmdir(tmp)


main()
