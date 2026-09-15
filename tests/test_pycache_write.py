"""Importing a .py writes its __pycache__ entry, and a stale one is refused.

The module imported here has a name nothing else uses, so this process's first
import of it is the one under test.  The temporary directory is built by hand:
`tempfile` is not among the modules this interpreter ships.
"""

import importlib
import os
import sys


# The tag is this interpreter's own, so a python3 in the same tree is not
# handed bytecode from our compiler.  Both tags are READ; only ours is written.
SUFFIX = getattr(sys.implementation, "cache_tag", "cpython-312")
SUFFIX = "." + SUFFIX + ".pyc"
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
        src3 = os.path.join(tmp, "pycachedprobe3.py")
        cache3 = os.path.join(tmp, "__pycache__", "pycachedprobe3" + SUFFIX)
        write(src3, "VALUE = 4\n", 1600000000)
        src4 = os.path.join(tmp, "pycachedprobe4.py")
        write(src4, "VALUE = 5\n", 1600000000)
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

        # __cached__ names the cache file, absolute, whether or not one has
        # been written; __file__ names the source.
        mod = sys.modules["pycachedprobe"]
        print("cached:", mod.__cached__ == os.path.abspath(cache))
        print("file:", mod.__file__ == os.path.abspath(src))

        # The cache file gets the source's permissions, with the write bit
        # forced on so a read-only source still yields a replaceable cache.
        old_mask = os.umask(0o022)
        try:
            os.chmod(src3, 0o400)
            import pycachedprobe3
            print("readonly source:", pycachedprobe3.VALUE)
            print("cache mode:", oct(os.stat(cache3).st_mode & 0o777))
        finally:
            os.umask(old_mask)

        # A bare "<name>.pyc" beside the source is CPython's sourceless form,
        # and is found when nothing else answers.
        legacy = os.path.join(tmp, "pycachedprobe4.pyc")
        import pycachedprobe4
        os.rename(os.path.join(tmp, "__pycache__",
                               "pycachedprobe4" + SUFFIX), legacy)
        os.unlink(src4)
        del sys.modules["pycachedprobe4"]
        # The directory changed under a finder that caches its listing.
        importlib.invalidate_caches()
        import pycachedprobe4 as sourceless
        print("sourceless:", sourceless.VALUE)
        print("sourceless cached:", sourceless.__cached__ ==
              os.path.abspath(legacy))

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
        for name in ("pycachedprobe", "pycachedprobe2", "pycachedprobe3",
                     "pycachedprobe4"):
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
