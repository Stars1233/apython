"""mmap - memory-mapped file support.

The split src/modules/zlib.asm's header states as a contract, and the one
lib/pyexpat.py records the reasoning for.  `_mmapcore` is the mapping itself:
the mmap/munmap/mremap/msync/madvise calls, the raw bytes, and the search over
them, all bounds-checked against the handle's own length before anything
touches the mapping.  This is everything a Python program sees -- the object,
the cursor, the ACCESS_* translation, readline, the slice protocol and every
default.

What is missing, deliberately
-----------------------------
`memoryview(m)` and the buffer protocol.  A Python class cannot export a
buffer here, so `memoryview(m)`, `m.__buffer__` and anything that writes into
an mmap through a buffer refuse.  Reading and writing through the object's own
slice syntax works, which is what the stdlib's own users of mmap do.  That is
recorded in DIVERGENCES.md rather than approximated.
"""

import os as _os

import _mmapcore as _core

error = OSError

ACCESS_DEFAULT = 0
ACCESS_READ = 1
ACCESS_WRITE = 2
ACCESS_COPY = 3

PROT_READ = 1
PROT_WRITE = 2
PROT_EXEC = 4

MAP_SHARED = 0x01
MAP_PRIVATE = 0x02
MAP_ANONYMOUS = 0x20
MAP_ANON = MAP_ANONYMOUS
MAP_POPULATE = 0x8000
MAP_STACK = 0x20000

MADV_NORMAL = 0
MADV_RANDOM = 1
MADV_SEQUENTIAL = 2
MADV_WILLNEED = 3
MADV_DONTNEED = 4
MADV_FREE = 8
MADV_REMOVE = 9
MADV_DONTFORK = 10
MADV_DOFORK = 11
MADV_HWPOISON = 100
MADV_MERGEABLE = 12
MADV_UNMERGEABLE = 13
MADV_HUGEPAGE = 14
MADV_NOHUGEPAGE = 15
MADV_DONTDUMP = 16
MADV_DODUMP = 17

PAGESIZE = 4096
ALLOCATIONGRANULARITY = PAGESIZE

SEEK_SET = 0
SEEK_CUR = 1
SEEK_END = 2

def _index(value):
    """PyNumber_Index, with CPython's wording.

    Every ssize_t argument on this object goes through it: CPython's argument
    clinic converts before it does anything else, and a `__index__` with a
    side effect -- closing the mapping, say -- has to run at the same point.
    """
    if isinstance(value, int):
        return value
    try:
        method = type(value).__index__
    except AttributeError:
        raise TypeError("'%s' object cannot be interpreted as an integer"
                        % (type(value).__name__,)) from None
    result = method(value)
    if not isinstance(result, int):
        raise TypeError("__index__ returned non-int (type %s)"
                        % (type(result).__name__,))
    return result


__all__ = ["mmap", "error", "ACCESS_DEFAULT", "ACCESS_READ", "ACCESS_WRITE",
           "ACCESS_COPY", "PROT_READ", "PROT_WRITE", "PROT_EXEC",
           "MAP_SHARED", "MAP_PRIVATE", "MAP_ANONYMOUS", "MAP_ANON",
           "MAP_POPULATE", "MAP_STACK", "PAGESIZE", "ALLOCATIONGRANULARITY"]


class mmap:
    """A file, or anonymous memory, seen as a mutable sequence of bytes."""

    def __new__(cls, fileno, length, flags=None, prot=None,
                access=ACCESS_DEFAULT, offset=0):
        """The mapping is made HERE, not in __init__.

        CPython's mmap is a C type that does all of this in tp_new and leaves
        tp_init as object's, and test_mmap's own subclass depends on it: its
        __new__ supplies the descriptor -- `mmap.mmap.__new__(klass, -1,
        *args)` -- and then type_call passes the ORIGINAL argument list to
        __init__, which is one argument short of a constructor.
        """
        self = super().__new__(cls)
        self._build(fileno, length, flags, prot, access, offset)
        return self

    def __init__(self, *args, **kwargs):
        # Everything happened in __new__; this is object.__init__ with a
        # signature that accepts whatever the constructor was given.
        pass

    def _build(self, fileno, length, flags, prot, access, offset):
        # flags and prot default through None rather than through their real
        # defaults, because `access` is a shorthand for a (prot, flags) pair
        # and CPython refuses to be given both.
        given_flags, given_prot = flags, prot
        if flags is None:
            flags = MAP_SHARED
        if prot is None:
            prot = PROT_READ | PROT_WRITE
        if length < 0:
            raise OverflowError("memory mapped length must be positive")
        if offset < 0:
            raise OverflowError("memory mapped offset must be positive")
        # An unaligned offset is not checked here: CPython hands it to
        # mmap(2), which refuses it with EINVAL, and the OSError is what a
        # caller sees.

        # access is a shorthand for a (prot, flags) pair, and CPython refuses
        # to be given both.
        if access != ACCESS_DEFAULT and (given_flags is not None
                                         or given_prot is not None):
            raise ValueError("mmap can't specify both access and flags, prot.")
        if access == ACCESS_READ:
            flags, prot = MAP_SHARED, PROT_READ
        elif access == ACCESS_WRITE:
            flags, prot = MAP_SHARED, PROT_READ | PROT_WRITE
        elif access == ACCESS_COPY:
            flags, prot = MAP_PRIVATE, PROT_READ | PROT_WRITE
        elif access != ACCESS_DEFAULT:
            raise ValueError("mmap invalid access parameter.")

        self._closed = False
        self._access = access
        # Writability is decided by the PROT bits, not by `access` alone: a
        # mapping made with prot=PROT_READ and the default access has to
        # refuse a write with a TypeError, because doing it would be a SIGSEGV
        # rather than an error.
        self._writable = bool(prot & PROT_WRITE)
        self._offset = offset
        self._pos = 0
        self._fd = -1
        self._handle = -1

        if fileno == -1 or fileno is None:
            flags |= MAP_ANONYMOUS
            # A zero-length anonymous mapping is NOT "cannot mmap an empty
            # file" -- there is no file.  CPython lets mmap(2) refuse it, and
            # the EINVAL arrives as an OSError.
            fd = -1
        else:
            # The mapping outlives the caller's file object, so the descriptor
            # is duplicated -- CPython's does the same, and resize() needs one
            # to ftruncate.
            fd = _os.dup(fileno)
            self._fd = fd
            try:
                size = _os.fstat(fd).st_size
            except OSError:
                _os.close(fd)
                self._fd = -1
                raise
            if length == 0:
                if size == 0:
                    _os.close(fd)
                    self._fd = -1
                    raise ValueError("cannot mmap an empty file")
                if offset > size:
                    _os.close(fd)
                    self._fd = -1
                    raise ValueError(
                        "mmap offset is greater than file size")
                length = size - offset
            elif offset + length > size:
                _os.close(fd)
                self._fd = -1
                raise ValueError("mmap length is greater than file size")

        try:
            self._handle = _core.map(fd, length, prot, flags, offset)
        except BaseException:
            if self._fd != -1:
                _os.close(self._fd)
                self._fd = -1
            raise

    # -- lifetime ---------------------------------------------------------
    def close(self):
        if self._closed:
            return
        self._closed = True
        if self._handle >= 0:
            _core.unmap(self._handle)
            self._handle = -1
        if self._fd != -1:
            _os.close(self._fd)
            self._fd = -1

    def __del__(self):
        try:
            self.close()
        except Exception:
            pass

    @property
    def closed(self):
        return self._closed

    def __enter__(self):
        return self

    def __exit__(self, *exc):
        self.close()
        return False

    _ACCESS_NAMES = {
        ACCESS_DEFAULT: "ACCESS_DEFAULT",
        ACCESS_READ: "ACCESS_READ",
        ACCESS_WRITE: "ACCESS_WRITE",
        ACCESS_COPY: "ACCESS_COPY",
    }

    def __repr__(self):
        if self._closed:
            return "<mmap.mmap closed=True>"
        return ("<mmap.mmap closed=False, access=%s, length=%d, pos=%d, "
                "offset=%d>"
                % (self._ACCESS_NAMES.get(self._access, self._access),
                   _core.size(self._handle), self._pos, self._offset))

    def _check(self):
        if self._closed:
            raise ValueError("mmap closed or invalid")

    def _check_writable(self):
        self._check()
        if not self._writable:
            raise TypeError("mmap can't modify a readonly memory map.")

    def _check_resizable(self):
        self._check()
        if self._access not in (ACCESS_WRITE, ACCESS_DEFAULT):
            raise TypeError(
                "mmap can't resize a readonly or copy-on-write memory map.")

    # -- size -------------------------------------------------------------
    def __len__(self):
        self._check()
        return _core.size(self._handle)

    def size(self):
        """The size of the FILE, which is not the size of the mapping.

        An anonymous mapping has no file, and CPython does not special-case
        that: it fstats a descriptor of -1 and the EBADF comes back as an
        OSError.  So does this.
        """
        self._check()
        return _os.fstat(self._fd).st_size

    def resize(self, newsize):
        self._check_resizable()
        newsize = _index(newsize)
        self._check_resizable()
        if newsize < 0:
            raise ValueError("new size out of range")
        if self._fd != -1:
            # The FILE has to reach past the mapping's own start, or every
            # page of the remapped window is beyond the end of the file and
            # touching one is a SIGBUS rather than an error.
            _os.ftruncate(self._fd, self._offset + newsize)
        _core.resize(self._handle, newsize if newsize else 1)

    # -- the cursor -------------------------------------------------------
    def tell(self):
        self._check()
        return self._pos

    def seek(self, dist, whence=SEEK_SET):
        self._check()
        dist = _index(dist)
        whence = _index(whence)
        self._check()
        length = _core.size(self._handle)
        if whence == SEEK_SET:
            where = dist
        elif whence == SEEK_CUR:
            where = self._pos + dist
        elif whence == SEEK_END:
            where = length + dist
        else:
            raise ValueError("unknown seek type")
        if where < 0 or where > length:
            raise ValueError("seek out of range")
        self._pos = where

    # -- reading ----------------------------------------------------------
    def read(self, n=None):
        self._check()
        if n is not None:
            # CPython parses the count through __index__ before it does
            # anything else, and re-checks the mapping afterwards -- the
            # conversion is arbitrary code and can close it.
            n = _index(n)
            self._check()
        length = _core.size(self._handle)
        # resize() can leave the cursor past the end -- CPython allows it and
        # read() then answers b"" -- so both the start and the remainder have
        # a floor of zero.
        start = min(self._pos, length)
        left = max(0, length - start)
        if n is None or n < 0 or n > left:
            n = left
        data = _core.read(self._handle, start, n)
        self._pos += len(data)
        return data

    def read_byte(self):
        self._check()
        if self._pos >= _core.size(self._handle):
            raise ValueError("read byte out of range")
        b = _core.read(self._handle, self._pos, 1)
        self._pos += 1
        return b[0]

    def readline(self):
        self._check()
        length = _core.size(self._handle)
        end = _core.find(self._handle, b"\n", self._pos, length, 0)
        if end < 0:
            end = length
        else:
            end += 1
        data = _core.read(self._handle, self._pos, end - self._pos)
        self._pos = end
        return data

    # -- writing ----------------------------------------------------------
    def write(self, data):
        self._check_writable()
        n = _core.write(self._handle, self._pos, data)
        self._pos += n
        return n

    def write_byte(self, value):
        self._check_writable()
        _core.write(self._handle, self._pos, bytes([value]))
        self._pos += 1

    # -- searching --------------------------------------------------------
    def _window(self, start, end):
        if start is not None:
            start = _index(start)
        if end is not None:
            end = _index(end)
        self._check()
        length = _core.size(self._handle)
        if start is None:
            start = self._pos
        elif start < 0:
            start = max(0, length + start)
        if end is None:
            end = length
        elif end < 0:
            end = max(0, length + end)
        return min(start, length), min(end, length)

    def find(self, sub, start=None, end=None):
        self._check()
        a, b = self._window(start, end)
        return _core.find(self._handle, sub, a, b, 0)

    def rfind(self, sub, start=None, end=None):
        self._check()
        a, b = self._window(start, end)
        return _core.find(self._handle, sub, a, b, 1)

    # -- the rest ---------------------------------------------------------
    def flush(self, offset=None, size=None):
        self._check()
        if offset is not None:
            offset = _index(offset)
        if size is not None:
            size = _index(size)
        self._check()
        length = _core.size(self._handle)
        if offset is None and size is None:
            offset, size = 0, length
        elif size is None:
            raise TypeError("flush() takes no arguments or two arguments")
        elif offset is None:
            offset = 0
        if offset < 0 or size < 0 or offset + size > length:
            raise ValueError("flush values out of range")
        _core.flush(self._handle, offset, size)

    def move(self, dest, src, count):
        self._check_writable()
        dest, src, count = _index(dest), _index(src), _index(count)
        self._check_writable()
        _core.move(self._handle, dest, src, count)

    def madvise(self, option, start=0, length=None):
        self._check()
        option, start = _index(option), _index(start)
        if length is not None:
            length = _index(length)
        self._check()
        mapped = _core.size(self._handle)
        if length is None:
            length = mapped
        if start < 0 or start >= mapped:
            raise ValueError("madvise start out of bounds")
        if length < 0:
            raise ValueError("madvise length invalid")
        if (1 << 63) - 1 - start < length:
            raise OverflowError("madvise length too large")
        if start + length > mapped:
            length = mapped - start
        _core.advise(self._handle, option, start, length)

    # -- the sequence protocol --------------------------------------------
    def _index(self, i, length):
        if i < 0:
            i += length
        if i < 0 or i >= length:
            raise IndexError("mmap index out of range")
        return i

    def __getitem__(self, key):
        self._check()
        length = _core.size(self._handle)
        if isinstance(key, slice):
            # key.indices runs the bounds' own __index__, which is arbitrary
            # code and can close this mapping -- CPython re-checks after every
            # such call for exactly that reason.
            start, stop, step = key.indices(length)
            self._check()
            if step == 1:
                n = stop - start
                return _core.read(self._handle, start, n) if n > 0 else b""
            return bytes([_core.read(self._handle, i, 1)[0]
                          for i in range(start, stop, step)])
        i = self._index(_index(key), length)
        self._check()
        return _core.read(self._handle, i, 1)[0]

    def __setitem__(self, key, value):
        self._check_writable()
        length = _core.size(self._handle)
        if isinstance(key, slice):
            start, stop, step = key.indices(length)
            self._check_writable()
            if step == 1:
                n = stop - start
                if n < 0:
                    n = 0
                if len(value) != n:
                    raise IndexError(
                        "mmap slice assignment is wrong size")
                if n:
                    _core.write(self._handle, start, bytes(value))
                return
            indices = list(range(start, stop, step))
            if len(value) != len(indices):
                raise IndexError("mmap slice assignment is wrong size")
            for i, v in zip(indices, value):
                _core.write(self._handle, i, bytes([v]))
            return
        i = self._index(_index(key), length)
        self._check_writable()
        if not isinstance(value, int):
            raise TypeError("mmap item value must be an int")
        if not 0 <= value < 256:
            raise ValueError("mmap item value must be in range(0, 256)")
        _core.write(self._handle, i, bytes([value]))

    def __delitem__(self, key):
        self._check_writable()
        if isinstance(key, slice):
            raise TypeError("mmap object doesn't support slice deletion")
        raise TypeError("mmap doesn't support item deletion")
