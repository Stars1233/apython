"""_bz2 - the BZ2Compressor and BZ2Decompressor objects over _bz2core.

The same split lib/zlib.py and lib/_io.py use.  _bz2core owns the bz_stream,
the growing output buffer and the handle table -- the parts that have to be
written in assembly over libbzip2 -- and everything a program touches is here:
the two objects, their attributes, their argument checking and the exception
classes.  CPython's own Lib/bz2.py then runs verbatim on top.

The core reports a failure as ValueError, because a module written in
assembly cannot raise a class defined in Python.  It uses CPython's exact
sentence for each libbzip2 return code and _ERRORS below maps the sentence
back to the class CPython raises.  The mapping is by exact text; both halves
say so, and a sentence changed on one side without the other falls through to
OSError rather than silently becoming the wrong class.
"""

import _bz2core

__all__ = ["BZ2Compressor", "BZ2Decompressor"]

_COMPRESS = 0
_DECOMPRESS = 1

_BZ_RUN = 0
_BZ_FINISH = 2

_ERRORS = {
    "Invalid data stream": OSError,
    "Out of memory": MemoryError,
    "Compressed file ended before the end-of-stream marker was reached":
        EOFError,
    "Internal error - calls to libbzip2 in wrong order": RuntimeError,
    "Internal error - invalid parameters": ValueError,
    "Unknown I/O error": OSError,
}


def _translate(exc):
    text = str(exc)
    cls = _ERRORS.get(text, OSError)
    if cls is MemoryError:
        return MemoryError()
    return cls(text)


class _Stream:
    """What the two objects share: one core handle, ended once."""

    def __init__(self, mode, level):
        self._handle = -1
        try:
            self._handle = _bz2core.stream_new(mode, level)
        except ValueError as e:
            raise _translate(e) from None

    def _feed(self, data, action, max_length=-1):
        if self._handle < 0:
            raise ValueError("stream is already finished")
        try:
            return _bz2core.stream_feed(self._handle, data, action, max_length)
        except ValueError as e:
            raise _translate(e) from None

    def _close(self):
        h = self._handle
        self._handle = -1
        if h >= 0:
            _bz2core.stream_free(h)

    def __del__(self):
        # A compressor holds 900 KB of block buffer at level 9, so a loop over
        # a thousand files that dropped its compressors would notice.
        try:
            self._close()
        except Exception:
            pass

    def __reduce__(self):
        raise TypeError("cannot pickle '%s' object" % (type(self).__name__,))


class BZ2Compressor(_Stream):
    """Incremental bzip2 compression.

    compress() returns whatever is ready, which for bzip2 is usually nothing
    until a 900 KB block fills; flush() returns the rest and ends the stream.
    """

    def __init__(self, compresslevel=9):
        compresslevel = _index(compresslevel, "compresslevel")
        if not 1 <= compresslevel <= 9:
            raise ValueError("compresslevel must be between 1 and 9")
        self._flushed = False
        super().__init__(_COMPRESS, compresslevel)

    def compress(self, data):
        if self._flushed:
            raise ValueError("Compressor has been flushed")
        return self._feed(data, _BZ_RUN)

    def flush(self):
        if self._flushed:
            # CPython words the two refusals differently, and a caller sees
            # the difference.
            raise ValueError("Repeated call to flush()")
        out = self._feed(b"", _BZ_FINISH)
        self._flushed = True
        self._close()
        return out


class BZ2Decompressor(_Stream):
    """Incremental bzip2 decompression.

    `needs_input` is False while a max_length call has left input parked in
    the core; the caller then sends b"" rather than more data, which is what
    _compression.DecompressReader does.
    """

    # CPython's is a C type whose struct is zero-filled by the allocator, so
    # `BZ2Decompressor.__new__(BZ2Decompressor).decompress(b"")` answers b""
    # rather than raising -- there is a test named for the crash it used to
    # be.  These class attributes are that zero-filled struct.
    eof = False
    needs_input = True
    unused_data = b""
    _handle = -1

    def __init__(self):
        self.eof = False
        self.unused_data = b""
        self.needs_input = True
        super().__init__(_DECOMPRESS, 0)

    def decompress(self, data, max_length=-1):
        if self.eof:
            raise EOFError("End of stream already reached")
        max_length = _index(max_length, "max_length")
        if self._handle < 0:
            # Never initialised: see the class attributes above.  A closed
            # handle cannot reach here, because eof is checked first.
            _check_buffer(data)
            return b""
        out = self._feed(data, _BZ_RUN, max_length)
        self._refresh()
        return out

    def _refresh(self):
        eof, needs_input, unused = _bz2core.stream_state(self._handle)
        self.eof = eof
        self.needs_input = needs_input
        if unused:
            self.unused_data += unused
        if eof:
            # Nothing more will come out, and the block buffer can go now
            # rather than at collection.  unused_data has already been copied
            # out of the core.
            self.needs_input = False
            self._close()


def _check_buffer(data):
    """The argument is still checked on the path that does nothing with it."""
    if isinstance(data, (bytes, bytearray, memoryview)):
        return
    raise TypeError("a bytes-like object is required, not '%s'"
                    % (type(data).__name__,))


def _index(value, name):
    """PyNumber_Index, with CPython's wording.

    Both arguments here are C integers in CPython's argument clinic, so a
    float is a TypeError rather than a silent truncation.
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
