"""zlib: the module surface over the _zlibcore stream.

The same split lib/_io.py and lib/_socket.py use.  _zlibcore owns the
z_stream, the growing output buffer and the handle table -- the parts that
have to be written in assembly over libz -- and everything a program touches
is here: the Compress and Decompress objects, the constants, `zlib.error`,
the keyword arguments and every default.  So each core call takes a fixed
number of positional arguments and answers with a bytes, an int or a tuple.

The core reports a failure as ValueError, because it has no way to raise a
class defined in Python.  Every entry point here turns that into
`zlib.error`, which is what a program catches.
"""

import _zlibcore

__all__ = [
    "adler32", "compress", "compressobj", "crc32", "decompress",
    "decompressobj", "error", "DEFLATED", "DEF_BUF_SIZE", "DEF_MEM_LEVEL",
    "MAX_WBITS", "ZLIB_VERSION", "ZLIB_RUNTIME_VERSION",
    "Z_BEST_COMPRESSION", "Z_BEST_SPEED", "Z_BLOCK", "Z_DEFAULT_COMPRESSION",
    "Z_DEFAULT_STRATEGY", "Z_FILTERED", "Z_FINISH", "Z_FIXED", "Z_FULL_FLUSH",
    "Z_HUFFMAN_ONLY", "Z_NO_COMPRESSION", "Z_NO_FLUSH", "Z_PARTIAL_FLUSH",
    "Z_RLE", "Z_SYNC_FLUSH", "Z_TREES",
]

# Private, and named in __all__ nowhere -- but _compression.DecompressReader
# imports it by name, so gzip and tarfile need it to exist.


class error(Exception):
    """Raised for a corrupt stream, or for a stream used after its end."""


MAX_WBITS = 15
DEFLATED = 8
DEF_MEM_LEVEL = 8
DEF_BUF_SIZE = 16384

Z_NO_COMPRESSION = 0
Z_BEST_SPEED = 1
Z_BEST_COMPRESSION = 9
Z_DEFAULT_COMPRESSION = -1

Z_FILTERED = 1
Z_HUFFMAN_ONLY = 2
Z_RLE = 3
Z_FIXED = 4
Z_DEFAULT_STRATEGY = 0

Z_NO_FLUSH = 0
Z_PARTIAL_FLUSH = 1
Z_SYNC_FLUSH = 2
Z_FULL_FLUSH = 3
Z_FINISH = 4
Z_BLOCK = 5
Z_TREES = 6

ZLIB_VERSION = _zlibcore.ZLIB_VERSION
ZLIB_RUNTIME_VERSION = _zlibcore.ZLIB_VERSION

_DEFLATE = 0
_INFLATE = 1


def crc32(data, value=0):
    """The CRC-32 of data, continuing from value.

    libz's own, so it agrees with every other zlib: a .zip records the crc32
    of what it holds, and a reader that computes a different one refuses the
    file.
    """
    try:
        return _zlibcore.crc32(data, value & 0xffffffff)
    except ValueError as e:
        raise error(str(e)) from None


def adler32(data, value=1):
    """The Adler-32 checksum of data, continuing from value."""
    try:
        return _zlibcore.adler32(data, value & 0xffffffff)
    except ValueError as e:
        raise error(str(e)) from None


class _Stream:
    """What Compress and Decompress share: one core handle, closed once."""

    def __init__(self, mode, level, wbits, memLevel, strategy):
        self._handle = -1
        try:
            self._handle = _zlibcore.stream_new(
                mode, level, wbits, memLevel, strategy)
        except ValueError as e:
            raise error(str(e)) from None

    def _feed(self, data, flush, max_length=0):
        if self._handle < 0:
            raise error("stream is already finished")
        try:
            return _zlibcore.stream_feed(self._handle, data, flush, max_length)
        except ValueError as e:
            raise error(str(e)) from None

    def _close(self):
        h = self._handle
        self._handle = -1
        if h >= 0:
            _zlibcore.stream_free(h)

    def __del__(self):
        # libz's deflate window is 256 KB at the default settings, so a loop
        # over a thousand files that dropped its compressobjs would notice.
        try:
            self._close()
        except Exception:
            pass


class Compress(_Stream):
    """What compressobj() returns."""

    def __init__(self, level=Z_DEFAULT_COMPRESSION, method=DEFLATED,
                 wbits=MAX_WBITS, memLevel=DEF_MEM_LEVEL,
                 strategy=Z_DEFAULT_STRATEGY, zdict=None):
        if zdict is not None:
            raise error("zdict is not supported")
        if method != DEFLATED:
            # libz's own refusal, and CPython's: a ValueError, not zlib.error.
            raise ValueError("Invalid initialization option")
        super().__init__(_DEFLATE, level, wbits, memLevel, strategy)
        self._flushed = False

    def compress(self, data):
        if self._flushed:
            raise error("compressor has already been flushed")
        return self._feed(data, Z_NO_FLUSH)

    def flush(self, mode=Z_FINISH):
        if self._flushed:
            raise error("compressor has already been flushed")
        out = self._feed(b"", mode)
        if mode == Z_FINISH:
            self._flushed = True
            self._close()
        return out

    def copy(self):
        # libz has deflateCopy, but the handle table has no way to hand back
        # a second handle onto a copied stream without a core entry point of
        # its own.  Nothing in the stdlib's compression stack copies a
        # compressor; refusing says so rather than answering wrongly.
        raise error("Compress.copy() is not supported")


class Decompress(_Stream):
    """What decompressobj() returns."""

    def __init__(self, wbits=MAX_WBITS, zdict=b""):
        if zdict:
            raise error("zdict is not supported")
        super().__init__(_INFLATE, 0, wbits, DEF_MEM_LEVEL,
                         Z_DEFAULT_STRATEGY)
        self.unused_data = b""
        self.unconsumed_tail = b""
        self.eof = False

    def _refresh(self):
        eof, tail, unused = _zlibcore.stream_state(self._handle)
        self.eof = eof
        self.unconsumed_tail = tail
        if unused:
            self.unused_data += unused

    def decompress(self, data, max_length=0):
        out = self._feed(data, Z_NO_FLUSH, max_length)
        self._refresh()
        return out

    def flush(self, length=DEF_BUF_SIZE):
        if self._handle < 0:
            return b""
        out = self._feed(b"", Z_FINISH)
        self._refresh()
        self._close()
        return out

    def copy(self):
        raise error("Decompress.copy() is not supported")


class _ZlibDecompressor(_Stream):
    """The one-shot-per-block decompressor gzip and tarfile read through.

    CPython added it in 3.11 and it is private, but _compression.DecompressReader
    is written against it and nothing else -- so `tarfile.open(..., "r:gz")`
    needs it by name.  It differs from Decompress in what it promises about
    its input: `needs_input` says whether the caller must read more, which is
    how the reader knows when to go back to the file.
    """

    def __init__(self, wbits=MAX_WBITS, zdict=b""):
        if zdict:
            raise error("zdict is not supported")
        super().__init__(_INFLATE, 0, wbits, DEF_MEM_LEVEL,
                         Z_DEFAULT_STRATEGY)
        self.eof = False
        self.needs_input = True
        self.unused_data = b""

    def decompress(self, data, max_length=-1):
        if max_length < 0:
            max_length = 0
        out = self._feed(data, Z_NO_FLUSH, max_length)
        eof, tail, unused = _zlibcore.stream_state(self._handle)
        self.eof = eof
        if unused:
            self.unused_data += unused
        # Buffered input left over means the caller must not read more yet.
        self.needs_input = not tail
        return out


def compressobj(level=Z_DEFAULT_COMPRESSION, method=DEFLATED,
                wbits=MAX_WBITS, memLevel=DEF_MEM_LEVEL,
                strategy=Z_DEFAULT_STRATEGY, zdict=None):
    """A streaming compressor.

    wbits carries zlib's three conventions in one number: positive is a zlib
    wrapper, negative is raw deflate -- which is what zipfile and tarfile ask
    for -- and 16 added on top is a gzip wrapper.
    """
    return Compress(level, method, wbits, memLevel, strategy, zdict)


def decompressobj(wbits=MAX_WBITS, zdict=b""):
    """A streaming decompressor.  wbits is read as it is for compressobj()."""
    return Decompress(wbits, zdict)


def compress(data, /, level=Z_DEFAULT_COMPRESSION, wbits=MAX_WBITS):
    """Compress data in one call."""
    c = Compress(level, DEFLATED, wbits, DEF_MEM_LEVEL, Z_DEFAULT_STRATEGY)
    return c.compress(data) + c.flush()


def decompress(data, /, wbits=MAX_WBITS, bufsize=DEF_BUF_SIZE):
    """Decompress data in one call.

    bufsize is accepted and ignored: the core grows its output buffer by
    doubling and does not need to be told how large the answer will be.
    """
    d = Decompress(wbits)
    out = d.decompress(data)
    out += d.flush()
    if not d.eof:
        raise error("Error -5 while decompressing data: incomplete or "
                    "truncated stream")
    return out
