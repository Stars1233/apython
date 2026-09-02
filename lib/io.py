"""io - the public interface to the streams _io and _pyio implement.

Mirrors CPython's Lib/io.py, and is here for the same reason theirs is: _io
holds the implementations, this holds the public names and the abstract base
classes, which are the _io ones given a metaclass so that register() and
isinstance() work against anything that merely behaves like a stream.
"""

__all__ = ["BlockingIOError", "open", "open_code", "IOBase", "RawIOBase",
           "FileIO", "BytesIO", "StringIO", "BufferedIOBase",
           "BufferedReader", "BufferedWriter", "BufferedRandom", "BufferedRWPair",
           "TextIOBase", "TextIOWrapper", "UnsupportedOperation",
           "IncrementalNewlineDecoder", "DEFAULT_BUFFER_SIZE",
           "SEEK_SET", "SEEK_CUR", "SEEK_END", "text_encoding"]

import _io

from _io import (BlockingIOError, BufferedIOBase, BufferedRWPair,
                 BufferedRandom, BufferedReader, BufferedWriter, BytesIO,
                 DEFAULT_BUFFER_SIZE, FileIO, IOBase,
                 IncrementalNewlineDecoder, RawIOBase, StringIO, TextIOBase,
                 TextIOWrapper, UnsupportedOperation, open, open_code,
                 text_encoding)

SEEK_SET = 0
SEEK_CUR = 1
SEEK_END = 2

UnsupportedOperation.__module__ = "io"

# The concrete classes are not subclasses of the abstract ones -- FileIO comes
# from _io and derives from _io._RawIOBase, not from _pyio.RawIOBase -- so
# isinstance() has to be told.  CPython's io.py does exactly this, and for the
# same reason.
RawIOBase.register(FileIO)
for _klass in (BytesIO, BufferedReader, BufferedWriter, BufferedRandom,
               BufferedRWPair):
    BufferedIOBase.register(_klass)
for _klass in (StringIO, TextIOWrapper):
    TextIOBase.register(_klass)
del _klass
