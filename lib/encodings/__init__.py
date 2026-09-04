"""encodings - the codec search function, and the codecs it finds.

CPython's encodings/ is one module per codec: two hundred files, and nearly
all of them the same module with a different 256-entry table in it.  This is
the one module, and `_tables.py` -- generated from CPython's own package by
src/compiler/gen_encodings.py -- is the tables.

`search_function` is what the registry consults.  It answers for the flat
single-byte codecs, which is the whole charmap family: the cpNNN and
iso8859_N pages, koi8, mac_*, tis_620, the EBCDIC cp037 group.  It answers
None for everything else, which sends the lookup on to the built-in search
function in `_codecs` -- utf-8, ascii, latin-1 and the fixed-width UTF
families, none of which is a table.  The multi-byte CJK codecs and the
transforms (base64_codec, bz2_codec) are neither, and are not here.

Unlike CPython's, this package has no per-codec submodule: `import
encodings.cp1252` does not work, only `codecs.lookup("cp1252")` does.  The
submodules exist in CPython so that each can be imported on its own; here the
table is already in memory once `_tables` is.
"""
import _codecs

from . import aliases as _aliases_mod
from . import _tables

aliases = _aliases_mod.aliases

_cache = {}
_unknown = "--unknown--"
_ASCII = "".join(chr(i) for i in range(128))


def normalize_encoding(encoding):
    """CPython's normalize_encoding: runs of non-alphanumerics become one '_'.

    -> str, the name a codec module would be called.  'iso-8859-15' and
    'ISO 8859 15' both come out 'iso8859_15'; a digit directly after a letter
    keeps them together, which is why 'utf-8' is 'utf_8' and not 'utf8'.
    """
    if isinstance(encoding, bytes):
        encoding = str(encoding, "ascii")
    chars = []
    punct = False
    for c in encoding:
        if c.isalnum() or c == ".":
            if punct and chars:
                chars.append("_")
            chars.append(c)
            punct = False
        else:
            punct = True
    return "".join(chars)


def _table(name):
    """-> the 256-char decoding table for `name`, or None if it has none."""
    t = _tables.TABLES.get(name)
    if t is None:
        return None
    if name in _tables.ASCII_HALF:
        t = _ASCII + t
    return t


class _Charmap:
    """One codec: the pair of closures the registry hands back.

    A method rather than a closure over the table so the encoding map is built
    once, on the first encode, instead of on every call -- charmap_build walks
    all 256 entries, and a decode never needs it.
    """

    def __init__(self, name, decoding_table):
        self.name = name
        self.decoding_table = decoding_table
        self.encoding_table = None

    def encode(self, input, errors="strict"):
        if self.encoding_table is None:
            self.encoding_table = _codecs.charmap_build(self.decoding_table)
        return _codecs.charmap_encode(input, errors, self.encoding_table)

    def decode(self, input, errors="strict"):
        return _codecs.charmap_decode(input, errors, self.decoding_table)


def search_function(encoding):
    """-> a CodecInfo for `encoding`, or None to let another search try.

    The registry calls this with an already-normalised name, but a program may
    call it directly with anything, so normalise again and resolve aliases the
    way CPython's does.
    """
    entry = _cache.get(encoding, _unknown)
    if entry is not _unknown:
        return entry

    norm = normalize_encoding(encoding)
    name = aliases.get(norm.replace(".", "_"), norm)
    table = _table(name)
    if table is None and name != norm:
        table = _table(norm)
    if table is None:
        _cache[encoding] = None
        return None

    codec = _Charmap(name, table)
    entry = _codecs._CodecInfo(codec.encode, codec.decode,
                               _tables.NAMES.get(name, name))
    _cache[encoding] = entry
    return entry
