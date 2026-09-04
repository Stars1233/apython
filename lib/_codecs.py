"""_codecs - the codec registry and the built-in stateless codecs.

CPython's _codecs is a C module.  apython encodes and decodes UTF-8 natively
in str.encode and bytes.decode, so what is left is the registry, the error
handlers, and the handful of single-byte codecs that everything else in
encodings/ is built out of.

The registry is CPython's: search functions are consulted in registration
order and their answers cached under the normalised name.
"""

_search_functions = []
_cache = {}
_error_registry = {}


def _normalize(encoding):
    """CPython's normalizestring: spaces to underscores, lowercased."""
    out = []
    last_was_punct = False
    for ch in encoding:
        if ch in " -":
            ch = "_"
        out.append(ch)
    return "".join(out).lower()


def register(search_function):
    if not callable(search_function):
        raise TypeError("argument must be callable")
    _search_functions.append(search_function)


def unregister(search_function):
    try:
        _search_functions.remove(search_function)
    except ValueError:
        return
    _cache.clear()


_bootstrapped = False


def _bootstrap():
    """Register encodings.search_function, as CPython's registry init does.

    In CPython this happens in C, inside the first lookup: the interpreter
    imports the `encodings` package and registers its search function ahead of
    anything the program registers itself.  Doing it lazily is what keeps the
    circle from closing -- `encodings` imports `codecs`, which imports this
    module.  Without it `_search_functions` stayed empty and every lookup
    raised LookupError, including the utf-8 one that TextIOWrapper starts from.
    """
    global _bootstrapped
    if _bootstrapped:
        return
    _bootstrapped = True
    # Ours goes on the end, so CPython's encodings package wins wherever it is
    # importable: it has the two hundred codecs this does not.
    _search_functions.append(_builtin_search)
    try:
        import encodings
    except ImportError:
        # encodings is CPython's own Python package, not something apython
        # ships.  Without it the registry holds what a program registers, and
        # the handful below.
        return
    _search_functions.insert(0, encodings.search_function)


class _CodecInfo(tuple):
    """codecs.CodecInfo without the codecs module.

    lookup() checks the shape -- a 4-tuple -- and everything downstream of it
    reaches for .encode and .decode by name, so a plain tuple will not do.
    """

    def __new__(cls, encode, decode, name):
        self = tuple.__new__(cls, (encode, decode, None, None))
        self.encode = encode
        self.decode = decode
        self.name = name
        return self


def _builtin_search(name):
    """The codecs this module implements itself, without an encodings package.

    apython does not ship CPython's encodings/ -- two hundred modules, most of
    them a 256-entry table -- so without this the registry was empty and every
    lookup raised, including the ones for the three codecs the interpreter can
    already do.  These are the ones expressible without a table.
    """
    entry = _BUILTIN_CODECS.get(_ALIASES.get(name, name))
    if entry is None:
        return None
    return _CodecInfo(entry[0], entry[1], name)


def lookup(encoding):
    if not isinstance(encoding, str):
        raise TypeError("lookup() argument must be str, not "
                        + type(encoding).__name__)
    name = _normalize(encoding)
    entry = _cache.get(name)
    if entry is not None:
        return entry
    _bootstrap()
    for search in _search_functions:
        entry = search(name)
        if entry is None:
            continue
        # CodecInfo is a 4-tuple subclass, and CPython checks the shape here
        # rather than letting a bad search function poison the cache.
        if not isinstance(entry, tuple) or len(entry) != 4:
            raise TypeError("codec search functions must return 4-tuples")
        _cache[name] = entry
        return entry
    raise LookupError("unknown encoding: " + encoding)


# --- error handlers -------------------------------------------------------
#
# Each takes the exception and returns (replacement, resume position).

def strict_errors(exc):
    raise exc


def ignore_errors(exc):
    return ("", exc.end)


def replace_errors(exc):
    if isinstance(exc, UnicodeEncodeError):
        return ("?", exc.end)
    return ("�", exc.end)


def xmlcharrefreplace_errors(exc):
    parts = []
    for ch in exc.object[exc.start:exc.end]:
        parts.append("&#" + str(ord(ch)) + ";")
    return ("".join(parts), exc.end)


def backslashreplace_errors(exc):
    # A decode error's .object is bytes, and iterating bytes gives ints; an
    # encode error's is a str.  Both reach here, and CPython escapes each byte
    # of the first and each character of the second.
    parts = []
    for item in exc.object[exc.start:exc.end]:
        n = item if isinstance(item, int) else ord(item)
        if n > 0xFFFF:
            parts.append("\\U%08x" % n)
        elif n > 0xFF:
            parts.append("\\u%04x" % n)
        else:
            parts.append("\\x%02x" % n)
    return ("".join(parts), exc.end)


def namereplace_errors(exc):
    return backslashreplace_errors(exc)


def register_error(name, handler):
    if not callable(handler):
        raise TypeError("handler must be callable")
    _error_registry[name] = handler


def lookup_error(name):
    try:
        return _error_registry[name]
    except KeyError:
        raise LookupError("unknown error handler name '" + name + "'")


for _n, _h in (("strict", strict_errors),
               ("ignore", ignore_errors),
               ("replace", replace_errors),
               ("xmlcharrefreplace", xmlcharrefreplace_errors),
               ("backslashreplace", backslashreplace_errors),
               ("namereplace", namereplace_errors),
               ("surrogateescape", ignore_errors),
               ("surrogatepass", strict_errors)):
    _error_registry[_n] = _h
del _n, _h


# --- the stateless codecs -------------------------------------------------
#
# In CPython every function below is a C builtin, and a C builtin assigned to a
# class attribute does not bind: `_buffer_decode = codecs.utf_8_decode` in
# encodings/utf_8.py, and `encode = codecs.utf_8_encode` in its StreamWriter,
# stay plain callables that never see a self.  A Python `def` is a descriptor
# and binds, so those attributes turned into methods and every call arrived
# with one argument too many.  A plain callable object is not a descriptor, so
# wrapping restores the C behaviour exactly where it matters.

class _Builtin:
    """A callable that is not a descriptor, the way a C function is."""

    __slots__ = ("_fn", "__name__")

    def __init__(self, fn):
        self._fn = fn
        self.__name__ = fn.__name__

    def __call__(self, *args, **kw):
        return self._fn(*args, **kw)

    def __repr__(self):
        return "<built-in function %s>" % self.__name__


def _as_bytes(data):
    if isinstance(data, bytes):
        return data
    if isinstance(data, bytearray):
        return bytes(data)
    return bytes(data)


def utf_8_encode(s, errors=None):
    return (s.encode("utf-8", errors or "strict"), len(s))


def _utf_8_decode(data, errors=None, final=False):
    b = _as_bytes(data)
    if not final:
        # An incremental decoder is handed arbitrary chunks, so the tail may
        # be half of a character.  Hold back an incomplete sequence and report
        # how much was actually consumed; the caller keeps the rest and
        # prepends it to the next chunk.
        n = len(b)
        i = n - 1
        limit = n - 4
        while i >= 0 and i > limit:
            c = b[i]
            if c < 0x80:
                break               # a complete one-byte character
            if c >= 0xC0:
                # The start of a sequence.  Its length is written in the
                # leading bits; if the chunk is shorter than that, hold it.
                if c >= 0xF0:
                    need = 4
                elif c >= 0xE0:
                    need = 3
                else:
                    need = 2
                if n - i < need:
                    b = b[:i]
                break
            i -= 1                  # a continuation byte: keep walking back
    errors = errors or "strict"
    if errors in ("strict", "ignore", "replace"):
        return (b.decode("utf-8", errors), len(b))
    return (_utf_8_decode_handled(b, errors), len(b))


def _utf_8_decode_handled(b, errors):
    """UTF-8 decoding for the handlers bytes.decode does not know itself.

    It cannot delegate: the assembly fast path sends an unknown handler name
    back here, so `b.decode("utf-8", errors)` with one of these would be an
    infinite recursion between the two.  It decodes in strict mode instead --
    which the fast path does do -- and drives the handler over each run the
    strict pass rejects, which is what CPython's decoder does too.
    """
    out = []
    i = 0
    n = len(b)
    handler = lookup_error(errors)
    while i < n:
        try:
            out.append(bytes(b[i:]).decode("utf-8", "strict"))
            break
        except UnicodeDecodeError as exc:
            out.append(bytes(b[i:i + exc.start]).decode("utf-8", "strict"))
            start = i + exc.start
            end = i + exc.end
            replacement, resume = handler(
                UnicodeDecodeError("utf-8", bytes(b), start, end, exc.reason))
            if resume < 0:
                resume += n
            if resume <= start or resume > n:
                raise IndexError("position %d from error handler out of bounds"
                                 % resume)
            out.append(replacement)
            i = resume
    return "".join(out)


utf_8_decode = _Builtin(_utf_8_decode)
utf_8_encode = _Builtin(utf_8_encode)


def _encode_charset(codec, s, errors, limit, reason):
    """Encode a str whose characters must all be below `limit`.

    This is where the error handlers actually run.  CPython hands a handler
    the whole RUN of consecutive unencodable characters and lets it say where
    to resume, which is what makes backslashreplace produce one escape per
    character and xmlcharrefreplace one entity per character rather than one
    for the run.  The str.encode fast path in assembly knows only strict,
    ignore and replace; anything else -- and every strict failure, so that the
    exception carries its five fields -- arrives here.
    """
    errors = errors or "strict"
    out = bytearray()
    i = 0
    n = len(s)
    while i < n:
        o = ord(s[i])
        if o < limit:
            out.append(o)
            i += 1
            continue
        j = i
        while j < n and ord(s[j]) >= limit:
            j += 1
        exc = UnicodeEncodeError(codec, s, i, j, reason)
        replacement, resume = lookup_error(errors)(exc)
        if resume < 0:
            resume += n
        if resume <= i or resume > n:
            raise IndexError("position %d from error handler out of bounds"
                             % resume)
        if isinstance(replacement, (bytes, bytearray)):
            out.extend(replacement)
        else:
            for ch in replacement:
                v = ord(ch)
                if v >= limit:
                    raise UnicodeEncodeError(codec, s, i, j, reason)
                out.append(v)
        i = resume
    return (bytes(out), n)


def _decode_charset(codec, data, errors, limit, reason):
    """The mirror image: every byte at or above `limit` is undecodable."""
    b = _as_bytes(data)
    errors = errors or "strict"
    out = []
    i = 0
    n = len(b)
    while i < n:
        if b[i] < limit:
            out.append(chr(b[i]))
            i += 1
            continue
        j = i
        while j < n and b[j] >= limit:
            j += 1
        exc = UnicodeDecodeError(codec, b, i, j, reason)
        replacement, resume = lookup_error(errors)(exc)
        if resume < 0:
            resume += n
        if resume <= i or resume > n:
            raise IndexError("position %d from error handler out of bounds"
                             % resume)
        out.append(replacement)
        i = resume
    return ("".join(out), n)


def ascii_encode(s, errors=None):
    return _encode_charset("ascii", s, errors, 128,
                           "ordinal not in range(128)")


def ascii_decode(data, errors=None, final=False):
    return _decode_charset("ascii", data, errors, 128,
                           "ordinal not in range(128)")


def latin_1_encode(s, errors=None):
    return _encode_charset("latin-1", s, errors, 256,
                           "ordinal not in range(256)")


def latin_1_decode(data, errors=None, final=False):
    b = _as_bytes(data)
    return ("".join(chr(x) for x in b), len(b))


# The names encodings/ uses for the single-byte codecs.
iso8859_1_encode = latin_1_encode
iso8859_1_decode = latin_1_decode


def charmap_build(decoding_table):
    """The str -> {codepoint: byte} mapping an encoding table is used through."""
    table = {}
    for i, ch in enumerate(decoding_table):
        if ch != "￾":
            table[ord(ch)] = i
    return table


def charmap_decode(data, errors=None, mapping=None):
    b = _as_bytes(data)
    if mapping is None:
        return latin_1_decode(b, errors)
    out = []
    for byte in b:
        ch = mapping[byte] if not isinstance(mapping, dict) else mapping.get(byte)
        if ch is None or ch == "￾":
            if errors in (None, "strict"):
                raise UnicodeDecodeError(
                    "charmap", b, 0, len(b), "character maps to <undefined>")
            continue
        out.append(ch)
    return ("".join(out), len(b))


def charmap_encode(s, errors=None, mapping=None):
    if mapping is None:
        return latin_1_encode(s, errors)
    out = []
    for ch in s:
        v = mapping.get(ord(ch)) if isinstance(mapping, dict) else None
        if v is None:
            if errors in (None, "strict"):
                raise UnicodeEncodeError(
                    "charmap", s, 0, len(s), "character maps to <undefined>")
            continue
        out.append(v)
    return (bytes(out), len(s))


def readbuffer_encode(data, errors=None):
    if isinstance(data, str):
        return (data.encode(), len(data))
    b = _as_bytes(data)
    return (b, len(b))


def escape_encode(data, errors=None):
    b = _as_bytes(data)
    out = []
    for byte in b:
        if byte == 92:
            out.append("\\\\")
        elif byte == 10:
            out.append("\\n")
        elif byte == 13:
            out.append("\\r")
        elif byte == 9:
            out.append("\\t")
        elif byte < 32 or byte >= 127:
            out.append("\\x%02x" % byte)
        else:
            out.append(chr(byte))
    return ("".join(out).encode(), len(b))


def escape_decode(data, errors=None):
    if isinstance(data, str):
        b = data.encode()
    else:
        b = _as_bytes(data)
    out = bytearray()
    i = 0
    n = len(b)
    while i < n:
        c = b[i]
        if c != 92:
            out.append(c)
            i += 1
            continue
        i += 1
        if i >= n:
            out.append(92)
            break
        c = b[i]
        i += 1
        if c == 110:
            out.append(10)
        elif c == 114:
            out.append(13)
        elif c == 116:
            out.append(9)
        elif c == 92:
            out.append(92)
        elif c == 120 and i + 1 < n:
            out.append(int(chr(b[i]) + chr(b[i + 1]), 16))
            i += 2
        else:
            out.append(92)
            out.append(c)
    return (bytes(out), n)


unicode_escape_encode = escape_encode
unicode_escape_decode = escape_decode
raw_unicode_escape_encode = escape_encode
raw_unicode_escape_decode = escape_decode

# Every codec function encodings/*.py may assign to a class attribute has to
# be non-binding, not just utf-8's: ascii.py, latin_1.py and every
# charmap-based module do `encode = codecs.<name>_encode` in their StreamWriter
# and `decode = codecs.<name>_decode` in their StreamReader.
for _n in ("ascii_encode", "ascii_decode", "latin_1_encode", "latin_1_decode",
           "charmap_decode", "charmap_encode", "escape_encode", "escape_decode"):
    globals()[_n] = _Builtin(globals()[_n])
del _n

iso8859_1_encode = latin_1_encode
iso8859_1_decode = latin_1_decode
unicode_escape_encode = escape_encode
unicode_escape_decode = escape_decode
raw_unicode_escape_encode = escape_encode
raw_unicode_escape_decode = escape_decode


def encode(obj, encoding="utf-8", errors="strict"):
    return lookup(encoding).encode(obj, errors)[0]


def decode(obj, encoding="utf-8", errors="strict"):
    return lookup(encoding).decode(obj, errors)[0]


# --- the codecs the built-in search function offers -------------------------
#
# CPython's encodings/ is two hundred modules, most of them a 256-entry table.
# These are the ones this module can express without one, which is enough for
# the interpreter to bootstrap its own I/O and for the common cases of
# str.encode: the three the assembly already does, the BOM'd form of utf-8,
# and the fixed-width UTF families.

def utf_8_sig_encode(s, errors=None):
    return (b"\xef\xbb\xbf" + s.encode("utf-8", errors or "strict"), len(s))


def utf_8_sig_decode(data, errors=None, final=False):
    b = _as_bytes(data)
    if b[:3] == b"\xef\xbb\xbf":
        b = b[3:]
    return (b.decode("utf-8", errors or "strict"), len(b))


def _utf_n_encode(s, errors, width, big):
    out = bytearray()
    for ch in s:
        n = ord(ch)
        if width == 2:
            if n > 0xFFFF:
                n -= 0x10000
                hi = 0xD800 + (n >> 10)
                lo = 0xDC00 + (n & 0x3FF)
                units = (hi, lo)
            else:
                units = (n,)
            for u in units:
                if big:
                    out.append(u >> 8)
                    out.append(u & 0xFF)
                else:
                    out.append(u & 0xFF)
                    out.append(u >> 8)
        else:
            b = [n & 0xFF, (n >> 8) & 0xFF, (n >> 16) & 0xFF, (n >> 24) & 0xFF]
            if big:
                b.reverse()
            out.extend(b)
    return (bytes(out), len(s))


def _utf_n_decode(data, errors, width, big, codec):
    b = _as_bytes(data)
    if len(b) % width:
        raise UnicodeDecodeError(codec, b, len(b) - len(b) % width, len(b),
                                 "truncated data")
    out = []
    i = 0
    while i < len(b):
        chunk = b[i:i + width]
        if big:
            chunk = bytes(reversed(chunk))
        n = 0
        for k in range(width - 1, -1, -1):
            n = (n << 8) | chunk[k]
        if width == 2 and 0xD800 <= n < 0xDC00 and i + 4 <= len(b):
            nxt = b[i + 2:i + 4]
            if big:
                nxt = bytes(reversed(nxt))
            lo = nxt[0] | (nxt[1] << 8)
            if 0xDC00 <= lo < 0xE000:
                n = 0x10000 + ((n - 0xD800) << 10) + (lo - 0xDC00)
                i += 2
        out.append(chr(n))
        i += width
    return ("".join(out), len(b))


def utf_16_le_encode(s, errors=None):
    return _utf_n_encode(s, errors, 2, False)


def utf_16_be_encode(s, errors=None):
    return _utf_n_encode(s, errors, 2, True)


def utf_16_le_decode(data, errors=None, final=False):
    return _utf_n_decode(data, errors, 2, False, "utf-16-le")


def utf_16_be_decode(data, errors=None, final=False):
    return _utf_n_decode(data, errors, 2, True, "utf-16-be")


def utf_16_encode(s, errors=None):
    body, n = _utf_n_encode(s, errors, 2, False)
    return (b"\xff\xfe" + body, n)


def utf_16_decode(data, errors=None, final=False):
    b = _as_bytes(data)
    if b[:2] == b"\xff\xfe":
        return _utf_n_decode(b[2:], errors, 2, False, "utf-16")
    if b[:2] == b"\xfe\xff":
        return _utf_n_decode(b[2:], errors, 2, True, "utf-16")
    return _utf_n_decode(b, errors, 2, False, "utf-16")


def utf_32_le_encode(s, errors=None):
    return _utf_n_encode(s, errors, 4, False)


def utf_32_be_encode(s, errors=None):
    return _utf_n_encode(s, errors, 4, True)


def utf_32_le_decode(data, errors=None, final=False):
    return _utf_n_decode(data, errors, 4, False, "utf-32-le")


def utf_32_be_decode(data, errors=None, final=False):
    return _utf_n_decode(data, errors, 4, True, "utf-32-be")


def utf_32_encode(s, errors=None):
    body, n = _utf_n_encode(s, errors, 4, False)
    return (b"\xff\xfe\x00\x00" + body, n)


def utf_32_decode(data, errors=None, final=False):
    b = _as_bytes(data)
    if b[:4] == b"\xff\xfe\x00\x00":
        return _utf_n_decode(b[4:], errors, 4, False, "utf-32")
    if b[:4] == b"\x00\x00\xfe\xff":
        return _utf_n_decode(b[4:], errors, 4, True, "utf-32")
    return _utf_n_decode(b, errors, 4, False, "utf-32")


_BUILTIN_CODECS = {
    "utf_8": (utf_8_encode, utf_8_decode),
    "utf_8_sig": (utf_8_sig_encode, utf_8_sig_decode),
    "ascii": (ascii_encode, ascii_decode),
    "latin_1": (latin_1_encode, latin_1_decode),
    "utf_16": (utf_16_encode, utf_16_decode),
    "utf_16_le": (utf_16_le_encode, utf_16_le_decode),
    "utf_16_be": (utf_16_be_encode, utf_16_be_decode),
    "utf_32": (utf_32_encode, utf_32_decode),
    "utf_32_le": (utf_32_le_encode, utf_32_le_decode),
    "utf_32_be": (utf_32_be_encode, utf_32_be_decode),
    "unicode_escape": (unicode_escape_encode, unicode_escape_decode),
    "raw_unicode_escape": (raw_unicode_escape_encode,
                           raw_unicode_escape_decode),
}

# The aliases CPython's encodings.aliases carries for those codecs.
_ALIASES = {
    "u8": "utf_8", "utf": "utf_8", "utf8": "utf_8", "cp65001": "utf_8",
    "utf8_ucs2": "utf_8", "utf8_ucs4": "utf_8",
    "utf_8_sig": "utf_8_sig",
    "us_ascii": "ascii", "us": "ascii", "ansi_x3.4_1968": "ascii",
    "ansi_x3_4_1968": "ascii", "646": "ascii", "ibm367": "ascii",
    "latin": "latin_1", "latin1": "latin_1", "l1": "latin_1",
    "iso_8859_1": "latin_1", "iso8859_1": "latin_1", "8859": "latin_1",
    "cp819": "latin_1", "ibm819": "latin_1", "iso_ir_100": "latin_1",
    "iso_8859_1_1987": "latin_1",
    "u16": "utf_16", "utf16": "utf_16",
    "unicodelittleunmarked": "utf_16_le", "utf_16le": "utf_16_le",
    "unicodebigunmarked": "utf_16_be", "utf_16be": "utf_16_be",
    "u32": "utf_32", "utf32": "utf_32",
    "utf_32le": "utf_32_le", "utf_32be": "utf_32_be",
    "unicode_internal": "utf_32_le",
    "unicodeescape": "unicode_escape",
    "rawunicodeescape": "raw_unicode_escape",
}
