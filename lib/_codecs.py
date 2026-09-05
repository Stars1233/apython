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
    circle from closing -- `encodings` imports this module.  Without it
    `_search_functions` stayed empty and every lookup raised LookupError,
    including the utf-8 one that TextIOWrapper starts from.
    """
    global _bootstrapped
    if _bootstrapped:
        return
    _bootstrapped = True
    # `encodings` answers only for the codecs that are a 256-entry table, and
    # returns None for the rest, so the order between the two is a matter of
    # which is asked first and not of which wins.  It goes in front because
    # that is where CPython's registry init puts it.
    _search_functions.append(_builtin_search)
    try:
        import encodings
    except ImportError:
        # Not fatal: without the package the registry still holds whatever a
        # program registers, plus the tableless codecs below.
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


class _Whole:
    """A stateless decoder called on a complete bytes object.

    The decoders here take a `final` flag, because an incremental decoder
    hands them arbitrary chunks and a truncated character at the end of one is
    not an error.  A CodecInfo's `decode` is the other case: it is given the
    whole input, so a truncated character at the end of THAT is an error, and
    CPython's registered decoders pass final=True for exactly this reason --
    `b"+A".decode("utf-7")` raises where `_codecs.utf_7_decode(b"+A")` does
    not.  Not a descriptor, for the reason _Builtin is not.
    """

    __slots__ = ("_fn",)

    def __init__(self, fn):
        self._fn = fn

    def __call__(self, data, errors=None):
        return self._fn(data, errors, True)


def _builtin_search(name):
    """The codecs this module implements itself: the ones that are not a table.

    utf-8 and its BOM'd form, ascii, latin-1, the six fixed-width UTF-16 and
    UTF-32 forms, utf-7, and the two escape codecs.  Everything else CPython
    ships that this tree has is a 256-entry decoding table, and those live in
    `encodings/`, which is consulted first.
    """
    key = _ALIASES.get(name, name)
    entry = _BUILTIN_CODECS.get(key)
    if entry is None:
        return None
    # The name a CodecInfo reports is the one CPython's own encodings module
    # registers -- 'utf-8', not the 'utf_8' the lookup normalised to -- and
    # TextIOWrapper.encoding is read from it.
    return _CodecInfo(entry[0], _Whole(entry[1]),
                      _BUILTIN_NAMES.get(key, key))


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


def _charmap_get(mapping, byte):
    """-> the character byte `byte` decodes to, or None if it decodes to none.

    A decoding table is a 256-character str, and CPython marks a hole in one
    with U+FFFE; a mapping may also be a dict, where a hole is a missing key
    or an explicit None.
    """
    if isinstance(mapping, str):
        if byte >= len(mapping):
            return None
        ch = mapping[byte]
    else:
        ch = mapping.get(byte)
    if ch is None or ch == "\ufffe":
        return None
    return ch


def charmap_decode(data, errors=None, mapping=None):
    """-> (str, bytes consumed) for `data` read through `mapping`.

    Unlike the encode direction, which hands the handler a whole run, CPython's
    charmap decoder reports ONE byte at a time: `b"a\x81\x8dz"` under 'replace'
    gives two replacement characters, not one.  A handler may still say where
    to resume and skip the rest of a run itself.
    """
    b = _as_bytes(data)
    if mapping is None:
        return latin_1_decode(b, errors)
    errors = errors or "strict"
    out = []
    i = 0
    n = len(b)
    while i < n:
        ch = _charmap_get(mapping, b[i])
        if ch is not None:
            out.append(ch)
            i += 1
            continue
        j = i + 1
        exc = UnicodeDecodeError("charmap", b, i, j,
                                 "character maps to <undefined>")
        replacement, resume = lookup_error(errors)(exc)
        if resume < 0:
            resume += n
        if resume <= i or resume > n:
            raise IndexError("position %d from error handler out of bounds"
                             % resume)
        out.append(replacement)
        i = resume
    return ("".join(out), n)


def charmap_encode(s, errors=None, mapping=None):
    """-> (bytes, characters consumed) for `s` written through `mapping`.

    `mapping` is the {code point: byte} dict charmap_build makes.  A handler's
    str replacement goes back through the same mapping, so a character the
    replacement cannot spell either is a hard failure rather than a silent
    hole.
    """
    if mapping is None:
        return latin_1_encode(s, errors)
    errors = errors or "strict"
    out = bytearray()
    i = 0
    n = len(s)
    while i < n:
        v = mapping.get(ord(s[i]))
        if v is not None:
            out.append(v)
            i += 1
            continue
        j = i
        while j < n and mapping.get(ord(s[j])) is None:
            j += 1
        exc = UnicodeEncodeError("charmap", s, i, j,
                                 "character maps to <undefined>")
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
                v = mapping.get(ord(ch))
                if v is None:
                    raise UnicodeEncodeError("charmap", s, i, j,
                                             "character maps to <undefined>")
                out.append(v)
        i = resume
    return (bytes(out), n)


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


# --- utf-7 -----------------------------------------------------------------
#
# RFC 2152: ASCII passes through, everything else goes into a modified base64
# run between '+' and '-'.  The run is over UTF-16 code UNITS, so a character
# above the BMP becomes a surrogate pair inside it -- which is why the
# accumulator here is 16 bits wide and not 21.

_U7_B64 = ("ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/")
_U7_FROM = {}
for _i, _c in enumerate(_U7_B64):
    _U7_FROM[ord(_c)] = _i
del _i, _c

# Everything below 128 except these is written out as itself.  The set is
# CPython's: RFC 2152's set D and set O, plus tab, newline and return, less
# '+' (which starts a run), '\\', '~' and the C0 controls.
_U7_INDIRECT = frozenset(range(0, 9)) | frozenset((11, 12)) | \
    frozenset(range(14, 32)) | frozenset((ord("+"), ord("\\"),
                                          ord("~"), 127))


def _u7_direct(o):
    """-> True if code point `o` is written to a utf-7 stream as itself."""
    return o < 128 and o not in _U7_INDIRECT


def utf_7_encode(s, errors=None):
    """-> (bytes, characters consumed): `s` as RFC 2152 modified UTF-7.

    The closing '-' is written only when the character that follows the run
    would otherwise be read as part of it -- a base64 character, or a '-'.
    Anything else ends the run implicitly, which is why 'a\u2603]' comes out
    '+JgM]' and not '+JgM-]'.
    """
    out = bytearray()
    bits = 0
    acc = 0
    shifted = False
    for ch in s:
        o = ord(ch)
        if shifted:
            if _u7_direct(o):
                if bits:
                    out.append(ord(_U7_B64[(acc << (6 - bits)) & 0x3F]))
                    bits = 0
                    acc = 0
                shifted = False
                if o == 0x2D or chr(o) in _U7_B64:
                    out.append(0x2D)
                out.append(o)
                continue
        elif o == 0x2B:               # '+' outside a run is written '+-'
            out += b"+-"
            continue
        elif _u7_direct(o):
            out.append(o)
            continue
        else:
            out.append(0x2B)
            shifted = True
            bits = 0
            acc = 0
        units = (o,)
        if o > 0xFFFF:
            o -= 0x10000
            units = (0xD800 + (o >> 10), 0xDC00 + (o & 0x3FF))
        for u in units:
            acc = (acc << 16) | u
            bits += 16
            while bits >= 6:
                bits -= 6
                out.append(ord(_U7_B64[(acc >> bits) & 0x3F]))
            acc &= (1 << bits) - 1
    if shifted:
        if bits:
            out.append(ord(_U7_B64[(acc << (6 - bits)) & 0x3F]))
        out.append(0x2D)
    return (bytes(out), len(s))


def utf_7_decode(data, errors=None, final=False):
    """-> (str, bytes consumed) for a UTF-7 stream.

    A run that is still open when the data ends is an error only when `final`;
    otherwise the caller keeps the tail, so report consuming only up to where
    the run began.  A run that ends badly reports from the '+' that opened it
    through the character that closed it, which is CPython's span and not the
    one byte that noticed.
    """
    b = _as_bytes(data)
    errors = errors or "strict"
    out = []
    n = len(b)
    i = 0
    shifted = False
    shift_start = 0
    seen = 0
    bits = 0
    acc = 0
    surrogate = 0

    def fail(start, end, reason):
        """-> where to resume, having appended whatever the handler gave."""
        exc = UnicodeDecodeError("utf7", b, start, end, reason)
        replacement, resume = lookup_error(errors)(exc)
        if resume < 0:
            resume += n
        if resume < 0 or resume > n:
            raise IndexError("position %d from error handler out of bounds"
                             % resume)
        out.append(replacement)
        return resume

    while i < n:
        c = b[i]
        if shifted:
            if c in _U7_FROM:
                acc = (acc << 6) | _U7_FROM[c]
                bits += 6
                seen += 1
                i += 1
                if bits >= 16:
                    bits -= 16
                    unit = (acc >> bits) & 0xFFFF
                    acc &= (1 << bits) - 1
                    if surrogate:
                        if 0xDC00 <= unit <= 0xDFFF:
                            out.append(chr(0x10000
                                           + ((surrogate - 0xD800) << 10)
                                           + (unit - 0xDC00)))
                            surrogate = 0
                            continue
                        out.append(chr(surrogate))
                        surrogate = 0
                    if 0xD800 <= unit <= 0xDBFF:
                        surrogate = unit
                    else:
                        out.append(chr(unit))
                continue
            # Anything else closes the run.  '-' is absorbed; anything else is
            # read again outside it -- unless the run was bad, in which case
            # the span reported covers it.
            shifted = False
            if bits >= 6:
                i = fail(shift_start, i + 1,
                         "partial character in shift sequence")
            elif bits and acc:
                i = fail(shift_start, i + 1,
                         "non-zero padding bits in shift sequence")
            elif not seen:
                i = fail(shift_start, i + 1, "ill-formed sequence")
            else:
                if surrogate and c < 128:
                    out.append(chr(surrogate))
                if c == 0x2D:
                    i += 1
            surrogate = 0
            bits = 0
            acc = 0
            continue
        if c == 0x2B:
            if i + 1 < n and b[i + 1] == 0x2D:
                out.append("+")
                i += 2
                continue
            shifted = True
            shift_start = i
            seen = 0
            bits = 0
            acc = 0
            surrogate = 0
            i += 1
            continue
        if c < 128:
            out.append(chr(c))
            i += 1
            continue
        i = fail(i, i + 1, "unexpected special character")

    if shifted:
        if not final:
            return ("".join(out), shift_start)
        if surrogate or bits >= 6 or (bits and acc):
            fail(shift_start, n, "unterminated shift sequence")
    return ("".join(out), n)



def encode(obj, encoding="utf-8", errors="strict"):
    return lookup(encoding).encode(obj, errors)[0]


def decode(obj, encoding="utf-8", errors="strict"):
    return lookup(encoding).decode(obj, errors)[0]


# --- the codecs the built-in search function offers -------------------------
#
# The ones that are not a 256-entry table, which is what makes them belong
# here rather than in `encodings/`: the three the assembly already does, the
# BOM'd form of utf-8, the fixed-width UTF families, and utf-7, which is a
# state machine over modified base64 rather than a mapping.

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


def _utf_n_decode(data, errors, width, big, codec, offset=0):
    """Decode UTF-16 or UTF-32, running the error handler on every refusal.

    Only a truncated tail was detected at all, and it RAISED rather than
    consulting `errors` -- so `b"\x00".decode("utf-16", "replace")` was a
    UnicodeDecodeError where CPython answers a replacement character, and a
    lone surrogate or a code point past 0x10FFFF was silently turned into a
    character no str should hold.

    `offset` is how many bytes of BOM were taken before this was called: the
    positions in the exception, and the count returned, are of the WHOLE
    input, which is what the caller passed and what a handler will index.
    """
    b = bytes(_as_bytes(data))
    errors = errors or "strict"
    out = []
    i = 0
    n = len(b)
    # The exception carries the bytes the CALLER passed, BOM included, so a
    # handler that indexes exc.object sees what it was given.
    whole = b if not offset else _bom_prefix(width, big) + b

    def fail(start, end, reason):
        """The handler decides; a strict one raises out of here."""
        exc = UnicodeDecodeError(codec, whole, start + offset, end + offset,
                                 reason)
        replacement, resume = lookup_error(errors)(exc)
        if resume < 0:
            resume += n + offset
        resume -= offset
        if resume <= start or resume > n:
            raise IndexError("position %d from error handler out of bounds"
                             % (resume + offset,))
        return replacement, resume

    while i < n:
        if n - i < width:
            replacement, i = fail(i, n, "truncated data")
            out.append(replacement)
            continue
        chunk = b[i:i + width]
        if big:
            chunk = bytes(reversed(chunk))
        v = 0
        for k in range(width - 1, -1, -1):
            v = (v << 8) | chunk[k]
        if width == 2:
            if 0xD800 <= v < 0xDC00:
                if i + 4 > n:
                    replacement, i = fail(i, i + 2, "unexpected end of data")
                    out.append(replacement)
                    continue
                nxt = b[i + 2:i + 4]
                if big:
                    nxt = bytes(reversed(nxt))
                lo = nxt[0] | (nxt[1] << 8)
                if not (0xDC00 <= lo < 0xE000):
                    replacement, i = fail(i, i + 2,
                                          "illegal UTF-16 surrogate")
                    out.append(replacement)
                    continue
                v = 0x10000 + ((v - 0xD800) << 10) + (lo - 0xDC00)
                out.append(chr(v))
                i += 4
                continue
            if 0xDC00 <= v < 0xE000:
                replacement, i = fail(i, i + 2, "illegal encoding")
                out.append(replacement)
                continue
        else:
            if v > 0x10FFFF:
                replacement, i = fail(i, i + 4,
                                      "code point not in range(0x110000)")
                out.append(replacement)
                continue
            if 0xD800 <= v < 0xE000:
                replacement, i = fail(
                    i, i + 4,
                    "code point in surrogate code point "
                    "range(0xd800, 0xe000)")
                out.append(replacement)
                continue
        out.append(chr(v))
        i += width
    return ("".join(out), n + offset)


def _bom_prefix(width, big):
    if width == 2:
        return b"\xfe\xff" if big else b"\xff\xfe"
    return b"\x00\x00\xfe\xff" if big else b"\xff\xfe\x00\x00"


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
    # The exception names the endianness the BOM resolved to -- CPython
    # reports 'utf-16-le' for a little-endian stream, not 'utf-16'.
    b = _as_bytes(data)
    if b[:2] == b"\xff\xfe":
        return _utf_n_decode(b[2:], errors, 2, False, "utf-16-le", 2)
    if b[:2] == b"\xfe\xff":
        return _utf_n_decode(b[2:], errors, 2, True, "utf-16-be", 2)
    return _utf_n_decode(b, errors, 2, False, "utf-16-le")


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
        return _utf_n_decode(b[4:], errors, 4, False, "utf-32-le", 4)
    if b[:4] == b"\x00\x00\xfe\xff":
        return _utf_n_decode(b[4:], errors, 4, True, "utf-32-be", 4)
    return _utf_n_decode(b, errors, 4, False, "utf-32-le")


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
    "utf_7": (utf_7_encode, utf_7_decode),
    "unicode_escape": (unicode_escape_encode, unicode_escape_decode),
    "raw_unicode_escape": (raw_unicode_escape_encode,
                           raw_unicode_escape_decode),
}

# What each of them answers to.  CPython's encodings/latin_1.py registers
# itself as 'iso8859-1', not as 'latin_1'.
_BUILTIN_NAMES = {
    "utf_8": "utf-8", "utf_8_sig": "utf-8-sig",
    "latin_1": "iso8859-1",
    "utf_16": "utf-16", "utf_16_le": "utf-16-le", "utf_16_be": "utf-16-be",
    "utf_32": "utf-32", "utf_32_le": "utf-32-le", "utf_32_be": "utf-32-be",
    "utf_7": "utf-7",
    "unicode_escape": "unicode-escape",
    "raw_unicode_escape": "raw-unicode-escape",
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
    "u7": "utf_7", "utf7": "utf_7", "unicode_1_1_utf_7": "utf_7",
    "u16": "utf_16", "utf16": "utf_16",
    "unicodelittleunmarked": "utf_16_le", "utf_16le": "utf_16_le",
    "unicodebigunmarked": "utf_16_be", "utf_16be": "utf_16_be",
    "u32": "utf_32", "utf32": "utf_32",
    "utf_32le": "utf_32_le", "utf_32be": "utf_32_be",
    "unicode_internal": "utf_32_le",
    "unicodeescape": "unicode_escape",
    "rawunicodeescape": "raw_unicode_escape",
}
