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
    try:
        import encodings
    except ImportError:
        # encodings is CPython's own Python package, not something apython
        # ships; without it the registry holds only what a program registers.
        return
    _search_functions.insert(0, encodings.search_function)


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
    parts = []
    for ch in exc.object[exc.start:exc.end]:
        n = ord(ch)
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
    return (s.encode(), len(s))


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
    return (b.decode(), len(b))


utf_8_decode = _Builtin(_utf_8_decode)
utf_8_encode = _Builtin(utf_8_encode)


def ascii_encode(s, errors=None):
    out = []
    for ch in s:
        if ord(ch) > 127:
            if errors in (None, "strict"):
                raise UnicodeEncodeError(
                    "ascii", s, 0, len(s), "ordinal not in range(128)")
            if errors == "ignore":
                continue
            out.append("?")
        else:
            out.append(ch)
    return ("".join(out).encode(), len(s))


def ascii_decode(data, errors=None, final=False):
    b = _as_bytes(data)
    for byte in b:
        if byte > 127:
            if errors in (None, "strict"):
                raise UnicodeDecodeError(
                    "ascii", b, 0, len(b), "ordinal not in range(128)")
    return ("".join(chr(x) for x in b if x <= 127), len(b))


def latin_1_encode(s, errors=None):
    out = []
    for ch in s:
        n = ord(ch)
        if n > 255:
            if errors in (None, "strict"):
                raise UnicodeEncodeError(
                    "latin-1", s, 0, len(s), "ordinal not in range(256)")
            if errors == "ignore":
                continue
            out.append(63)
        else:
            out.append(n)
    return (bytes(out), len(s))


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
