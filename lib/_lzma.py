"""_lzma - the LZMACompressor and LZMADecompressor objects over _lzmacore.

The same split lib/_bz2.py and lib/zlib.py use.  _lzmacore owns the
lzma_stream, the filter chain's option structs, the growing output buffer and
the handle table -- the parts that have to be written in assembly over
liblzma -- and everything a program touches is here: the two objects, the
constants, LZMAError, the filter DICTS and every default.  CPython's own
Lib/lzma.py then runs verbatim on top.

A filter chain crosses the boundary as a list of TUPLES OF INTS rather than as
the dicts a caller writes, because the option names, the defaults and the
per-filter error wordings are Python's business while the struct layout is the
assembly's.  _spec_to_tuple below is that translation, and it is the only
place that knows both.

The core reports a failure as a ValueError carrying CPython's exact sentence,
because a module written in assembly cannot raise a class defined in Python;
_ERRORS maps the sentence back to the class CPython raises.  The mapping is by
exact text, and a sentence changed on one side without the other falls through
to LZMAError rather than silently becoming something else.
"""

import _lzmacore

__all__ = [
    "CHECK_NONE", "CHECK_CRC32", "CHECK_CRC64", "CHECK_SHA256",
    "CHECK_ID_MAX", "CHECK_UNKNOWN",
    "FILTER_LZMA1", "FILTER_LZMA2", "FILTER_DELTA", "FILTER_X86",
    "FILTER_IA64", "FILTER_ARM", "FILTER_ARMTHUMB", "FILTER_POWERPC",
    "FILTER_SPARC",
    "FORMAT_AUTO", "FORMAT_XZ", "FORMAT_ALONE", "FORMAT_RAW",
    "MF_HC3", "MF_HC4", "MF_BT2", "MF_BT3", "MF_BT4",
    "MODE_FAST", "MODE_NORMAL", "PRESET_DEFAULT", "PRESET_EXTREME",
    "LZMACompressor", "LZMADecompressor", "LZMAError", "is_check_supported",
]

FORMAT_AUTO = 0
FORMAT_XZ = 1
FORMAT_ALONE = 2
FORMAT_RAW = 3

CHECK_NONE = 0
CHECK_CRC32 = 1
CHECK_CRC64 = 4
CHECK_SHA256 = 10
CHECK_ID_MAX = 15
CHECK_UNKNOWN = 16

FILTER_LZMA1 = 0x4000000000000001
FILTER_LZMA2 = 0x21
FILTER_DELTA = 0x03
FILTER_X86 = 0x04
FILTER_POWERPC = 0x05
FILTER_IA64 = 0x06
FILTER_ARM = 0x07
FILTER_ARMTHUMB = 0x08
FILTER_SPARC = 0x09

MF_HC3 = 0x03
MF_HC4 = 0x04
MF_BT2 = 0x12
MF_BT3 = 0x13
MF_BT4 = 0x14

MODE_FAST = 1
MODE_NORMAL = 2

PRESET_DEFAULT = 6
PRESET_EXTREME = 1 << 31

_LZMA_RUN = 0
_LZMA_FINISH = 3

_BCJ_FILTERS = (FILTER_X86, FILTER_POWERPC, FILTER_IA64, FILTER_ARM,
                FILTER_ARMTHUMB, FILTER_SPARC)


class LZMAError(Exception):
    """Call to liblzma failed."""


_ERRORS = {
    "Unsupported integrity check": LZMAError,
    "Out of memory": MemoryError,
    "Memory usage limit exceeded": LZMAError,
    "Input format not supported by decoder": LZMAError,
    "Invalid or unsupported options": LZMAError,
    "Corrupt input data": LZMAError,
    "Insufficient buffer space": LZMAError,
    "Internal error": LZMAError,
    "Invalid compression preset": LZMAError,
}


def _translate(exc):
    text = str(exc)
    cls = _ERRORS.get(text)
    if cls is MemoryError:
        return MemoryError()
    if cls is None:
        # Not one of liblzma's own: the core's own argument refusals, which
        # CPython raises as ValueError too.
        return ValueError(text)
    return cls(text)


def _uint64(value):
    """PyLong_AsUnsignedLongLong, with CPython's wording.

    A float is a TypeError and not a truncation, and a negative int is an
    OverflowError rather than a huge unsigned one -- which is what
    `LZMADecompressor(memlimit=7.3e9)` and `preset=-1` turn on.
    """
    if isinstance(value, bool) or not isinstance(value, int):
        if not isinstance(value, int):
            raise TypeError("an integer is required")
    if value < 0:
        raise OverflowError("can't convert negative int to unsigned")
    return value


def _uint32(value, what):
    """CPython's uint32_converter: PyLong_AsUnsignedLongLong, then the range."""
    value = _uint64(value)
    if value > 0xffffffff:
        raise OverflowError("Value too large for uint32_t type")
    return value


# --- the filter translation ---------------------------------------------------

_LZMA_OPTNAMES = ("id", "preset", "dict_size", "lc", "lp", "pb", "mode",
                  "nice_len", "mf", "depth")
_LZMA_ORDER = ("dict_size", "lc", "lp", "pb", "mode", "nice_len", "mf",
               "depth")


def _spec_to_tuple(spec):
    """One filter dict as the fixed int tuple src/modules/lzma.asm reads.

    -1 in an LZMA slot means "leave whatever the preset put there", which is
    safe as a sentinel because a real value is a uint32 and -1 never is.

    Every refusal here is CPython's, and there are three kinds: the `id`
    lookup fails the way subscripting that object fails (so `[b"wobsite"]`
    says "byte indices must be integers"), a dict with no `id` is a
    ValueError naming the entry, and ANY failure inside a filter's own
    options -- an unknown name, a bad type, a value out of range -- becomes
    the one ValueError CPython words per filter kind.
    """
    try:
        filter_id = spec["id"]
    except KeyError:
        raise ValueError('Filter specifier must have an "id" entry') from None

    if filter_id in (FILTER_LZMA1, FILTER_LZMA2):
        try:
            for name in spec:
                if name not in _LZMA_OPTNAMES:
                    raise ValueError
            preset = _uint32(spec.get("preset", PRESET_DEFAULT), "preset")
            out = [filter_id, preset]
            for name in _LZMA_ORDER:
                if name in spec:
                    out.append(_uint32(spec[name], name))
                else:
                    out.append(-1)
        except (TypeError, ValueError, OverflowError, KeyError):
            raise ValueError("Invalid filter specifier for LZMA filter"
                             ) from None
        return tuple(out)

    if filter_id == FILTER_DELTA:
        try:
            for name in spec:
                if name not in ("id", "dist"):
                    raise ValueError
            dist = _uint32(spec.get("dist", 1), "dist")
        except (TypeError, ValueError, OverflowError, KeyError):
            raise ValueError("Invalid filter specifier for delta filter"
                             ) from None
        return (filter_id, dist)

    if filter_id in _BCJ_FILTERS:
        try:
            for name in spec:
                if name not in ("id", "start_offset"):
                    raise ValueError
            start = _uint32(spec.get("start_offset", 0), "start_offset")
        except (TypeError, ValueError, OverflowError, KeyError):
            raise ValueError("Invalid filter specifier for BCJ filter"
                             ) from None
        return (filter_id, start)

    raise ValueError("Invalid filter ID: %s" % (filter_id,))


def _chain_to_tuples(filters):
    """The whole chain.

    CPython asks for the SEQUENCE length, which is why a mapping is refused
    here rather than iterated: `filters={}` is a TypeError there and would
    otherwise quietly be an empty chain.  An empty LIST is not refused -- it
    reaches liblzma, which calls it an internal error, and CPython lets it.
    """
    if filters is None:
        return None
    if hasattr(filters, "keys"):
        raise TypeError("%s is not a sequence" % (type(filters).__name__,))
    try:
        length = len(filters)
    except TypeError:
        raise TypeError("object of type '%s' has no len()"
                        % (type(filters).__name__,)) from None
    if length > 4:
        raise ValueError(
            "Too many filters - liblzma supports a maximum of 4")
    return [_spec_to_tuple(filters[i]) for i in range(length)]


def _tuple_to_spec(filter_id, fields):
    """The inverse, for _decode_filter_properties.

    Only the fields that filter really carries appear, which is what CPython's
    build_filter_spec does: lc/lp/pb/dict_size for LZMA1, dict_size for LZMA2,
    dist for delta, and start_offset for a BCJ filter that was given one.
    """
    lc, lp, pb, dict_size, dist, start_offset = fields
    spec = {"id": filter_id}
    if filter_id == FILTER_LZMA1:
        spec["lc"] = lc
        spec["lp"] = lp
        spec["pb"] = pb
        spec["dict_size"] = dict_size
    elif filter_id == FILTER_LZMA2:
        spec["dict_size"] = dict_size
    elif filter_id == FILTER_DELTA:
        spec["dist"] = dist
    elif filter_id in _BCJ_FILTERS:
        if start_offset >= 0:
            spec["start_offset"] = start_offset
    else:
        raise ValueError("Invalid filter ID: %s" % (filter_id,))
    return spec


def _encode_filter_properties(filterspec):
    """The filter's properties as they appear in a raw stream's header."""
    try:
        return _lzmacore.encode_filter_props(_spec_to_tuple(filterspec))
    except ValueError as e:
        raise _translate(e) from None


def _decode_filter_properties(filter_id, encoded_props):
    """The inverse: the dict a caller would have written."""
    try:
        fields = _lzmacore.decode_filter_props(filter_id, encoded_props)
    except ValueError as e:
        raise _translate(e) from None
    return _tuple_to_spec(filter_id, fields)


def is_check_supported(check_id):
    """Whether this build of liblzma can verify that integrity check."""
    return _lzmacore.check_is_supported(check_id)


# --- the two objects ----------------------------------------------------------

class _Stream:
    """What the two objects share: one core handle, ended once."""

    _handle = -1

    def _feed(self, data, action, max_length=-1):
        if self._handle < 0:
            raise ValueError("stream is already finished")
        try:
            return _lzmacore.stream_feed(self._handle, data, action,
                                         max_length)
        except ValueError as e:
            raise _translate(e) from None

    def _close(self):
        h = self._handle
        self._handle = -1
        if h >= 0:
            _lzmacore.stream_free(h)

    def __del__(self):
        # An encoder at preset 9 holds a 64 MB dictionary, which a loop over
        # even a few files would notice.
        try:
            self._close()
        except Exception:
            pass

    def __reduce__(self):
        raise TypeError("cannot pickle '%s' object" % (type(self).__name__,))


class LZMACompressor(_Stream):
    """Incremental compression, in any of the three container formats."""

    def __init__(self, format=FORMAT_XZ, check=-1, preset=None, filters=None):
        # `format` and `check` are C ints in CPython's argument clinic, so a
        # non-integer is refused before anything else -- and the refusal names
        # the format, which is why the range check is here and not in the
        # core: the number is easy to say in Python.
        format = _index(format)
        check = _index(check)
        if format not in (FORMAT_XZ, FORMAT_ALONE, FORMAT_RAW):
            raise ValueError("Invalid container format: %s" % (format,))
        if format != FORMAT_XZ and check not in (-1, CHECK_NONE):
            raise ValueError("Integrity checks are only supported by "
                             "FORMAT_XZ")
        if preset is not None and filters is not None:
            raise ValueError("Cannot specify both preset and filter chain")
        if preset is None:
            preset_value = PRESET_DEFAULT
        else:
            preset_value = _uint32(preset, "preset")
        if format == FORMAT_XZ and check == -1:
            check = CHECK_CRC64
        elif format != FORMAT_XZ:
            check = CHECK_NONE
        if format == FORMAT_RAW and filters is None:
            raise ValueError("Must specify filters for FORMAT_RAW")

        chain = _chain_to_tuples(filters)
        self._flushed = False
        try:
            self._handle = _lzmacore.encoder(format, check, preset_value,
                                             chain)
        except ValueError as e:
            raise _translate(e) from None

    def compress(self, data):
        if self._flushed:
            raise ValueError("Compressor has been flushed")
        return self._feed(data, _LZMA_RUN)

    def flush(self):
        if self._flushed:
            raise ValueError("Repeated call to flush()")
        self._flushed = True
        out = self._feed(b"", _LZMA_FINISH)
        self._close()
        return out


class LZMADecompressor(_Stream):
    """Incremental decompression.

    `needs_input` is False while a max_length call has left input parked in
    the core; the caller then sends b"" rather than more data, which is what
    _compression.DecompressReader does.
    """

    # CPython's is a C type whose struct is zero-filled by the allocator, so
    # an instance made with __new__ alone answers rather than raising.
    eof = False
    needs_input = True
    unused_data = b""
    check = CHECK_UNKNOWN

    def __init__(self, format=FORMAT_AUTO, memlimit=None, filters=None):
        format = _index(format)
        if format not in (FORMAT_AUTO, FORMAT_XZ, FORMAT_ALONE, FORMAT_RAW):
            raise ValueError("Invalid container format: %s" % (format,))
        if memlimit is not None and format == FORMAT_RAW:
            raise ValueError("Cannot specify memory limit with FORMAT_RAW")
        if format == FORMAT_RAW and filters is None:
            raise ValueError("Must specify filters for FORMAT_RAW")
        if format != FORMAT_RAW and filters is not None:
            raise ValueError("Cannot specify filters except with FORMAT_RAW")

        self.eof = False
        self.unused_data = b""
        self.needs_input = True
        self.check = CHECK_UNKNOWN
        chain = _chain_to_tuples(filters)
        if memlimit is None:
            limit = -1
        else:
            limit = _uint64(memlimit)
            if limit >= 1 << 63:
                # The core carries the limit in a signed word and spells "no
                # limit" as -1, which is what liblzma's UINT64_MAX means; a
                # limit this large is that in all but name.
                limit = -1
        try:
            self._handle = _lzmacore.decoder(format, limit, chain)
        except ValueError as e:
            raise _translate(e) from None
        if format in (FORMAT_ALONE, FORMAT_RAW):
            # Neither container carries an integrity check, and CPython says
            # so up front rather than leaving it UNKNOWN for ever.
            self.check = CHECK_NONE

    def decompress(self, data, max_length=-1):
        if self.eof:
            raise EOFError("Already at end of stream")
        max_length = _index(max_length)
        if self._handle < 0:
            _check_buffer(data)
            return b""
        out = self._feed(data, _LZMA_RUN, max_length)
        self._refresh()
        return out

    def _refresh(self):
        eof, needs_input, unused, check = _lzmacore.stream_state(self._handle)
        self.eof = eof
        self.needs_input = needs_input
        if check != CHECK_UNKNOWN:
            self.check = check
        if unused:
            self.unused_data += unused
        if eof:
            self.needs_input = False
            self._close()


def _check_buffer(data):
    """The argument is still checked on the path that does nothing with it."""
    if isinstance(data, (bytes, bytearray, memoryview)):
        return
    raise TypeError("a bytes-like object is required, not '%s'"
                    % (type(data).__name__,))


def _index(value):
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
