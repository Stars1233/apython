"""binascii - conversions between binary and the ASCII encodings of it.

CPython's is C.  Here the two halves that matter -- hex and base64 -- are
built on `bytes.hex`, `bytes.fromhex` and a table, which is what base64,
quopri, uu and plistlib come in behind.  crc32 is the standard table-driven
CRC-32, so it agrees with CPython bit for bit.
"""

__all__ = ["Error", "Incomplete", "a2b_base64", "b2a_base64", "a2b_hex",
           "b2a_hex", "hexlify", "unhexlify", "crc32", "a2b_qp", "b2a_qp",
           "a2b_uu", "b2a_uu", "crc_hqx"]


class Error(ValueError):
    """binascii.Error, which really is a ValueError in CPython too."""


class Incomplete(Exception):
    pass


def _as_bytes(data, name="argument"):
    if isinstance(data, str):
        try:
            return data.encode("ascii")
        except UnicodeEncodeError:
            raise ValueError("string argument should contain only ASCII "
                             "characters") from None
    if isinstance(data, (bytes, bytearray, memoryview)):
        return bytes(data)
    raise TypeError("argument should be a bytes-like object or ASCII string, "
                    "not '%s'" % (type(data).__name__,))


# -- hex --------------------------------------------------------------------
def b2a_hex(data, sep=None, bytes_per_sep=1):
    data = _as_bytes(data)
    if sep is None:
        return data.hex().encode("ascii")
    return data.hex(sep if isinstance(sep, str) else sep.decode("ascii"),
                    bytes_per_sep).encode("ascii")


hexlify = b2a_hex


def a2b_hex(hexstr):
    hexstr = _as_bytes(hexstr)
    if len(hexstr) % 2:
        raise Error("Odd-length string")
    try:
        return bytes.fromhex(hexstr.decode("ascii"))
    except ValueError:
        raise Error("Non-hexadecimal digit found") from None


unhexlify = a2b_hex


# -- base64 -----------------------------------------------------------------
_B64_CHARS = (b"ABCDEFGHIJKLMNOPQRSTUVWXYZ"
              b"abcdefghijklmnopqrstuvwxyz"
              b"0123456789+/")
_B64_INDEX = {c: i for i, c in enumerate(_B64_CHARS)}


def b2a_base64(data, *, newline=True):
    data = _as_bytes(data)
    out = bytearray()
    for i in range(0, len(data), 3):
        chunk = data[i:i + 3]
        n = len(chunk)
        v = chunk[0] << 16
        if n > 1:
            v |= chunk[1] << 8
        if n > 2:
            v |= chunk[2]
        out.append(_B64_CHARS[(v >> 18) & 0x3f])
        out.append(_B64_CHARS[(v >> 12) & 0x3f])
        out.append(_B64_CHARS[(v >> 6) & 0x3f] if n > 1 else 0x3d)
        out.append(_B64_CHARS[v & 0x3f] if n > 2 else 0x3d)
    if newline:
        out.append(0x0a)
    return bytes(out)


def a2b_base64(data, *, strict_mode=False):
    """Decode base64, following CPython's binascii.c state for state.

    The one thing that is not obvious from the outside: a pad sequence that
    completes a quad ends the DECODE, not just the quad.  `a2b_base64
    (b'YQ==YWJj')` is b'a', and the second group is never looked at -- which
    is what makes a concatenation of several base64 lines decode to only its
    first.  Treating '=' as a character to skip decoded all of them, and
    b'YQ==YWJj' came back as b'a\\x06\\x16&'.

    The counting is CPython's too: a quad is carried one character at a time
    in `leftchar`, and the error at the end depends on how many characters of
    a quad were left over, not on how many pads followed them.
    """
    data = _as_bytes(data)
    if strict_mode and data[:1] == b"=":
        raise Error("Leading padding not allowed")
    out = bytearray()
    padding_started = False
    quad_pos = 0
    leftchar = 0
    pads = 0
    for i, ch in enumerate(data):
        if ch == 0x3d:                          # '='
            padding_started = True
            if strict_mode and quad_pos == 0:
                raise Error("Excess padding not allowed")
            pads += 1
            if quad_pos >= 2 and quad_pos + pads >= 4:
                # The quad is already decoded; everything after it is not
                # input any more.
                if strict_mode and i + 1 < len(data):
                    raise Error("Excess data after padding")
                return bytes(out)
            continue
        idx = _B64_INDEX.get(ch)
        if idx is None:
            if strict_mode:
                raise Error("Only base64 data is allowed")
            continue
        if strict_mode and padding_started:
            raise Error("Discontinuous padding not allowed")
        pads = 0
        if quad_pos == 0:
            quad_pos = 1
            leftchar = idx
        elif quad_pos == 1:
            quad_pos = 2
            out.append(((leftchar << 2) | (idx >> 4)) & 0xff)
            leftchar = idx & 0x0f
        elif quad_pos == 2:
            quad_pos = 3
            out.append(((leftchar << 4) | (idx >> 2)) & 0xff)
            leftchar = idx & 0x03
        else:
            quad_pos = 0
            out.append(((leftchar << 6) | idx) & 0xff)
            leftchar = 0
    if quad_pos == 1:
        # The count CPython reports is of DATA characters, worked back out of
        # how many bytes were written, not of the characters left over.
        raise Error("Invalid base64-encoded string: number of data characters "
                    "(%d) cannot be 1 more than a multiple of 4"
                    % (len(out) // 3 * 4 + 1,))
    if quad_pos != 0:
        raise Error("Incorrect padding")
    return bytes(out)


# -- crc32 ------------------------------------------------------------------
_CRC_TABLE = None


def _crc_table():
    global _CRC_TABLE
    if _CRC_TABLE is None:
        table = []
        for i in range(256):
            c = i
            for _ in range(8):
                c = (c >> 1) ^ (0xedb88320 if c & 1 else 0)
            table.append(c)
        _CRC_TABLE = table
    return _CRC_TABLE


def crc32(data, crc=0):
    data = _as_bytes(data)
    table = _crc_table()
    crc = (~crc) & 0xffffffff
    for b in data:
        crc = table[(crc ^ b) & 0xff] ^ (crc >> 8)
    return (~crc) & 0xffffffff


def crc_hqx(data, crc=0):
    data = _as_bytes(data)
    for b in data:
        crc = ((crc << 8) & 0xff00) ^ _hqx_table()[((crc >> 8) & 0xff) ^ b]
    return crc & 0xffff


_HQX_TABLE = None


def _hqx_table():
    global _HQX_TABLE
    if _HQX_TABLE is None:
        table = []
        for i in range(256):
            c = i << 8
            for _ in range(8):
                c = ((c << 1) ^ 0x1021) & 0xffff if c & 0x8000 else (c << 1) & 0xffff
            table.append(c)
        _HQX_TABLE = table
    return _HQX_TABLE


# -- quoted-printable and uuencode ------------------------------------------
_HEXDIGITS = b"0123456789ABCDEF"


def _qp_escape(out, b):
    out.append(0x3d)                  # '='
    out.append(_HEXDIGITS[b >> 4])
    out.append(_HEXDIGITS[b & 0xf])


def b2a_qp(data, quotetabs=False, istext=True, header=False):
    """Quoted-printable, including the two rules that make it that.

    Whitespace at the end of a line -- or at the end of the data -- is
    escaped, since a mail transport is free to strip it; and a line longer
    than 76 characters is broken with a soft break, `=` and a newline, which
    the decoder removes.  Without either, this encoded but did not round-trip
    through anything that reflows or trims.
    """
    data = _as_bytes(data)
    out = bytearray()
    n = len(data)
    linelen = 0
    i = 0
    while i < n:
        b = data[i]
        if istext and b in (0x0a, 0x0d):
            out.append(b)
            if b == 0x0a:
                linelen = 0
            i += 1
            continue

        if b in (0x20, 0x09) and not quotetabs:
            # A space or tab is literal unless it ends the line: then it has
            # to be escaped, or a transport may eat it.
            j = i + 1
            while j < n and data[j] in (0x20, 0x09):
                j += 1
            at_end = j >= n or (istext and data[j] in (0x0a, 0x0d))
            if not at_end:
                width = 1
                enc = None
            else:
                width = 3
                enc = b
        elif b == 0x20 and header:
            width = 1
            enc = None
        elif 0x21 <= b <= 0x3c or 0x3e <= b <= 0x7e:
            width = 1
            enc = None
        else:
            width = 3
            enc = b

        # 76 columns including the soft break's own `=`.
        if linelen + width > 75:
            out.append(0x3d)
            out.append(0x0a)
            linelen = 0

        if b == 0x20 and header and enc is None:
            out.append(0x5f)          # '_'
        elif enc is None:
            out.append(b)
        else:
            _qp_escape(out, enc)
        linelen += width
        i += 1
    return bytes(out)


def a2b_qp(data, header=False):
    data = _as_bytes(data)
    out = bytearray()
    i = 0
    n = len(data)
    while i < n:
        b = data[i]
        if b == 0x3d:                 # '='
            if i + 1 < n and data[i + 1] in (0x0a, 0x0d):
                i += 2
                if i <= n and data[i - 1] == 0x0d and i < n and data[i] == 0x0a:
                    i += 1
                continue
            if i + 2 < n:
                try:
                    out.append(int(data[i + 1:i + 3].decode("ascii"), 16))
                    i += 3
                    continue
                except ValueError:
                    pass
            out.append(b)
            i += 1
        elif header and b == 0x5f:    # '_'
            out.append(0x20)
            i += 1
        else:
            out.append(b)
            i += 1
    return bytes(out)


def b2a_uu(data, *, backtick=False):
    data = _as_bytes(data)
    if len(data) > 45:
        raise Error("At most 45 bytes at once")
    def enc(v):
        if v == 0 and backtick:
            return 0x60
        return 0x20 + (v & 0x3f) if v else (0x60 if backtick else 0x20)
    out = bytearray()
    out.append(enc(len(data)) if len(data) else (0x60 if backtick else 0x20))
    padded = data + b"\x00" * ((3 - len(data) % 3) % 3)
    for i in range(0, len(padded), 3):
        v = (padded[i] << 16) | (padded[i + 1] << 8) | padded[i + 2]
        for shift in (18, 12, 6, 0):
            out.append(enc((v >> shift) & 0x3f))
    out.append(0x0a)
    return bytes(out)


def a2b_uu(data):
    data = _as_bytes(data)
    if not data:
        return b""
    length = (data[0] - 0x20) & 0x3f
    body = data[1:].rstrip(b"\n\r")
    out = bytearray()
    for i in range(0, len(body), 4):
        chunk = body[i:i + 4] + b" " * (4 - len(body[i:i + 4]))
        v = 0
        for c in chunk:
            v = (v << 6) | ((c - 0x20) & 0x3f)
        out.append((v >> 16) & 0xff)
        out.append((v >> 8) & 0xff)
        out.append(v & 0xff)
    return bytes(out[:length])
