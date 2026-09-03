"""_struct - packing and unpacking binary layouts.

CPython puts this in C for speed.  Here it is Python over `int.to_bytes` and
`int.from_bytes`, which do the integer half exactly; the float codes go
through `math.frexp` and `math.ldexp`, which is enough to build and take
apart an IEEE-754 word without a bit-level primitive.

The format language is CPython's, including the two alignment rules: `@`
(the default) pads each field to its natural alignment, and `<`, `>`, `=` and
`!` do not pad at all.
"""

import math

__all__ = ["calcsize", "pack", "pack_into", "unpack", "unpack_from",
           "iter_unpack", "Struct", "error"]


class error(Exception):
    """struct.error"""


# code -> (size, alignment, kind), where kind is one of
# 'x' pad, 'i' signed int, 'u' unsigned int, 'f' float, 'b' bool,
# 'c' one char, 's' bytes, 'p' pascal string
_TABLE = {
    'x': (1, 1, 'x'),
    'c': (1, 1, 'c'),
    'b': (1, 1, 'i'),
    'B': (1, 1, 'u'),
    '?': (1, 1, 'b'),
    'h': (2, 2, 'i'),
    'H': (2, 2, 'u'),
    'i': (4, 4, 'i'),
    'I': (4, 4, 'u'),
    'l': (4, 4, 'i'),
    'L': (4, 4, 'u'),
    'q': (8, 8, 'i'),
    'Q': (8, 8, 'u'),
    'e': (2, 2, 'f'),
    'f': (4, 4, 'f'),
    'd': (8, 8, 'f'),
    's': (1, 1, 's'),
    'p': (1, 1, 'p'),
    'P': (8, 8, 'u'),
    'n': (8, 8, 'i'),
    'N': (8, 8, 'u'),
}

# Native mode widens the C types to what this platform actually has.
_NATIVE = {
    'l': (8, 8, 'i'),
    'L': (8, 8, 'u'),
    'P': (8, 8, 'u'),
}

_BYTE_ORDERS = {
    '@': ('little', True),
    '=': ('little', False),
    '<': ('little', False),
    '>': ('big', False),
    '!': ('big', False),
}


def _parse(fmt):
    """(byteorder, [(code, count, size, align, kind), ...], total_size)."""
    if isinstance(fmt, (bytes, bytearray)):
        fmt = fmt.decode('ascii')
    if not isinstance(fmt, str):
        raise TypeError("Struct() argument 1 must be a str or bytes object, "
                        "not %s" % (type(fmt).__name__,))
    native = True
    order = 'little'
    i = 0
    if fmt and fmt[0] in _BYTE_ORDERS:
        order, native = _BYTE_ORDERS[fmt[0]]
        i = 1
    if fmt[:1] == '@' or i == 0:
        native = True

    items = []
    size = 0
    n = len(fmt)
    while i < n:
        c = fmt[i]
        if c in ' \t\n':
            i += 1
            continue
        count = None
        start = i
        while i < n and fmt[i].isdigit():
            i += 1
        if i > start:
            count = int(fmt[start:i])
        if i >= n:
            raise error("repeat count given without format specifier")
        c = fmt[i]
        i += 1
        if c not in _TABLE:
            if c in _BYTE_ORDERS:
                raise error("bad char in struct format")
            raise error("bad char in struct format")
        csize, calign, kind = _TABLE[c]
        if native and c in _NATIVE:
            csize, calign, kind = _NATIVE[c]
        if c in ('n', 'N', 'P') and not native:
            # These three are native-only: their width is the platform's, so
            # there is nothing for a byte order to mean.
            raise error("bad char in struct format")
        if count is None:
            count = 1
        if native and calign > 1:
            pad = (-size) % calign
            if pad:
                items.append(('x', pad, 1, 1, 'x'))
                size += pad
        if kind in ('s', 'p'):
            items.append((c, count, count, 1, kind))
            size += count
        else:
            items.append((c, count, csize, calign, kind))
            size += csize * count
    return order, items, size


# -- IEEE-754, without a bit-level primitive ---------------------------------
_FLOAT_CODES = {2: 'e', 4: 'f', 8: 'd'}

_FLOAT_FORMATS = {
    2: (5, 10, 15),      # half:   exponent bits, mantissa bits, bias
    4: (8, 23, 127),     # single
    8: (11, 52, 1023),   # double
}


def _float_to_bits(x, size):
    ebits, mbits, bias = _FLOAT_FORMATS[size]
    x = float(x)
    sign = 0
    if math.copysign(1.0, x) < 0:
        sign = 1
        x = -x
    if math.isnan(x):
        return (sign << (ebits + mbits)) | (((1 << ebits) - 1) << mbits) \
            | (1 << (mbits - 1))
    if math.isinf(x):
        return (sign << (ebits + mbits)) | (((1 << ebits) - 1) << mbits)
    if x == 0.0:
        return sign << (ebits + mbits)

    m, e = math.frexp(x)          # x == m * 2**e, 0.5 <= m < 1
    e -= 1                        # so that 1 <= m*2 < 2
    m *= 2.0
    if e < 1 - bias:
        # Subnormal: no implicit leading one, and the exponent is pinned.
        frac = int(round(math.ldexp(m, mbits + e + bias - 1)))
        if frac >= (1 << mbits):
            # Rounding carried into the smallest normal.
            return (sign << (ebits + mbits)) | (1 << mbits)
        return (sign << (ebits + mbits)) | frac
    frac = int(round(math.ldexp(m - 1.0, mbits)))
    if frac == (1 << mbits):
        frac = 0
        e += 1
    if e + bias >= (1 << ebits) - 1:
        raise OverflowError("float too large to pack with %s format"
                            % (_FLOAT_CODES[size],))
    return (sign << (ebits + mbits)) | ((e + bias) << mbits) | frac


def _bits_to_float(bits, size):
    ebits, mbits, bias = _FLOAT_FORMATS[size]
    sign = (bits >> (ebits + mbits)) & 1
    exp = (bits >> mbits) & ((1 << ebits) - 1)
    frac = bits & ((1 << mbits) - 1)
    if exp == (1 << ebits) - 1:
        if frac:
            return float('nan')
        return float('-inf') if sign else float('inf')
    if exp == 0:
        value = math.ldexp(frac, 1 - bias - mbits)
    else:
        value = math.ldexp(frac + (1 << mbits), exp - bias - mbits)
    return -value if sign else value


def _check_int(v, code):
    if isinstance(v, bool):
        return int(v)
    if isinstance(v, int):
        return v
    if hasattr(v, '__index__'):
        return v.__index__()
    raise error("required argument is not an integer")


def _pack_one(order, code, size, kind, v):
    if kind == 'i' or kind == 'u':
        v = _check_int(v, code)
        signed = kind == 'i'
        lo = -(1 << (size * 8 - 1)) if signed else 0
        hi = (1 << (size * 8 - 1)) - 1 if signed else (1 << (size * 8)) - 1
        if v < lo or v > hi:
            raise error("'%s' format requires %d <= number <= %d"
                        % (code, lo, hi))
        return v.to_bytes(size, order, signed=signed)
    if kind == 'b':
        return (b'\x01' if v else b'\x00')
    if kind == 'f':
        try:
            bits = _float_to_bits(v, size)
        except (TypeError, ValueError):
            raise error("required argument is not a float") from None
        return bits.to_bytes(size, order)
    if kind == 'c':
        if not isinstance(v, (bytes, bytearray)) or len(v) != 1:
            raise error("char format requires a bytes object of length 1")
        return bytes(v)
    raise error("bad char in struct format")


def _unpack_one(order, code, size, kind, chunk):
    if kind == 'i' or kind == 'u':
        return int.from_bytes(chunk, order, signed=(kind == 'i'))
    if kind == 'b':
        return chunk[0] != 0
    if kind == 'f':
        return _bits_to_float(int.from_bytes(chunk, order), size)
    if kind == 'c':
        return bytes(chunk)
    raise error("bad char in struct format")


class Struct:
    """A compiled format.  CPython caches these; so does struct.py above."""

    __slots__ = ("format", "size", "_order", "_items")

    def __init__(self, format):
        self._order, self._items, self.size = _parse(format)
        self.format = format

    def pack(self, *args):
        out = []
        i = 0
        for code, count, size, align, kind in self._items:
            if kind == 'x':
                out.append(b'\x00' * count)
                continue
            if kind == 's':
                if i >= len(args):
                    raise error("pack expected %d items for packing (got %d)"
                                % (self._nargs(), len(args)))
                v = args[i]
                i += 1
                if not isinstance(v, (bytes, bytearray)):
                    raise error("argument for 's' must be a bytes object")
                v = bytes(v[:count])
                out.append(v + b'\x00' * (count - len(v)))
                continue
            if kind == 'p':
                if i >= len(args):
                    raise error("pack expected %d items for packing (got %d)"
                                % (self._nargs(), len(args)))
                v = args[i]
                i += 1
                if not isinstance(v, (bytes, bytearray)):
                    raise error("argument for 'p' must be a bytes object")
                v = bytes(v[:min(count - 1, 255)])
                out.append(bytes([len(v)]) + v
                           + b'\x00' * (count - 1 - len(v)))
                continue
            for _ in range(count):
                if i >= len(args):
                    raise error("pack expected %d items for packing (got %d)"
                                % (self._nargs(), len(args)))
                out.append(_pack_one(self._order, code, size, kind, args[i]))
                i += 1
        if i != len(args):
            raise error("pack expected %d items for packing (got %d)"
                        % (self._nargs(), len(args)))
        return b''.join(out)

    def _nargs(self):
        n = 0
        for code, count, size, align, kind in self._items:
            if kind == 'x':
                continue
            if kind in ('s', 'p'):
                n += 1
            else:
                n += count
        return n

    def pack_into(self, buffer, offset, *args):
        data = self.pack(*args)
        if offset < 0:
            offset += len(buffer)
        if offset < 0 or offset + len(data) > len(buffer):
            raise error("pack_into requires a buffer of at least %d bytes for "
                        "packing %d bytes at offset %d (actual buffer size is "
                        "%d)" % (offset + len(data), len(data), offset,
                                 len(buffer)))
        buffer[offset:offset + len(data)] = data

    def unpack(self, buffer):
        if len(buffer) != self.size:
            raise error("unpack requires a buffer of %d bytes" % (self.size,))
        return self._unpack_from(buffer, 0)

    def unpack_from(self, buffer, offset=0):
        if offset < 0:
            offset += len(buffer)
        if offset < 0 or len(buffer) - offset < self.size:
            raise error("unpack_from requires a buffer of at least %d bytes "
                        "for unpacking %d bytes at offset %d (actual buffer "
                        "size is %d)"
                        % (self.size + offset, self.size, offset,
                           len(buffer)))
        return self._unpack_from(buffer, offset)

    def _unpack_from(self, buffer, offset):
        buffer = bytes(buffer)
        out = []
        pos = offset
        for code, count, size, align, kind in self._items:
            if kind == 'x':
                pos += count
                continue
            if kind == 's':
                out.append(buffer[pos:pos + count])
                pos += count
                continue
            if kind == 'p':
                n = buffer[pos]
                if n > count - 1:
                    n = count - 1
                out.append(buffer[pos + 1:pos + 1 + n])
                pos += count
                continue
            for _ in range(count):
                out.append(_unpack_one(self._order, code, size, kind,
                                       buffer[pos:pos + size]))
                pos += size
        return tuple(out)

    def iter_unpack(self, buffer):
        if self.size == 0:
            raise error("cannot iteratively unpack with a struct of length 0")
        if len(buffer) % self.size:
            raise error("iterative unpacking requires a buffer of a multiple "
                        "of %d bytes" % (self.size,))
        for off in range(0, len(buffer), self.size):
            yield self._unpack_from(buffer, off)


_cache = {}


def _compile(fmt):
    key = fmt if not isinstance(fmt, (bytes, bytearray)) else bytes(fmt)
    try:
        s = _cache.get(key)
    except TypeError:
        return Struct(fmt)
    if s is None:
        s = Struct(fmt)
        if len(_cache) >= 100:
            _cache.clear()
        _cache[key] = s
    return s


def _clearcache():
    _cache.clear()


def calcsize(fmt):
    return _compile(fmt).size


def pack(fmt, *args):
    return _compile(fmt).pack(*args)


def pack_into(fmt, buffer, offset, *args):
    return _compile(fmt).pack_into(buffer, offset, *args)


def unpack(fmt, buffer):
    return _compile(fmt).unpack(buffer)


def unpack_from(fmt, buffer, offset=0):
    return _compile(fmt).unpack_from(buffer, offset)


def iter_unpack(fmt, buffer):
    return _compile(fmt).iter_unpack(buffer)
