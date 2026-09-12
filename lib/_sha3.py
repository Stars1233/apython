"""_sha3 - SHA-3 and SHAKE, FIPS 202.

Here for the same reason `_sha1` and `_sha2` are: `hashlib` will not give out a
digest it has no module for, and without this one `import hashlib` prints eight
tracebacks to stderr and then has no `sha3_*` or `shake_*` names at all, while
still advertising them in `algorithms_available`.

Everything below is one sponge over Keccak-f[1600].  The variants differ only
in their RATE -- how many of the 200 state bytes each block touches -- and in
the domain byte the padding starts with: 0x06 for SHA-3, 0x1F for SHAKE.

The SHAKEs are the reason this cannot quite follow `_sha2`'s shape.  They are
extendable-output functions, so `digest()` takes a REQUIRED length, their
`digest_size` is 0, and they keep squeezing past one block.  And every
constructor here is a CLASS rather than a function, because CPython's are:
`_sha3.sha3_256` is a type, and `hashlib` hands it straight to callers who
then read attributes off it.
"""

def _tobytes(data):
    """The bytes-like argument every update() and constructor takes.

    NOT `bytes(data)`: `bytes(100)` is a hundred zero bytes and `bytes([1])`
    is b"\\x01", so an int or a list passed where a message was meant became a
    legitimate-looking buffer instead of an error.  memoryview refuses an int,
    a list and a None, which is the set CPython refuses, with CPython's
    wording.
    """
    if isinstance(data, str):
        raise TypeError("Strings must be encoded before hashing")
    if isinstance(data, (bytes, bytearray)):
        return data
    try:
        return memoryview(data).tobytes()
    except TypeError:
        raise TypeError("object supporting the buffer API required") from None


class _Immutable(type):
    """CPython's hash types are C types with no settable attributes.

    `test_hashlib.test_readonly_types` asserts exactly that, for every
    constructor it knows, and a plain Python class is mutable -- so the
    refusal has to come from a metaclass.  Instances are unaffected; it is the
    TYPE that is frozen.  _md5, _sha1, _sha2 and _blake2 each carry a copy,
    rather than sharing one from somewhere, because every one of these modules
    has to be importable on its own.
    """

    def __setattr__(cls, name, value):
        raise TypeError("cannot set %r attribute of immutable type %r"
                        % (name, cls.__name__))

    def __delattr__(cls, name):
        raise TypeError("cannot delete %r attribute of immutable type %r"
                        % (name, cls.__name__))


_M = 0xFFFFFFFFFFFFFFFF

_RC = (
    0x0000000000000001, 0x0000000000008082, 0x800000000000808A,
    0x8000000080008000, 0x000000000000808B, 0x0000000080000001,
    0x8000000080008081, 0x8000000000008009, 0x000000000000008A,
    0x0000000000000088, 0x0000000080008009, 0x000000008000000A,
    0x000000008000808B, 0x800000000000008B, 0x8000000000008089,
    0x8000000000008003, 0x8000000000008002, 0x8000000000000080,
    0x000000000000800A, 0x800000008000000A, 0x8000000080008081,
    0x8000000000008080, 0x0000000080000001, 0x8000000080008008,
)

# Keccak's rotation offsets, indexed [x][y].
_ROT = (
    (0, 36, 3, 41, 18),
    (1, 44, 10, 45, 2),
    (62, 6, 43, 15, 61),
    (28, 55, 25, 21, 56),
    (27, 20, 39, 8, 14),
)


def _pi_rot_table():
    """rho and pi as one flat list of (destination, source, rotation).

    A lane is at index x + 5*y.  pi sends (x, y) to (y, (2x + 3y) % 5), and rho
    rotates it by _ROT[x][y] on the way; precomputing the pair keeps the
    permutation a single loop over 25 entries instead of two nested ones.
    """
    table = []
    for x in range(5):
        for y in range(5):
            table.append((y + 5 * ((2 * x + 3 * y) % 5), x + 5 * y, _ROT[x][y]))
    return tuple(table)


_PI_ROT = _pi_rot_table()


def _keccak_f(a):
    """The permutation, in place on a list of 25 lanes."""
    for rc in _RC:
        # theta
        c0 = a[0] ^ a[5] ^ a[10] ^ a[15] ^ a[20]
        c1 = a[1] ^ a[6] ^ a[11] ^ a[16] ^ a[21]
        c2 = a[2] ^ a[7] ^ a[12] ^ a[17] ^ a[22]
        c3 = a[3] ^ a[8] ^ a[13] ^ a[18] ^ a[23]
        c4 = a[4] ^ a[9] ^ a[14] ^ a[19] ^ a[24]
        d0 = c4 ^ (((c1 << 1) | (c1 >> 63)) & _M)
        d1 = c0 ^ (((c2 << 1) | (c2 >> 63)) & _M)
        d2 = c1 ^ (((c3 << 1) | (c3 >> 63)) & _M)
        d3 = c2 ^ (((c4 << 1) | (c4 >> 63)) & _M)
        d4 = c3 ^ (((c0 << 1) | (c0 >> 63)) & _M)
        for i in range(0, 25, 5):
            a[i] ^= d0
            a[i + 1] ^= d1
            a[i + 2] ^= d2
            a[i + 3] ^= d3
            a[i + 4] ^= d4

        # rho and pi
        b = [0] * 25
        for dst, src, r in _PI_ROT:
            v = a[src]
            if r:
                b[dst] = ((v << r) | (v >> (64 - r))) & _M
            else:
                b[dst] = v

        # chi, one row of five lanes at a time
        for i in range(0, 25, 5):
            b0 = b[i]
            b1 = b[i + 1]
            b2 = b[i + 2]
            b3 = b[i + 3]
            b4 = b[i + 4]
            a[i] = b0 ^ (~b1 & _M & b2)
            a[i + 1] = b1 ^ (~b2 & _M & b3)
            a[i + 2] = b2 ^ (~b3 & _M & b4)
            a[i + 3] = b3 ^ (~b4 & _M & b0)
            a[i + 4] = b4 ^ (~b0 & _M & b1)

        # iota
        a[0] ^= rc


class _Keccak(metaclass=_Immutable):
    """The sponge.  Subclasses supply _rate, _dsbyte and the attributes."""

    _rate = 0
    _dsbyte = 0x06

    # Three attributes CPython's _sha3 objects expose and test_hashlib reads
    # back.  Derived from the rate rather than tabulated, because the whole
    # point of a sponge is that capacity + rate is always the state size.
    @property
    def _rate_bits(self):
        return self._rate * 8

    @property
    def _capacity_bits(self):
        return 1600 - self._rate * 8

    @property
    def _suffix(self):
        return bytes([self._dsbyte])

    def __init__(self, data=b"", *, usedforsecurity=True):
        # usedforsecurity is accepted and ignored, as in _sha2: it selects a
        # FIPS-restricted provider in CPython's OpenSSL build and means
        # nothing to a Python implementation.
        self._state = [0] * 25
        self._buf = b""
        # Unconditional: a FALSY non-buffer has to be refused too, and
        # `if data:` skipped the check for None and 0 entirely.
        self.update(data)

    def update(self, data):
        data = _tobytes(data)
        buf = self._buf + data
        rate = self._rate
        n = len(buf) - (len(buf) % rate)
        for i in range(0, n, rate):
            self._absorb(buf[i:i + rate])
        self._buf = buf[n:]
        return None

    def _absorb(self, block):
        a = self._state
        for i in range(0, self._rate, 8):
            a[i >> 3] ^= int.from_bytes(block[i:i + 8], "little")
        _keccak_f(a)

    def copy(self):
        other = self.__class__()
        other._state = list(self._state)
        other._buf = self._buf
        return other

    def _squeeze(self, length):
        """Pad a COPY, then squeeze `length` bytes out of it.

        A copy, because `digest()` must not consume the object: `h.digest()`
        twice answers the same thing, and `h.update(x); h.digest()` still
        works afterwards.  _sha1 and _sha2 finish a copy for the same reason.
        """
        clone = self.copy()
        a = clone._state
        rate = self._rate

        # pad10*1, with the domain byte folded into the first pad byte.  When
        # the remainder is exactly rate-1 the two ends land in the same byte,
        # which is why this is an OR rather than two writes.
        block = bytearray(rate)
        block[:len(clone._buf)] = clone._buf
        block[len(clone._buf)] |= self._dsbyte
        block[rate - 1] |= 0x80
        clone._absorb(bytes(block))

        out = bytearray()
        while len(out) < length:
            for i in range(0, rate, 8):
                out += a[i >> 3].to_bytes(8, "little")
            if len(out) < length:
                _keccak_f(a)
        return bytes(out[:length])


class _Sha3(_Keccak):
    """The four fixed-width digests.  `digest()` takes no argument."""

    def digest(self):
        return self._squeeze(self.digest_size)

    def hexdigest(self):
        return self.digest().hex()


class _Shake(_Keccak):
    """The two extendable-output functions.

    `digest_size` is 0 -- there is no natural one -- and the length is a
    REQUIRED positional argument, which is the whole difference between an
    XOF and a hash.
    """

    _dsbyte = 0x1F
    digest_size = 0

    # CPython refuses a negative length, anything it cannot fit in a C
    # unsigned long, and -- measured, the boundary is exact -- any length at
    # or above 1 << 29.
    _MAX_LENGTH = 1 << 29

    @classmethod
    def _check_length(cls, length):
        length = length.__index__()
        if length < 0:
            raise ValueError("value must be positive")
        if length >= 1 << 64:
            raise OverflowError(
                "Python int too large to convert to C unsigned long")
        if length >= cls._MAX_LENGTH:
            raise ValueError("length is too large")
        return length

    def digest(self, length):
        return self._squeeze(self._check_length(length))

    def hexdigest(self, length):
        return self._squeeze(self._check_length(length)).hex()


class sha3_224(_Sha3):
    name = "sha3_224"
    digest_size = 28
    block_size = 144
    _rate = 144


class sha3_256(_Sha3):
    name = "sha3_256"
    digest_size = 32
    block_size = 136
    _rate = 136


class sha3_384(_Sha3):
    name = "sha3_384"
    digest_size = 48
    block_size = 104
    _rate = 104


class sha3_512(_Sha3):
    name = "sha3_512"
    digest_size = 64
    block_size = 72
    _rate = 72


class shake_128(_Shake):
    name = "shake_128"
    block_size = 168
    _rate = 168


class shake_256(_Shake):
    name = "shake_256"
    block_size = 136
    _rate = 136


implementation = "python"
