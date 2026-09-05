"""_sha1 - SHA-1.

FIPS 180-4's first algorithm, and the one `hashlib` reaches for by name.  It
is here for the same reason _sha2 is: `hashlib` will not give out a digest it
has no module for, and several of CPython's own Lib/ modules ask for one.
"""

_M32 = 0xFFFFFFFF


def _rotl(x, n):
    return ((x << n) | (x >> (32 - n))) & _M32


class _Sha1:
    name = "sha1"
    block_size = 64
    digest_size = 20

    def __init__(self):
        self._h = [0x67452301, 0xEFCDAB89, 0x98BADCFE, 0x10325476, 0xC3D2E1F0]
        self._buf = b""
        self._len = 0

    def update(self, data):
        if isinstance(data, str):
            raise TypeError("Strings must be encoded before hashing")
        data = bytes(data)
        self._len += len(data)
        buf = self._buf + data
        n = len(buf) - (len(buf) % 64)
        for i in range(0, n, 64):
            self._compress(buf[i:i + 64])
        self._buf = buf[n:]

    def _compress(self, block):
        w = list(int.from_bytes(block[i:i + 4], "big") for i in range(0, 64, 4))
        for i in range(16, 80):
            w.append(_rotl(w[i - 3] ^ w[i - 8] ^ w[i - 14] ^ w[i - 16], 1))
        a, b, c, d, e = self._h
        for i in range(80):
            if i < 20:
                f = (b & c) | (~b & _M32 & d)
                k = 0x5A827999
            elif i < 40:
                f = b ^ c ^ d
                k = 0x6ED9EBA1
            elif i < 60:
                f = (b & c) | (b & d) | (c & d)
                k = 0x8F1BBCDC
            else:
                f = b ^ c ^ d
                k = 0xCA62C1D6
            t = (_rotl(a, 5) + f + e + k + w[i]) & _M32
            e, d, c, b, a = d, c, _rotl(b, 30), a, t
        self._h = [(x + y) & _M32 for x, y in zip(self._h, (a, b, c, d, e))]

    def copy(self):
        other = _Sha1()
        other._h = list(self._h)
        other._buf = self._buf
        other._len = self._len
        return other

    def digest(self):
        clone = self.copy()
        block = clone._buf + b"\x80"
        while len(block) % 64 != 56:
            block += b"\0"
        block += ((clone._len * 8) & ((1 << 64) - 1)).to_bytes(8, "big")
        for i in range(0, len(block), 64):
            clone._compress(block[i:i + 64])
        return b"".join(x.to_bytes(4, "big") for x in clone._h)

    def hexdigest(self):
        return self.digest().hex()


def sha1(data=b"", *, usedforsecurity=True):
    obj = _Sha1()
    if data:
        obj.update(data)
    return obj
