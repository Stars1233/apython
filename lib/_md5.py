"""_md5 - MD5.

RFC 1321.  Broken as a cryptographic hash and still asked for by name all
over the standard library, which is why `hashlib` has to be able to give it
out: a module that wants it for a cache key or an etag does not care.

Its rounds are the one place a table beats writing them out -- the shift
amounts and the message index have no structure to read, they are the
constants of the algorithm -- so both are tables here and the round is one
loop.
"""

_M32 = 0xFFFFFFFF

# The per-round sine constants: floor(abs(sin(i + 1)) * 2**32).
_T = tuple(
    (0xD76AA478, 0xE8C7B756, 0x242070DB, 0xC1BDCEEE, 0xF57C0FAF, 0x4787C62A,
     0xA8304613, 0xFD469501, 0x698098D8, 0x8B44F7AF, 0xFFFF5BB1, 0x895CD7BE,
     0x6B901122, 0xFD987193, 0xA679438E, 0x49B40821, 0xF61E2562, 0xC040B340,
     0x265E5A51, 0xE9B6C7AA, 0xD62F105D, 0x02441453, 0xD8A1E681, 0xE7D3FBC8,
     0x21E1CDE6, 0xC33707D6, 0xF4D50D87, 0x455A14ED, 0xA9E3E905, 0xFCEFA3F8,
     0x676F02D9, 0x8D2A4C8A, 0xFFFA3942, 0x8771F681, 0x6D9D6122, 0xFDE5380C,
     0xA4BEEA44, 0x4BDECFA9, 0xF6BB4B60, 0xBEBFBC70, 0x289B7EC6, 0xEAA127FA,
     0xD4EF3085, 0x04881D05, 0xD9D4D039, 0xE6DB99E5, 0x1FA27CF8, 0xC4AC5665,
     0xF4292244, 0x432AFF97, 0xAB9423A7, 0xFC93A039, 0x655B59C3, 0x8F0CCC92,
     0xFFEFF47D, 0x85845DD1, 0x6FA87E4F, 0xFE2CE6E0, 0xA3014314, 0x4E0811A1,
     0xF7537E82, 0xBD3AF235, 0x2AD7D2BB, 0xEB86D391))

_S = (7, 12, 17, 22) * 4 + (5, 9, 14, 20) * 4 + \
     (4, 11, 16, 23) * 4 + (6, 10, 15, 21) * 4


def _rotl(x, n):
    return ((x << n) | (x >> (32 - n))) & _M32


class _Md5:
    name = "md5"
    block_size = 64
    digest_size = 16

    def __init__(self):
        self._h = [0x67452301, 0xEFCDAB89, 0x98BADCFE, 0x10325476]
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
        m = [int.from_bytes(block[i:i + 4], "little") for i in range(0, 64, 4)]
        a, b, c, d = self._h
        for i in range(64):
            if i < 16:
                f = (b & c) | (~b & _M32 & d)
                g = i
            elif i < 32:
                f = (d & b) | (~d & _M32 & c)
                g = (5 * i + 1) % 16
            elif i < 48:
                f = b ^ c ^ d
                g = (3 * i + 5) % 16
            else:
                f = c ^ (b | (~d & _M32))
                g = (7 * i) % 16
            tmp = d
            d = c
            c = b
            b = (b + _rotl((a + f + _T[i] + m[g]) & _M32, _S[i])) & _M32
            a = tmp
        self._h = [(x + y) & _M32 for x, y in zip(self._h, (a, b, c, d))]

    def copy(self):
        other = _Md5()
        other._h = list(self._h)
        other._buf = self._buf
        other._len = self._len
        return other

    def digest(self):
        clone = self.copy()
        block = clone._buf + b"\x80"
        while len(block) % 64 != 56:
            block += b"\0"
        block += ((clone._len * 8) & ((1 << 64) - 1)).to_bytes(8, "little")
        for i in range(0, len(block), 64):
            clone._compress(block[i:i + 64])
        return b"".join(x.to_bytes(4, "little") for x in clone._h)

    def hexdigest(self):
        return self.digest().hex()


def md5(data=b"", *, usedforsecurity=True):
    obj = _Md5()
    if data:
        obj.update(data)
    return obj
