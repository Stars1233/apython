"""_sha2 - SHA-224, SHA-256, SHA-384 and SHA-512.

CPython's is C, and behind `hashlib` it is what `random` reaches for --
`from hashlib import sha512 as _sha512`, three lines into the module -- so
nine of CPython's own Lib/ modules stop here rather than at anything they
name themselves.

The algorithms are FIPS 180-4 and are the same shape twice: SHA-256 over
32-bit words with a 64-byte block, SHA-512 over 64-bit words with a 128-byte
block, differing in the constants, the rotation amounts and the length field.
Writing them out is the whole module; the halves are kept apart rather than
parameterised, because the rotate amounts are the algorithm and hiding them
behind a table would make neither easier to check against the standard.
"""

_K256 = (
    0x428A2F98, 0x71374491, 0xB5C0FBCF, 0xE9B5DBA5, 0x3956C25B, 0x59F111F1,
    0x923F82A4, 0xAB1C5ED5, 0xD807AA98, 0x12835B01, 0x243185BE, 0x550C7DC3,
    0x72BE5D74, 0x80DEB1FE, 0x9BDC06A7, 0xC19BF174, 0xE49B69C1, 0xEFBE4786,
    0x0FC19DC6, 0x240CA1CC, 0x2DE92C6F, 0x4A7484AA, 0x5CB0A9DC, 0x76F988DA,
    0x983E5152, 0xA831C66D, 0xB00327C8, 0xBF597FC7, 0xC6E00BF3, 0xD5A79147,
    0x06CA6351, 0x14292967, 0x27B70A85, 0x2E1B2138, 0x4D2C6DFC, 0x53380D13,
    0x650A7354, 0x766A0ABB, 0x81C2C92E, 0x92722C85, 0xA2BFE8A1, 0xA81A664B,
    0xC24B8B70, 0xC76C51A3, 0xD192E819, 0xD6990624, 0xF40E3585, 0x106AA070,
    0x19A4C116, 0x1E376C08, 0x2748774C, 0x34B0BCB5, 0x391C0CB3, 0x4ED8AA4A,
    0x5B9CCA4F, 0x682E6FF3, 0x748F82EE, 0x78A5636F, 0x84C87814, 0x8CC70208,
    0x90BEFFFA, 0xA4506CEB, 0xBEF9A3F7, 0xC67178F2,
)

_K512 = (
    0x428A2F98D728AE22, 0x7137449123EF65CD, 0xB5C0FBCFEC4D3B2F,
    0xE9B5DBA58189DBBC, 0x3956C25BF348B538, 0x59F111F1B605D019,
    0x923F82A4AF194F9B, 0xAB1C5ED5DA6D8118, 0xD807AA98A3030242,
    0x12835B0145706FBE, 0x243185BE4EE4B28C, 0x550C7DC3D5FFB4E2,
    0x72BE5D74F27B896F, 0x80DEB1FE3B1696B1, 0x9BDC06A725C71235,
    0xC19BF174CF692694, 0xE49B69C19EF14AD2, 0xEFBE4786384F25E3,
    0x0FC19DC68B8CD5B5, 0x240CA1CC77AC9C65, 0x2DE92C6F592B0275,
    0x4A7484AA6EA6E483, 0x5CB0A9DCBD41FBD4, 0x76F988DA831153B5,
    0x983E5152EE66DFAB, 0xA831C66D2DB43210, 0xB00327C898FB213F,
    0xBF597FC7BEEF0EE4, 0xC6E00BF33DA88FC2, 0xD5A79147930AA725,
    0x06CA6351E003826F, 0x142929670A0E6E70, 0x27B70A8546D22FFC,
    0x2E1B21385C26C926, 0x4D2C6DFC5AC42AED, 0x53380D139D95B3DF,
    0x650A73548BAF63DE, 0x766A0ABB3C77B2A8, 0x81C2C92E47EDAEE6,
    0x92722C851482353B, 0xA2BFE8A14CF10364, 0xA81A664BBC423001,
    0xC24B8B70D0F89791, 0xC76C51A30654BE30, 0xD192E819D6EF5218,
    0xD69906245565A910, 0xF40E35855771202A, 0x106AA07032BBD1B8,
    0x19A4C116B8D2D0C8, 0x1E376C085141AB53, 0x2748774CDF8EEB99,
    0x34B0BCB5E19B48A8, 0x391C0CB3C5C95A63, 0x4ED8AA4AE3418ACB,
    0x5B9CCA4F7763E373, 0x682E6FF3D6B2B8A3, 0x748F82EE5DEFB2FC,
    0x78A5636F43172F60, 0x84C87814A1F0AB72, 0x8CC702081A6439EC,
    0x90BEFFFA23631E28, 0xA4506CEBDE82BDE9, 0xBEF9A3F7B2C67915,
    0xC67178F2E372532B, 0xCA273ECEEA26619C, 0xD186B8C721C0C207,
    0xEADA7DD6CDE0EB1E, 0xF57D4F7FEE6ED178, 0x06F067AA72176FBA,
    0x0A637DC5A2C898A6, 0x113F9804BEF90DAE, 0x1B710B35131C471B,
    0x28DB77F523047D84, 0x32CAAB7B40C72493, 0x3C9EBE0A15C9BEBC,
    0x431D67C49C100D4C, 0x4CC5D4BECB3E42B6, 0x597F299CFC657E2A,
    0x5FCB6FAB3AD6FAEC, 0x6C44198C4A475817,
)

_M32 = 0xFFFFFFFF
_M64 = 0xFFFFFFFFFFFFFFFF


def _rotr32(x, n):
    return ((x >> n) | (x << (32 - n))) & _M32


def _rotr64(x, n):
    return ((x >> n) | (x << (64 - n))) & _M64


class _Sha2:
    """One of the four, whichever the constructor chose.

    The state is the eight working words, the bytes not yet in a block, and
    how many bytes have been fed in -- which is all `copy()` has to duplicate
    and all `digest()` has to finish without disturbing.
    """

    def __init__(self, name, h, block_size, digest_size, wide):
        self.name = name
        self._h = list(h)
        self.block_size = block_size
        self.digest_size = digest_size
        self._wide = wide
        self._buf = b""
        self._len = 0

    def update(self, data):
        if isinstance(data, str):
            raise TypeError("Strings must be encoded before hashing")
        data = bytes(data)
        self._len += len(data)
        buf = self._buf + data
        bs = self.block_size
        n = len(buf) - (len(buf) % bs)
        for i in range(0, n, bs):
            self._compress(buf[i:i + bs])
        self._buf = buf[n:]

    def _compress(self, block):
        if self._wide:
            self._compress512(block)
        else:
            self._compress256(block)

    def _compress256(self, block):
        w = list(int.from_bytes(block[i:i + 4], "big") for i in range(0, 64, 4))
        for i in range(16, 64):
            s0 = _rotr32(w[i - 15], 7) ^ _rotr32(w[i - 15], 18) ^ (w[i - 15] >> 3)
            s1 = _rotr32(w[i - 2], 17) ^ _rotr32(w[i - 2], 19) ^ (w[i - 2] >> 10)
            w.append((w[i - 16] + s0 + w[i - 7] + s1) & _M32)
        a, b, c, d, e, f, g, h = self._h
        for i in range(64):
            s1 = _rotr32(e, 6) ^ _rotr32(e, 11) ^ _rotr32(e, 25)
            ch = (e & f) ^ (~e & _M32 & g)
            t1 = (h + s1 + ch + _K256[i] + w[i]) & _M32
            s0 = _rotr32(a, 2) ^ _rotr32(a, 13) ^ _rotr32(a, 22)
            maj = (a & b) ^ (a & c) ^ (b & c)
            t2 = (s0 + maj) & _M32
            h, g, f, e = g, f, e, (d + t1) & _M32
            d, c, b, a = c, b, a, (t1 + t2) & _M32
        self._h = [(x + y) & _M32
                   for x, y in zip(self._h, (a, b, c, d, e, f, g, h))]

    def _compress512(self, block):
        w = list(int.from_bytes(block[i:i + 8], "big") for i in range(0, 128, 8))
        for i in range(16, 80):
            s0 = _rotr64(w[i - 15], 1) ^ _rotr64(w[i - 15], 8) ^ (w[i - 15] >> 7)
            s1 = _rotr64(w[i - 2], 19) ^ _rotr64(w[i - 2], 61) ^ (w[i - 2] >> 6)
            w.append((w[i - 16] + s0 + w[i - 7] + s1) & _M64)
        a, b, c, d, e, f, g, h = self._h
        for i in range(80):
            s1 = _rotr64(e, 14) ^ _rotr64(e, 18) ^ _rotr64(e, 41)
            ch = (e & f) ^ (~e & _M64 & g)
            t1 = (h + s1 + ch + _K512[i] + w[i]) & _M64
            s0 = _rotr64(a, 28) ^ _rotr64(a, 34) ^ _rotr64(a, 39)
            maj = (a & b) ^ (a & c) ^ (b & c)
            t2 = (s0 + maj) & _M64
            h, g, f, e = g, f, e, (d + t1) & _M64
            d, c, b, a = c, b, a, (t1 + t2) & _M64
        self._h = [(x + y) & _M64
                   for x, y in zip(self._h, (a, b, c, d, e, f, g, h))]

    def copy(self):
        other = _Sha2(self.name, self._h, self.block_size, self.digest_size,
                      self._wide)
        other._buf = self._buf
        other._len = self._len
        return other

    def digest(self):
        """Finish a COPY: a hash object may be digested and then updated."""
        clone = self.copy()
        bs = clone.block_size
        lenfield = 16 if clone._wide else 8
        bits = (clone._len * 8) & ((1 << (lenfield * 8)) - 1)
        block = clone._buf + b"\x80"
        while len(block) % bs != bs - lenfield:
            block += b"\0"
        block += bits.to_bytes(lenfield, "big")
        for i in range(0, len(block), bs):
            clone._compress(block[i:i + bs])
        wide = 8 if clone._wide else 4
        out = b"".join(x.to_bytes(wide, "big") for x in clone._h)
        return out[:clone.digest_size]

    def hexdigest(self):
        return self.digest().hex()


_H224 = (0xC1059ED8, 0x367CD507, 0x3070DD17, 0xF70E5939,
         0xFFC00B31, 0x68581511, 0x64F98FA7, 0xBEFA4FA4)
_H256 = (0x6A09E667, 0xBB67AE85, 0x3C6EF372, 0xA54FF53A,
         0x510E527F, 0x9B05688C, 0x1F83D9AB, 0x5BE0CD19)
_H384 = (0xCBBB9D5DC1059ED8, 0x629A292A367CD507, 0x9159015A3070DD17,
         0x152FECD8F70E5939, 0x67332667FFC00B31, 0x8EB44A8768581511,
         0xDB0C2E0D64F98FA7, 0x47B5481DBEFA4FA4)
_H512 = (0x6A09E667F3BCC908, 0xBB67AE8584CAA73B, 0x3C6EF372FE94F82B,
         0xA54FF53A5F1D36F1, 0x510E527FADE682D1, 0x9B05688C2B3E6C1F,
         0x1F83D9ABFB41BD6B, 0x5BE0CD19137E2179)


def _make(name, h, block_size, digest_size, wide, data=b"", *,
          usedforsecurity=True):
    obj = _Sha2(name, h, block_size, digest_size, wide)
    if data:
        obj.update(data)
    return obj


def sha224(data=b"", *, usedforsecurity=True):
    return _make("sha224", _H224, 64, 28, False, data)


def sha256(data=b"", *, usedforsecurity=True):
    return _make("sha256", _H256, 64, 32, False, data)


def sha384(data=b"", *, usedforsecurity=True):
    return _make("sha384", _H384, 128, 48, True, data)


def sha512(data=b"", *, usedforsecurity=True):
    return _make("sha512", _H512, 128, 64, True, data)
