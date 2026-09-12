"""_blake2 - BLAKE2b and BLAKE2s, RFC 7693.

This one is not optional even once `_hashlib` exists.  `hashlib` keeps a set
called `__block_openssl_constructor` holding exactly `{'blake2b', 'blake2s'}`
and sends both to the builtin module UNCONDITIONALLY, because OpenSSL's BLAKE2
supports neither keying nor the tree parameters -- it offers only the plain
`blake2b512`/`blake2s256` digests.  So these two always come from here.

That is also why this module carries a much wider surface than `_sha1` or
`_sha2`: the whole parameter block is addressable.  `digest_size`, `key`,
`salt` and `person` are the four anyone uses; `fanout`, `depth`, `leaf_size`,
`node_offset`, `node_depth`, `inner_size` and `last_node` are the tree-hashing
ones, and they are here because CPython's are and `test_hashlib` sets them.

Both constructors are CLASSES, which is forced rather than chosen: callers read
`blake2b.MAX_DIGEST_SIZE` and the other three limits off the CONSTRUCTOR, so a
plain factory function like `_sha2.sha256` cannot serve.
"""

BLAKE2B_SALT_SIZE = 16
BLAKE2B_PERSON_SIZE = 16
BLAKE2B_MAX_KEY_SIZE = 64
BLAKE2B_MAX_DIGEST_SIZE = 64

BLAKE2S_SALT_SIZE = 8
BLAKE2S_PERSON_SIZE = 8
BLAKE2S_MAX_KEY_SIZE = 32
BLAKE2S_MAX_DIGEST_SIZE = 32

_IV64 = (
    0x6A09E667F3BCC908, 0xBB67AE8584CAA73B, 0x3C6EF372FE94F82B,
    0xA54FF53A5F1D36F1, 0x510E527FADE682D1, 0x9B05688C2B3E6C1F,
    0x1F83D9ABFB41BD6B, 0x5BE0CD19137E2179,
)

_IV32 = (
    0x6A09E667, 0xBB67AE85, 0x3C6EF372, 0xA54FF53A,
    0x510E527F, 0x9B05688C, 0x1F83D9AB, 0x5BE0CD19,
)

_SIGMA = (
    (0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15),
    (14, 10, 4, 8, 9, 15, 13, 6, 1, 12, 0, 2, 11, 7, 5, 3),
    (11, 8, 12, 0, 5, 2, 15, 13, 10, 14, 3, 6, 7, 1, 9, 4),
    (7, 9, 3, 1, 13, 12, 11, 14, 2, 6, 5, 10, 4, 0, 15, 8),
    (9, 0, 5, 7, 2, 4, 10, 15, 14, 1, 11, 12, 6, 8, 3, 13),
    (2, 12, 6, 10, 0, 11, 8, 3, 4, 13, 7, 5, 15, 14, 1, 9),
    (12, 5, 1, 15, 14, 13, 4, 10, 0, 7, 6, 3, 9, 2, 8, 11),
    (13, 11, 7, 14, 12, 1, 3, 9, 5, 0, 15, 4, 8, 6, 2, 10),
    (6, 15, 14, 9, 11, 3, 0, 8, 12, 2, 13, 7, 1, 4, 10, 5),
    (10, 2, 8, 4, 7, 6, 1, 5, 15, 11, 9, 14, 3, 12, 13, 0),
)


class _Immutable(type):
    """CPython's hash types are C types with no settable attributes, and
    `test_hashlib.test_readonly_types` asserts it for every constructor it
    knows.  A plain Python class is mutable, so the refusal comes from here;
    instances are unaffected, it is the TYPE that is frozen.
    """

    def __setattr__(cls, name, value):
        raise TypeError("cannot set %r attribute of immutable type %r"
                        % (name, cls.__name__))

    def __delattr__(cls, name):
        raise TypeError("cannot delete %r attribute of immutable type %r"
                        % (name, cls.__name__))


def _byte_field(value, name, low, high):
    """One of the single-byte parameter-block fields, range-checked.

    CPython refuses each of these rather than truncating, and `test_hashlib`
    walks every value in range and both values just outside it.  The wordings
    are CPython's, including `inner_size`'s -- "must be between 0 and is 64"
    reads like a typo because it is one, in blake2module.c.
    """
    value = value.__index__()
    if not low <= value <= high:
        raise ValueError("%s must be between %d and %d" % (name, low, high))
    return value


def _wide_field(value, name, bits):
    """leaf_size and node_offset: positive, and small enough for their width.

    CPython separates the two failures -- ValueError for a negative and
    OverflowError for too large -- and test_hashlib asserts each by type.
    """
    value = value.__index__()
    if value < 0:
        raise ValueError("value must be positive")
    if value >> bits:
        raise OverflowError("%s is too large" % name)
    return value


def _as_bytes(value, what):
    if isinstance(value, str):
        raise TypeError("Strings must be encoded before hashing")
    return bytes(value)


class _Blake2(metaclass=_Immutable):
    """The compression function and the sponge around it, for both widths.

    Subclasses supply the word size, the IV, the rotation constants, the round
    count and the limits; everything else here is shared, because BLAKE2b and
    BLAKE2s differ only in those numbers.
    """

    _bits = 0                   # word size
    _mask = 0
    _iv = ()
    _rounds = 0
    _rot = ()                   # the four G rotations
    SALT_SIZE = 0
    PERSON_SIZE = 0
    MAX_KEY_SIZE = 0
    MAX_DIGEST_SIZE = 0

    def __init__(self, data=b"", /, *, digest_size=None, key=b"", salt=b"",
                 person=b"", fanout=1, depth=1, leaf_size=0, node_offset=0,
                 node_depth=0, inner_size=0, last_node=False,
                 usedforsecurity=True):
        # data is positional-only, as CPython's is: both `blake2b(data=b"")`
        # and `blake2b(string=b"")` are TypeErrors there.
        if isinstance(data, str):
            # Checked here rather than left to update(), which is not reached
            # for an EMPTY string -- and `blake2b("")` is a TypeError.
            raise TypeError("Strings must be encoded before hashing")
        if digest_size is None:
            digest_size = self.MAX_DIGEST_SIZE
        digest_size = digest_size.__index__()
        if not 1 <= digest_size <= self.MAX_DIGEST_SIZE:
            raise ValueError("digest_size must be between 1 and %d bytes"
                             % self.MAX_DIGEST_SIZE)
        key = _as_bytes(key, "key")
        salt = _as_bytes(salt, "salt")
        person = _as_bytes(person, "person")
        if len(key) > self.MAX_KEY_SIZE:
            raise ValueError("maximum key length is %d bytes"
                             % self.MAX_KEY_SIZE)
        if len(salt) > self.SALT_SIZE:
            raise ValueError("maximum salt length is %d bytes"
                             % self.SALT_SIZE)
        if len(person) > self.PERSON_SIZE:
            raise ValueError("maximum person length is %d bytes"
                             % self.PERSON_SIZE)

        fanout = _byte_field(fanout, "fanout", 0, 255)
        depth = _byte_field(depth, "depth", 1, 255)
        node_depth = _byte_field(node_depth, "node_depth", 0, 255)
        # inner_size's upper bound is the digest limit, not 255, and its
        # message is CPython's own malformed one.
        inner_size = inner_size.__index__()
        if not 0 <= inner_size <= self.MAX_DIGEST_SIZE:
            raise ValueError("inner_size must be between 0 and is %d"
                             % self.MAX_DIGEST_SIZE)
        leaf_size = _wide_field(leaf_size, "leaf_size", 32)
        node_offset = _wide_field(node_offset, "node_offset",
                                  64 if self._bits == 64 else 48)

        self.digest_size = digest_size
        self._last_node = bool(last_node)
        self._keyed = bool(key)

        # The parameter block is XORed into the IV.  Its first word carries
        # the four single-byte fields; salt and person occupy the last four
        # words (two for BLAKE2s).
        w = self._bits // 8
        p = bytearray(self.block_size // 2)
        p[0] = digest_size
        p[1] = len(key)
        p[2] = fanout
        p[3] = depth
        p[4:8] = leaf_size.to_bytes(4, "little")
        if w == 8:
            p[8:16] = node_offset.to_bytes(8, "little")
            p[16] = node_depth
            p[17] = inner_size
            p[32:32 + len(salt)] = salt
            p[48:48 + len(person)] = person
        else:
            # BLAKE2s packs node_offset into six bytes, so it shares a word
            # with node_depth and inner_size.
            p[8:14] = node_offset.to_bytes(6, "little")
            p[14] = node_depth
            p[15] = inner_size
            p[16:16 + len(salt)] = salt
            p[24:24 + len(person)] = person

        self._h = [self._iv[i] ^ int.from_bytes(p[i * w:(i + 1) * w], "little")
                   for i in range(8)]
        self._t = 0             # bytes compressed so far
        self._buf = b""

        # A keyed BLAKE2 prepends one zero-padded block of key material, and
        # it is a full block even for a one-byte key.
        if key:
            self.update(key + b"\0" * (self.block_size - len(key)))

        if data:
            self.update(data)

    def update(self, data):
        if isinstance(data, str):
            raise TypeError("Strings must be encoded before hashing")
        data = bytes(data)
        buf = self._buf + data
        bs = self.block_size
        # The LAST block is never compressed here: the final call needs the
        # finalisation flag, and whether a block is final is not known until
        # more data fails to arrive.  So keep one block back, always.
        i = 0
        while len(buf) - i > bs:
            self._t += bs
            self._compress(buf[i:i + bs], False)
            i += bs
        self._buf = buf[i:]
        return None

    def _compress(self, block, final):
        mask = self._mask
        bits = self._bits
        r1, r2, r3, r4 = self._rot
        w = bits // 8
        m = [int.from_bytes(block[i * w:(i + 1) * w], "little")
             for i in range(16)]
        v = list(self._h) + list(self._iv)
        v[12] ^= self._t & mask
        v[13] ^= (self._t >> bits) & mask
        if final:
            v[14] ^= mask
            if self._last_node:
                v[15] ^= mask

        for r in range(self._rounds):
            s = _SIGMA[r % 10]
            # The eight G applications of one round: four columns, then four
            # diagonals.
            for a, b, c, d, x, y in (
                (0, 4, 8, 12, s[0], s[1]),
                (1, 5, 9, 13, s[2], s[3]),
                (2, 6, 10, 14, s[4], s[5]),
                (3, 7, 11, 15, s[6], s[7]),
                (0, 5, 10, 15, s[8], s[9]),
                (1, 6, 11, 12, s[10], s[11]),
                (2, 7, 8, 13, s[12], s[13]),
                (3, 4, 9, 14, s[14], s[15]),
            ):
                va = v[a]
                vb = v[b]
                vc = v[c]
                vd = v[d]
                va = (va + vb + m[x]) & mask
                vd ^= va
                vd = ((vd >> r1) | (vd << (bits - r1))) & mask
                vc = (vc + vd) & mask
                vb ^= vc
                vb = ((vb >> r2) | (vb << (bits - r2))) & mask
                va = (va + vb + m[y]) & mask
                vd ^= va
                vd = ((vd >> r3) | (vd << (bits - r3))) & mask
                vc = (vc + vd) & mask
                vb ^= vc
                vb = ((vb >> r4) | (vb << (bits - r4))) & mask
                v[a] = va
                v[b] = vb
                v[c] = vc
                v[d] = vd

        h = self._h
        for i in range(8):
            h[i] ^= v[i] ^ v[i + 8]

    def copy(self):
        other = self.__class__(digest_size=self.digest_size)
        other._h = list(self._h)
        other._t = self._t
        other._buf = self._buf
        other._last_node = self._last_node
        other._keyed = self._keyed
        return other

    def digest(self):
        # A copy, so the object stays updatable and digest() is repeatable.
        clone = self.copy()
        block = clone._buf + b"\0" * (self.block_size - len(clone._buf))
        clone._t += len(clone._buf)
        clone._compress(block, True)
        w = self._bits // 8
        out = b"".join(x.to_bytes(w, "little") for x in clone._h)
        return out[:self.digest_size]

    def hexdigest(self):
        return self.digest().hex()


class blake2b(_Blake2):
    name = "blake2b"
    block_size = 128
    _bits = 64
    _mask = 0xFFFFFFFFFFFFFFFF
    _iv = _IV64
    _rounds = 12
    _rot = (32, 24, 16, 63)
    SALT_SIZE = BLAKE2B_SALT_SIZE
    PERSON_SIZE = BLAKE2B_PERSON_SIZE
    MAX_KEY_SIZE = BLAKE2B_MAX_KEY_SIZE
    MAX_DIGEST_SIZE = BLAKE2B_MAX_DIGEST_SIZE


class blake2s(_Blake2):
    name = "blake2s"
    block_size = 64
    _bits = 32
    _mask = 0xFFFFFFFF
    _iv = _IV32
    _rounds = 10
    _rot = (16, 12, 8, 7)
    SALT_SIZE = BLAKE2S_SALT_SIZE
    PERSON_SIZE = BLAKE2S_PERSON_SIZE
    MAX_KEY_SIZE = BLAKE2S_MAX_KEY_SIZE
    MAX_DIGEST_SIZE = BLAKE2S_MAX_DIGEST_SIZE
