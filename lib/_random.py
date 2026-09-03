"""_random - the Mersenne Twister behind random.Random.

CPython puts MT19937 in C for speed.  This is the same generator, in Python:
the same state, the same tempering, and the same init_by_array seeding, so a
given seed produces the same sequence CPython produces.  random.Random
subclasses this and gets everything else from it.
"""

_N = 624
_M = 397
_MATRIX_A = 0x9908b0df
_UPPER_MASK = 0x80000000
_LOWER_MASK = 0x7fffffff
_MASK32 = 0xffffffff


class Random:
    """MT19937.  The five methods random.Random actually calls."""

    __slots__ = ("_mt", "_mti", "_gauss_next")

    def __init__(self, x=None):
        self._mt = [0] * _N
        self._mti = _N + 1
        self._gauss_next = None
        self.seed(x)

    # -- seeding ---------------------------------------------------------
    def _init_genrand(self, s):
        mt = self._mt
        mt[0] = s & _MASK32
        for i in range(1, _N):
            mt[i] = (1812433253 * (mt[i - 1] ^ (mt[i - 1] >> 30)) + i) & _MASK32
        self._mti = _N

    def _init_by_array(self, key):
        self._init_genrand(19650218)
        mt = self._mt
        i = 1
        j = 0
        k = _N if _N > len(key) else len(key)
        while k:
            mt[i] = ((mt[i] ^ ((mt[i - 1] ^ (mt[i - 1] >> 30)) * 1664525))
                     + key[j] + j) & _MASK32
            i += 1
            j += 1
            if i >= _N:
                mt[0] = mt[_N - 1]
                i = 1
            if j >= len(key):
                j = 0
            k -= 1
        k = _N - 1
        while k:
            mt[i] = ((mt[i] ^ ((mt[i - 1] ^ (mt[i - 1] >> 30)) * 1566083941))
                     - i) & _MASK32
            i += 1
            if i >= _N:
                mt[0] = mt[_N - 1]
                i = 1
            k -= 1
        mt[0] = 0x80000000

    def seed(self, a=None, version=2):
        """CPython's rule, from _randommodule.c: None means the OS entropy
        source, an exact int is used by absolute value, and anything else is
        seeded from hash(a) cast to an unsigned word.  random.Random.seed does
        the sha512 hashing of str and bytes ITSELF, in Python, and hands this
        an int -- so the reproducible path is the int one, and it is
        bit-for-bit CPython's."""
        if a is None:
            try:
                import posix
                a = int.from_bytes(posix.urandom(32), "big")
            except Exception:
                import time
                a = int(time.time() * 1000000)
        elif isinstance(a, int):
            a = abs(a)
        else:
            a = hash(a) & 0xffffffffffffffff

        key = []
        if a == 0:
            key = [0]
        while a:
            key.append(a & _MASK32)
            a >>= 32
        self._init_by_array(key)

    # -- generation ------------------------------------------------------
    def _genrand_uint32(self):
        mt = self._mt
        if self._mti >= _N:
            if self._mti == _N + 1:
                self._init_genrand(5489)
            for kk in range(_N - _M):
                y = (mt[kk] & _UPPER_MASK) | (mt[kk + 1] & _LOWER_MASK)
                mt[kk] = mt[kk + _M] ^ (y >> 1) ^ (_MATRIX_A if y & 1 else 0)
            for kk in range(_N - _M, _N - 1):
                y = (mt[kk] & _UPPER_MASK) | (mt[kk + 1] & _LOWER_MASK)
                mt[kk] = (mt[kk + (_M - _N)] ^ (y >> 1)
                          ^ (_MATRIX_A if y & 1 else 0))
            y = (mt[_N - 1] & _UPPER_MASK) | (mt[0] & _LOWER_MASK)
            mt[_N - 1] = mt[_M - 1] ^ (y >> 1) ^ (_MATRIX_A if y & 1 else 0)
            self._mti = 0

        y = mt[self._mti]
        self._mti += 1
        y ^= y >> 11
        y ^= (y << 7) & 0x9d2c5680
        y ^= (y << 15) & 0xefc60000
        y &= _MASK32
        y ^= y >> 18
        return y

    def random(self):
        """A double in [0, 1), from 53 bits -- CPython's split of the two
        words is what makes the sequences match."""
        a = self._genrand_uint32() >> 5
        b = self._genrand_uint32() >> 6
        return (a * 67108864.0 + b) * (1.0 / 9007199254740992.0)

    def getrandbits(self, k):
        if not isinstance(k, int):
            raise TypeError("number of bits must be an integer")
        if k < 0:
            raise ValueError("number of bits must be non-negative")
        if k == 0:
            return 0
        if k <= 32:
            return self._genrand_uint32() >> (32 - k)
        words = (k + 31) // 32
        result = 0
        shift = 0
        for i in range(words):
            bits = k - 32 * i
            if bits > 32:
                bits = 32
            r = self._genrand_uint32() >> (32 - bits)
            result |= r << shift
            shift += bits
        return result

    def getstate(self):
        return tuple(self._mt) + (self._mti,)

    def setstate(self, state):
        if not isinstance(state, tuple):
            raise TypeError("state vector must be a tuple")
        if len(state) != _N + 1:
            raise ValueError("state vector is the wrong size")
        self._mt = [int(v) & _MASK32 for v in state[:_N]]
        mti = int(state[_N])
        if mti < 0 or mti > _N:
            raise ValueError("invalid state")
        self._mti = mti
