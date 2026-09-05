# hashlib's digests, which were all missing because each is a C module.
#
# CPython's hashlib asks _sha2, _sha1, _md5 and friends for a constructor and
# raises when none answers; nine of its own Lib/ modules stop there, most of
# them through `random`, which does `from hashlib import sha512` three lines
# in.  The algorithms are FIPS 180-4 and RFC 1321, in Python.
#
# The digests are checked against known answers rather than against another
# implementation: a hash that agrees with itself proves nothing.
import _md5
import _sha1
import _sha2

VECTORS = [
    (_md5.md5, b"", "d41d8cd98f00b204e9800998ecf8427e"),
    (_md5.md5, b"abc", "900150983cd24fb0d6963f7d28e17f72"),
    (_md5.md5, b"a" * 1000, "cabe45dcc9ae5b66ba86600cca6b8ba8"),
    (_sha1.sha1, b"", "da39a3ee5e6b4b0d3255bfef95601890afd80709"),
    (_sha1.sha1, b"abc", "a9993e364706816aba3e25717850c26c9cd0d89d"),
    (_sha2.sha224, b"abc",
     "23097d223405d8228642a477bda255b32aadbce4bda0b3f7e36c9da7"),
    (_sha2.sha256, b"",
     "e3b0c44298fc1c149afbf4c8996fb92427ae41e4649b934ca495991b7852b855"),
    (_sha2.sha256, b"abc",
     "ba7816bf8f01cfea414140de5dae2223b00361a396177a9cb410ff61f20015ad"),
    (_sha2.sha384, b"abc",
     "cb00753f45a35e8bb5a03d699ac65007272c32ab0eded1631a8b605a43ff5bed"
     "8086072ba1e7cc2358baeca134c825a7"),
    (_sha2.sha512, b"abc",
     "ddaf35a193617abacc417349ae20413112e6fa4e89a97ea20a9eeee64b55d39a"
     "2192992a274fc1a836ba3c23a3feebbd454d4423643ce80e2a9ac94fa54ca49f"),
    (_sha2.sha512, b"",
     "cf83e1357eefb8bdf1542850d66d8007d620e4050b5715dc83f4a921d36ce9ce"
     "47d0d13c5d85f2b0ff8318d2877eec2f63b931bd47417a81a538327af927da3e"),
]

for fn, data, want in VECTORS:
    got = fn(data).hexdigest()
    print("%-8s %-6s %s" % (fn(b"").name, len(data), got == want))

print()
print("-- the block boundaries, where the padding decides")
for n in (0, 1, 54, 55, 56, 63, 64, 65, 110, 111, 112, 119, 120, 127, 128, 129):
    d = b"x" * n
    print(n, _sha2.sha256(d).hexdigest()[:16], _sha2.sha512(d).hexdigest()[:16],
          _sha1.sha1(d).hexdigest()[:16], _md5.md5(d).hexdigest()[:16])

print()
print("-- incremental, copy, and a digest that does not disturb the state")
h = _sha2.sha256()
h.update(b"a")
h.update(b"b")
first = h.hexdigest()
c = h.copy()
c.update(b"c")
print(first == _sha2.sha256(b"ab").hexdigest())
print(c.hexdigest() == _sha2.sha256(b"abc").hexdigest())
print(h.hexdigest() == first)
print(_sha2.sha256().digest_size, _sha2.sha256().block_size)
print(_sha2.sha512().digest_size, _sha2.sha512().block_size)
try:
    _sha2.sha256("a string")
except TypeError as e:
    print("TypeError:", e)
