# _hashlib: OpenSSL's digests, HMAC, PBKDF2 and scrypt.
#
# Two things this buys that the pure-Python modules cannot.  `pbkdf2_hmac` and
# `scrypt` have no honest Python form at the iteration counts anyone uses -- a
# password today is 600,000 rounds -- and bulk hashing goes three or four
# orders of magnitude faster, which is what makes CPython's own
# test_sha256_update_over_4gb finish rather than time out.
#
# Merely importing _hashlib changes hashlib for the whole process:
# `hashlib.py` rebinds `new` and its constructor lookup the moment the import
# succeeds, so every digest except blake2 starts coming from OpenSSL.  That is
# why the interesting assertion here is an EQUALITY one: the same message
# hashed through OpenSSL and through lib/_sha2.py, lib/_sha3.py and
# lib/_blake2.py must agree, byte for byte, for all fourteen algorithms.  Two
# independent implementations agreeing is worth more than either matching a
# printed constant.
#
# The oracle is python3, which has its own OpenSSL _hashlib.  What is NOT
# compared is the CONTENTS of openssl_md_meth_names: CPython's is everything
# the linked provider offers -- 19 names here, with ripemd160 and sm3 among
# them -- while ours is deliberately hashlib's own fourteen.  Only the
# relationship to algorithms_guaranteed is compared, because that is the part
# a program can depend on.

import _hashlib
import _blake2
import _md5
import _sha1
import _sha2
import _sha3
import hashlib

MSGS = [b"", b"a", b"abc", b"a" * 63, b"a" * 64, b"a" * 65, b"a" * 127,
        b"a" * 128, b"a" * 136, b"a" * 168, b"a" * 1000, bytes(range(256))]

BUILTIN = {
    "md5": _md5.md5,
    "sha1": _sha1.sha1,
    "sha224": _sha2.sha224,
    "sha256": _sha2.sha256,
    "sha384": _sha2.sha384,
    "sha512": _sha2.sha512,
    "sha3_224": _sha3.sha3_224,
    "sha3_256": _sha3.sha3_256,
    "sha3_384": _sha3.sha3_384,
    "sha3_512": _sha3.sha3_512,
    "blake2b": _blake2.blake2b,
    "blake2s": _blake2.blake2s,
}
XOF = ("shake_128", "shake_256")


print("== OpenSSL and the pure-Python modules agree, for every algorithm ==")
for name in sorted(BUILTIN):
    ok = True
    for m in MSGS:
        want = BUILTIN[name](m).hexdigest()
        got = _hashlib.new(name, m).hexdigest()
        if got != want:
            ok = False
            print("   MISMATCH %s %d: %s != %s" % (name, len(m), got, want))
    print("%-10s %s" % (name, ok))

print()
print("== and for the XOFs, at every length either side of the rate ==")
for name in XOF:
    builtin = getattr(_sha3, name)
    ok = True
    for m in MSGS:
        for length in (0, 1, 16, 32, 135, 136, 167, 168, 169, 400):
            if builtin(m).hexdigest(length) != _hashlib.new(name, m).hexdigest(length):
                ok = False
    print("%-10s %s" % (name, ok))

print()
print("== the objects declare themselves the same way ==")
for name in sorted(BUILTIN) + list(XOF):
    a = _hashlib.new(name)
    b = BUILTIN[name]() if name in BUILTIN else getattr(_sha3, name)()
    print("%-10s name=%-10s digest_size %-3d==%-3d  block_size %-4d==%d"
          % (name, a.name, a.digest_size, b.digest_size,
             a.block_size, b.block_size))

print()
print("== update, copy and repeated digest behave as they must ==")
for name in ("md5", "sha256", "sha3_512"):
    h = _hashlib.new(name)
    for chunk in (b"a" * 100, b"b" * 50, b"", b"c"):
        h.update(chunk)
    one = _hashlib.new(name, b"a" * 100 + b"b" * 50 + b"c")
    k = _hashlib.new(name, b"ab")
    j = k.copy()
    k.update(b"c")
    j.update(b"Z")
    twice = _hashlib.new(name, b"abc")
    print("%-10s incremental=%s copy=%s %s repeatable=%s"
          % (name,
             h.hexdigest() == one.hexdigest(),
             k.hexdigest() == _hashlib.new(name, b"abc").hexdigest(),
             j.hexdigest() == _hashlib.new(name, b"abZ").hexdigest(),
             twice.hexdigest() == twice.hexdigest()))
# A digest must not consume the object.
h = _hashlib.new("sha256", b"abc")
h.hexdigest()
h.update(b"d")
print("updatable after digest:",
      h.hexdigest() == _hashlib.new("sha256", b"abcd").hexdigest())
x = _hashlib.new("shake_128", b"ab")
y = x.copy()
x.update(b"c")
print("xof copy independent:",
      x.hexdigest(8) == _hashlib.new("shake_128", b"abc").hexdigest(8),
      y.hexdigest(8) == _hashlib.new("shake_128", b"ab").hexdigest(8))

print()
print("== pbkdf2_hmac, RFC 6070 ==")
for name, pw, salt, it, dklen, want in (
    ("sha1", b"password", b"salt", 1, 20, "0c60c80f961f0e71f3a9b524af6012062fe037a6"),
    ("sha1", b"password", b"salt", 2, 20, "ea6c014dc72d6f8ccd1ed92ace1d41f0d8de8957"),
    ("sha1", b"password", b"salt", 4096, 20, "4b007901b765489abead49d926f721d065a429c1"),
    ("sha1", b"passwordPASSWORDpassword",
     b"saltSALTsaltSALTsaltSALTsaltSALTsalt", 4096, 25,
     "3d2eec4fe41c849b80c8d83662c0e44a8b291a964cf2f07038"),
    ("sha256", b"password", b"salt", 1, 32,
     "120fb6cffcf8b32c43e7225256c4f837a86548c92ccc35480805987cb70be17b"),
):
    got = hashlib.pbkdf2_hmac(name, pw, salt, it, dklen).hex()
    print("%-7s it=%-5d %s" % (name, it, got == want))
print("default dklen is the digest size:",
      len(hashlib.pbkdf2_hmac("sha512", b"p", b"s", 1)) == 64)

print()
print("== scrypt, RFC 7914 ==")
for pw, salt, n, r, p, want in (
    (b"", b"", 16, 1, 1,
     "77d6576238657b203b19ca42c18a0497f16b4844e3074ae8dfdffa3fede21442"
     "fcd0069ded0948f8326a753a0fc81f17e8d3e0fb2e0d3628cf35e20c38d18906"),
    (b"password", b"NaCl", 1024, 8, 16,
     "fdbabe1c9d3472007856e7190d01e9fe7c6ad7cbc8237830e77376634b373162"
     "2eaf30d92e22a3886ff109279d9830dac727afb94a83ee6d8360cbdfa2cc0640"),
):
    print("n=%-5d %s" % (n, hashlib.scrypt(pw, salt=salt, n=n, r=r, p=p).hex() == want))
print("dklen is honoured:", len(hashlib.scrypt(b"", salt=b"", n=16, r=1, p=1, dklen=16)))

print()
print("== HMAC, RFC 4231 ==")
for name, key, msg, want in (
    ("sha256", b"\x0b" * 20, b"Hi There",
     "b0344c61d8db38535ca8afceaf0bf12b881dc200c9833da726e9376c2e32cff7"),
    ("sha256", b"Jefe", b"what do ya want for nothing?",
     "5bdcc146bf60754e6a042426089575c75a003f089d2739839dec58b964ec3843"),
    ("sha512", b"\x0b" * 20, b"Hi There",
     "87aa7cdea5ef619d4ff0b4241a1d6cb02379f4e2ce4ec2787ad0b30545e17cde"
     "daa833b7d6b8a702038b274eaea3f4e4be9d914eeb61f1702e696c203a126854"),
):
    got = _hashlib.hmac_digest(key, msg, name).hex()
    print("%-7s %s" % (name, got == want))
h = _hashlib.hmac_new(b"key", b"", "sha256")
h.update(b"The quick brown fox ")
h.update(b"jumps over the lazy dog")
print("incremental hmac:",
      h.hexdigest() == "f7bc83f430538424b13298e6aa6fb143ef4d59a14946175997479dbc2d1a3cd8")
print("hmac copy:", _hashlib.hmac_new(b"k", b"abc", "sha256").copy().hexdigest()
      == _hashlib.hmac_new(b"k", b"abc", "sha256").hexdigest())
print("hmac repeatable:", h.hexdigest() == h.hexdigest())
hm = _hashlib.hmac_new(b"k", None, "sha256")
print("hmac msg=None means absent:",
      hm.hexdigest() == _hashlib.hmac_new(b"k", b"", "sha256").hexdigest())
print("hmac name and sizes:", hm.name, hm.digest_size, hm.block_size)

print()
print("== compare_digest ==")
print("equal:", _hashlib.compare_digest(b"foobar", b"foobar"))
print("differing:", _hashlib.compare_digest(b"foobar", b"foobaz"))
print("shorter:", _hashlib.compare_digest(b"foobar", b"foo"))
print("empty:", _hashlib.compare_digest(b"", b""))
print("bytearray:", _hashlib.compare_digest(bytearray(b"ab"), b"ab"))
print("memoryview:", _hashlib.compare_digest(memoryview(b"ab"), b"ab"))
print("ascii str:", _hashlib.compare_digest("ab", "ab"), _hashlib.compare_digest("ab", "ac"))


class mybytes(bytes):
    def __eq__(self, other):
        return False


print("bytes subclass:", _hashlib.compare_digest(mybytes(b"ab"), mybytes(b"ab")),
      _hashlib.compare_digest(mybytes(b"ab"), b"ab"))

print()
print("== the types are not instantiable and not mutable ==")
for T in (_hashlib.HASH, _hashlib.HASHXOF, _hashlib.HMAC):
    try:
        T()
    except TypeError as e:
        print("%-8s %s" % (T.__name__, e))
    try:
        T.value = False
        print("%-8s MUTABLE" % T.__name__)
    except TypeError as e:
        print("%-8s immutable: %s" % (T.__name__, "immutable type" in str(e)))

print()
print("== a non-buffer is refused wherever a message is taken ==")
for label, fn in (
    ("new(None)", lambda: _hashlib.new("sha256", None)),
    ("new(0)", lambda: _hashlib.new("sha256", 0)),
    ("new([1])", lambda: _hashlib.new("sha256", [1])),
    ("new(str)", lambda: _hashlib.new("sha256", "text")),
    ("update(None)", lambda: _hashlib.new("sha256").update(None)),
    ("update(str)", lambda: _hashlib.new("sha256").update("text")),
    ("compare_digest(1, 2)", lambda: _hashlib.compare_digest(1, 2)),
    ("compare_digest(1, b'')", lambda: _hashlib.compare_digest(1, b"")),
    ("compare_digest(str, bytes)", lambda: _hashlib.compare_digest("a", b"a")),
    ("hmac msg=0", lambda: _hashlib.hmac_new(b"k", 0, "sha256")),
):
    try:
        fn()
        print("%-28s ACCEPTED" % label)
    except (TypeError, ValueError) as e:
        print("%-28s %s: %s" % (label, type(e).__name__, e))

# The TYPE only for these two: an unknown name gets OpenSSL's own error-queue
# text in CPython ("[digital envelope routines] unsupported"), which is not
# worth reproducing, and the type is what hashlib and hmac branch on.
for label, fn in (("unknown name", lambda: _hashlib.new("nosuch")),
                  ("non-str name", lambda: _hashlib.new(7))):
    try:
        fn()
        print("%-28s ACCEPTED" % label)
    except (TypeError, ValueError) as e:
        print("%-28s %s (ValueError=%s)"
              % (label, type(e).__name__, isinstance(e, ValueError)))

print()
print("== a shake length is validated ==")
# Only that it IS validated, not how: CPython's OpenSSL HASHXOF has no length
# check at all here.  `digest(-1)` gives it
# `SystemError: Negative size passed to PyBytes_FromStringAndSize` and
# `digest(2**32)` makes it try to allocate four gigabytes.  Refusing both is
# deliberate and recorded in DIVERGENCES.md; comparing the wording against a
# SystemError would be comparing against a bug.
h = _hashlib.new("shake_128")
refused = 0
for length in (-1, -10, 1 << 29, 2 ** 32, 2 ** 64 + 10):
    for meth in (h.digest, h.hexdigest):
        try:
            meth(length)
        except (ValueError, OverflowError):
            refused += 1
        except BaseException:
            pass
print("lengths refused out of 10:", refused >= 2)
print("a sane length still works:", len(h.digest(32)), len(h.hexdigest(32)))

print()
print("== pbkdf2 and scrypt refuse what they must ==")
for label, fn in (
    ("pbkdf2 iterations=0", lambda: hashlib.pbkdf2_hmac("sha1", b"p", b"s", 0)),
    ("pbkdf2 iterations=-1", lambda: hashlib.pbkdf2_hmac("sha1", b"p", b"s", -1)),
    ("pbkdf2 dklen=0", lambda: hashlib.pbkdf2_hmac("sha1", b"p", b"s", 1, 0)),
    ("pbkdf2 dklen=-1", lambda: hashlib.pbkdf2_hmac("sha1", b"p", b"s", 1, -1)),
    ("pbkdf2 unknown hash", lambda: hashlib.pbkdf2_hmac("nosuch", b"p", b"s", 1)),
    ("scrypt n=1", lambda: hashlib.scrypt(b"p", salt=b"s", n=1, r=8, p=1)),
    ("scrypt n=15", lambda: hashlib.scrypt(b"p", salt=b"s", n=15, r=8, p=1)),
    ("scrypt n=0", lambda: hashlib.scrypt(b"p", salt=b"s", n=0, r=8, p=1)),
    ("scrypt r=0", lambda: hashlib.scrypt(b"p", salt=b"s", n=2, r=0, p=1)),
    ("scrypt p=0", lambda: hashlib.scrypt(b"p", salt=b"s", n=2, r=8, p=0)),
    ("scrypt dklen=0", lambda: hashlib.scrypt(b"p", salt=b"s", n=2, r=8, p=1, dklen=0)),
    ("scrypt maxmem=-1", lambda: hashlib.scrypt(b"p", salt=b"s", n=2, r=8, p=1, maxmem=-1)),
    ("scrypt no keywords", lambda: hashlib.scrypt(b"p")),
    ("scrypt positional", lambda: hashlib.scrypt(b"p", b"s")),
    ("scrypt n=None", lambda: hashlib.scrypt(b"p", salt=b"s", n=None, r=8, p=1)),
    ("scrypt str password", lambda: hashlib.scrypt("p", salt=b"s", n=2, r=8, p=1)),
):
    try:
        fn()
        print("%-24s ACCEPTED" % label)
    except (TypeError, ValueError, OverflowError) as e:
        print("%-24s %s" % (label, type(e).__name__))

print()
print("== what hashlib reports, now that OpenSSL is behind it ==")
print("guaranteed is a subset of available:",
      hashlib.algorithms_guaranteed <= hashlib.algorithms_available)
print("every guaranteed name is an attribute:",
      sorted(n for n in hashlib.algorithms_guaranteed if not hasattr(hashlib, n)))
print("md_meth_names covers the twelve OpenSSL serves:",
      sorted(hashlib.algorithms_guaranteed - {"blake2b", "blake2s"})
      == sorted(n for n in _hashlib.openssl_md_meth_names
                if n in hashlib.algorithms_guaranteed
                and n not in ("blake2b", "blake2s")))
print("no OpenSSL alias leaked into available:",
      "blake2b512" not in hashlib.algorithms_available,
      "sha3-512" not in hashlib.algorithms_available)
print("blake2 has no openssl_ constructor:",
      not hasattr(_hashlib, "openssl_blake2b"),
      not hasattr(_hashlib, "openssl_blake2s"))
print("blake2 still keyed, so it came from _blake2:",
      hashlib.blake2b(b"abc", key=b"k").hexdigest()
      == _blake2.blake2b(b"abc", key=b"k").hexdigest())
print("get_fips_mode:", _hashlib.get_fips_mode())
print("file_digest over a BytesIO:", end=" ")
import io

print(hashlib.file_digest(io.BytesIO(b"abc" * 1000), "sha256").hexdigest()
      == hashlib.sha256(b"abc" * 1000).hexdigest())
