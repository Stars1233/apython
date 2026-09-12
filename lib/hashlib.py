"""hashlib - the fourteen guaranteed digests.

CPython's own `hashlib.py` is a dispatcher over up to six accelerator modules
(`_md5`, `_sha1`, `_sha2`, `_sha3`, `_blake2` and OpenSSL's `_hashlib`), and
most of its length is deciding between them and reporting what is missing.
This is the same public surface written for one arrangement: the five builtin
modules are always there, so the table below is a table rather than a cascade
of `try: import`.

The two details worth keeping from CPython's version, because programs depend
on them:

  * the constructors are the module's own attributes -- `hashlib.sha256` -- as
    well as reachable through `new(name)`.  Code does both.
  * `algorithms_guaranteed` names what must exist, and `algorithms_available`
    what does.  Here they are equal.  In CPython the second is the first plus
    whatever OpenSSL adds, which is why callers are supposed to consult it
    rather than assume; when `_hashlib` arrives here it joins the same way.

`pbkdf2_hmac` and `scrypt` are deliberately absent, exactly as they are in a
CPython built without OpenSSL: `hashlib.py` imports them from `_hashlib` and
lets the ImportError through.  A caller must not find a slower Python
substitute under a name that promises a C one.
"""

import _blake2
import _md5
import _sha1
import _sha2
import _sha3

__all__ = ("md5", "sha1", "sha224", "sha256", "sha384", "sha512",
           "blake2b", "blake2s",
           "sha3_224", "sha3_256", "sha3_384", "sha3_512",
           "shake_128", "shake_256",
           "new", "algorithms_guaranteed", "algorithms_available",
           "file_digest")

md5 = _md5.md5
sha1 = _sha1.sha1
sha224 = _sha2.sha224
sha256 = _sha2.sha256
sha384 = _sha2.sha384
sha512 = _sha2.sha512
sha3_224 = _sha3.sha3_224
sha3_256 = _sha3.sha3_256
sha3_384 = _sha3.sha3_384
sha3_512 = _sha3.sha3_512
shake_128 = _sha3.shake_128
shake_256 = _sha3.shake_256
blake2b = _blake2.blake2b
blake2s = _blake2.blake2s

# CPython accepts the uppercase spellings through new() -- its
# __get_builtin_constructor matches {'SHA1', 'sha1'} and the rest -- so a
# caller that passes an OpenSSL-style name still gets a digest.
_BY_NAME = {
    "md5": md5, "MD5": md5,
    "sha1": sha1, "SHA1": sha1,
    "sha224": sha224, "SHA224": sha224,
    "sha256": sha256, "SHA256": sha256,
    "sha384": sha384, "SHA384": sha384,
    "sha512": sha512, "SHA512": sha512,
    "sha3_224": sha3_224, "sha3_256": sha3_256,
    "sha3_384": sha3_384, "sha3_512": sha3_512,
    "shake_128": shake_128, "shake_256": shake_256,
    "blake2b": blake2b, "blake2s": blake2s,
}

algorithms_guaranteed = frozenset((
    "md5", "sha1", "sha224", "sha256", "sha384", "sha512",
    "blake2b", "blake2s",
    "sha3_224", "sha3_256", "sha3_384", "sha3_512",
    "shake_128", "shake_256",
))
algorithms_available = set(algorithms_guaranteed)


def new(name, data=b"", **kwargs):
    """new(name, data=b'', **kwargs) - a new hashing object by name."""
    try:
        ctor = _BY_NAME[name]
    except (KeyError, TypeError):
        raise ValueError("unsupported hash type " + str(name)) from None
    return ctor(data, **kwargs)


def file_digest(fileobj, digest, /, *, _bufsize=2 ** 18):
    """Hash the contents of a file-like object opened in binary mode."""
    if isinstance(digest, str):
        digestobj = new(digest)
    else:
        digestobj = digest()

    if hasattr(fileobj, "getbuffer"):
        digestobj.update(fileobj.getbuffer())
        return digestobj

    if not (hasattr(fileobj, "readinto")
            and hasattr(fileobj, "readable")
            and fileobj.readable()):
        raise ValueError(
            "'%r' is not a file-like object in binary reading mode." % (fileobj,)
        )

    buf = bytearray(_bufsize)
    view = memoryview(buf)
    while True:
        size = fileobj.readinto(buf)
        if size == 0:
            break
        digestobj.update(view[:size])
    return digestobj
