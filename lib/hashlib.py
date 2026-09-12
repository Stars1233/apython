"""hashlib - the fourteen guaranteed digests.

CPython's own `hashlib.py` is a dispatcher over up to six accelerator modules
and most of its length is deciding between them and reporting what is missing.
This is the same public surface written for one arrangement, and the
arrangement is CPython's: OpenSSL serves what it can, and the pure-Python
modules serve what it cannot and stand in if it is unavailable.

Two things decide the routing, both of them CPython's:

  * **blake2b and blake2s always come from `_blake2`**, never from OpenSSL.
    CPython keeps a set literally called `__block_openssl_constructor` holding
    those two names, because OpenSSL's BLAKE2 supports neither keying nor the
    tree parameters -- it offers only the plain `blake2b512`/`blake2s256`
    digests.  Routing them to OpenSSL would silently lose `key=`.
  * **`pbkdf2_hmac` and `scrypt` exist only when `_hashlib` does.**  CPython
    imports them from it and lets the ImportError through, and so does this: a
    caller must not find a slower Python substitute under a name that promises
    a C one.

`algorithms_guaranteed` names what must exist and `algorithms_available` what
does, which is why callers are supposed to consult the second.  Here it is the
first plus whatever OpenSSL adds -- `blake2b512` and `blake2s256` on this
build.
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

algorithms_guaranteed = frozenset((
    "md5", "sha1", "sha224", "sha256", "sha384", "sha512",
    "blake2b", "blake2s",
    "sha3_224", "sha3_256", "sha3_384", "sha3_512",
    "shake_128", "shake_256",
))
algorithms_available = set(algorithms_guaranteed)

# The pure-Python implementations, always reachable by name.  They are what
# `new()` falls back to, what blake2 always uses, and what
# `test.support.import_fresh_module('hashlib', blocked=['_hashlib'])` exercises
# on its own.
_BUILTIN = {
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
    "shake_128": _sha3.shake_128,
    "shake_256": _sha3.shake_256,
    "blake2b": _blake2.blake2b,
    "blake2s": _blake2.blake2s,
}

# CPython's own name: the two that never go to OpenSSL.
_BLOCK_OPENSSL = frozenset(("blake2b", "blake2s"))

try:
    import _hashlib
except ImportError:
    _hashlib = None
else:
    algorithms_available |= set(_hashlib.openssl_md_meth_names)

_CONSTRUCTORS = dict(_BUILTIN)
if _hashlib is not None:
    for _name in algorithms_guaranteed:
        if _name in _BLOCK_OPENSSL:
            continue
        _openssl = getattr(_hashlib, "openssl_" + _name, None)
        if _openssl is None:
            continue
        try:
            # CPython probes by CALLING it: the function can exist while the
            # digest is refused by a security policy, and the probe is the
            # only way to find out.
            _openssl(usedforsecurity=False)
        except (AttributeError, ValueError):
            continue
        _CONSTRUCTORS[_name] = _openssl
    del _name, _openssl

md5 = _CONSTRUCTORS["md5"]
sha1 = _CONSTRUCTORS["sha1"]
sha224 = _CONSTRUCTORS["sha224"]
sha256 = _CONSTRUCTORS["sha256"]
sha384 = _CONSTRUCTORS["sha384"]
sha512 = _CONSTRUCTORS["sha512"]
sha3_224 = _CONSTRUCTORS["sha3_224"]
sha3_256 = _CONSTRUCTORS["sha3_256"]
sha3_384 = _CONSTRUCTORS["sha3_384"]
sha3_512 = _CONSTRUCTORS["sha3_512"]
shake_128 = _CONSTRUCTORS["shake_128"]
shake_256 = _CONSTRUCTORS["shake_256"]
blake2b = _CONSTRUCTORS["blake2b"]
blake2s = _CONSTRUCTORS["blake2s"]

# CPython accepts the uppercase spellings through new() -- its
# __get_builtin_constructor matches {'SHA1', 'sha1'} and the rest -- so a
# caller passing an OpenSSL-style name still gets a digest.
for _lower in ("md5", "sha1", "sha224", "sha256", "sha384", "sha512"):
    _CONSTRUCTORS[_lower.upper()] = _CONSTRUCTORS[_lower]
del _lower

if _hashlib is not None:
    # These two have no honest pure-Python form at the iteration counts anyone
    # uses, so they appear only when the C module does -- exactly as in a
    # CPython built without OpenSSL.
    pbkdf2_hmac = _hashlib.pbkdf2_hmac
    scrypt = _hashlib.scrypt
    __all__ = __all__ + ("pbkdf2_hmac", "scrypt")


def new(name, data=b"", **kwargs):
    """new(name, data=b'', **kwargs) - a new hashing object by name."""
    if name in _BLOCK_OPENSSL:
        # blake2 takes keyword parameters OpenSSL cannot express, so it always
        # goes to the builtin -- kwargs and all.
        return _BUILTIN[name](data, **kwargs)
    try:
        ctor = _CONSTRUCTORS[name]
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
