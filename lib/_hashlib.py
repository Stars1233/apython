"""_hashlib - OpenSSL's digests, HMAC, PBKDF2 and scrypt.

The public half of the `_hashlibcore`/`_hashlib` split, the same one
`_zlibcore`/`zlib` and `_iocore`/`_io` use.  `src/modules/hashlib.asm` does
what is genuinely C -- the EVP_MD_CTX and HMAC_CTX calls and the handle table
-- and everything here is the surface CPython's `hashlib.py` and `hmac.py` are
written against: the HASH, HASHXOF and HMAC objects, the sixteen
`openssl_<name>` constructors, the keyword arguments and every default.

**Merely importing this module changes hashlib's behaviour for the whole
process.**  `hashlib.py:170-178` does `import _hashlib` and, if it succeeds,
rebinds `new` to `__hash_new` and `__get_hash` to `__get_openssl_constructor`.
So every digest except blake2 starts coming from OpenSSL instead of from
`lib/_sha2.py` and friends.  That is the intent -- it is three or four orders
of magnitude faster, and it is the only way to have `pbkdf2_hmac` and `scrypt`
at all -- but it is also why this module must be complete before it exists:
`hashlib` probes each name by CALLING `openssl_<name>(usedforsecurity=False)`
and only falls back to the builtin on AttributeError or ValueError.
"""

import _hashlibcore as _core

__all__ = ("HASH", "HASHXOF", "HMAC", "UnsupportedDigestmodError",
           "new", "openssl_md_meth_names", "get_fips_mode",
           "pbkdf2_hmac", "scrypt", "hmac_new", "hmac_digest",
           "compare_digest")


class _HashType(type):
    """The hash types are C types in CPython: immutable, and not instantiable.

    `test_hashlib.test_readonly_types` sets an attribute on `type(h)` for
    every constructor it knows and requires a TypeError; a plain Python class
    is mutable, so the refusal comes from here.  Instances are unaffected --
    it is the TYPE that is frozen.  lib/_sha3.py and the other three carry the
    same metaclass for the same test.
    """

    def __setattr__(cls, name, value):
        raise TypeError("cannot set %r attribute of immutable type %r"
                        % (name, cls.__name__))

    def __delattr__(cls, name):
        raise TypeError("cannot delete %r attribute of immutable type %r"
                        % (name, cls.__name__))


class UnsupportedDigestmodError(ValueError):
    """Raised for a digestmod this module cannot serve.

    `hmac.py` catches this by name, at lines 61 and 199, to fall back to its
    own Python implementation -- so it is the polite way to decline rather
    than an error a caller has to handle.
    """


class HASH(metaclass=_HashType):
    """A digest object: update, digest, hexdigest, copy, and four attributes.

    The four attributes are read ONCE, at construction, out of one
    `_core.info()` call.  Asking the core per attribute access would cross the
    boundary for something that cannot change.

    Not directly instantiable: CPython gives these types
    Py_TPFLAGS_DISALLOW_INSTANTIATION, because an instance with no EVP_MD_CTX
    behind it has nothing to do.  `_wrap` is the way in, and it is what
    `new()` and `copy()` use.
    """

    __slots__ = ("_h", "name", "digest_size", "block_size", "_xof")

    def __new__(cls, *args, **kwargs):
        raise TypeError("cannot create '%s.%s' instances"
                        % (cls.__module__, cls.__name__))

    @classmethod
    def _wrap(cls, handle):
        self = object.__new__(cls)
        self._h = handle
        name, digest_size, block_size, xof = _core.info(handle)
        self.name = name
        self.digest_size = digest_size
        self.block_size = block_size
        self._xof = xof
        return self

    def update(self, data):
        _core.update(self._h, _bytes_arg(data))
        return None

    def digest(self):
        return _core.digest(self._h)

    def hexdigest(self):
        return _core.digest(self._h).hex()

    def copy(self):
        return type(self)._wrap(_core.copy(self._h))

    def __repr__(self):
        return "<%s _hashlib.%s object @ 0x%x>" % (
            self.name, type(self).__name__, id(self))

    def __del__(self):
        # A __del__ runs on a path the collector chooses, and the handle may
        # already be gone if this object was reached through a cycle whose
        # tp_clear ran first.  The core tolerates a stale index, and
        # lib/zlib.py's __del__ is defensive here for the same reason.
        try:
            _core.free(self._h)
        except Exception:
            pass


class HASHXOF(HASH):
    """A shake object.

    `digest_size` is 0 and the LENGTH is a required positional argument --
    that is the whole difference between an XOF and a hash.  CPython's
    _hashlib reports 0 here too, even though the underlying EVP_MD has a
    nominal size, so the attribute is overwritten rather than passed through.
    """

    __slots__ = ()

    @classmethod
    def _wrap(cls, handle):
        self = HASH._wrap.__func__(cls, handle)
        self.digest_size = 0
        return self

    def digest(self, length):
        return _core.xof_digest(self._h, _xof_length(length))

    def hexdigest(self, length):
        return _core.xof_digest(self._h, _xof_length(length)).hex()


class HMAC(metaclass=_HashType):
    """An HMAC object, with the same surface as HASH.

    A separate class rather than a subclass: every call goes to a different
    core entry point, because an HMAC_CTX and an EVP_MD_CTX are different
    things and the core refuses one where the other is wanted.
    """

    __slots__ = ("_h", "name", "digest_size", "block_size")

    def __new__(cls, *args, **kwargs):
        raise TypeError("cannot create '%s.%s' instances"
                        % (cls.__module__, cls.__name__))

    @classmethod
    def _wrap(cls, handle):
        self = object.__new__(cls)
        self._h = handle
        name, digest_size, block_size, _ = _core.hmac_info(handle)
        # CPython reports the HMAC's name as "hmac-sha256", not "sha256".
        self.name = "hmac-" + name
        self.digest_size = digest_size
        self.block_size = block_size
        return self

    def update(self, data):
        _core.hmac_update(self._h, _bytes_arg(data))
        return None

    def digest(self):
        return _core.hmac_digest(self._h)

    def hexdigest(self):
        return _core.hmac_digest(self._h).hex()

    def copy(self):
        return HMAC._wrap(_core.hmac_copy(self._h))

    def __repr__(self):
        return "<%s _hashlib.HMAC object @ 0x%x>" % (self.name, id(self))

    def __del__(self):
        try:
            _core.hmac_free(self._h)
        except Exception:
            pass


def _bytes_arg(data):
    """The core takes bytes or bytearray; everything else is converted here.

    NOT `bytes(data)`: `bytes(100)` is a hundred zero bytes, so an int passed
    where a digest was meant became a legitimate-looking buffer instead of an
    error, and `hmac.compare_digest(100, 200)` compared two 100- and 200-byte
    runs of zeros.  memoryview refuses an int, a list and a None, which is the
    same set CPython refuses and for the same reason.

    A str is refused rather than encoded, with CPython's wording -- guessing
    an encoding is worse than the error.
    """
    if isinstance(data, str):
        raise TypeError("Strings must be encoded before hashing")
    if isinstance(data, (bytes, bytearray)):
        return data
    try:
        return memoryview(data).tobytes()
    except TypeError:
        raise TypeError("object supporting the buffer API required") from None


def _index(value):
    """CPython's wording for a non-integer where an integer was wanted.

    `value.__index__()` on a None answers AttributeError, and test_hashlib
    requires ValueError, OverflowError or TypeError for every one of scrypt's
    parameters -- so the conversion has to name the problem itself.
    """
    try:
        return value.__index__()
    except AttributeError:
        raise TypeError("'%s' object cannot be interpreted as an integer"
                        % type(value).__name__) from None


def _xof_length(length):
    """CPython's three refusals for a shake output length."""
    length = _index(length)
    if length < 0:
        raise ValueError("value must be positive")
    if length >= 1 << 64:
        raise OverflowError("Python int too large to convert to C unsigned long")
    if length >= 1 << 29:
        raise ValueError("length is too large")
    return length


def new(name, data=b"", *, usedforsecurity=True):
    """new(name, data=b'', *, usedforsecurity=True) -> a digest object."""
    if not isinstance(name, str):
        raise TypeError("name must be a string")
    try:
        handle = _core.new(name, usedforsecurity)
    except ValueError:
        # CPython raises UnsupportedDigestmodError here, carrying OpenSSL's
        # own error-queue text ("[digital envelope routines] unsupported").
        # The TYPE is what hashlib and hmac branch on; the text is not worth
        # reproducing.
        raise UnsupportedDigestmodError(
            "unsupported hash type %s" % (name,)) from None
    _, _, _, xof = _core.info(handle)
    obj = HASHXOF._wrap(handle) if xof else HASH._wrap(handle)
    # Unconditional: a FALSY non-buffer -- None or 0 -- has to be refused too,
    # and `if data:` skipped the check entirely for both, so
    # `hashlib.sha256(None)` quietly answered the empty digest.
    obj.update(data)
    return obj


openssl_md_meth_names = frozenset(_core.md_names())


def _make_constructor(name):
    """One `openssl_<name>`.

    `hashlib.__get_openssl_constructor` PROBES each of these by calling it
    with `usedforsecurity=False` and treats AttributeError or ValueError as
    "use the builtin instead" -- so a name this module cannot serve has to
    raise rather than be quietly absent.  Only the names the linked provider
    actually answered to are bound at all, which is what openssl_md_meth_names
    already reflects.
    """

    def constructor(data=b"", *, usedforsecurity=True):
        return new(name, data, usedforsecurity=usedforsecurity)

    constructor.__name__ = "openssl_" + name
    constructor.__qualname__ = constructor.__name__
    return constructor


# blake2b and blake2s are listed in openssl_md_meth_names but get NO
# openssl_ constructor, which is exactly what CPython does and is not an
# oversight there.  hashlib routes both to the builtin unconditionally, so an
# OpenSSL constructor for them could only ever be reached by a caller poking
# at this module directly -- and it would accept none of the keyword
# parameters that are the whole reason blake2 is blocked.
# `test_hashlib` reads this the same way: it adds
# `getattr(_hashlib, 'openssl_' + name, None)` to the constructors it tests
# against the full blake2 keyword surface, so publishing one makes four tests
# fail with "unexpected keyword argument".
_NO_CONSTRUCTOR = frozenset(("blake2b", "blake2s"))

for _name in sorted(openssl_md_meth_names):
    if _name in _NO_CONSTRUCTOR:
        continue
    globals()["openssl_" + _name] = _make_constructor(_name)
    __all__ = __all__ + ("openssl_" + _name,)
del _name


def get_fips_mode():
    """0 -- this module installs no providers, so FIPS mode is never on."""
    return 0


def pbkdf2_hmac(hash_name, password, salt, iterations, dklen=None):
    """PBKDF2-HMAC, with OpenSSL doing the iteration.

    dklen defaults to the digest size, which is what CPython does by asking
    the EVP_MD rather than by tabulating.
    """
    if not isinstance(hash_name, str):
        raise TypeError("hash_name must be a string")
    if dklen is None:
        # One throwaway digest object to learn the size.  It is the honest
        # way to get it: the answer belongs to the linked provider.
        dklen = new(hash_name).digest_size
    else:
        dklen = _index(dklen)
        if dklen < 1:
            raise ValueError("key length must be greater than 0.")
    iterations = _index(iterations)
    if iterations < 1:
        raise ValueError("iteration value must be greater than 0.")
    return _core.pbkdf2(hash_name, _bytes_arg(password), _bytes_arg(salt),
                        iterations, dklen)


def scrypt(password, *, salt=None, n=None, r=None, p=None, maxmem=0, dklen=64):
    """scrypt, RFC 7914.  Every parameter but the password is keyword-only,
    as CPython's is.
    """
    if salt is None or n is None or r is None or p is None:
        raise TypeError("salt, n, r and p are required")
    n = _index(n)
    r = _index(r)
    p = _index(p)
    dklen = _index(dklen)
    maxmem = _index(maxmem)
    if maxmem < 0:
        raise ValueError("maxmem must not be negative.")
    if n < 2 or (n & (n - 1)):
        raise ValueError("n must be a power of 2.")
    if r < 1:
        raise ValueError("r must be a positive integer.")
    if p < 1:
        raise ValueError("p must be a positive integer.")
    if dklen < 1:
        raise ValueError("dklen must be greater than 0.")
    return _core.scrypt(_bytes_arg(password), _bytes_arg(salt),
                        n, r, p, maxmem, dklen)


def _digestmod_name(digestmod):
    """The name to hand the core, or a refusal hmac.py knows how to absorb.

    `hmac.py:15` takes `_functype = type(_hashopenssl.openssl_sha256)` and
    then tests `isinstance(digestmod, (str, _functype))`.  In CPython that
    type is `builtin_function_or_method`; here it is an ordinary `function`,
    so a caller's `digestmod=lambda: ...` ALSO passes that test and arrives
    here.  Answering UnsupportedDigestmodError is what sends hmac.py down its
    own Python path, which is the correct outcome -- see DIVERGENCES.md.
    """
    if isinstance(digestmod, str):
        return digestmod
    name = getattr(digestmod, "__name__", None)
    if isinstance(name, str) and name.startswith("openssl_"):
        return name[len("openssl_"):]
    raise UnsupportedDigestmodError(
        "unsupported hash type %r" % (digestmod,))


def hmac_new(key, msg=b"", digestmod=None):
    """A new HMAC object.  digestmod is required, as CPython's is."""
    if digestmod is None:
        raise UnsupportedDigestmodError("digestmod is required")
    name = _digestmod_name(digestmod)
    try:
        obj = HMAC._wrap(_core.hmac_new(_bytes_arg(key), name))
    except ValueError:
        raise UnsupportedDigestmodError(
            "unsupported hash type %s" % (name,)) from None
    # None means "no message" HERE, and only here: `hmac.py` passes its own
    # msg=None default straight through, and CPython's hmac_new accepts it.
    # `new()` refuses a None in the same position, because there it is a
    # caller's mistake rather than an absent argument.  Measured against
    # CPython both ways; 0 and [1] are refused in both.
    if msg is not None:
        obj.update(msg)
    return obj


def hmac_digest(key, msg, digest):
    """The one-shot form, which is what hmac.digest() uses."""
    return hmac_new(key, msg, digest).digest()


def _is_buffer(value):
    if isinstance(value, (bytes, bytearray, memoryview)):
        return True
    try:
        memoryview(value)
    except TypeError:
        return False
    return True


def _compare_type_error(a, b):
    """CPython's two wordings, and the rule that picks between them.

    Measured, because the rule is not the obvious one: if EITHER side supports
    the buffer protocol, the error names the side that does not --
    `compare_digest(1, b"")` is "a bytes-like object is required, not 'int'".
    Only when NEITHER does does it name both, with its own typo intact:
    `compare_digest(1, 2)` is "unsupported operand types(s) or combination of
    types: 'int' and 'int'".
    """
    if _is_buffer(a) or _is_buffer(b):
        bad = a if not _is_buffer(a) else b
        return TypeError("a bytes-like object is required, not %r"
                         % type(bad).__name__)
    return TypeError("unsupported operand types(s) or combination of types: "
                     "%r and %r" % (type(a).__name__, type(b).__name__))


def compare_digest(a, b):
    """A constant-time comparison.

    CRYPTO_memcmp in the core, not `a == b`: the whole point is that the time
    taken must not depend on where the first differing byte is.  A str is
    refused unless both sides are ASCII, which is CPython's rule.
    """
    if isinstance(a, str) and isinstance(b, str):
        try:
            a = a.encode("ascii")
            b = b.encode("ascii")
        except UnicodeEncodeError:
            raise TypeError("comparing strings with non-ASCII characters is "
                            "not supported") from None
    else:
        # Both sides are checked before either is converted, so the message
        # can name whichever one is wrong.  An int is not a digest, and
        # `bytes(100)` would have made it look like one.
        if not (_is_buffer(a) and _is_buffer(b)):
            raise _compare_type_error(a, b)
    return _core.compare_digest(_bytes_arg(a), _bytes_arg(b))
