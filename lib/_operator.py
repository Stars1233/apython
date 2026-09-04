"""_operator - the C accelerator behind operator.py.

operator.py falls back to its own pure-Python definitions when this is
missing, so only the one name that has NO fallback needs to be here:
`_compare_digest`, which hmac imports directly and which is what kept hmac,
and everything behind it, from loading.
"""


def _compare_digest(a, b):
    """A comparison whose running time does not depend on where the first
    difference is.

    CPython's is C, and the point of it is the same there: an attacker timing
    a digest comparison learns nothing about the prefix that matched.  Both
    arguments must be the same kind -- str against bytes is a TypeError, and
    a str is accepted only if it is ASCII, exactly as CPython has it.
    """
    if isinstance(a, str) and isinstance(b, str):
        try:
            a = a.encode("ascii")
            b = b.encode("ascii")
        except UnicodeEncodeError:
            raise TypeError("comparing strings with non-ASCII characters is "
                            "not supported") from None
    elif isinstance(a, (bytes, bytearray)) and \
            isinstance(b, (bytes, bytearray)):
        pass
    else:
        # CPython has two wordings here.  When ONE side is buffer-like the
        # complaint is about the other, in the buffer protocol's words; when
        # neither is, it names both types.
        if isinstance(a, (bytes, bytearray)):
            raise TypeError("a bytes-like object is required, not '%s'"
                            % (type(b).__name__,))
        if isinstance(b, (bytes, bytearray)):
            raise TypeError("a bytes-like object is required, not '%s'"
                            % (type(a).__name__,))
        raise TypeError("unsupported operand types(s) or combination of "
                        "types: '%s' and '%s'"
                        % (type(a).__name__, type(b).__name__))

    # The lengths are compared without short-circuiting too: the loop runs
    # over the longer of the two either way.
    result = len(a) ^ len(b)
    if len(a) == 0 or len(b) == 0:
        return result == 0
    for i in range(max(len(a), len(b))):
        result |= a[i % len(a)] ^ b[i % len(b)]
    return result == 0
