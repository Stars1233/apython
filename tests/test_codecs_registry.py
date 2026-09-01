"""_codecs: the pieces CPython implements in C, and where that shows.

Two behaviours here are not about codecs at all, they are about what a C
function is:

  * A C builtin assigned to a class attribute does not bind.  Every module in
    encodings/ does exactly that -- `_buffer_decode = codecs.utf_8_decode`,
    `encode = codecs.utf_8_encode` -- so a Python `def` in its place became a
    method and every call arrived with a self it did not want.
  * utf_8_decode with final=False must hold back an incomplete trailing
    sequence and report how much it consumed.  Decoding the whole chunk
    raises on a character split across two reads, which is the normal case
    for an incremental decoder.
"""

import _codecs


def check(label, fn):
    try:
        got = fn()
    except Exception as exc:
        got = type(exc).__name__ + ": " + str(exc)
    print(label.ljust(30), repr(got))


# --- stateless round trips ---
check("utf_8_encode", lambda: _codecs.utf_8_encode("héllo"))
check("utf_8_decode", lambda: _codecs.utf_8_decode(b"h\xc3\xa9llo"))
check("ascii_encode", lambda: _codecs.ascii_encode("abc"))
check("ascii_decode", lambda: _codecs.ascii_decode(b"abc"))
check("latin_1_encode", lambda: _codecs.latin_1_encode("ab\xff"))
check("latin_1_decode", lambda: _codecs.latin_1_decode(b"ab\xff"))

# --- final=False holds back a split character ---
check("split 2-byte, not final", lambda: _codecs.utf_8_decode(b"h\xc3", "strict", False))
check("split 2-byte, resumed", lambda: _codecs.utf_8_decode(b"\xc3\xa9", "strict", False))
check("split 3-byte, 1 of 3", lambda: _codecs.utf_8_decode(b"a\xe2", "strict", False))
check("split 3-byte, 2 of 3", lambda: _codecs.utf_8_decode(b"a\xe2\x82", "strict", False))
check("whole 3-byte", lambda: _codecs.utf_8_decode(b"a\xe2\x82\xac", "strict", False))
check("split 4-byte, 3 of 4", lambda: _codecs.utf_8_decode(b"\xf0\x9f\x92", "strict", False))
check("whole 4-byte", lambda: _codecs.utf_8_decode(b"\xf0\x9f\x92\xa9", "strict", False))
check("ascii tail", lambda: _codecs.utf_8_decode(b"abc", "strict", False))
check("empty", lambda: _codecs.utf_8_decode(b"", "strict", False))


# --- a codec function assigned to a class attribute does not bind ---
class Holder:
    enc = _codecs.utf_8_encode
    dec = _codecs.utf_8_decode
    adec = _codecs.ascii_decode


check("attr encode via instance", lambda: Holder().enc("ab"))
check("attr decode via instance", lambda: Holder().dec(b"ab"))
check("attr decode via class", lambda: Holder.dec(b"ab"))
check("attr ascii via instance", lambda: Holder().adec(b"ab"))
check("still callable directly", lambda: _codecs.utf_8_encode("ab"))


# Subclassing the holder must not change any of it -- encodings/*.py reaches
# these through StreamReader/StreamWriter subclasses.
class Sub(Holder):
    pass


check("through a subclass", lambda: Sub().dec(b"ab"))

# --- the registry ---
check("register a non-callable", lambda: _codecs.register(42))


def _search(name):
    if name == "rot_bogus":
        return "sentinel"
    return None


_codecs.register(_search)
check("lookup a registered name", lambda: _codecs.lookup("rot bogus"))
check("lookup normalises", lambda: _codecs.lookup("ROT-BOGUS"))
check("lookup an unknown name", lambda: _codecs.lookup("no_such_encoding_xyz"))
check("lookup a non-str", lambda: _codecs.lookup(42))
_codecs.unregister(_search)
check("after unregister", lambda: _codecs.lookup("rot_bogus"))
