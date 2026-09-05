# str.encode and bytes.decode reach the codec registry.
#
# The interpreter can do utf-8, ascii and latin-1 itself, and used to raise
# LookupError for every other name -- so a codec the registry would have found
# was refused before anyone asked it, and so were the three error handlers
# that are not strict, ignore or replace.  Both now fall through to
# `_codecs.encode` / `_codecs.decode`, where the registry, the cache, the
# search functions and the handlers live.
#
# The strict encode failure goes the same way, which is how the exception
# gained its five fields: an error handler is handed the exception and reads
# the span it has to replace out of it, so `encoding`, `object`, `start`,
# `end` and `reason` are the whole point of it.

import _codecs


def check(label, fn):
    try:
        print(label.ljust(24), fn())
    except Exception as exc:
        print(label.ljust(24), type(exc).__name__ + ":", exc)


print("=== round trips ===")
for enc in ("utf-8", "ascii", "latin-1", "utf-16", "utf-16-le", "utf-16-be",
            "utf-32", "utf-32-le", "utf-32-be", "utf-8-sig",
            "UTF_8", "Latin1", "u8", "iso-8859-1"):
    text = "hello" if enc == "ascii" else "héllo wörld"
    encoded = text.encode(enc)
    print(enc.ljust(12), repr(encoded)[:46].ljust(46), encoded.decode(enc) == text)

print("=== astral, surrogate pairs ===")
for enc in ("utf-8", "utf-16", "utf-16-be", "utf-32", "utf-32-be"):
    text = "a\U0001D11Eb"
    print(enc.ljust(12), text.encode(enc).decode(enc) == text)

print("=== the error handlers ===")
for handler in ("strict", "ignore", "replace", "backslashreplace",
                "xmlcharrefreplace"):
    check("ascii " + handler, lambda h=handler: "aé€b".encode("ascii", h))
for handler in ("strict", "ignore", "replace", "backslashreplace"):
    check("latin-1 " + handler, lambda h=handler: "aé€b".encode("latin-1", h))
for handler in ("strict", "ignore", "replace", "backslashreplace"):
    check("decode " + handler, lambda h=handler: b"a\xff\xfeb".decode("utf-8", h))

print("=== the five fields ===")
try:
    "aé€b".encode("ascii")
except UnicodeEncodeError as exc:
    print("encode", exc.encoding, repr(exc.object), exc.start, exc.end,
          exc.reason)
try:
    b"a\xffb".decode("utf-8")
except UnicodeDecodeError as exc:
    print("decode", exc.encoding, exc.object, exc.start, exc.end, exc.reason)
try:
    "é".encode("latin-1")
except UnicodeEncodeError as exc:
    print("latin-1 encodes it   ", "unreachable")
except Exception as exc:
    print("latin-1 encodes it   ", type(exc).__name__)
else:
    print("latin-1 encodes it   ", "yes")

print("=== a handler of one's own ===")


def shout(exc):
    return ("[" + exc.object[exc.start:exc.end].upper() + "]", exc.end)


_codecs.register_error("shout", shout)
check("registered handler", lambda: "aéb".encode("ascii", "shout"))
check("unknown handler", lambda: "aéb".encode("ascii", "nosuch"))
check("unknown, clean string", lambda: "abc".encode("ascii", "nosuch"))
check("unknown decode handler", lambda: b"a\xffb".decode("utf-8", "nosuch"))
check("unknown, clean bytes", lambda: b"abc".decode("utf-8", "nosuch"))

print("=== a codec of one's own ===")


class ShoutInfo(tuple):
    def __new__(cls, encode, decode):
        self = tuple.__new__(cls, (encode, decode, None, None))
        self.encode = encode
        self.decode = decode
        return self


def shoutcase_encode(s, errors=None):
    out = []
    for ch in s:
        o = ord(ch)
        if 97 <= o <= 122:
            o = (o - 97 + 13) % 26 + 97
        out.append(o)
    return (bytes(out), len(s))


def shoutcase_decode(data, errors=None):
    return (shoutcase_encode(bytes(data).decode("ascii"))[0].decode("ascii"),
            len(data))


def search(name):
    if name == "shoutcase":
        return ShoutInfo(rot13_encode, shoutcase_decode)
    return None


_codecs.register(search)
# str.encode refuses a codec that is not a text encoding, which is CPython's
# rule and not this bridge's business; _codecs.encode takes any of them.
check("registered codec", lambda: _codecs.encode("hello", "shout-case"))
check("and back", lambda: _codecs.decode(_codecs.encode("hello", "shout-case"),
                                         "shout-case"))

print("=== the failures ===")
check("no such codec", lambda: "x".encode("definitely-not-a-codec"))
check("name as written", lambda: b"x".decode("Not-A-Codec"))
check("encoding not a str", lambda: "x".encode(5))
check("errors not a str", lambda: "x".encode("utf-8", 5))
print("done")
