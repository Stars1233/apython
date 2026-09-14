# A codec error handler checks what it was handed.
#
# Each of the built-in handlers reads .object, .start and .end off its
# argument.  Handed something that is not really a UnicodeError they raised
# AttributeError -- "'FakeUnicodeError' object has no attribute 'object'" --
# where CPython refuses with a TypeError naming the type.  That is ninety of
# CPython's test_codeccallbacks between the two shapes it tries.
#
# The check has to read the REAL type, not isinstance: the thing
# test_codeccallbacks passes is a class whose __class__ SAYS
# UnicodeDecodeError, and isinstance honours that.  CPython uses
# PyObject_TypeCheck, which does not.
#
# Which handler accepts which exception is not uniform, and the differences
# are CPython's: xmlcharrefreplace and namereplace are encode-only, the
# surrogate pair refuse a translate error, and ignore, replace and
# backslashreplace take all three.
import _codecs

HANDLERS = ["strict", "ignore", "replace", "xmlcharrefreplace",
            "backslashreplace", "namereplace", "surrogateescape",
            "surrogatepass"]

# --- a real exception of each kind, to every handler --------------------
reals = [
    ("encode", UnicodeEncodeError("utf-8", "ab", 0, 1, "r")),
    ("decode", UnicodeDecodeError("utf-8", b"ab", 0, 1, "r")),
    ("translate", UnicodeTranslateError("ab", 0, 1, "r")),
]
for name in HANDLERS:
    h = _codecs.lookup_error(name)
    for kind, e in reals:
        try:
            print("%-18s %-10s -> %r" % (name, kind, h(e)))
        except Exception as ex:
            # The TYPE of what a handler re-raises, not its message: str() of
            # a UnicodeTranslateError is still the args tuple here, where
            # CPython says "can't translate character '\x61' in position 0".
            # unicode_error_str is written for the five-argument encode and
            # decode shapes and answers nothing for the four-argument
            # translate one, so exc_str falls back to the repr.
            print("%-18s %-10s -> %s" % (name, kind, type(ex).__name__))

# --- something that only CLAIMS to be one ------------------------------
for cls in (UnicodeEncodeError, UnicodeDecodeError, UnicodeTranslateError):
    class FakeStr(str):
        __class__ = cls

    class FakeExc(Exception):
        __class__ = cls

    for name in HANDLERS:
        h = _codecs.lookup_error(name)
        for fake in (FakeStr(), FakeExc()):
            try:
                h(fake)
                out = "NO ERROR"
            except TypeError as ex:
                out = "TypeError: %s" % ex
            except Exception as ex:
                out = "%s" % type(ex).__name__
            print("%-22s %-18s %-8s %s"
                  % (cls.__name__, name, type(fake).__name__, out))

# --- and an int, which has none of the attributes at all ----------------
for name in HANDLERS:
    try:
        _codecs.lookup_error(name)(42)
        print("%-18s int -> NO ERROR" % name)
    except TypeError as ex:
        print("%-18s int -> TypeError: %s" % (name, ex))

# --- the handlers still work through a real encode/decode ---------------
print("ignore:", "aሴb".encode("ascii", "ignore"))
print("replace enc:", "aሴb".encode("ascii", "replace"))
print("replace dec:", b"a\xffb".decode("ascii", "replace"))
print("xmlcharrefreplace:", "aሴb".encode("ascii", "xmlcharrefreplace"))
print("backslashreplace enc:", "aሴb".encode("ascii", "backslashreplace"))
print("backslashreplace dec:", b"a\xffb".decode("ascii", "backslashreplace"))
print("namereplace:", "aሴb".encode("ascii", "namereplace"))
# The decoded string holds a lone surrogate, so it is not printed: writing
# one to stdout is a UnicodeEncodeError in CPython and is tolerated here,
# which is a different divergence.  What matters is that the two halves are
# exact inverses.
_esc = b"a\xffb".decode("utf-8", "surrogateescape")
print("surrogateescape:", len(_esc), [ord(c) for c in _esc])
print("round trip:", _esc.encode("utf-8", "surrogateescape"))

# --- partial input, which is the other half of a handler's contract -----
# A decoder is handed the bytes that arrived, not a whole character: it
# consumes the complete units and reports how many bytes it used, so the tail
# can be carried into the next call.  `final` is that distinction, and the
# UTF-16 and UTF-32 decoders here ACCEPTED the flag and passed it to nobody,
# so a trailing half-unit was always an error:
# _codecs.utf_16_le_decode(b"\x00", "strict", False) raised.
for enc, dec in (("utf-16-le", _codecs.utf_16_le_decode),
                 ("utf-16-be", _codecs.utf_16_be_decode),
                 ("utf-32-le", _codecs.utf_32_le_decode),
                 ("utf-32-be", _codecs.utf_32_be_decode),
                 ("utf-8", _codecs.utf_8_decode)):
    data = "héllo wörld 日本".encode(enc)
    for step in (1, 2, 3, 5, 7):
        out = ""
        buf = b""
        for i in range(0, len(data), step):
            buf += data[i:i + step]
            text, used = dec(buf, "strict", False)
            out += text
            buf = buf[used:]
        text, used = dec(buf, "strict", True)
        out += text
        assert out == "héllo wörld 日本", (enc, step, out)
    print("%-10s incremental over every chunk size" % enc)

# The tail is still an error once there is no more input.
try:
    _codecs.utf_16_le_decode(b"\x00", "strict", True)
except UnicodeDecodeError as e:
    print("truncated at final:", e.reason)
print("truncated with replace:", _codecs.utf_16_le_decode(b"\x00", "replace", True))
print("partial is not:", _codecs.utf_16_le_decode(b"\x00", "strict", False))
print("survived")
