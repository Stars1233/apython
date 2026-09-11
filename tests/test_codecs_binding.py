# _codecs' entry points must not bind when stored in a class body.
#
# CPython's encodings/utf_16_le.py does
#
#     class StreamWriter(codecs.StreamWriter):
#         encode = codecs.utf_16_le_encode
#     class StreamReader(codecs.StreamReader):
#         decode = codecs.utf_16_le_decode
#
# and that works there because CPython's _codecs is C: a
# builtin_function_or_method has no __get__ and does not bind.  Ours is Python
# (lib/_codecs.py), so `self` arrived as the data argument and every stream
# read died on "cannot convert 'StreamReader' object to bytes" -- 5,282 error
# lines across test_codecs and the six test_codecencodings_* modules, which is
# the largest single cluster in the whole sweep.
#
# A module-level staticmethod is the fix: callable directly since 3.10, not a
# descriptor in a class body, and it forwards __name__, __module__,
# __qualname__ and __doc__, so nothing that introspects the codec notices.
import _codecs

# Called directly, as codecs.py's CodecInfo does.
print(_codecs.utf_8_encode("héllo"))
print(_codecs.utf_8_decode(b"h\xc3\xa9llo"))
print(_codecs.utf_16_le_decode(b"a\x00b\x00"))
print(_codecs.latin_1_encode("ab"))
print(_codecs.ascii_decode(b"ab"))

# Stored in a class body, as encodings/*.py does.  The receiver must NOT be
# passed: these are the exact three shapes CPython's encodings modules use.
class Reader:
    decode = _codecs.utf_16_le_decode
    _buffer_decode = _codecs.utf_16_le_decode


class Writer:
    encode = _codecs.utf_16_le_encode


print(Reader().decode(b"a\x00"))
print(Reader()._buffer_decode(b"a\x00", "strict", True))
print(Writer().encode("a"))

# Reached through the class as well as the instance.
print(Reader.decode(b"b\x00"))

# The introspection the stdlib does on them still answers.
for f in (_codecs.utf_8_encode, _codecs.utf_16_le_decode):
    print(f.__name__, f.__module__, callable(f))

# The two names encodings/utf_16.py and utf_32.py reach for and that were
# simply absent -- the incremental decoders call them on every chunk.
f = _codecs.utf_16_ex_decode
for data, bo in ((b"\xff\xfea\x00", 0), (b"\xfe\xff\x00a", 0), (b"a\x00", 0),
                 (b"\x00a", 1), (b"a\x00", -1), (b"\xff\xfea\x00", -1),
                 (b"", 0)):
    print("%-18r bo=%-3d -> %r" % (data, bo, f(data, None, bo, True)))

g = _codecs.utf_32_ex_decode
for data, bo in ((b"\xff\xfe\x00\x00a\x00\x00\x00", 0),
                 (b"\x00\x00\xfe\xff\x00\x00\x00a", 0),
                 (b"a\x00\x00\x00", 0),
                 (b"\x00\x00\x00a", 1)):
    print("%-30r bo=%-3d -> %r" % (data, bo, g(data, None, bo, True)))

# And the whole point, in the shape codecs.py's StreamReader actually uses:
# a class that inherits a read loop and supplies `decode` from a class body.
# codecs itself is CPython's file and is not in this tree's lib/, so the real
# codecs.getreader round trip is exercised by the sweep rather than here.
class BaseReader:
    def __init__(self, stream):
        self.stream = stream

    def read(self):
        data = self.stream.read()
        return self.decode(data, "strict")[0]


class Utf16Reader(BaseReader):
    decode = _codecs.utf_16_le_decode


class Utf8Reader(BaseReader):
    decode = _codecs.utf_8_decode


import io

print(repr(Utf16Reader(io.BytesIO("hello".encode("utf-16-le"))).read()))
print(repr(Utf8Reader(io.BytesIO(b"h\xc3\xa9llo")).read()))
